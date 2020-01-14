{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Middleware.Authentication (
    validateDigitalSignature,
    UserReqAuth(..),
    UserReq(..),
    payloadIORef
) where

import Types.User
import Crypto.Secp256k1
import Network.Wai
import Data.Validation
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import qualified Crypto.Hash.SHA256 as SHA256
import GHC.Generics
import Control.Monad.Trans.Class
import Web.Scotty.Trans
import Network.Wai
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Types.Status
import Blaze.ByteString.Builder (fromByteString)
import Exception.Handling
import Debug.Trace
import Data.Validation
import Control.Lens
import Control.Concurrent
import Data.IORef
-- import Data.Global
import System.IO.Unsafe (unsafePerformIO)


data UserReq payload = UserReq {
  auth :: UserReqAuth,
  payload :: payload
} deriving (Show, Eq, Generic)

-- data AuthenticationError = AuthenticationError {
--     exceptionType :: String,
--     exceptionCode :: Int,
--     exceptionMessage :: String
-- } deriving (Show, Eq, Generic)

-- instance ToJSON AuthenticationError
-- instance FromJSON AuthenticationError


-- | Build a successful JSON response
-- jsonResponse :: ToJSON a => a -> Response
-- jsonResponse
--   = responseBuilder status200 [(hContentType, "application/json")]
--   . fromEncoding . toEncoding
--   


-- Note: Since the introduction of the digital signature validating middleware,
--       we carry out the parsing of the request body and retrieval of its json content
--       in the middleware. Which means that once we reach the handler function (i.e `post "/registerUser" ... `) stage,
--       it is not possible to parse the request body anymore.
--       Thus, we need a way to pass the result of the parsing from the middleware to the handler function.
--       Unfortunately, I can't think of a cleaner way to do it than to use a global/top mutable variable.

-- declareIORef "payloadIORef" [t| Value |] [e| Null  |]
-- declareIORef "payloadIORef" [t| Char |] [e| 'x' |]
payloadIORef :: IORef Value
payloadIORef = unsafePerformIO (newIORef Null)

jsonResponse :: ToJSON a => a -> Response
jsonResponse
    = responseBuilder status200 [("Content-Type","application/json")]
    . fromEncoding . toEncoding

validateDigitalSignature :: Middleware
validateDigitalSignature app req respond = do -- IO ResponseReceived
    bodyChunks <- getReqBodyChunks req []
    let body = BSL.fromChunks bodyChunks
    let authDetails = getAuthDetails body
    let payload = getPayload body
    let payloadString = getPayloadString body
    case authDetails of
        Nothing -> respond $ jsonResponse $ AuthenticationException 1503 $ ["No public key or digital signature was provided in the request"]
        Just auth ->
            validation
                (\errors -> respond $ jsonResponse $ AuthenticationException 1503 $ Prelude.map show errors) -- ToDO: Writing error code 1502 can lead to mistakes on the part of devs
                (\sig -> do
                    case payload of
                        Nothing -> error "This should NOT happen"
                        Just pyld -> do
                            -- Note: This line writes the IORef created by a `declareIORef` (safe-globals library) at the top level.
                            _ <- writeIORef payloadIORef (Object pyld)
                            return ()
                    app req respond)
                (validateSignature "Public Key" "Digital Signature" auth payloadString)

    where

        getReqBodyChunks :: Request -> [BS8.ByteString] -> IO [BS8.ByteString]
        getReqBodyChunks req bodyAcc = do
            chunk <- getRequestBodyChunk req
            if BS8.null chunk then return bodyAcc
            else getReqBodyChunks req (chunk : bodyAcc)

        getAuthDetails :: BSL.ByteString -> Maybe UserReqAuth
        getAuthDetails body = do 
            result <- Data.Aeson.decode body :: Maybe Object
            authDetails <- flip parseMaybe result $ \obj -> do 
                auth <- obj .: "auth"
                public_key <- auth .: "public_key"
                digital_signature <- auth .: "digital_signature"
                return $ UserReqAuth { public_key = public_key, digital_signature = digital_signature }
            return authDetails

        -- getPayload :: BSL.ByteString -> Maybe String
        -- getPayload body = do
        --     result <- Data.Aeson.decode body :: Maybe Object
        --     payloadDetails <- flip parseMaybe result $ \obj -> do
        --         payload <- obj .: "payload" :: Parser Object
        --         return payload
        --     return $ BS8.unpack $ BSL.toStrict $ Data.Aeson.encode payloadDetails

        getPayload :: BSL.ByteString -> Maybe Object
        getPayload body = do
            result <- Data.Aeson.decode body :: Maybe Object
            flip parseMaybe result $ \obj -> do
                payload <- obj .: "payload" :: Parser Object
                return payload
            

        getPayloadString :: BSL.ByteString -> Maybe String
        getPayloadString body = do
            payload <- getPayload body
            return $ BS8.unpack $ BSL.toStrict $ Data.Aeson.encode payload

--- Public Key & Signature Validation ---

data UserReqAuth = UserReqAuth {
  public_key :: String,
  digital_signature :: String
} deriving (Show, Eq, Generic)


instance FromJSON UserReqAuth
instance ToJSON UserReqAuth

data DigitalSignatureValidationError = InvalidPublicKey FieldName 
                                       | InvalidSignature FieldName
                                       | CouldNotGetSignature FieldName
                                       | CouldNotGetMessage FieldName
                                       | InexistentPayload FieldName

instance Show DigitalSignatureValidationError where
    show (InvalidPublicKey fieldName) = "The '" ++ fieldName ++ "' is invalid"
    show (InvalidSignature fieldName) = "The '" ++ fieldName ++ "' is invalid for the message/payload"
    show (CouldNotGetSignature fieldName) = "Could not get the signature as found in '" ++ fieldName ++ "'"
    show (CouldNotGetMessage fieldName) = "Could not get the message as found in '" ++ fieldName ++ "'"
    show (InexistentPayload fieldName) = "No payload was found in the request"


validateSignature :: (Show payload) =>
                     FieldName 
                     -> FieldName 
                     -> UserReqAuth
                     -> Maybe payload 
                     -> Validation [DigitalSignatureValidationError] Sig
validateSignature pubKeyFieldName sigFieldName userReqAuth Nothing = _Failure # [InexistentPayload "payload"]
validateSignature pubKeyFieldName sigFieldName userReqAuth (Just payload) = 
    fromEither $ do
        let pkStr  = public_key userReqAuth
        let sigStr = digital_signature userReqAuth
        
        -- ToDO: Put this back
        -- let msgStr = show payload
        let msgStr = "abc"

        pk  <- getPublicKey "Public Key" pkStr
        sig <- getSig "Digital Signature" sigStr
        msg <- getMsg "Message" msgStr


        -- ToDO: Comment this out again!
        -- if (trace ("verifySig: " ++ (BS8.unpack $ B16.encode $ (exportSig sig)) ) (verifySig pk sig msg) ) then Right sig 
        --if (trace ("verifySig: " ++ show pk) (verifySig pk sig msg)) then Right sig
        if (verifySig pk sig msg) then Right sig
        else Left $ [InvalidSignature sigFieldName]

    where

        getPublicKey :: FieldName -> String -> Either [DigitalSignatureValidationError] PubKey
        getPublicKey fieldName pubKeyStr = 
            let mPubKey = importPubKey (fst $ B16.decode $ BS8.pack pubKeyStr)
            in case mPubKey of
                Just pk -> Right pk
                Nothing -> Left $ [InvalidPublicKey fieldName]

        getSig :: FieldName -> String -> Either [DigitalSignatureValidationError] Sig
        getSig fieldName sigStr = 
            let mbs = importSig (fst $ B16.decode $ BS8.pack sigStr)
            in case mbs of
              Nothing -> Left [(CouldNotGetSignature fieldName)]
              Just sig -> Right sig

        -- ToDO: Use this constructor?
        -- Msg32 $ toShort $ fst $ B16.decode "f5cbe7d88182a4b8e400f96b06128921864a18187d114c8ae8541b566c8ace00"
        --                                    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

        -- ToDO: How to generate a signature in hex format, using openssl?

        -- ToDO: Fix this!
        -- getMsg :: (FromJSON a, ToJSON a, Show a) => FieldName -> a -> Either [DigitalSignatureValidationError] Msg
        getMsg :: FieldName -> String -> Either [DigitalSignatureValidationError] Msg
        getMsg fieldName payload = -- Payload is a string representing the json object coming from the wire

            -- ToDO: Fix this!
            -- let payloadSha256 = SHA256.hash $ (BSL.toStrict . encode) payload -- First convert the JSON encoding of the message to sha256
            let payloadSha256 = SHA256.hash $ BS8.pack payload
            in case (msg payloadSha256) of
                Nothing -> Left [CouldNotGetMessage fieldName]
                Just m -> Right m

            -- let payloadSha256 = SHA256.hash $ BS8.pack payload
            -- in case (msg . fst . B16.decode . B16.encode) payloadSha256 of
            --     Nothing -> Left [CouldNotGetMessage fieldName]
            --     Just m -> Right m