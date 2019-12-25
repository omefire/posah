{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Types.User (
    UserRegistrationInfo(..),
    validateUserRegistrationInfo
) where

import Data.Aeson
import GHC.Generics
import Data.Validation
import Control.Lens
import Web.Scotty.Trans
import Exception.Handling
import Data.String.Utils
import Data.Char (isDigit)
import qualified Data.List as DL
import Crypto.Secp256k1 --(verifySig, importPubKey, PubKey(..), Sig(..), Msg(..), importSig)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Base64 as B64
import Data.String (fromString)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Types
import Crypto.Secp256k1
import Data.String.Conversions   (ConvertibleStrings, cs)
import Crypto.Secp256k1.Internal
import qualified Crypto.Hash.SHA256 as SHA256
import System.IO.Unsafe (unsafePerformIO)


data DigitalSignature = DigitalSignature {
    dsigSignature :: String,
    dsigPublicKey :: String
} deriving (Show, Eq, Generic)

instance FromJSON DigitalSignature
instance ToJSON DigitalSignature

data UserRegistrationInfo = UserRegistrationInfo {
    userRegistrationPhoneNumber :: String,
    userRegistrationFirstName :: String,
    userRegistrationLastName :: String,
    digitalSignature :: DigitalSignature
} deriving (Show, Eq, Generic)

instance FromJSON UserRegistrationInfo
instance ToJSON UserRegistrationInfo

type FieldName = String
data UserRegistrationInfoValidationError = MustNotBeEmpty FieldName | MustNotContainPunctuation FieldName | InvalidPhoneNumber FieldName PhoneNumberError
                                           | InvalidPublicKey FieldName | InvalidPublicKeyOrPrivateKey FieldName FieldName
                                           | InvalidSignature FieldName | CouldNotGetSignature FieldName
                                           | MessageSignatureInvalid FieldName | CouldNotGetMessage FieldName

instance Show UserRegistrationInfoValidationError where
    show ( MustNotBeEmpty fieldName ) = "The '" ++ fieldName ++ "' must not be empty"
    show ( MustNotContainPunctuation fieldName ) = "The '" ++ fieldName ++ "' must not contain punctuation"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustStartWith6) ) = "The '" ++ fieldName ++ "' must start with digit 6"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustBe9Digits) ) = "The '" ++ fieldName ++ "' must contain 9 digits"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustOnlyBeDigits) ) = "The '" ++ fieldName ++ "' must only contain digits"
    show ( InvalidPublicKey fieldName ) = "The '" ++ fieldName ++ "' is invalid"
    show ( InvalidPublicKeyOrPrivateKey pubKeyFieldName sigFieldName ) = " The '" ++ pubKeyFieldName ++ "' and/or '" ++ sigFieldName ++ "' is invalid"
    show ( InvalidSignature fieldName ) = "The '" ++ fieldName ++ "' is an invalid signature"
    show ( CouldNotGetSignature fieldName ) = "The '" ++ fieldName ++ "' could not be gotten"
    show ( MessageSignatureInvalid fieldName ) = "The '" ++ fieldName ++ "' is an invalid signature"
    show ( CouldNotGetMessage fieldName ) = "The '" ++ fieldName ++ "' could not be gotten"

mustNotBeEmpty :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] String
mustNotBeEmpty fieldName value = if value /= []
                                 then _Success # value
                                 else _Failure # [MustNotBeEmpty fieldName]

mustOnlyBeDigits :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] String
mustOnlyBeDigits fieldName value@(digit:digits) = if (DL.all isDigit value)
                                                  then _Success # value
                                                  else _Failure # [InvalidPhoneNumber fieldName PhoneNumberMustOnlyBeDigits]
                            


validateUserRegistrationInfo :: UserRegistrationInfo -> Validation [UserRegistrationInfoValidationError] UserRegistrationInfo                                 
validateUserRegistrationInfo uinfo = pure uinfo <*
                         mustNotBeEmpty "First Name" (userRegistrationFirstName uinfo) <*
                         mustNotBeEmpty "Last Name"  (userRegistrationLastName uinfo) <*
                         mustNotBeEmpty "Phone Numer" (userRegistrationPhoneNumber uinfo) <*
                         mustNotBeEmpty "Digital Signature" ((dsigSignature . digitalSignature) uinfo) <*
                         mustNotBeEmpty "Public Key" ((dsigPublicKey . digitalSignature) uinfo) <*
                         isPhoneNumberValid "Phone Number" ((userRegistrationPhoneNumber) uinfo) <*
                         validateSignature "Public Key" "Digital Signature" uinfo

--- Phone Number Validation ---

data PhoneNumberError = PhoneNumberMustStartWith6 | PhoneNumberMustBe9Digits | PhoneNumberMustOnlyBeDigits

newtype PhoneNumber = PhoneNumber String deriving Show
newtype NineDigitsPhoneNumber = NineDigitsPhoneNumber String deriving Show

phoneNumberMustBeNineDigits :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] NineDigitsPhoneNumber
phoneNumberMustBeNineDigits fieldName value | (Prelude.length value == 9) = _Success # NineDigitsPhoneNumber value
                                            | otherwise           = _Failure # [InvalidPhoneNumber fieldName PhoneNumberMustBe9Digits]

phoneNumberMustStartWith6 :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] String
phoneNumberMustStartWith6 fieldName value@(x:xs)  | (x == '6') = _Success # value
                                                  | otherwise  = _Failure # [InvalidPhoneNumber fieldName PhoneNumberMustStartWith6]                                            

isPhoneNumberValid :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] PhoneNumber
isPhoneNumberValid fieldName value = let phNumber = strip value 
                                     in
                                        pure (PhoneNumber phNumber) <*
                                        mustNotBeEmpty fieldName phNumber <*
                                        mustOnlyBeDigits fieldName phNumber <*
                                        phoneNumberMustBeNineDigits fieldName phNumber <*
                                        phoneNumberMustStartWith6 fieldName phNumber


--- Public Key & Signature Validation ---

validateSignature :: FieldName 
                     -> FieldName 
                     -> UserRegistrationInfo 
                     -> Validation [UserRegistrationInfoValidationError] Sig
validateSignature pubKeyFieldName sigFieldName uinfo = 
    fromEither $ do
        let pkStr = ( (dsigPublicKey . digitalSignature) uinfo)
        let sigStr = ( (dsigSignature . digitalSignature) uinfo)
        let msgStr = ( (dsigSignature . digitalSignature) uinfo)

        pk  <- getPublicKey "Public Key" pkStr
        sig <- getSig "Digital Signature" sigStr
        msg <- getMsg "Message" msgStr

        -- if (trace "verifySig" (verifySig pk sig msg) ) then Right sig 
        if (verifySig pk sig msg) then Right sig
        else Left $ [InvalidSignature sigFieldName]

    where

        getPublicKey :: FieldName -> String -> Either [UserRegistrationInfoValidationError] PubKey
        getPublicKey fieldName pubKeyStr = 
            let mPubKey = importPubKey (fst $ B16.decode $ BS8.pack pubKeyStr) 
            in case mPubKey of
            Just pk -> Right pk
            Nothing -> Left $ [InvalidPublicKey fieldName]

        getSig :: FieldName -> String -> Either [UserRegistrationInfoValidationError] Sig
        getSig fieldName sigStr = 
            let mbs = importSig (fst $ B16.decode $ BS8.pack sigStr)
            in case mbs of
              Nothing -> Left [(CouldNotGetSignature fieldName)]
              Just sig -> Right sig

        getMsg :: (FromJSON a, ToJSON a) => FieldName -> a -> Either [UserRegistrationInfoValidationError] Msg
        getMsg fieldName payload = -- Payload is the json object coming from the wire
            let payloadSha256 = SHA256.hash $ (BSL.toStrict . encode) payload -- First convert the JSON encoding of the message to sha256
            in case (msg . fst . B16.decode . B16.encode) payloadSha256 of
                Nothing -> Left [CouldNotGetMessage fieldName]
                Just m -> Right m


-- https://davidederosa.com/basic-blockchain-programming/elliptic-curve-digital-signatures/

-- Generate private key: $ openssl ecparam -name secp256k1 -genkey -out ec-priv.pem
-- Generate public key: $ openssl ec -in ec-priv.pem -pubout -out ec-pub.pem
-- Sign Message & encode it in hex: openssl dgst -sha256 -hex -sign ec-priv.pem ex-message.txt
-- Use the hex in JSON:
-- {
-- 	"userRegistrationPhoneNumber": "699873018",
-- 	"userRegistrationFirstName": "Omar",
-- 	"userRegistrationLastName": "Mefire",
-- 	
-- 	"digitalSignature": {
-- 		"dsigSignature": "30440220511baa8f7035e658810b20bbe8f964a7f556336b8dba31e0dea516ea835e6a1f02203ed189d1b4c0893b907a8159b10a4f2c95d0d7b7e85a60e0414cfe80afb0dab8",
-- 		"dsigPublicKey": "0202a406624211f2abbdc68da3df929f938c3399dd79fac1b51b0e4ad1d26a47aa"
-- 	}
-- }

-- 03ed916094056733141774e5b79ab48dec70c57c6c6d423e34db24038334e01c
-- f5cbe7d88182a4b8e400f96b06128921864a18187d114c8ae8541b566c8ace00
-- msg = Msg32 $ toShort $ fst $ B16.decode
--         "f5cbe7d88182a4b8e400f96b06128921864a18187d114c8ae8541b566c8ace00"