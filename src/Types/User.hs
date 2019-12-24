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


data DigitalSignature = DigitalSignature {
    dsigSignature :: String,
    dsigPublicKey :: String
} deriving (Show, Eq, Generic)

instance FromJSON DigitalSignature
instance ToJSON DigitalSignature

data UserRegistrationInfo = UserRegistrationInfo {
    -- userRegistrationInfoPublicKey :: String,
    -- userRegistrationInfoDigitalSignature :: String,
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
                                           | MessageSignatureInvalid FieldName

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
                         -- isPublicKeyValid "Public Key" ((fromString . dsigPublicKey . digitalSignature) uinfo)
                         -- isSignatureValid "Public Key" "Signature" (getPublicKey uinfo) (getSig uinfo) (Types.User.getMsg uinfo)

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


--- Public Key Infrastructure Validation ---

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
              Nothing -> Left [(InvalidSignature fieldName)]
              Just sig -> Right sig

        getMsg :: FieldName -> String -> Either [UserRegistrationInfoValidationError] Msg
        getMsg fieldName msgStr = error "getMsg"


-- Exple public key:
-- 04dded4203dac96a7e85f2c374a37ce3e9c9a155a72b64b4551b0bfe779dd4470512213d5ed790522c042dee8e85c4c0ec5f96800b72bc5940c8bc1c5e11e4fcbf

-- >> createnewaddress()
-- "3GtVqYDKWbxMY3BxeZJPszYfpUAnu6xFkP"
-- >> signmessage("3GtVqYDKWbxMY3BxeZJPszYfpUAnu6xFkP", "test")
-- "H65VvmPa8nPN396VyfoTvhRs26A7v2ZfJj+DMz5u1pIdeuh30AsM6tUCWuIagByVhO6N+C/iGWkd7dIAZhDpVAA="

-- https://davidederosa.com/basic-blockchain-programming/elliptic-curve-digital-signatures/