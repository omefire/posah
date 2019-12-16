{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List (all)

data UserRegistrationInfo = UserRegistrationInfo {
    userRegistrationInfoPublicKey :: String,
    userRegistrationInfoDigitalSignature :: String,
    userRegistrationInfoPhoneNumber :: String,
    userRegistrationFirstName :: String,
    userRegistrationLastName :: String
} deriving (Show, Eq, Generic)

instance FromJSON UserRegistrationInfo
instance ToJSON UserRegistrationInfo

type FieldName = String
data UserRegistrationInfoValidationError = MustNotBeEmpty FieldName | MustNotContainPunctuation FieldName | InvalidPhoneNumber FieldName PhoneNumberError

instance Show UserRegistrationInfoValidationError where
    show ( MustNotBeEmpty fieldName ) = "The '" ++ fieldName ++ "' must not be empty"
    show ( MustNotContainPunctuation fieldName ) = "The '" ++ fieldName ++ "' must not contain punctuation"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustStartWith6) ) = "The '" ++ fieldName ++ "' must start with digit 6"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustBe9Digits) ) = "The '" ++ fieldName ++ "' must contain 9 digits"
    show ( InvalidPhoneNumber fieldName (PhoneNumberMustOnlyBeDigits) ) = "The '" ++ fieldName ++ "' must only contain digits"

mustNotBeEmpty :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] String
mustNotBeEmpty fieldName value = if value /= []
                                 then _Success # value
                                 else _Failure # [MustNotBeEmpty fieldName]

mustOnlyBeDigits :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] String
mustOnlyBeDigits fieldName value@(digit:digits) = if (all isDigit value)
                                                  then _Success # value
                                                  else _Failure # [InvalidPhoneNumber fieldName PhoneNumberMustOnlyBeDigits]
                            


validateUserRegistrationInfo :: UserRegistrationInfo -> Validation [UserRegistrationInfoValidationError] UserRegistrationInfo                                 
validateUserRegistrationInfo uinfo = pure uinfo <*
                         mustNotBeEmpty "First Name" (userRegistrationFirstName uinfo) <*
                         mustNotBeEmpty "Last Name"  (userRegistrationLastName uinfo) <*
                         mustNotBeEmpty "Phone Numer" (userRegistrationInfoPhoneNumber uinfo) <*
                         mustNotBeEmpty "Digital Signature" (userRegistrationInfoDigitalSignature uinfo) <*
                         mustNotBeEmpty "Public Key" (userRegistrationInfoPublicKey uinfo) <*
                         isPhoneNumberValid "Phone Number" (userRegistrationInfoPhoneNumber uinfo)

--- Phone Number Validation ---

data PhoneNumberError = PhoneNumberMustStartWith6 | PhoneNumberMustBe9Digits | PhoneNumberMustOnlyBeDigits

newtype PhoneNumber = PhoneNumber String deriving Show
newtype NineDigitsPhoneNumber = NineDigitsPhoneNumber String deriving Show

phoneNumberMustBeNineDigits :: FieldName -> String -> Validation [UserRegistrationInfoValidationError] NineDigitsPhoneNumber
phoneNumberMustBeNineDigits fieldName value | (length value == 9) = _Success # NineDigitsPhoneNumber value
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