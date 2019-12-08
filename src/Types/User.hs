{-# LANGUAGE DeriveGeneric #-}

module Types.User where

import Data.Aeson
import GHC.Generics

data UserRegistrationInfo = UserRegistrationInfo {
    userRegistrationInfoPublicKey :: String,
    userRegistrationInfoDigitalSignature :: String,
    userRegistrationInfoPhoneNumber :: String,
    userRegistrationFirstName :: String,
    userRegistrationLastName :: String
} deriving (Show, Eq, Generic)

instance FromJSON UserRegistrationInfo
instance ToJSON UserRegistrationInfo