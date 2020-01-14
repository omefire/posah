{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Prelude hiding (catch)
import Control.Exception

import Web.Scotty.Trans
import Data.Text

import Exception.Handling
import Types.User

import Control.Monad.Trans.Except(ExceptT(..))
import Data.Validation

import Data.Aeson
import GHC.Generics

import Middleware.Authentication

import Control.Concurrent
import System.IO.Unsafe
import Control.Monad.IO.Class

import Data.IORef


-- | ToDO: Use SSL/TLS to enhance security/privacy
main = (flip catch) handler $ scottyT 3000 id $ do

  defaultHandler handleAppExceptions

  middleware validateDigitalSignature
  
  -- | ToDO: Create haddock documentation pages for the API.

  -- How to invoke "/registerUser" API endpoint:

  -- curl -X POST -H "Content-Type: application/json" \
  -- -d '"UserId": 0, \
  --      "UserPublicKey": "03df51984d6b8b8b1cc693e239491f77a36c9e9dfe4a486e9972a18e03610a0d22", \
  --      "UserDigitalSignature": "03df51984d6b8b8b1", \
  --      "UserPhoneNumber": "699683212", \
  --      "UserFirstName": "Omar", \
  --      "UserLastName": "Mefire" \
  --     }' http://localhost:3000/registerUser --header "Content-Type:application/json"

  -- Returns one of these: 
  --  - Something went wrong
  --  - AuthenticationError { }
  --  - @Exception.Handling.ServerException@: RegistrationInfoInvalid | UserAlreadyRegisteredException
  --  - User { "UserId": 123, "UserPublicKey": "03df51984d6b8b8b1cc693e239491f77a36c9e9dfe4a486e9972a18e03610a0d22", "UserFirstName": "Omar", "UserLastName": "Mefire", "UserPhoneNumber": "699862045" }

-- curl -X POST -H "Content-Type: application/json" -d \  
-- '{ \
--   "auth": { \
--     "public_key": "", \
--     "digital_signature": "", \
--   }, \
--   \
--   "payload": { \
--     "first_name": "", \
--     "last_name": "", \
--     "phone_number": "" \
--   } \
-- }' http://localhost:3000/registerUser --header "Content-Type:application/json"
  
  post "/registerUser" $ do
    -- _ <- error "registerUser"
    -- let result = Data.Aeson.decode body :: Maybe Object
    -- _ <- liftIO $ putStrLn (show result)
    -- payload :: UserRegistrationInfo <- liftIO retrievePayload
    (Object payload) <- liftIO $ readIORef payloadIORef -- payload is of type: Hashmap Text Value
    let registrationInfo = 

    userReq :: UserReq UserRegistrationInfo <- jsonData
    let registrationInfo = (payload userReq)
    validation
      (\errors -> raise $ RegistrationInfoInvalid 1502 $ Prelude.map (\e -> show e) errors) -- ToDO: Writing error code 1502 can lead to mistakes on the part of devs
      (\newUser -> do
        _ <- liftAndCatchIO $ putStrLn "Test" --createNewUser userRegistrationInfo
        Web.Scotty.Trans.json newUser)
      (validateUserRegistrationInfo registrationInfo)


-- data UserReqPayload = UserRegistrationInfo | ListActiveOffers deriving (Show, Eq, Generic)

data ListActiveOffers = ListActiveOffers deriving (Show, Eq, Generic)
instance FromJSON ListActiveOffers
instance ToJSON ListActiveOffers

-- ToDO: Can I force a to be of type `ListActiveOffers` or `UserRegistrationInfo`?
-- ... Can I use a typeclass for that?
-- data UserReq payload = UserReq { 
--   auth :: UserReqAuth,
--   payload :: payload
-- } deriving (Show, Eq, Generic)

-- data UserReq payload where
--   auth :: UserReq -> UserReqAuth
--   payload :: (Show payload) => UserReq -> payload

-- data UserReq = UserReq {
--   auth :: UserReqAuth,
--   payload :: UserReqPayload -- ToDO: Can Haskell allow me to do something like `UserRegistrationInfo | ListActiveOffers`? => GADTs?
-- } deriving (Show, Eq, Generic)

instance (FromJSON payload) => FromJSON (UserReq payload)
instance (ToJSON payload) => ToJSON (UserReq payload)