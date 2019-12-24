{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (catch)
import Control.Exception

import Web.Scotty.Trans
import Data.Text

import Exception.Handling
import Types.User

import Control.Monad.Trans.Except(ExceptT(..))
import Data.Validation

-- ToDO: Use SSL/TLS to enhance security/privacy
main = (flip catch) handler $ scottyT 3000 id $ do

  defaultHandler handleAppExceptions

  -- ToDO: Create haddock documentation pages for the API.

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
  --  - @Exception.Handling.ServerException@: RegistrationInfoInvalid | UserAlreadyRegisteredException
  --  - User { "UserId": 123, "UserPublicKey": "03df51984d6b8b8b1cc693e239491f77a36c9e9dfe4a486e9972a18e03610a0d22", "UserFirstName": "Omar", "UserLastName": "Mefire", "UserPhoneNumber": "699862045" }
  
  post "/registerUser" $ do
    incomingUserRegistrationInfo :: UserRegistrationInfo <- jsonData
    validation
      (\errors -> raise $ RegistrationInfoInvalid 1502 $ Prelude.map (\e -> show e) errors) -- ToDO: Writing error code 1502 can lead to mistakes on the part of devs
      (\newUser -> do
        _ <- liftAndCatchIO $ putStrLn "Test" --createNewUser userRegistrationInfo
        Web.Scotty.Trans.json newUser)
      (validateUserRegistrationInfo incomingUserRegistrationInfo)