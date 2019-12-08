{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (catch)
import Control.Exception

import Web.Scotty.Trans
import Data.Text

import Exception.Handling
import Types.User


main = (flip catch) handler $ scottyT 3000 id $ do

  defaultHandler handleAppExceptions

  -- curl -X POST -H "Content-Type: application/json" \
  -- -d '{"userRegistrationInfoPublicKey": "03df51984d6b8b8b1cc693e239491f77a36c9e9dfe4a486e9972a18e03610a0d22", \
  --      "userRegistrationInfoDigitalSignature": "03df51984d6b8b8b1", \
  --      "userRegistrationInfoPhoneNumber": "699683212", \
  --      "userRegistrationFirstName": "Omar", \
  --      "userRegistrationLastName": "Mefire" \
  --     }' http://localhost:3000/registerUser --header "Content-Type:application/json"
  post "/registerUser" $ do
    userRegistrationInfo :: UserRegistrationInfo <- jsonData
    -- validate the incoming data
    error "Test"