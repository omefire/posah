{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Main where

import Prelude hiding (catch)
import Control.Exception

import Web.Scotty.Trans
import Data.Text

import Exception.Handling
import Types.User

import Control.Monad.Trans.Except(ExceptT(..))
import Data.Validation

import GHC.Generics

import Middleware.Authentication

import Control.Concurrent
import System.IO.Unsafe
import Control.Monad.IO.Class

import Data.IORef

import Data.Aeson.Types
import Data.Aeson

import DB.User
import DB.ConnectionInfo as CI

import qualified Database.PostgreSQL.Simple as PSQL
import Data.String.Interpolate
import qualified Data.ByteString.Char8 as BC


-- ToDO:
-- * Read connection details off of a credentials.json file
-- * Use the connect function to create a ConnectionInfo object that will be returned to user
--getConnectionInfo :: IO CI.ConnectionInfo
--getConnectionInfo = do
--  return $ ConnectionInfo {
--    host = "localhost",
--    dbname = "db",
--    user = "user",
--    password = "password",
--    port = "5432"
--  }
--getConnectionInfo = do
--  eConnInfo <- liftIO $ CI.getConnectionInfo
--  case eConnInfo of
--    Left err -> throwError err505 { errBody = BLC.pack err }
--    Right connInfo -> do
--      conn <- liftIO $ connect ConnectInfo {connectHost = host connInfo
--                                          ,connectPort = (fromIntegral $ port connInfo)
--                                          ,connectDatabase = database connInfo
--                                          ,connectPassword = password connInfo
--                                          ,connectUser = user connInfo
--                                          }

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
    -- Note: Read & interpret the incoming payload information from an IORef
    -- The payload information was saved in the IORef by the authentication middleware
    payload <- liftIO $ readIORef payloadIORef -- payload is of type: Object. type Object = Hashmap Text Value
    let mRegistrationInfo = flip parseMaybe payload $ \reg -> do
          first_name <- reg .: "userRegistrationFirstName"
          last_name <- reg .: "userRegistrationLastName"
          phone_number <- reg .: "userRegistrationPhoneNumber"
          email <- reg .: "userRegistrationEmail"
          -- public_key <- reg .: "userRegistrationPublicKey"
          return $ UserRegistrationInfo { 
                                          userRegistrationFirstName = first_name, 
                                          userRegistrationLastName = last_name, 
                                          userRegistrationPhoneNumber = phone_number,
                                          userRegistrationEmail = email,
                                          userRegistrationPublicKey = "abc"
                                        }

    case mRegistrationInfo of
      Nothing -> let errors = ["An invalid userRegistrationInfo type was passed into this request"]
                 in raise $ RegistrationInfoInvalid 1502 $ Prelude.map (\e -> show e) errors -- ToDO: Writing error code 1502 can lead to mistakes on the part of devs

      Just registrationInfo -> do
        validation
         (\errors -> raise $ RegistrationInfoInvalid 1502 $ Prelude.map (\e -> show e) errors) -- ToDO: Writing error code 1502 can lead to mistakes on the part of devs
         (\newUser -> do
           -- ToDO: What if there's an exception during DB saving? Or if the saving does not succeed?
           -- ToDO: What if an exception is thrown in the next lines of code?
           -- ToDO: Test this with a live database
           connInfo <- liftAndCatchIO getConnectionInfo
           let connString = [i|host='#{host connInfo}' dbname='#{dbname connInfo}' user='#{user connInfo}' password='#{password connInfo}' port='#{port connInfo}'|]
           _ <- liftAndCatchIO $ putStrLn connString
           conn <- liftAndCatchIO $ PSQL.connectPostgreSQL $ BC.pack connString -- Main.getConnectionInfo
           _ <- liftAndCatchIO $ createNewUser conn registrationInfo
           -- If User creation succeeds, send an email + SMS with the confirmation code to the user
           Web.Scotty.Trans.json newUser)
         (validateUserRegistrationInfo registrationInfo)

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