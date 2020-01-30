{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.ConnectionInfo (getConnectionInfo, ConnectionInfo(..)) where

import System.IO (readFile, stderr, hPutStrLn)
import GHC.Generics
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Control.Exception (catch, IOException, throw, Exception)


data ConnectionInfo = ConnectionInfo {
  host :: String,
  dbname :: String,
  user :: String,
  password :: String,
  port :: String
} deriving (Show, Generic)

instance FromJSON ConnectionInfo
instance ToJSON ConnectionInfo

-- ToDO: What if 'credentials.json' file does NOT exist?
-- ToDO: What if 'credentials.json' file contains the wrong format for the expected JSON?
-- ToDO: What if 'credentials.json' is empty?
jsonFile :: FilePath
jsonFile = "credentials.json"

data JSONException = InvalidJSONInCredentialsFile deriving Show
instance Exception JSONException

getConnectionInfo :: IO ConnectionInfo
getConnectionInfo = do
  json <- B.readFile jsonFile
  let mJson = decode json :: Maybe ConnectionInfo
  case mJson of
    Nothing -> throw InvalidJSONInCredentialsFile
    Just json -> return json