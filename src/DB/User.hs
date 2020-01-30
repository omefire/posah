{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.User ( 
    createNewUser 
)
where

import Types.User
import Opaleye
import Opaleye.Trans
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PSQL
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Arrow (returnA)
import Control.Exception.Safe (throwString)


data UserP a b c d e f g = UserP
  { userID :: a
  , userFirstName :: b
  , userLastName :: c
  , userEmail :: d
  , userPhoneNumber :: e
  , userConfirmed :: f
  , userPublicKey :: g
  } deriving (Show, Eq)

type WriteUser = UserP (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool) (Column PGText)
type ReadUser = UserP (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool) (Column PGText)

makeAdaptorAndInstance "pUser" ''UserP

userTable :: Table (WriteUser) (ReadUser)
userTable = Table "Users" $ pUser UserP
  { userID = optional "UserID"
  , userFirstName = required "FirstName"
  , userLastName = required "LastName"
  , userEmail = required "Email"
  , userPhoneNumber = required "PhoneNumber"
  , userConfirmed = required "Confirmed"
  , userPublicKey = required "PublicKey"
  }

-- ToDO:
-- 1. Insert new user into the DB
-- 2. Generate a random code
-- 3. Return the random code to the user's email and phone number via SMS
createNewUser :: PSQL.Connection -> UserRegistrationInfo -> IO Int
createNewUser conn uri = do
    res <- runOpaleyeT conn $ transaction $ insertUser uri
    case res of
        -- ToDO: Test the Nothing path
        Nothing -> throwString $ "An error occured while inserting the user: " <> (userRegistrationFirstName uri) <> " " <> (userRegistrationLastName uri) <> ", " <> (userRegistrationPhoneNumber uri)
        Just uId -> return uId

insertUser :: UserRegistrationInfo -> Transaction (Maybe Int)
insertUser uri = do
    mUserId <- ( insertReturningFirst userTable userID 
                  ( 
                      UserP Nothing 
                            (pgString $ userRegistrationFirstName uri) 
                            (pgString $ userRegistrationLastName uri)
                            (pgString $ userRegistrationEmail uri)
                            (pgString $ userRegistrationPhoneNumber uri)
                            (pgBool $ False)
                            (pgString $ userRegistrationPublicKey uri)
                      
                  ) :: Transaction (Maybe Int) 
                  
                  )
    return mUserId




