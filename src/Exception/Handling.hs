-- ---------------------------
-- Exception Handling Notes:
-- ---------------------------

-- * Clients that send requests to the server should expect two kinds of exceptions:
--     application-specific exceptions (such as InvalidDigitalSignatureException) __and__ internal server exceptions (e.g: StackOverflow, uncaught exceptions, etc...)
-- 
-- * For application type exceptions that happen on the server, a response similar to this will be returned:
-- 
--     HTTP/1.1 500 Internal Server Error
--     Transfer-Encoding: chunked
--     Date: Sun, 08 Dec 2019 01:40:53 GMT
--     Server: Warp/3.2.28
--     Content-Type: application/json; charset=utf-8
--     {"errorCode":1370,"errorType":"UserAlreadyRegisteredError","errorMessage":"Raka'kufiniel"}
-- 
-- * An 'Internal Server Error' will probably have a string for a body, instead of a JSON like above:
-- 
--     HTTP/1.1 500 Internal Server Error
--     Transfer-Encoding: chunked
--     Date: Sun, 08 Dec 2019 01:45:56 GMT
--     Server: Warp/3.2.28
--     Content-Type: text/plain; charset=utf-8
--     Something went wrong
-- 
-- * For application-specific exceptions, 
-- 
-- * This is how to raise and then transform an IO exception into an application specific one:
--     data RedditException = CommentFailed String deriving Show
--     instance Exception RedditException
--     liftAndCatchIO :: (ScottyError e, Monad m) => IO a -> ActionT e m ()
--     liftAndCatchIO $ throwIO $ CommentFailed "It failed, Omar"
-- 
-- * If you want to raise an application specific exception:
--     raise :: (ScottyError e, Monad m) => e -> ActionT e m a
--     raise $ UserAlreadyRegisteredError 1370 "Raka'kufiniel"
-- 
-- * This is how to return a successful response
--     json :: (A.ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
--     json $ UserRegistrationInfo {
--         userRegistrationInfoPublicKey = "03df51984d6b8b8b1cc693e239491f77a36c9e9dfe4a486e9972a18e03610a0d22",
--         userRegistrationInfoDigitalSignature = "03df51984d6b8b8b1",
--         userRegistrationInfoPhoneNumber = "699873015",
--         userRegistrationFirstName = "Omar",
--         userRegistrationLastName = "Mefire"
--         }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Exception.Handling (
    handler,
    handleAppExceptions,
    ExceptionCode,
    ServerException(RegistrationInfoInvalid)
) where
import GHC.Exception.Type
import GHC.IO (throwIO)
import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Types
import Data.String (fromString)
import Web.Scotty.Trans

import Control.Exception
import GHC.Generics
import GHC.Exts (fromList) -- Handle any unhandled application exception (i.e thrown by `raise` and not handled with local `rescue`)

handleAppExceptions :: Monad m => ServerException -> ActionT ServerException m ()
handleAppExceptions e = do
  status status500
  Web.Scotty.Trans.json e

type ExceptionCode = Int

data ServerException = 
  InternalServerException ExceptionCode String -- ExceptionCode = 1500 -- Used when we don't know what specific application-exception to use
  | UserAlreadyRegisteredException ExceptionCode String -- ExceptionCode = 1501
  | RegistrationInfoInvalid ExceptionCode [String] -- ExceptionCode = 1502
    deriving (Show, Eq, Generic)

instance ToJSON ServerException where
  toJSON (InternalServerException exCode exMsg) = object [
    "exceptionType" .= ("InternalServerException" :: String),
    "exceptionCode" .= exCode,
    "exceptionMessage" .= exMsg]

  toJSON (UserAlreadyRegisteredException exCode exMsg) = object [
    "exceptionType" .= ("UserAlreadyRegisteredException" :: String),
    "exceptionCode" .= exCode,
    "exceptionMessage" .= exMsg]
  
  toJSON (RegistrationInfoInvalid exCode exMsgs) = object [
    "exceptionType" .= ("RegistrationInfoInvalid" :: String),
    "exceptionCode" .= exCode,
    "exceptionMessage" .= exMsgs]

instance ScottyError ServerException where
  stringError = InternalServerException 1500 -- Used by scotty when transforming an IO exception into a ServerException. e.g: through `liftAndCatchIO`
  showError = fromString . show-- ToDO: Implement a proper logger that will save errors to a file or something

-- Note: to simulate and make this handler kick in, use `error "Test"` in any post/get statement
handler :: SomeException -> IO ()
handler e = putStrLn $ "Caught exception: " ++ show e

