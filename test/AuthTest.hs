{-# LANGUAGE OverloadedStrings #-}

module AuthTest where

import qualified Types as T
import Network.HTTP.Simple
import Data.Aeson (eitherDecode, decode, eitherDecodeFileStrict)
import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Control.Exception 
import qualified Data.ByteString.Lazy as BL

-- Function to test the user registration API
register_user_test :: FilePath -> IO ()
register_user_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Users)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right user -> do
            let request = setRequestPath "/register/user"
                        $ setRequestHost "localhost"
                        $ setRequestPort 5000
                        $ setRequestSecure False
                        $ setRequestMethod "POST"
                        $ setRequestBodyJSON user
                        $ defaultRequest
            result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
            case result of
                Left e -> handleHttpException e
                Right response -> do
                    let statusCode = getResponseStatusCode response
                    putStrLn $ "URL: " ++ "http://localhost:5000/register/user"
                    putStrLn $ "Status code: " ++ show statusCode
                    let responseBody = getResponseBody response
                    case eitherDecode responseBody :: Either String T.UserId of
                        Left err -> putStrLn $ "Error decoding JSON: " ++ err
                        Right userId -> print userId

-- Function to test the admin registration API
register_admin_test :: FilePath -> IO ()
register_admin_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Admins)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right admin -> do
            let request = setRequestPath "/register/admin"
                        $ setRequestHost "localhost"
                        $ setRequestPort 5000
                        $ setRequestSecure False
                        $ setRequestMethod "POST"
                        $ setRequestBodyJSON admin
                        $ defaultRequest
            result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
            case result of
                Left e -> handleHttpException e
                Right response -> do
                    let statusCode = getResponseStatusCode response
                    putStrLn $ "URL: " ++ "http://localhost:5000/register/admin"
                    putStrLn $ "Status code: " ++ show statusCode
                    let responseBody = getResponseBody response
                    case eitherDecode responseBody :: Either String T.AdminId of
                        Left err -> putStrLn $ "Error decoding JSON: " ++ err
                        Right userId -> print userId


-- Function to test the user login API
login_user_test :: FilePath -> IO ()
login_user_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Login)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right loginInfo -> do
            let request = setRequestPath "/login/user"
                        $ setRequestHost "localhost"
                        $ setRequestPort 5000
                        $ setRequestSecure False
                        $ setRequestMethod "POST"
                        $ setRequestBodyJSON loginInfo
                        $ defaultRequest
            result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
            case result of
                Left e -> handleHttpException e
                Right response -> do
                    let statusCode = getResponseStatusCode response
                    putStrLn $ "URL: " ++ "http://localhost:5000/login/user"
                    putStrLn $ "Status code: " ++ show statusCode
                    let responseBody = getResponseBody response
                    case eitherDecode responseBody :: Either String T.Result of
                        Left err -> putStrLn $ "Error decoding JSON: " ++ err
                        Right result -> print result

-- Function to test the admin login API
login_admin_test :: FilePath -> IO ()
login_admin_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Login)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right loginInfo -> do
            let request = setRequestPath "/login/admin"
                        $ setRequestHost "localhost"
                        $ setRequestPort 5000
                        $ setRequestSecure False
                        $ setRequestMethod "POST"
                        $ setRequestBodyJSON loginInfo
                        $ defaultRequest
            result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
            case result of
                Left e -> handleHttpException e
                Right response -> do
                    let statusCode = getResponseStatusCode response
                    putStrLn $ "URL: " ++ "http://localhost:5000/login/admin"
                    putStrLn $ "Status code: " ++ show statusCode
                    let responseBody = getResponseBody response
                    case eitherDecode responseBody :: Either String T.Result of
                        Left err -> putStrLn $ "Error decoding JSON: " ++ err
                        Right result -> print result



handleHttpException :: SomeException -> IO ()
handleHttpException e = do
    putStrLn "please verify following details!"
    putStrLn $ "HTTP exception occurred: " ++ show e