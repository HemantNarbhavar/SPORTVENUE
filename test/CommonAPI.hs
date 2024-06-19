{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module CommonAPI where

import qualified Types as T
import Network.HTTP.Simple
import Data.Aeson (eitherDecode, decode, eitherDecodeFileStrict)
import Control.Monad (void)
import Data.ByteString.Char8 (pack)
import Control.Exception 
import qualified Data.ByteString.Lazy as BL

-- Function to test the get facilities API
get_facilities_test :: IO ()
get_facilities_test = do
    let request = setRequestPath "/facilities"
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "GET"
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/facilities"
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.Facility] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right facilities -> print facilities

-- Function to call the Get_Facility API
get_facility_test :: Int -> IO ()
get_facility_test facilityId = do
        let request = setRequestPath (pack $ "/facility/" ++ show facilityId)
                    $ setRequestHost "localhost"
                    $ setRequestPort 5000
                    $ setRequestSecure False
                    $ defaultRequest
        result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
        case result of
            Left e          -> handleHttpException e
            Right response  -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/facility/" ++ show facilityId
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Facility of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right facility -> print facility

-- Function to test the get ratings API
get_ratings_test :: Int -> IO ()
get_ratings_test facilityId = do
    let request = setRequestPath (pack $ "/facility_ratings/" ++ show facilityId)
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "GET"
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/facility_ratings/" ++ show facilityId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.Ratings] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right ratings -> print ratings


-- Function to test the get top facility API
get_top_facility_test :: IO ()
get_top_facility_test = do
    let request = setRequestPath "/facility_ratings/top_5"
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "GET"
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/facility_ratings/top_5"
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.Facility] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right facilities -> print facilities


handleHttpException :: SomeException -> IO ()
handleHttpException e = do
    putStrLn "please verify following details!"
    putStrLn $ "HTTP exception occurred: " ++ show e