{-# LANGUAGE OverloadedStrings #-}

module AdminAPI where

import qualified Types as T
import Network.HTTP.Simple
import Data.Aeson (eitherDecode, eitherDecodeFileStrict)
import Data.ByteString.Char8 (pack)
import Control.Exception 
import qualified Data.ByteString.Lazy as BL


-- Function to test the add facility API
add_facility_test :: FilePath -> IO ()
add_facility_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Facility)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right facility -> do
                    let request = setRequestPath "/admin/add_facility"
                                $ setRequestHost "localhost"
                                $ setRequestPort 5000
                                $ setRequestSecure False
                                $ setRequestMethod "POST"
                                $ setRequestBodyJSON facility
                                $ defaultRequest
                    result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                    case result1 of
                        Left e -> handleHttpException e
                        Right response -> do
                            let statusCode = getResponseStatusCode response
                            putStrLn $ "URL: " ++ "http://localhost:5000/admin/add_facility"
                            putStrLn $ "Status code: " ++ show statusCode
                            let responseBody = getResponseBody response
                            case eitherDecode responseBody :: Either String T.Result of
                                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                                Right res -> print res


-- Function to test the update facility API
update_facility_test :: Int -> FilePath -> IO ()
update_facility_test facilityId filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Facility)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right facility -> do
                    let request = setRequestPath (pack $ "/admin/update_facility/" ++ show facilityId)
                                $ setRequestHost "localhost"
                                $ setRequestPort 5000
                                $ setRequestSecure False
                                $ setRequestMethod "PUT"
                                $ setRequestBodyJSON facility
                                $ defaultRequest
                    result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                    case result1 of
                        Left e -> handleHttpException e
                        Right response -> do
                            let statusCode = getResponseStatusCode response
                            putStrLn $ "URL: " ++ "http://localhost:5000/admin/update_facility/<facility_id>"
                            putStrLn $ "Status code: " ++ show statusCode
                            let responseBody = getResponseBody response
                            case eitherDecode responseBody :: Either String T.Result of
                                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                                Right res -> print res


-- Function to test the delete facility API
delete_facility_test :: Int -> IO ()
delete_facility_test facilityId = do
                let request = setRequestPath (pack $ "/admin/delete_facility/" ++ show facilityId)
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "DELETE"
                            $ defaultRequest
                result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/admin/delete_facility/<facility_id>"
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res


-- Function to test the create group API
create_group_test :: FilePath -> IO ()
create_group_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Facility)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right group -> do
                    let request = setRequestPath "/admin/create_group"
                                $ setRequestHost "localhost"
                                $ setRequestPort 5000
                                $ setRequestSecure False
                                $ setRequestMethod "POST"
                                $ setRequestBodyJSON group
                                $ defaultRequest
                    result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                    case result1 of
                        Left e -> handleHttpException e
                        Right response -> do
                            let statusCode = getResponseStatusCode response
                            putStrLn $ "URL: " ++ "http://localhost:5000/admin/create_group"
                            putStrLn $ "Status code: " ++ show statusCode
                            let responseBody = getResponseBody response
                            case eitherDecode responseBody :: Either String T.Result of
                                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                                Right res -> print res


-- Function to test the update facility group API
update_facility_group_test :: Int -> Int -> IO ()
update_facility_group_test facilityId groupId = do
                let request = setRequestPath (pack $ "/admin/update_group/add_facility/" ++ show facilityId ++ "/" ++ show groupId)
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "PUT"
                            $ defaultRequest
                result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/admin/update_group/add_facility/<facility_id>/<group_id>"
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res

-- Function to test the remove facility group API
remove_facility_group_test :: Int -> IO ()
remove_facility_group_test facilityId = do
                let request = setRequestPath (pack $ "/admin/update_group/remove_facility/" ++ show facilityId)
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "PUT"
                            $ defaultRequest
                result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/admin/update_group/remove_facility/<facility_id>"
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res


-- Function to test the delete group API
delete_group_test :: Int -> IO ()
delete_group_test groupId = do
    let request = setRequestPath (pack $ "/admin/delete_group/" ++ show groupId)
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "DELETE"
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/admin/delete_group/" ++ show groupId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.Result of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right res -> print res


-- Function to test the set Facility status API
set_holiday_test :: FilePath -> IO ()
set_holiday_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.FacilityStatus)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right facilitySt -> do
                    let request = setRequestPath "/admin/set_holiday"
                                $ setRequestHost "localhost"
                                $ setRequestPort 5000
                                $ setRequestSecure False
                                $ setRequestMethod "POST"
                                $ setRequestBodyJSON facilitySt
                                $ defaultRequest
                    result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                    case result1 of
                        Left e -> handleHttpException e
                        Right response -> do
                            let statusCode = getResponseStatusCode response
                            putStrLn $ "URL: " ++ "http://localhost:5000/admin/set_holiday"
                            putStrLn $ "Status code: " ++ show statusCode
                            let responseBody = getResponseBody response
                            case eitherDecode responseBody :: Either String T.Result of
                                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                                Right res -> print res


-- Function to test the set Facility status by group API
set_holiday_group_test :: Int -> FilePath -> IO ()
set_holiday_group_test groupId filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.FacilityStatus)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right facilitySt -> do
                    let request = setRequestPath (pack $ "/admin/set_holiday/group" ++ show groupId)
                                $ setRequestHost "localhost"
                                $ setRequestPort 5000
                                $ setRequestSecure False
                                $ setRequestMethod "POST"
                                $ setRequestBodyJSON facilitySt
                                $ defaultRequest
                    result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                    case result1 of
                        Left e -> handleHttpException e
                        Right response -> do
                            let statusCode = getResponseStatusCode response
                            putStrLn $ "URL: " ++ "http://localhost:5000/admin/set_holiday/group"
                            putStrLn $ "Status code: " ++ show statusCode
                            let responseBody = getResponseBody response
                            case eitherDecode responseBody :: Either String T.Result of
                                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                                Right res -> print res

-- Function to test the delete facility status API
delete_holiday_test :: Int -> IO ()
delete_holiday_test statusId = do
    let request = setRequestPath (pack $ "/admin/remove_holiday/" ++ show statusId)
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "DELETE"
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/admin/remove_holiday/" ++ show statusId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.Result of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right res -> print res

-- Function to test the update grouped facilities API
update_grouped_facilities_test :: Int -> T.Facility -> IO ()
update_grouped_facilities_test groupId facility = do
    let request = setRequestPath (pack $ "/admin/group/" ++ show groupId)
                $ setRequestHost "localhost"
                $ setRequestPort 5000
                $ setRequestSecure False
                $ setRequestMethod "PUT"
                $ setRequestBodyJSON facility
                $ defaultRequest
    result <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
    case result of
        Left e -> handleHttpException e
        Right response -> do
            let statusCode = getResponseStatusCode response
            putStrLn $ "URL: " ++ "http://localhost:5000/admin/group/" ++ show groupId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.Result of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right res -> print res


-- Function to test the activate booking API
activate_booking_test :: FilePath -> IO ()
activate_booking_test filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.BookingToken)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right token -> do
                let request = setRequestPath "/user/booking/activate"
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "PUT"
                            $ setRequestBodyJSON token
                            $ defaultRequest
                result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result1 of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/user/booking/activate"
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res


-- Function to test the add rating API
add_rating_test :: Int -> FilePath -> IO ()
add_rating_test facilityId filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Ratings)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right rating -> do
                let request = setRequestPath (pack $ "/user/add_facility_rating/" ++ show facilityId)
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "POST"
                            $ setRequestBodyJSON rating
                            $ defaultRequest
                result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result1 of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/user/add_facility_rating/" ++ show facilityId
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res


handleHttpException :: SomeException -> IO ()
handleHttpException e = do
    putStrLn "please verify following details!"
    putStrLn $ "HTTP exception occurred: " ++ show e