{-# LANGUAGE OverloadedStrings #-}

module UserAPI where

import qualified Types as T
import Network.HTTP.Simple
import Data.Aeson (eitherDecode, eitherDecodeFileStrict)
import Data.ByteString.Char8 (pack)
import Control.Exception 
import qualified Data.ByteString.Lazy as BL
import Data.Time.Calendar.OrdinalDate (Day)
import qualified Data.Text as Tx
import Data.Time.Format


-- Function to test the book facility API
book_facility_test :: Int -> Int -> FilePath -> IO ()
book_facility_test facilityId slotId filePath = do
    result <- eitherDecodeFileStrict filePath :: IO (Either String T.Bookings)
    case result of
        Left err -> putStrLn $ "Error reading JSON file: " ++ err
        Right bookings -> do
                let request = setRequestPath (pack $ "/user/book_facility/" ++ show facilityId ++ "/" ++ show slotId)
                            $ setRequestHost "localhost"
                            $ setRequestPort 5000
                            $ setRequestSecure False
                            $ setRequestMethod "POST"
                            $ setRequestBodyJSON bookings
                            $ defaultRequest
                result1 <- try $ httpLBS request :: IO (Either SomeException (Response BL.ByteString))
                case result1 of
                    Left e -> handleHttpException e
                    Right response -> do
                        let statusCode = getResponseStatusCode response
                        putStrLn $ "URL: " ++ "http://localhost:5000/user/book_facility/" ++ show facilityId ++ "/" ++ show slotId
                        putStrLn $ "Status code: " ++ show statusCode
                        let responseBody = getResponseBody response
                        case eitherDecode responseBody :: Either String T.Result of
                            Left err -> putStrLn $ "Error decoding JSON: " ++ err
                            Right res -> print res

-- Function to test the cancel booking API
cancle_booking_test :: Int -> IO ()
cancle_booking_test bookingId = do
    let request = setRequestPath (pack $ "/user/cancel_booking/" ++ show bookingId)
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/cancel_booking/" ++ show bookingId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.Result of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right res -> print res


-- Function to test the get bookings API
get_bookings_test :: IO ()
get_bookings_test = do
    let request = setRequestPath "/user/bookings"
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/bookings"
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.Bookings] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right bookings -> print bookings

-- Function to test the get booking API
get_booking_test :: Int -> IO ()
get_booking_test bookingId = do
    let request = setRequestPath (pack $ "/user/booking/" ++ show bookingId)
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/booking/" ++ show bookingId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.Bookings of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right booking -> print booking


-- Function to test the get booking status API
get_booking_status_test :: Int -> IO ()
get_booking_status_test bookingId = do
    let request = setRequestPath (pack $ "/user/booking/" ++ show bookingId ++ "/status")
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/booking/" ++ show bookingId ++ "/status"
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String T.BookingStatusType of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right status -> print status


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

-- Function to test the search available slots API
search_available_slots_test :: Int -> IO ()
search_available_slots_test facilityId = do
    let request = setRequestPath (pack $ "/user/search_available_slots/" ++ show facilityId)
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/search_available_slots/" ++ show facilityId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.FacilitySlots] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right slots -> print slots


-- Function to test the search available slots by day API
search_available_slots_day_test :: Day -> Int -> IO ()
search_available_slots_day_test day facilityId = do
    let formattedDay = formatDay day
    let request = setRequestPath (pack $ "/user/search_available_slots/" ++ Tx.unpack formattedDay ++ "/" ++ show facilityId)
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
            putStrLn $ "URL: " ++ "http://localhost:5000/user/search_available_slots/" ++ Tx.unpack formattedDay ++ "/" ++ show facilityId
            putStrLn $ "Status code: " ++ show statusCode
            let responseBody = getResponseBody response
            case eitherDecode responseBody :: Either String [T.FacilitySlots] of
                Left err -> putStrLn $ "Error decoding JSON: " ++ err
                Right slots -> print slots
    where
        -- Function to format Day to Text as YYYY-MM-DD
        formatDay :: Day -> Tx.Text
        formatDay = Tx.pack . formatTime defaultTimeLocale "%Y-%m-%d"

handleHttpException :: SomeException -> IO ()
handleHttpException e = do
    putStrLn "please verify following details!"
    putStrLn $ "HTTP exception occurred: " ++ show e