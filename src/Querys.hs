{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant return" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


module Querys where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant
import qualified Data.Text as Tx

import qualified Types as T
import Data.Time.Calendar.OrdinalDate (Day)
import Text.StringRandom
import Data.String (fromString)


-- Inserts the provided facility data into the 'facility' Relation.
add_facility :: Connection -> T.Facility -> Servant.Handler Tx.Text
add_facility conn facility = do
    let T.Facility { facility_name = fname
                , facility_sport = fsport
                , price_per_slot = fprice
                , slot_duration = fslot
                , open_time = fopen_time
                , close_time = fclose_time
                , facility_address = faddress
                , city = fcity
                , admin_id = fadmin
                } = facility
    _ <- liftIO $ execute conn
        "INSERT INTO facility (facility_name, facility_sport, price_per_slot, slot_duration, open_time, close_time, facility_address, city, admin_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (fname, fsport, fprice, fslot, fopen_time, fclose_time, faddress, fcity, fadmin)
    return "Facility Added Successfuly"

-- Update the facility data by facility_id and provided facility data to 'facility' Relation.
update_facility :: Connection -> Int -> T.Facility -> Servant.Handler Tx.Text
update_facility conn facilityId facility = do
    let T.Facility { facility_name = fname
                 , facility_sport = fsport
                 , price_per_slot = fprice
                 , open_time = fopen_time
                 , close_time = fclose_time
                 , facility_address = faddress
                 , city = fcity
                 } = facility
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do 
            _ <- liftIO $ execute conn
                "UPDATE facility SET facility_name = ?, facility_sport = ?, price_per_slot = ?, open_time = ?, close_time = ?, facility_address = ?, city = ? WHERE facility_id = ?"
                (fname, fsport, fprice, fopen_time, fclose_time, faddress, fcity, facilityId)
            return "Facility Updated Successfuly"
        else throwError err404 {errBody = "Invalid facility_id"}


-- Delete the facility data by facility_id of 'facility' Relation.
delete_facility :: Connection -> Int -> Servant.Handler Tx.Text
delete_facility conn facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do
            _ <- liftIO $ execute conn
                "DELETE FROM facility WHERE facility_id = ?"
                [facilityId]
            return "Facility Deleted Successfuly"
        else throwError err404 {errBody = "Invalid facility_id"}


-- Inserts the provided group data into the 'gropus' Relation. (by admin)
create_group :: Connection -> T.Groups -> Servant.Handler Tx.Text
create_group conn group = do
    let T.Groups {  group_name = gname
                ,   admin_id    = gadmin_id
            } = group
    _ <- liftIO $ execute conn
        "INSERT INTO groups (group_name, admin_id) VALUES (?, ?)"
        (gname,gadmin_id)
    return "Facility Group Added Successfuly"

-- Update Facility group field by facility_id (updating group_id to Facility Relation) 
update_facility_group :: Connection -> Int -> Int -> Servant.Handler Tx.Text
update_facility_group conn facilityId groupId = do
    resultF <- verifyID conn "facility" "facility_id" facilityId
    resultG <- verifyID conn "groups" "group_id" groupId
    if resultF == resultG
        then do
            _ <- liftIO $ execute conn
                "UPDATE facility SET group_id = ? WHERE facility_id = ?"
                (groupId, facilityId)
            return "Facility Added to group (group_id Updated) Successfuly"
        else throwError err404 {errBody = "Invalid facility_id or group_id"}
        

-- Remove Facility group field by facility_id (removing group_id from Facility Relation) 
remove_facility_group :: Connection -> Int -> Servant.Handler Tx.Text
remove_facility_group conn facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do
            _ <- liftIO $ execute conn
                "UPDATE facility SET group_id = ? WHERE facility_id = ?"
                (Nothing :: Maybe Int, facilityId)
            return "Facility removed from group Successfuly"
        else throwError err404 {errBody = "Invalid facility_id"}


-- delete the Group data by group_id fron 'groups' Relation
delete_group :: Connection -> Int -> Servant.Handler Tx.Text
delete_group conn groupId = do
    result <- verifyID conn "groups" "group_id" groupId
    if result
        then do
            _ <- liftIO $ execute conn
                "DELETE FROM groups WHERE group_id = ?"
                [groupId]
            return "Facility Group deleted Successfuly"
        else throwError err404 {errBody = "Invalid group_id"}

-- Setting Holiday (Inserting) 'facility_status' Relation
set_holiday :: Connection -> T.FacilityStatus -> Servant.Handler Tx.Text
set_holiday conn facilityStatus = do
    let T.FacilityStatus {  status      =   fstatus
                        ,   start_date  =   startDate
                        ,   end_date    =   endDate
                        ,   facility_id =   facilityId
                        } = facilityStatus
    _ <- liftIO $ execute conn
        "INSERT INTO facility_status (status, start_date, end_date, facility_id) VALUES (?, ?, ?, ?)"
        (fstatus, startDate, endDate, facilityId)
    return "FacilityStatus (Holiday/Maintenance/BookedForSubscriber) Added in Facility Status Successfuly"

-- Setting Holiday (Inserting) according group by group_id 'facility_status' Relation
set_holiday_group :: Connection -> Int -> T.FacilityStatus-> Servant.Handler Tx.Text
set_holiday_group conn groupId facilityStatus = do
    let T.FacilityStatus {  status      =   fstatus
                        ,   start_date  =   startDate
                        ,   end_date    =   endDate
                        } = facilityStatus
    result <- verifyID conn "groups" "group_id" groupId
    if result
        then do
            _ <- liftIO $ execute conn
                "INSERT INTO facility_status (status, start_date, end_date, facility_id) SELECT ?, ?, ?, facility_id FROM facility WHERE group_id = ?"
                (fstatus, startDate, endDate, groupId)
            return "FacilityStatus Updated Successfuly"
        else throwError err404 {errBody = "Invalid group_id"}


-- delete the Group data by group_id fron 'groups' Relation
delete_holiday :: Connection -> Int -> Servant.Handler Tx.Text
delete_holiday conn statusId = do
    result <- verifyID conn "facility_status" "status_id" statusId
    if result
        then do
            _ <- liftIO $ execute conn
                "DELETE FROM facility_status WHERE status_id = ?"
                [statusId]
            return "FacilityStatus Deleted Successfuly"
        else throwError err404 {errBody = "Invalid status_id"}
        

-- Update Facilities by group_id (Updating facilities of same group of Facility Relation) 
update_grouped_facilities :: Connection -> Int -> T.Facility -> Servant.Handler Tx.Text
update_grouped_facilities conn groupId facility = do
    let T.Facility { facility_name = fname
                 , facility_sport = fsport
                 , price_per_slot = fprice
                 , open_time = fopen_time
                 , close_time = fclose_time
                 , facility_address = faddress
                 , city = fcity
                 , group_id = fgroup_id
                 } = facility
    result <- verifyID conn "groups" "group_id" groupId
    if result
        then do
            _ <- liftIO $ execute conn
                "UPDATE facility SET facility_name = ?, facility_sport = ?, price = ?, open_time = ?, close_time = ?, facility_address = ?, city = ?, group_id = ? WHERE group_id = ?"
                (fname, fsport, fprice, fopen_time, fclose_time, faddress, fcity, fgroup_id, groupId)
            return "Facilities Updated by group Successfully"
        else throwError err404 {errBody = "Invalid group_id"}


-- Get All facilities data from 'facility' Relation.
get_facilities :: Connection -> Servant.Handler [T.Facility]
get_facilities conn = do
    facilitys <- liftIO $ query_ conn
                "SELECT * FROM facility"
    return facilitys

-- Get facility data from 'facility' Relation.
get_facility :: Connection -> Int -> Servant.Handler T.Facility
get_facility conn facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do
            res <- liftIO $ query conn
                "SELECT * FROM facility WHERE facility_id = ?"
                [facilityId]
            return $ head res
        else throwError err404 {errBody = "Invalid facility_id"}


-- Book the Facility available by facility_id into the 'booking' Relation.
book_facility :: Connection -> Int -> Int -> T.Bookings -> Servant.Handler Tx.Text
book_facility conn facilityId slotId booking = do
    let T.Bookings {
        booking_status  =  bstatus,
        booking_date    =  bdate,
        user_id         =  buser_id
        } = booking
    resultU <- verifyID conn "users" "user_id" buser_id
    resultF <- verifyID conn "facility" "facility_id" facilityId
    resultFS <- verifyID conn "facility_slots" "slot_id" slotId
    if (resultF == (resultFS == resultU))
        then do 
            token <- liftIO generateToken
            bprice <- getPrice
            _ <- liftIO $ execute conn
                "INSERT INTO bookings (price, booking_status, booking_token, booking_date, user_id, slot_id) VALUES (?, ?, ?, ?, ?, ?)"
                (bprice, bstatus, token, bdate, buser_id, slotId)
            return token
        else throwError err404 {errBody = "Invalid facility_id, slot_id or user_id"}
        
    where
        -- Function that generate Random String as Token
        generateToken :: IO Tx.Text
        generateToken = stringRandomIO "[a-zA-Z0-9]{8}"
        -- Funtion that gives Price according to facility_id 
        getPrice :: Handler Int
        getPrice = do
            res <- liftIO $ query conn
                        "SELECT price_per_slot FROM facility WHERE facility_id = ?"
                        [facilityId]
            return $ fromOnly $ head res


-- Cancle the Booking by booking_id from the 'booking' Relation. (Updating it's Status to Canclled)
cancle_booking :: Connection -> Int -> Servant.Handler Tx.Text
cancle_booking conn bookingId = do
    result <- verifyID conn "bookings" "booking_id" bookingId
    if result
        then do
            res <- check_ten_minute
            if res
                then do
                    _ <- liftIO $ execute conn 
                        "UPDATE bookings SET booking_status = ? WHERE booking_id = ?" 
                        ("canclled" :: String , bookingId)
                    return "Booking Canclled Successfuly"
                else throwError err404 {errBody = "Invalid booking_id"} 
        else throwError err404 {errBody = "Invalid booking_id"} 
    where
    --     -- Function that checks booking will allow to cancle or not
        check_ten_minute :: Servant.Handler Bool
        check_ten_minute = do
            ans <- liftIO $ query conn
                    "SELECT CASE WHEN current_timestamp < created_on + INTERVAL '10 minute' THEN 'Successful' ELSE 'Unsccessful' END status FROM bookings WHERE booking_id = ?"   
                    (Only bookingId) :: Servant.Handler [Only Tx.Text] 
            case ans of
                [] -> return False
                (Only st):_ -> case st of
                                "Successful" -> return True
                                _            -> throwError err404 {errBody = "your are not able to cancle booking (10 mint)"}
                               

-- Get All bookings data from 'bookings' Relation.
get_bookings :: Connection -> Servant.Handler [T.Bookings]
get_bookings conn = do
    bookings <- liftIO $ query_ conn
                "SELECT * FROM bookings"
    return bookings


-- Get a booking data by booking_id from 'bookings' Relation.
get_booking :: Connection -> Int -> Servant.Handler T.Bookings
get_booking conn bookingId = do
    result <- verifyID conn "bookings" "booking_id" bookingId
    if result 
        then do
            booking <- liftIO $ query conn
                        "SELECT * FROM bookings WHERE booking_id = ?"
                        [bookingId]
            return $ head booking
        else throwError err404 {errBody = "Invalid booking_id"}
        

-- Get a booking status data by booking_id from 'bookings' Relation.
get_booking_status :: Connection -> Int -> Servant.Handler T.BookingStatusType
get_booking_status conn bookingId = do
    result <- verifyID conn "bookings" "booking_id" bookingId
    if result
        then do     
            status <- liftIO $ query conn
                        "SELECT booking_status FROM bookings WHERE booking_id = ?"
                        (Only bookingId) :: Servant.Handler [Only T.BookingStatusType]
            case status of
                [] -> throwError err404 {errBody = "Booking not found"}
                (Only st):_ -> return st
        else throwError err404 {errBody = "Invalid booking_id"}
 

-- Activate Booking Status from 'bookings' Relation
-- Verify and Validate Token given by user (Type -> BookingToken)
activate_booking :: Connection -> T.BookingToken -> Servant.Handler Tx.Text
activate_booking conn btoken = do
    let T.BookingToken {
        booking_id = btid,
        token = bttoken
    } = btoken
    result <- verifyID conn "bookings" "booking_id" btid
    if result
        then do
            bookingIds <- verifyToken btid bttoken
            case bookingIds of
                [] -> throwError err404 {errBody = "Invalid Token"}
                (Only bookingId):_ -> do
                                    _ <- liftIO $ execute conn
                                        "UPDATE bookings SET booking_status = ? WHERE booking_id =?"
                                        ("activate" :: String, bookingId)
                                    return "Booking Status Activated Successfuly"
        else throwError err404 {errBody = "Invalid booking_id"}
    where
        -- Function that verifies given token with bookings relation token
        verifyToken :: Int -> String -> Servant.Handler [Only Int]
        verifyToken btid bttoken= do
                bid <- liftIO $ query conn
                        "SELECT booking_id FROM bookings WHERE booking_id = ? AND booking_token = ?"
                        (btid, bttoken) :: Servant.Handler [Only Int]
                return bid


-- Inserts the provided rating data into the 'rating' Relation.
add_rating :: Connection -> Int -> T.Ratings -> Servant.Handler Tx.Text
add_rating conn facilityId rating = do
    let T.Ratings { rating = rrating
                , comment = rcomment
                , user_id   = ruser_id
                } = rating
    resultU <- verifyID conn "users" "user_id" ruser_id
    resultF <- verifyID conn "facility" "facility_id" facilityId
    if resultU == resultF
        then do
            _ <- liftIO $ execute conn
                "INSERT INTO ratings (rating, comment, user_id, facility_id) VALUES (?, ?, ?, ?)"
                -- (rrating, rcomment, rcreated_on, rupdated_on, ruser_id, facilityId)
                (rrating, rcomment, ruser_id, facilityId)
            return "Rating Added Successfuly"
        else throwError err404 {errBody = "Invalid facility_id or user_id"}

-- Get All Ratings data by facility_id from 'ratings' Relation
get_ratings :: Connection -> Int -> Servant.Handler [T.Ratings]
get_ratings conn facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do
            ratings <- liftIO $ query conn
                        "SELECT * FROM ratings WHERE facility_id = ?"
                        [facilityId]
            return ratings
        else throwError err404 {errBody = "Invalid facility_id"}

-- Get available slots by facility_id
search_available_slots :: Connection -> Int -> Servant.Handler [T.FacilitySlots]
search_available_slots conn facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result 
        then do
            slots <- liftIO $ query conn
                    "SELECT * FROM facility_slots WHERE facility_id = ? AND slot_id NOT IN (SELECT b.slot_id FROM bookings AS b)"
                    [facilityId]
            return slots
        else throwError err404 {errBody = "Invalid facility_id"}

-- Get available slots on perticular date by facility_id and day
search_available_slots_day :: Connection -> Day -> Int -> Servant.Handler [T.FacilitySlots]
search_available_slots_day conn day facilityId = do
    result <- verifyID conn "facility" "facility_id" facilityId
    if result
        then do
            slots <- liftIO $ query conn
                    "SELECT * FROM facility_slots WHERE facility_id = ? AND slot_id NOT IN (SELECT b.slot_id FROM bookings AS b WHERE b.booking_date = ?)"
                    (facilityId,day)
            return slots
        else throwError err404 {errBody = "Invalid facility_id"}


-- Get top 5 facilitys by rating
get_top_facility :: Connection -> Servant.Handler [T.Facility]
get_top_facility conn = do
    let queryStr = fromString $ "SELECT f.facility_id, f.facility_name, f.facility_sport, f.price_per_slot, f.slot_duration, f.open_time, f.close_time, f.facility_address, f.city, f.created_on, f.updated_on, f.group_id, f.admin_id " ++ 
                                "FROM facility AS f NATURAL JOIN " ++ 
                                "(SELECT facility_id, AVG(rating) AS avg_rating FROM ratings GROUP BY facility_id) AS r ORDER BY avg_rating DESC LIMIT 5"
    facilitys <- liftIO $ query_ conn queryStr
    return facilitys




-- -- Common Functions
-- -- Function for verifying facility id
-- verifyFacilityID :: Connection -> Int -> Handler Bool
-- verifyFacilityID conn facilityId = do
--     res <- liftIO $ query conn
--                 "SELECT facility_id FROM facility WHERE facility_id = ?"
--                 (Only facilityId) :: Handler [Only Int]
--     case res of
--         [] -> return False
--         (Only r):_ -> return True

-- -- Function for verifying group id
-- verifyGroupID :: Connection -> Int -> Handler Bool
-- verifyGroupID conn groupID = do
--     res <- liftIO $ query conn
--                 "SELECT group_id FROM groups WHERE group_id = ?"
--                 (Only groupID) :: Handler [Only Int]
--     case res of
--         [] -> return False
--         (Only r):_ -> return True

-- -- Function for verifying status_id
-- verifyFacilityStatusID :: Connection -> Int -> Handler Bool
-- verifyFacilityStatusID conn statusId = do
--     res <- liftIO $ query conn
--                 "SELECT status_id FROM facility_status WHERE status_id = ?"
--                 (Only statusId) :: Handler [Only Int]
--     case res of
--         [] -> return False
--         (Only r):_ -> return True

-- General Function for verifying all tables id 
verifyID :: Connection -> String -> String -> Int -> Handler Bool
verifyID conn table_name column_name column_id = do
    let queryStr = fromString $ "SELECT  " ++ column_name ++ 
                                " FROM " ++ table_name ++ 
                                " WHERE " ++ column_name ++ " = ?"
    res <- liftIO $ query conn queryStr (Only column_id) :: Handler [Only Int]
                -- ("SELECT  " ++ column_name ++ " FROM " ++ table_name ++ " WHERE " ++ column_name ++ " = ?")
    case res of
        [] -> return False
        (Only r):_ -> return True
