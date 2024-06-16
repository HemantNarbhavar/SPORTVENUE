{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


module Querys where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant
import qualified Data.Text as Tx

import qualified Types as T

import Text.StringRandom
-- import qualified Api as T



-- Inserts the provided facility data into the 'facility' Relation.
add_facility :: Connection -> T.Facility -> Servant.Handler ()
add_facility conn facility = do
    let T.Facility { facility_name = fname
                , facility_sport = fsport
                , price = fprice
                , open_time = fopen_time
                , close_time = fclose_time
                , facility_address = faddress
                , created_on = fcreated_on
                , updated_on = fupdated_on
                , group_id = fgroup_id
                } = facility
    _ <- liftIO $ execute conn
        "INSERT INTO facility (facility_name, facility_sport, price, open_time, close_time, facility_address, created_on, updated_on, group_id ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (fname, fsport, fprice, fopen_time, fclose_time, faddress, fcreated_on, fupdated_on, fgroup_id)
    return ()

-- Update the facility data by facility_id and provided facility data to 'facility' Relation.
update_facility :: Connection -> Int -> T.Facility -> Servant.Handler ()
update_facility conn facilityId facility = do
    let T.Facility { facility_name = fname
                 , facility_sport = fsport
                 , price = fprice
                 , open_time = fopen_time
                 , close_time = fclose_time
                 , facility_address = faddress
                 , created_on = fcreated_on
                 , updated_on = fupdated_on
                 , group_id = fgroup_id
                 } = facility
    _ <- liftIO $ execute conn
        "UPDATE facility SET facility_name = ?, facility_sport = ?, price = ?, open_time = ?, close_time = ?, facility_address = ?, created_on = ?, updated_on = ?, group_id = ? WHERE facility_id = ?"
        (fname, fsport, fprice, fopen_time, fclose_time, faddress, fcreated_on, fupdated_on, fgroup_id, facilityId)
    return ()


-- Delete the facility data by facility_id of 'facility' Relation.
delete_facility :: Connection -> Int -> Servant.Handler ()
delete_facility conn facilityId = do
    _ <- liftIO $ execute conn
        "DELETE FROM facility WHERE facility_id = ?"
        [facilityId]
    return ()

-- Inserts the provided group data into the 'gropus' Relation. (by admin)
create_group :: Connection -> T.Groups -> Servant.Handler ()
create_group conn group = do
    let T.Groups {  group_name = gname
                ,   created_on  = gcreated_on
                ,   updated_on  = gupdated_on
                ,   admin_id    = gadmin_id
            } = group
    _ <- liftIO $ execute conn
        "INSERT INTO groups (group_name, created_on, updated_on, admin_id) VALUES (?, ?, ?, ?)"
        (gname, gcreated_on, gupdated_on, gadmin_id)
    return ()

-- Update Facility group field by facility_id and group_id (adding group_id into Facility Relation) 
update_facility_group :: Connection -> Int -> Int -> Servant.Handler ()
update_facility_group conn facilityId groupId = do
    _ <- liftIO $ execute conn
        "UPDATE facility SET group_id = ? WHERE facility_id = ?"
        (groupId, facilityId)
    return ()


-- Remove Facility group field by facility_id (removing group_id from Facility Relation) 
remove_facility_group :: Connection -> Int -> Servant.Handler ()
remove_facility_group conn facilityId = do
    _ <- liftIO $ execute conn
        "UPDATE facility SET group_id = ? WHERE facility_id = ?"
        (Nothing :: Maybe Int, facilityId)
    return ()


-- delete the Group data by group_id fron 'groups' Relation
delete_group :: Connection -> Int -> Servant.Handler ()
delete_group conn groupId = do
    _ <- liftIO $ execute conn
        "DELETE FROM groups WHERE group_id = ?"
        [groupId]
    return ()

-- Setting Holiday (Inserting) 'facility_status' Relation
set_holiday :: Connection -> T.FacilityStatus -> Servant.Handler ()
set_holiday conn facilityStatus = do
    let T.FacilityStatus {  status      =   fstatus
                        ,   start_date  =   startDate
                        ,   end_date    =   endDate
                        ,   facility_id =   facilityId
                        } = facilityStatus
    _ <- liftIO $ execute conn
        "INSERT INTO facility_status (status, start_date, end_date, facility_id) VALUES (?, ?, ?, ?)"
        (fstatus, startDate, endDate, facilityId)
    return ()

-- Setting Holiday (Inserting) according group by group_id 'facility_status' Relation
set_holiday_group :: Connection -> Int -> T.FacilityStatus-> Servant.Handler ()
set_holiday_group conn groupId facilityStatus = do
    let T.FacilityStatus {  status      =   fstatus
                        ,   start_date  =   startDate
                        ,   end_date    =   endDate
                        } = facilityStatus
    _ <- liftIO $ execute conn
        "INSERT INTO facility_status (status, start_date, end_date, facility_id) SELECT ?, ?, ?, facility_id FROM facility WHERE group_id = ?"
        (fstatus, startDate, endDate, groupId)
    return ()


-- delete the Group data by group_id fron 'groups' Relation
delete_holiday :: Connection -> Int -> Servant.Handler ()
delete_holiday conn statusId = do
    _ <- liftIO $ execute conn
        "DELETE FROM facility_status WHERE status_id = ?"
        [statusId]
    return ()



-- Update Facilities by group_id (Updating facilities of same group of Facility Relation) 
update_grouped_facilities :: Connection -> Int -> T.Facility -> Servant.Handler ()
update_grouped_facilities conn groupId facility = do
    let T.Facility { facility_name = fname
                 , facility_sport = fsport
                 , price = fprice
                 , open_time = fopen_time
                 , close_time = fclose_time
                 , facility_address = faddress
                 , created_on = fcreated_on
                 , updated_on = fupdated_on
                 , group_id = fgroup_id
                 } = facility
    _ <- liftIO $ execute conn
        "UPDATE facility SET facility_name = ?, facility_sport = ?, price = ?, open_time = ?, close_time = ?, facility_address = ?, created_on = ?, updated_on = ?, group_id = ? WHERE group_id = ?"
        (fname, fsport, fprice, fopen_time, fclose_time, faddress, fcreated_on, fupdated_on, fgroup_id, groupId)
    return ()


-- Get All facilities data from 'facility' Relation.
get_facilities :: Connection -> Servant.Handler [T.Facility]
get_facilities conn = do
    facilitys <- liftIO $ query_ conn
                "SELECT * FROM facility"
    return facilitys

-- Get facility data from 'facility' Relation.
get_facility :: Connection -> Int -> Servant.Handler T.Facility
get_facility conn facilityId = do
    res <- liftIO $ query conn
           "SELECT * FROM facility WHERE facility_id = ?"
           [facilityId]
    return $ head res

-- Book the Facility available by facility_id into the 'booking' Relation.
book_facility :: Connection -> Int -> T.Bookings -> Servant.Handler Tx.Text
book_facility conn facilityId booking = do
    let T.Bookings {
        book_time       =  btime,
        min_duration_hour = bhour,
        -- price           =  bprice,
        booking_status =  bstatus,
        created_on      =  bcreated_on,
        updated_on      =  bupdated_on,
        user_id         =  buser_id
        } = booking
    token <- liftIO generateToken
    bprice <- getPrice
    _ <- liftIO $ execute conn
        "INSERT INTO bookings (book_time, min_duration_hour, price, booking_status, booking_token, created_on, updated_on, user_id, facility_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (btime, bhour, bprice * bhour, bstatus, token, bcreated_on, bupdated_on, buser_id, facilityId)
    return token
    where
        -- Function that generate Random String as Token
        generateToken :: IO Tx.Text
        generateToken = stringRandomIO "[a-zA-Z0-9]{8}"
        -- Funtion that gives Price according to facility_id 
        getPrice :: Handler Int
        getPrice = do
            res <- liftIO $ query conn
                        "SELECT price FROM facility WHERE facility_id = ?"
                        [facilityId]
            return $ fromOnly $ head res



-- get_facility :: Connection -> Int -> Servant.Handler T.Facility
-- get_facility conn facilityId = do
--     res <- liftIO $ query conn
--            "SELECT * FROM facility WHERE facility_id = ?"
--            [facilityId]
--     return $ head res