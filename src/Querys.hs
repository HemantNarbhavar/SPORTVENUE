{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Querys where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant

import qualified Types as T


-- Inserts the provided facility data into the 'facility' Relation.
add_facility :: Connection -> T.Facility -> Servant.Handler ()
add_facility conn facility = do
    let T.Facility { facility_name = fname
                , facility_sport = fsport
                , price = fprice
                , book_time = fbook_time
                , facility_address = faddress
                , created_on = fcreated_on
                , updated_on = fupdated_on
                , group_id = fgroup_id
                } = facility
    _ <- liftIO $ execute conn 
        "INSERT INTO facility (facility_name, facility_sport, price, book_time, facility_address, created_on, updated_on, group_id ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (fname, fsport, fprice, fbook_time, faddress, fcreated_on, fupdated_on, fgroup_id)
    return ()

-- Update the facility data by facility_id and provided facility data to 'facility' Relation.
update_facility :: Connection -> Int -> T.Facility -> Servant.Handler ()
update_facility conn facilityId facility = do
    let T.Facility { facility_name = fname
                 , facility_sport = fsport
                 , price = fprice
                 , book_time = fbook_time
                 , facility_address = faddress
                 , created_on = fcreated_on
                 , updated_on = fupdated_on
                 , group_id = fgroup_id
                 } = facility
    _ <- liftIO $ execute conn 
        "UPDATE facility SET facility_name = ?, facility_sport = ?, price = ?, book_time = ?, facility_address = ?, created_on = ?, updated_on = ?, group_id = ? WHERE facility_id = ?"
        (fname, fsport, fprice, fbook_time, faddress, fcreated_on, fupdated_on, fgroup_id, facilityId)
    return ()


-- Delete the facility data by facility_id of 'facility' Relation.
delete_facility :: Connection -> Int -> Servant.Handler ()
delete_facility conn facilityId = do
    _ <- liftIO $ execute conn "DELETE FROM facility WHERE facility_id = ?"
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

-- Get All facilitys data from 'facility' Relation.
facilities :: Connection -> Servant.Handler [T.Facility]
facilities conn = do   
    facilitys <- liftIO $ query_ conn "SELECT * FROM facility"
    return facilitys

