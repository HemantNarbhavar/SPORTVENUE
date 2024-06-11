{-# LANGUAGE OverloadedStrings #-}

module Querys where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant

import Types

facilities :: Connection -> Servant.Handler [Facility]
facilities conn = do   
    facilitys <- liftIO $ query_ conn "SELECT * FROM facility"
    return facilitys


add_facility :: Connection -> Facility -> Servant.Handler ()
add_facility conn facility = do
    -- liftIO $ do
        _ <- liftIO $ execute conn "INSERT INTO facility (facility_name, facility_sport, price, book_time, facility_address, created_on, updated_on, group_id ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                    ( facility_name facility
                                , facility_sport facility
                                , price facility
                                , book_time facility
                                , facility_address facility
                                , created_on facility
                                , updated_on facility
                                , group_id facility
                                )
        return ()