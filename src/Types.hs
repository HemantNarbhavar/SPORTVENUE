{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Types where

import Data.Time.Calendar (fromGregorian)
import Data.Time (UTCTime(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)


-- Custome Data type for Facility Relation
data Facility = Facility {
    facility_id         ::  Maybe Int,
    facility_name       ::  String,
    facility_sport      ::  String,
    price               ::  Int,
    book_time           ::  TimeOfDay,
    facility_address    ::  String,
    created_on          ::  UTCTime,
    updated_on          ::  UTCTime,
    group_id            ::  Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

-- Custome Data type for Groups Relation
data Groups = Groups {
    group_id    :: Maybe Int,
    group_name  :: String,
    created_on  :: UTCTime,
    updated_on  :: UTCTime,
    admin_id    :: Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- showData :: Facility
-- showData = Facility { facility_id = (Just 1), 
--                       facility_name = "hskjsk", 
--                       facility_sport = "kjhdjs", 
--                       price = 100,
--                       book_hour = "10:00:00",
--                       facility_address = "kdlslsj",
--                       created_on = "2023-06-11 15:30:00+05:30",
--                       updated_on = "2023-06-11 15:30:00+05:30",
--                       group_id = Nothing
-- }