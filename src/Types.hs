{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

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

-- data Facility = Facility
--   { facilityId      :: Maybe Int
--   , facilityName    :: String
--   , facilitySport   :: String
--   , price           :: Int
--   , book_hour        :: TimeOfDay
--   , facilityAddress :: String
--   , createdOn       :: UTCTime
--   , updatedOn       :: UTCTime
--   , groupId         :: Maybe Int
--   } deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

-- showFacility :: Facility
-- showFacility = Facility
--                     { facilityId = Just 1
--                     , facilityName = "Sample Facility"
--                     , facilitySport = "Football"
--                     , price = 100
--                     , book_hour = TimeOfDay 10 0 0 -- Example time: 10:00:00
--                     , facilityAddress = "123 Main Street"
--                     , createdOn = UTCTime (fromGregorian 2022 6 16) 0 -- Example UTC time: June 16, 2022
--                     , updatedOn = UTCTime (fromGregorian 2022 6 16) 0 -- Example UTC time: June 17, 2022
--                     , groupId = Just 123
--                     }


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