{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Types where


import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField 

import Data.Time (UTCTime(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text
import Data.Text.Encoding (decodeUtf8)


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



-- Define the custom data type for the ENUM facility_ st
data FacilityStatusType = Maintenance | Holiday | BookedForSubscriber
    deriving (Show, Eq, Generic, FromJSON, ToJSON)


instance FromField FacilityStatusType where
  fromField field mdata = do
    typname <- typename field
    case (typname, mdata) of
      ("facility_status_type", Just bs) ->
        case unpack (decodeUtf8 bs) of
          "maintenance" -> return Maintenance
          "holiday" -> return Holiday
          "bookedforSubscriber" -> return BookedForSubscriber
          _ -> returnError ConversionFailed field "Unexpected FacilityStatusType value"
      _ -> returnError Incompatible field "Not a FacilityStatusType"

instance ToField FacilityStatusType where
  toField Maintenance = toField ("maintenance" :: Text)
  toField Holiday = toField ("holiday" :: Text)
  toField BookedForSubscriber = toField ("bookedforSubscriber" :: Text)


-- Custome Data type for Facility_status
data FacilityStatus = FacilityStatus
    { status_id     :: Maybe Int
    , status        :: FacilityStatusType
    , start_date    :: UTCTime
    , end_date      :: UTCTime
    , facility_id   :: Maybe Int
    } deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Custome Data type for Users 
data Users = Users {
    user_id         ::  Maybe Int,
    user_name       ::  String,
    user_email      ::  String,
    user_pass       ::  String,
    phone_number    ::  String, 
    user_address    ::  String,
    created_on      ::  UTCTime,
    updated_on      ::  UTCTime
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

-- Define the custom data type for the ENUM booking_st
data BookingStatusType = Booked | Canclled | Activate
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromField BookingStatusType where
   fromField field mdata = do
    typname <- typename field
    case (typname, mdata) of
      ("booking_status_type", Just bs) ->
        case unpack (decodeUtf8 bs) of
          "booked" -> return Booked
          "canclled" -> return Canclled
          "activate" -> return Activate
          _ -> returnError ConversionFailed field "Unexpected BookingStatusType value"
      _ -> returnError Incompatible field "Not a BookingStatusType"

instance ToField BookingStatusType where
  toField Booked = toField ("booked" :: Text)
  toField Canclled = toField ("canclled" :: Text)
  toField Activate = toField ("activate" :: Text)
  

-- Custome Data type for Bookings
data Bookings = Bookings {
    booking_id      ::  Maybe Int,
    book_time       ::  TimeOfDay,
    min_duration_hour    :: Int, 
    price           ::  Int,
    booking_status ::  BookingStatusType,
    booking_token   ::  Maybe String,
    created_on      ::  UTCTime,
    updated_on      ::  UTCTime,
    user_id         ::  Maybe Int,
    facility_id     ::  Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)