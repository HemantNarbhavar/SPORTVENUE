{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE StandaloneDeriving #-}


module Types where


import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField 

import Data.Time (UTCTime(..))
import Data.Time.LocalTime (TimeOfDay(..))
import Database.PostgreSQL.Simple (FromRow, ToRow)
-- import Database.PostgreSQL.Simple.Time (Date)
import Data.Time.Calendar.OrdinalDate (Day)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Scientific

-- Custome Data type for Admins Relation
data Admins = Admins {
    admin_id      ::    Maybe Int,
    admin_name    ::    String,
    admin_email   ::    String,
    admin_pass    ::    String,
    phone_number  ::    Maybe String,
    created_on    ::    Maybe UTCTime,
    updated_on    ::    Maybe UTCTime
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Custome Data type for Users Relation
data Users = Users {
    user_id         ::  Maybe Int,
    user_name       ::  String,
    user_email      ::  String,
    user_pass       ::  String,
    phone_number    ::  Maybe String, 
    user_address    ::  Maybe String,
    created_on      ::  Maybe UTCTime,
    updated_on      ::  Maybe UTCTime
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Custome Data type for Groups Relation
data Groups = Groups {
    group_id    :: Maybe Int,
    group_name  :: String,
    created_on  :: Maybe UTCTime,
    updated_on  :: Maybe UTCTime,
    admin_id    :: Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)



-- Custome Data type for Facility Relation
data Facility = Facility {
    facility_id         ::  Maybe Int,
    facility_name       ::  String,
    facility_sport      ::  String,
    price_per_slot      ::  Int,
    slot_duration       ::  Int,
    open_time           ::  TimeOfDay,
    close_time          ::  TimeOfDay,
    facility_address    ::  String,
    city                ::  String,
    created_on          ::  Maybe UTCTime,
    updated_on          ::  Maybe UTCTime,
    group_id            ::  Maybe Int,
    admin_id            ::  Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Custome Data type for Slots of facility
data FacilitySlots = FacilitySlots {
    slot_id           :: Maybe Int,
    slot_start_time   :: TimeOfDay,
    slot_end_time     :: TimeOfDay,
    facility_id       :: Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Define the custom data type for the ENUM booking_st
data BookingStatusType = Booked | Canclled | Activate
    deriving (Show, Read, Enum, Eq, Generic, FromJSON, ToJSON, Bounded)


instance FromField BookingStatusType where
   fromField field mdata = do
    typname <- typename field
    case (typname, mdata) of
      ("booking_st", Just bs) ->
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
    price           ::  Maybe Int,
    booking_status  ::  BookingStatusType,
    booking_token   ::  Maybe String,
    booking_date    ::  Day,
    created_on      ::  Maybe UTCTime,
    updated_on      ::  Maybe UTCTime,
    user_id         ::  Maybe Int,
    slot_id         ::  Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Custome Data type for Booking Token for Verification
data BookingToken = BookingToken {
    booking_id    ::  Int,
    token         ::  String
} deriving (Show, Generic, FromJSON, ToJSON)


-- Custome Data type for WaitingList
data WaitingList = WaitingList {
    waitinglist_id    ::    Maybe Int,
    price             ::    Maybe Int,
    created_on        ::    Maybe UTCTime,
    updated_on        ::    Maybe UTCTime,
    user_id           ::    Maybe Int,
    slot_id           ::    Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)


-- Define the custom data type for the ENUM facility_ st
data FacilityStatusType = Maintenance | Holiday | BookedForSubscriber
    deriving (Show, Eq, Generic, FromJSON, ToJSON)


instance FromField FacilityStatusType where
  fromField field mdata = do
    typname <- typename field
    case (typname, mdata) of
      ("facility_st", Just bs) ->
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
data FacilityStatus = FacilityStatus{ 
    status_id     ::  Maybe Int,
    status        ::  FacilityStatusType,
    start_date    ::  UTCTime,
    end_date      ::  UTCTime,
    facility_id   ::  Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)



-- Custome Data type for Rating Relation
data Ratings = Ratings {
    rating_id     :: Maybe Int,
    rating        :: Scientific,
    comment       :: Maybe String,
    created_on    :: Maybe UTCTime,
    updated_on    :: Maybe UTCTime,
    user_id       :: Maybe Int,
    facility_id   :: Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)



-- Define the custom data type for the ENUM day_of_week
data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Show, Read, Enum, Eq, Generic, FromJSON, ToJSON, Bounded)
    
instance FromField DayOfWeek where
   fromField field mdata = do
    typname <- typename field
    case (typname, mdata) of
      ("day_of_week", Just bs) ->
        case unpack (decodeUtf8 bs) of
          "Sunday"    ->  return Sunday
          "Monday"    ->  return Monday
          "Tuesday"   ->  return Tuesday
          "Wednesday" ->  return Wednesday
          "Thursday"  ->  return Thursday
          "Friday"    ->  return Friday
          "Saturday"  ->  return Saturday
          _ -> returnError ConversionFailed field "Unexpected DayOfWeek value"
      _ -> returnError Incompatible field "Not a DayOfWeek"

instance ToField DayOfWeek where
  toField Sunday = toField ("Sunday" :: Text)
  toField Monday = toField ("Monday" :: Text)
  toField Tuesday = toField ("Tuesday" :: Text)
  toField Wednesday = toField ("Wednesday" :: Text)
  toField Thursday = toField ("Thursday" :: Text)
  toField Friday = toField ("Friday" :: Text)
  toField Saturday = toField ("Saturday" :: Text)


-- Custome Data type for Subscriptions Relation
data Subscriptions = Subscriptions {
    subscription_id     ::    Maybe Int,
    recurring_day       ::    DayOfWeek,
    price               ::    Maybe Int,
    start_on            ::    UTCTime,
    end_on              ::    Maybe UTCTime,
    created_on          ::    Maybe UTCTime,
    updated_on          ::    Maybe UTCTime,
    user_id             ::    Maybe Int,
    facility_id         ::    Maybe Int
} deriving (Show, Generic, FromJSON, ToJSON, FromRow, ToRow)
