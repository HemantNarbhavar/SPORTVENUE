{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Api where

import Database.PostgreSQL.Simple
import qualified Querys as Q
import Servant
import qualified Types as T
import Data.Time.Calendar.OrdinalDate (Day)
import qualified Data.Text as Tx
import qualified Types as Tx


-- API for add facility
-- POST http://localhost:5000/admin/add_facility
type Add_Facility =
  "admin"
    :> "add_facility"
    :> ReqBody '[JSON] T.Facility
    :> Post '[JSON] Tx.Text

-- API for update facility by facility_id
-- PUT http://localhost:5000/admin/update_facility/<facility_id>
type Update_Facility =
  "admin"
    :> "update_facility"
    :> Capture "facility_id" Int
    :> ReqBody '[JSON] T.Facility
    :> Put '[JSON] Tx.Text

-- API for delete facility by facility_id
-- DELETE http://localhost:5000/admin/delete_facility/<facility_id>
type Delete_Facility =
  "admin"
    :> "delete_facility"
    :> Capture "facility_id" Int
    :> Delete '[JSON] Tx.Text

-- API for Create group
-- POST http://localhost:5000/admin/create_group
type Create_Group =
  "admin"
    :> "create_group"
    :> ReqBody '[JSON] T.Groups
    :> Post '[JSON] Tx.Text

-- PUT http://localhost:5000/admin/update_group/add_facility/<facility_id>/<group_id>
-- API for add facility in group by facility_id and group_id
type Update_Facility_Group =
  "admin"
    :> "update_group"
    :> "add_facility"
    :> Capture "facility_id" Int
    :> Capture "group_id" Int
    :> Put '[JSON] Tx.Text

-- PUT http://localhost:5000/admin/update_group/remove_facility/<facility_id>
-- API for remove facility in group by facility_id
type Remove_Facility_Group =
  "admin"
    :> "update_group"
    :> "remove_facility"
    :> Capture "facility_id" Int
    :> Put '[JSON] Tx.Text

-- DELETE http://localhost:5000/admin/delete_group/<group_id>
-- API for delete group relation by group_id
type Delete_Group =
  "admin"
    :> "delete_group"
    :> Capture "group_id" Int
    :> Delete '[JSON] Tx.Text

-- POST http://localhost:5000/admin/set_holiday/facility
-- API for set holiday by admin
type Set_Holiday =
  "admin"
    :> "set_holiday"
    :> ReqBody '[JSON] T.FacilityStatus
    :> Post '[JSON] Tx.Text

-- POST http://localhost:5000/admin/set_holiday/group/<group_id>
-- API for set holiday to facilites of belongs to same group by group_id
type Set_Holiday_Group =
  "admin"
    :> "set_holiday"
    :> "group"
    :> Capture "group_id" Int
    :> ReqBody '[JSON] T.FacilityStatus
    :> Post '[JSON] Tx.Text

-- DELETE http://localhost:5000/admin/remove_holiday/<holiday_id>
-- API for delete holiday from by status_id 'facility_status' Relation
type Delete_Holiday =
  "admin"
    :> "remove_holiday"
    :> Capture "status_id" Int
    :> Delete '[JSON] Tx.Text

-- PUT http://localhost:5000/admin/group/<group_id>
-- Facility attributes that needs to be mass updated
-- API for mass update facilities by group_id 'facility' Relation
type Update_Grouped_Facilities =
    "admin"
    :> "group"
    :> Capture "group_id" Int
    :> ReqBody '[JSON] T.Facility
    :> Put '[JSON] ()

-- GET http://localhost:5000/facilities
-- API for Get facilities List 
type Get_Facilities =
    "facilities"
    :> Get '[JSON] [T.Facility]

-- GET http://localhost:5000/facility/<facility_id>
-- API for Get facility by facility_id
type Get_Facility =
    "facility"
    :> Capture "facility_id" Int
    :> Get '[JSON] T.Facility


-- POST http://localhost:5000/user/book_facility/<facility_id>/<slot_id>
-- API for Booking facility by facility_id
type Book_facility = 
    "user"
    :> "book_facility"
    :> Capture "facility_id" Int
    :> Capture "slot_id" Int
    :> ReqBody '[JSON] T.Bookings
    :> Post '[JSON] Tx.Text


-- PUT http://localhost:5000/user/cancel_booking/<booking_id>
-- API for cancle booking by booking_id
type Cancle_Booking = 
    "user"
    :> "cancle_booking"
    :> Capture "booking_id" Int
    :> Put '[JSON] Tx.Text

-- GET http://localhost:5000/user/bookings
-- API for get all bookings from 'bookings' Relation
type Get_Bookings = 
   "user"
   :> "bookings"
   :> Get '[JSON] [T.Bookings]

-- GET http://localhost:5000/user/booking/<booking_id>
-- API for get booking by booking_id from 'bookings' Relation
type Get_Booking =
   "user"
   :> "booking"
   :> Capture "booking_id" Int
   :> Get '[JSON] T.Bookings

-- GET http://localhost:5000/user/booking/<booking_id>/status
-- API for get booking status by booking_id from 'bookings' Relation
type Get_Booking_Status =
   "user"
   :> "booking"
   :> Capture "booking_id" Int
   :> "status"
   :> Get '[JSON] T.BookingStatusType

-- POST http://localhost:5000/user/booking/activate
-- API for activate booking using Token 
type Activate_Booking = 
   "user"
   :> "booking"
   :> "activate"
   :> ReqBody '[JSON] Tx.BookingToken
   :> Put '[JSON] Tx.Text


-- POST http://localhost:5000/user/add_facility_rating/<facility_id>
-- API for adding rating to facility in 'ratings' Relation
type Add_Rating = 
   "user"
   :> "add_facility_rating"
   :> Capture "facility_id" Int
   :> ReqBody '[JSON] Tx.Ratings
   :> Post '[JSON] ()

-- GET http://localhost:5000/facility_ratings/<facility_id>
-- API for get ratings by facility_id from 'ratings' Relation
type Get_Ratings = 
   "facility_ratings"
   :> Capture "facility_id" Int
   :> Get '[JSON] [Tx.Ratings]

-- GET http://localhost:5000/facility_ratings/top_5
-- API for get top 5 facility by rating
type Get_Top_Facility = 
    "facility_ratings"
    :> "top_5"
    :> Get '[JSON] [Tx.Facility]


-- Get http://localhost:5000/user/search_available_slots/<facility_id>
-- API for search slots for given facility by facility_id
type Search_Available_Slots =
   "user"
   :> "search_available_slots"
   :> Capture "facility_id" Int
   :> Get '[JSON] [Tx.FacilitySlots] 

-- Get http://localhost:5000/user/search_available_slots/<date>/<facility_id>
-- API for search slots on perticular date by day and facility_id
type Search_Available_Slots_Day = 
   "user"
   :> "search_available_slots"
   :> Capture "date" Day
   :> Capture "facility_id" Int
   :> Get '[JSON] [Tx.FacilitySlots] 




type Api =
  Get_Facilities
    :<|> Add_Facility
    :<|> Update_Facility
    :<|> Delete_Facility
    :<|> Create_Group
    :<|> Update_Facility_Group
    :<|> Remove_Facility_Group
    :<|> Delete_Group
    :<|> Set_Holiday
    :<|> Set_Holiday_Group
    :<|> Delete_Holiday
    :<|> Update_Grouped_Facilities
    :<|> Get_Facility
    :<|> Book_facility
    :<|> Cancle_Booking
    :<|> Get_Bookings
    :<|> Get_Booking
    :<|> Get_Booking_Status
    :<|> Activate_Booking
    :<|> Add_Rating
    :<|> Get_Ratings
    :<|> Get_Top_Facility
    :<|> Search_Available_Slots
    :<|> Search_Available_Slots_Day


server :: Connection -> Server Api
server conn =
  (Q.get_facilities conn)
    :<|> (Q.add_facility conn)
    :<|> (Q.update_facility conn)
    :<|> (Q.delete_facility conn)
    :<|> (Q.create_group conn)
    :<|> (Q.update_facility_group conn)
    :<|> (Q.remove_facility_group conn)
    :<|> (Q.delete_group conn)
    :<|> (Q.set_holiday conn)
    :<|> (Q.set_holiday_group conn)
    :<|> (Q.delete_holiday conn)
    :<|> (Q.update_grouped_facilities conn)
    :<|> (Q.get_facility conn)
    :<|> (Q.book_facility conn)
    :<|> (Q.cancle_booking conn)
    :<|> (Q.get_bookings conn)
    :<|> (Q.get_booking conn)
    :<|> (Q.get_booking_status conn)
    :<|> (Q.activate_booking conn)
    :<|> (Q.add_rating conn)
    :<|> (Q.get_ratings conn)
    :<|> (Q.get_top_facility conn)
    :<|> (Q.search_available_slots conn)
    :<|> (Q.search_available_slots_day conn)



app :: Connection -> Application
app conn = serve (Proxy :: Proxy Api) (server conn)