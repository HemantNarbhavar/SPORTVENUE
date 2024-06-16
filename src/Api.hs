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

import qualified Data.Text as Tx


-- API for add facility
-- POST http://localhost/admin/add_facility
type Add_Facility =
  "admin"
    :> "add_facility"
    :> ReqBody '[JSON] T.Facility
    :> Post '[JSON] ()

-- API for update facility by facility_id
-- PUT http://localhost/admin/update_facility/<facility_id>
type Update_Facility =
  "admin"
    :> "update_facility"
    :> Capture "facility_id" Int
    :> ReqBody '[JSON] T.Facility
    :> Put '[JSON] ()

-- API for delete facility by facility_id
-- DELETE http://localhost/admin/delete_facility/<facility_id>
type Delete_Facility =
  "admin"
    :> "delete_facility"
    :> Capture "facility_id" Int
    :> Delete '[JSON] ()

-- API for Create group
-- POST http://localhost/admin/create_group
type Create_Group =
  "admin"
    :> "create_group"
    :> ReqBody '[JSON] T.Groups
    :> Post '[JSON] ()

-- PUT http://localhost/admin/update_group/add_facility/<facility_id>/<group_id>
-- API for add facility in group by facility_id and group_id
type Update_Facility_Group =
  "admin"
    :> "update_group"
    :> "add_facility"
    :> Capture "facility_id" Int
    :> Capture "group_id" Int
    :> Put '[JSON] ()

-- PUT http://localhost/admin/update_group/remove_facility/<facility_id>
-- API for remove facility in group by facility_id
type Remove_Facility_Group =
  "admin"
    :> "update_group"
    :> "remove_facility"
    :> Capture "facility_id" Int
    :> Put '[JSON] ()

-- DELETE http://localhost/admin/delete_group/<group_id>
-- API for delete group relation by group_id
type Delete_Group =
  "admin"
    :> "delete_group"
    :> Capture "group_id" Int
    :> Delete '[JSON] ()

-- POST http://locahost/admin/set_holiday/facility
-- API for set holiday by admin
type Set_Holiday =
  "admin"
    :> "set_holiday"
    :> ReqBody '[JSON] T.FacilityStatus
    :> Post '[JSON] ()

-- POST http://locahost/admin/set_holiday/group/<group_id>
-- API for set holiday to facilites of belongs to same group by group_id
type Set_Holiday_Group =
  "admin"
    :> "set_holiday"
    :> "group"
    :> Capture "group_id" Int
    :> ReqBody '[JSON] T.FacilityStatus
    :> Post '[JSON] ()

-- DELETE http://locahost/admin/remove_holiday/<holiday_id>
-- API for delete holiday from by status_id 'facility_status' Relation
type Delete_Holiday =
  "admin"
    :> "remove_holiday"
    :> Capture "status_id" Int
    :> Delete '[JSON] ()

-- PUT http://localhost/admin/group/<group_id>
-- Facility attributes that needs to be mass updated
-- API for mass update facilities by group_id 'facility' Relation
type Update_Grouped_Facilities =
    "admin"
    :> "group"
    :> Capture "group_id" Int
    :> ReqBody '[JSON] T.Facility
    :> Put '[JSON] ()

-- GET http://locahost/facilities
-- API for Get facilities List 
type Get_Facilities =
    "facilities"
    :> Get '[JSON] [T.Facility]

-- GET http://locahost/facility/<facility_id>
-- API for Get facility by facility_id
type Get_Facility =
    "facility"
    :> Capture "facility_id" Int
    :> Get '[JSON] T.Facility


-- POST http://locahost/user/book_facility/<facility_id>
-- API for Booking facility by facility_id
type Book_facility = 
    "user"
    :> "book_facility"
    :> Capture "facility_id" Int
    :> ReqBody '[JSON] T.Bookings
    :> Post '[JSON] Tx.Text


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

app :: Connection -> Application
app conn = serve (Proxy :: Proxy Api) (server conn)