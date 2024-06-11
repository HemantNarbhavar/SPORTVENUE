{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import Database.PostgreSQL.Simple
import Servant

import qualified Types as T
import qualified Querys as Q

-- API for add facility
-- POST http://localhost/admin/add_facility
type Add_Facility = 
        "admin" 
        :> "add_facility" 
        :>  ReqBody '[JSON] T.Facility 
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
        :>  ReqBody '[JSON] T.FacilityStatus
        :>  Post '[JSON] ()

-- API for Get facilities List
-- GET http://locahost/facilities
type Facilities = 
        "facilities" 
    :> Get '[JSON] [T.Facility]


type Api = Facilities 
            :<|> Add_Facility
            :<|> Update_Facility 
            :<|> Delete_Facility
            :<|> Create_Group
            :<|> Update_Facility_Group
            :<|> Remove_Facility_Group
            :<|> Delete_Group
            :<|> Set_Holiday


server :: Connection -> Server Api
server conn = (Q.facilities conn) 
                :<|> (Q.add_facility conn)
                :<|> (Q.update_facility conn)
                :<|> (Q.delete_facility conn)
                :<|> (Q.create_group conn)
                :<|> (Q.update_facility_group conn)
                :<|> (Q.remove_facility_group conn)
                :<|> (Q.delete_group conn)
                :<|> (Q.set_holiday conn)

app :: Connection -> Application
app conn = serve (Proxy :: Proxy Api) (server conn)