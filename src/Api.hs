{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where

import Database.PostgreSQL.Simple
import Servant

import qualified Types 
import qualified Querys


type Add_Facility = "admin" :> 
                        "add_facility" :> 
                            ReqBody '[JSON] Types.Facility :>
                                Post '[JSON] ()

type Facilities = "facilities" :> Get '[JSON] [Types.Facility]


type Api = Facilities :<|> Add_Facility

server :: Connection -> Server Api
server conn = (Querys.facilities conn) 
                :<|> (Querys.add_facility conn)

app :: Connection -> Application
app conn = serve (Proxy :: Proxy Api) (server conn)