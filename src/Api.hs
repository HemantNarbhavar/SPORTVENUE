{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module Api where



import Database.PostgreSQL.Simple
import Servant

import qualified Types 
import qualified Querys

type Facilities = "facilities" :> Get '[JSON] [Types.Facility]

type Api = Facilities -- :<|> Add_Facility


-- facilites :: Connection -> Servant.Handler [Facility]
-- facilites conn = do   
--     facilitys <- liftIO $ query_ conn "SELECT * FROM facility"
--     return facilitys

server :: Connection -> Server Api
server conn = (Querys.facilities conn) -- :<|> (add_facility conn)

app :: Connection -> Application
app conn = serve (Proxy :: Proxy Api) (server conn)