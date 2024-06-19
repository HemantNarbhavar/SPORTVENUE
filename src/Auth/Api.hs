{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Api where

import Servant
import qualified Types as T
import qualified Data.Text as Tx


-- API for User Registration
-- POST http://localhost:5000/register/user
type UserRegister = 
    "register"
    :> "user"
    :> ReqBody '[JSON] T.Users
    :> Post '[JSON] T.UserId


-- API for Admin Registration
-- POST http://localhost:5000/register/admin
type AdminRegister = 
    "register"
    :> "admin"
    :> ReqBody '[JSON] T.Admins
    :> Post '[JSON] T.AdminId

-- API for User Login
-- POST http://localhost:5000/login/user
type UserLogin = 
    "login"
    :> "user"
    :> ReqBody '[JSON] T.Login
    :> Post '[JSON] T.Result


-- API for Admin Login
-- POST http://localhost:5000/login/admin
type AdminLogin = 
    "login"
    :> "admin"
    :> ReqBody '[JSON] T.Login
    :> Post '[JSON] T.Result


type AuthAPI = UserRegister :<|> UserLogin
                :<|> AdminRegister :<|> AdminLogin