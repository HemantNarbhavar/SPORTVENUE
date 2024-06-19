{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Auth.Query where


import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant
import qualified Data.ByteString.Char8 as B
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Types as T

-- Function to Register User Information
register_user :: Connection -> T.Users -> Servant.Handler T.UserId
register_user conn user = do
    let T.Users {
            user_name       = name,
            user_email      = email,
            user_pass       = pass,
            phone_number    = phone,
            user_address    = address
        } = user
    hashedPass <- createHashPass $ B.pack pass
    case hashedPass of
            Nothing -> throwError $ err500 { errBody = "Failed to hash password" }
            Just hashed -> do
                        res <- liftIO $ query conn
                            "INSERT INTO users (user_name, user_email, user_pass, phone_number, user_address) VALUES (?, ?, ?, ?, ?) RETURNING user_id"
                            (name, email, hashed, phone, address) :: Servant.Handler [Only Int]
                        return $ T.UserId $ fromOnly $ head res

-- Function to login user and authenticate a user based on provided login credentials.
login_user :: Connection -> T.Login -> Servant.Handler T.Result
login_user conn userInfo = do
    let T.Login {
            email       =   uemail,
            password    =   upassword
    } = userInfo
    res <- liftIO $ query conn
            "SELECT user_pass FROM users WHERE user_email = ?"
            [uemail]
    case res of
        [] -> throwError err404 {errBody = "Invalid User Email"}
        (Only pass):_ -> do
                        if (validateHashPassword pass $ B.pack upassword)
                            then return $ T.Result "User Login Successfuly"
                            else throwError err404 { errBody = "Incorrect password" }
                            


-- Function to Register Admin Information
register_admin :: Connection -> T.Admins -> Servant.Handler T.AdminId
register_admin conn admin = do
    let T.Admins {
            admin_name       = name,
            admin_email      = email,
            admin_pass       = pass,
            phone_number    = phone
        } = admin
    hashedPass <- createHashPass $ B.pack pass
    case hashedPass of
            Nothing -> throwError err404 { errBody = "Failed to hash password" }
            Just hashed -> do
                        res <- liftIO $ query conn
                            "INSERT INTO admins (admin_name, admin_email, admin_pass, phone_number) VALUES (?, ?, ?, ?) RETURNING admin_id"
                            (name, email, hashed, phone) :: Servant.Handler [Only Int]
                        return $ T.AdminId $ fromOnly $ head res

-- Function to login admin and authenticate a admin based on provided login credentials.
login_admin :: Connection -> T.Login -> Servant.Handler T.Result
login_admin conn adminInfo = do
    let T.Login {
            email       =   uemail,
            password    =   upassword
    } = adminInfo
    res <- liftIO $ query conn
            "SELECT admin_pass FROM admins WHERE admin_email = ?"
            [uemail]
    case res of
        [] -> throwError err404 {errBody = "Invalid Admin Email"}
        (Only pass):_ -> do
                        if (validateHashPassword pass $ B.pack upassword)
                            then return $ T.Result "Admin Login Successfuly"
                            else throwError err404 { errBody = "Incorrect password" }


-- Function to Create hash password for security
createHashPass :: B.ByteString -> Servant.Handler (Maybe B.ByteString)
createHashPass pass = liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy pass

-- Function to validate Password
validateHashPassword :: B.ByteString -> B.ByteString -> Bool
validateHashPassword hashpass pass = validatePassword hashpass pass
