{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where


import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
-- import qualified MyLib (someFunc)
import qualified Api
import qualified DBconnection as DB

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  connStr <- DB.configDB
  conn <- connectPostgreSQL connStr
  putStrLn "Running at http://localhost:5000"
  run 5000 (Api.app conn)
  close conn