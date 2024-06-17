{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where


import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
-- import qualified MyLib (someFunc)
import qualified Api

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  conn <- connectPostgreSQL "host=localhost dbname=sport user=adming10x password=1234"
  putStrLn "Running at http://localhost:5000"
  run 5000 (Api.app conn)
  close conn