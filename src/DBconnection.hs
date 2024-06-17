{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module DBconnection where

import GHC.Generics (Generic)
import System.Exit (exitFailure)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
-- import Data.Text
import qualified Data.ByteString.Char8 as BS

data DBConfig = DBConfig {
    host       ::   String,
    dbname     ::   String,
    user       ::   String,
    password   ::   String
} deriving (Show, Generic, FromJSON)

loadDBCongif :: FilePath -> IO (Maybe DBConfig)
loadDBCongif path = do
    configData <- B.readFile path
    return $ decode configData

configDB :: IO BS.ByteString
configDB = do
    config <- loadDBCongif "config.json"
    case config of
        Nothing -> do
            putStrLn "Error loading config"
            exitFailure
        Just cfg -> do
            let connStr = "host=" ++ host cfg ++
                          " dbname=" ++ dbname cfg ++
                          " user=" ++ user cfg ++
                          " password=" ++ password cfg
            return $ BS.pack connStr