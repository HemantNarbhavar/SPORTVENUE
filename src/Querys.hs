{-# LANGUAGE OverloadedStrings #-}

module Querys where

import Database.PostgreSQL.Simple
import Control.Monad.IO.Class (liftIO)
import Servant

import Types

facilities :: Connection -> Servant.Handler [Facility]
facilities conn = do   
    facilitys <- liftIO $ query_ conn "SELECT * FROM facility"
    return facilitys
