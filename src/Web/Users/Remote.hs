{-# LANGUAGE OverloadedStrings #-}

module Web.Users.Remote where

import Database.PostgreSQL.Simple

import Web.Users.Types
import Web.Users.Postgresql ()

runServer :: IO ()
runServer = do
  conn <- connectPostgreSQL ""
  return ()
