{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.ByteString.Char8 (pack)

import Database.Persist.Postgresql hiding (migrate)

import System.Environment

import Web.ZaloraTask.Model

main = do
  args <- getArgs
  withPostgresqlConn (pack $ head args) $ \conn ->
    flip runSqlPersistM conn $ runMigration migrate
