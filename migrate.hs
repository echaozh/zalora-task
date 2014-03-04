{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Main where

import Control.Monad.Logger

import Data.ByteString.Char8 (pack)

import Database.Persist.Postgresql hiding (migrate)

import System.Environment

import Model

main = do
  args <- getArgs
  withPostgresqlConn (pack $ head args) $ \conn ->
    runNoLoggingT . flip runSqlConn conn $ runMigration migrate
