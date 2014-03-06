{-# LANGUAGE FlexibleContexts, GADTs, QuasiQuotes, TypeFamilies #-}

module Web.ZaloraTask.Model where

import Data.Text.Lazy (Text)

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Shoe
  description Text
  color       Text
  size        Int
  photo       Text
  deriving Eq Show
|]
