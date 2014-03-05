module Web.ZaloraTask (zalora) where

import Data.ByteString (ByteString)

import Database.Persist.Postgresql (withPostgresqlPool)

import Network.Wai.Handler.Warp (Port)

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Types

zalora ∷ Port → ByteString → Int → IO ()
zalora port connStr poolSize = do
  withPostgresqlPool connStr poolSize $ \pool →
    scottyT port (runApp pool) (runApp pool) actions
