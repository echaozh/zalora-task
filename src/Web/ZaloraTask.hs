module Web.ZaloraTask (zalora) where

import Data.ByteString (ByteString)

import Database.Persist.Postgresql (withPostgresqlPool)

import Network.Wai.Handler.Warp (Port)

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Types

zalora :: Port -> FilePath -> ByteString -> Int -> Int -> IO ()
zalora port dir connStr poolSize pgSize = do
  withPostgresqlPool connStr poolSize $ \pool ->
    let config = AppConfig dir pool pgSize
    in scottyT port (runApp config) (runApp config) actions
