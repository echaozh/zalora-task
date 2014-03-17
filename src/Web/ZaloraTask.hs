module Web.ZaloraTask (zalora) where

import Data.ByteString (ByteString)

import Database.Persist.Postgresql (Connection, createPostgresqlPool)

import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger

import Web.Scottish
import Web.Scottish.Database

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Types

zalora :: Port -> FilePath -> ByteString -> Int -> Int -> IO ()
zalora port dir connStr poolSize pgSize = do
  scottish' port $ do
    setPool $ createPostgresqlPool connStr poolSize
    setPhotoDir dir
    setPageSize pgSize

    middleware logStdoutDev -- for development use, not to be in production

    routes
