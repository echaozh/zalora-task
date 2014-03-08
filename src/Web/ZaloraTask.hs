module Web.ZaloraTask (
  zalora,
  -- exported for testing purpose
  handleAppError) where

import Data.ByteString (ByteString)

import Database.Persist.Postgresql (Connection, withPostgresqlPool)

import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Types

zalora :: Port -> FilePath -> ByteString -> Int -> Int -> IO ()
zalora port dir connStr poolSize pgSize = do
  withPostgresqlPool connStr poolSize $ \p ->
    let config = AppConfig dir p pgSize
    in scottyT port (runApp config) (runApp config) $ do
      middleware logStdoutDev -- for development use, not to be in production
      handleAppError
      routes

handleAppError :: AppM Connection IO ()
handleAppError = defaultHandler $ \e -> status e
