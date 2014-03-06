{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}

module Web.ZaloraTask.Controller where

import Control.Exception
import Control.Monad.Reader

import Crypto.Hash.MD5

import Data.Aeson.TH
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Functor
import Data.Hex
import Data.Text.Lazy (Text, append, fromStrict, pack, unpack)
import Data.Text.Lazy.Encoding

import Database.Persist.Sql hiding (get)

import Network.HTTP.Types.Status

import Prelude.Unicode

import Web.PathPieces
import Web.Scotty.Trans

import qualified Web.ZaloraTask.Model as M
import Web.ZaloraTask.Types hiding (pool, photoDir)

data Shoe = Shoe {description ∷ Text, color ∷ Text, size ∷ Text, photo ∷ Text}
          deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''Shoe)

actions ∷ AppM Connection IO ()
actions = do
  handleAppError
  post "/shoes"     makeShoes
  get  "/shoes/:id" showShoes
  get  "/shoes"     listShoes

prepare ∷ Shoe → AppActionM Connection IO M.Shoe
prepare shoe = do
  photo' <- either (const $ raise badRequest400) return $ B64.decode $ encodeUtf8
            $ photo shoe
  dir <- getPhotoDir
  let path = dir ++ (BSC.unpack $ hex $ hash $ BS.toStrict photo') ++ ".jpg"
  liftIO $ BS.writeFile path photo'
  return $ M.Shoe (description shoe) (color shoe) (read ∘ unpack $ size shoe)
    $ pack path

makeShoes ∷ AppActionM Connection IO ()
makeShoes = do
  pool ← getPool
  shoe ← jsonData `rescue` \_ → raise badRequest400
  shoe' ← prepare shoe
  path ← liftIO $ handle (\e → return (e∷IOException) >> return Nothing)
         $ Just ∘ fromStrict ∘ toPathPiece ∘ unKey <$> ((flip runSqlPersistMPool
                                                         $ pool) $ insert shoe')
  maybe (raise internalServerError500) (redirect ∘ ("/shoes/"`append`)) path

showShoes ∷ AppActionM Connection IO ()
showShoes = return ()

listShoes ∷ AppActionM Connection IO ()
listShoes = return ()

handleAppError ∷ Monad m ⇒ AppM conn m ()
handleAppError = defaultHandler $ \e → status e
