{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}

module Web.ZaloraTask.Controller where

import Control.Exception
import Control.Monad.Reader

import Data.Aeson.TH
import Data.Text.Lazy (Text, append, fromStrict, unpack)

import Database.Persist.Sql hiding (get)

import Network.HTTP.Types.Status

import Prelude.Unicode

import Web.PathPieces
import Web.Scotty.Trans

import qualified Web.ZaloraTask.Model as M
import Web.ZaloraTask.Types

data Shoe = Shoe {description ∷ Text, color ∷ Text, size ∷ Text, photo ∷ Text}
          deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''Shoe)

actions ∷ AppM Connection IO ()
actions = do
  handleAppError
  post "/shoes"     makeShoes
  get  "/shoes/:id" showShoes
  get  "/shoes"     listShoes

prepare ∷ Shoe → IO M.Shoe
prepare shoe = do
  putStrLn $ "saving photo: " ++ unpack (photo shoe)
  let p = "/path/to/photo"
  return $ M.Shoe (description shoe) (color shoe) (read ∘ unpack $ size shoe) p

makeShoes ∷ AppActionM Connection IO ()
makeShoes = do
  pool ← lift ask
  shoe ← jsonData `rescue` \_ → raise badRequest400
  path ← liftIO $ handle (\e → return (e∷IOException) >> return Nothing) $ do
    shoe' ← prepare shoe
    shoeId ← flip runSqlPersistMPool pool $ insert shoe'
    return $ Just $ "/shoes/" `append` (fromStrict $ toPathPiece $ unKey shoeId)
  maybe (raise internalServerError500) redirect path

showShoes ∷ AppActionM Connection IO ()
showShoes = return ()

listShoes ∷ AppActionM Connection IO ()
listShoes = return ()

handleAppError ∷ Monad m ⇒ AppM conn m ()
handleAppError = defaultHandler $ \e → status e
