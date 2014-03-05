{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}

module Web.ZaloraTask.Controller where

import Control.Applicative
import Control.Monad.Reader

import Data.Aeson.TH
import Data.Text.Lazy (Text, append, fromStrict, unpack)

import Database.Persist.Sql hiding (get)

import Prelude.Unicode

import Web.PathPieces
import Web.Scotty.Trans

import qualified Web.ZaloraTask.Model as M
import Web.ZaloraTask.Types

data Shoe = Shoe {description ∷ Text, color ∷ Text, size ∷ Text, photo ∷ Text}
            deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''Shoe)

actions ∷ AppM Connection IO ()
actions = makeShoes >> showShoes >> listShoes

prepare ∷ Shoe → IO M.Shoe
prepare shoe = do
  putStrLn $ "saving photo: " ++ unpack (photo shoe)
  let p = "/path/to/photo"
  return $ M.Shoe (description shoe) (color shoe) (read ∘ unpack $ size shoe) p

makeShoes ∷ AppM Connection IO ()
makeShoes = post "/shoes" $ do
  pool ← lift ask
  shoe ← jsonData
  shoe' ← liftIO $ prepare shoe
  shoeId ← liftIO $ unKey <$> (flip runSqlPersistMPool pool $ insert shoe')
  redirect $ "/shoes/" `append` (fromStrict $ toPathPiece shoeId)

showShoes ∷ AppM Connection IO ()
showShoes = get "/shoes/:id" $ return ()

listShoes ∷ AppM Connection IO ()
listShoes = get "/shoes" $ return ()
