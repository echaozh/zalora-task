{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, UnicodeSyntax #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class

import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool)
import Data.Text.Lazy (Text, append, fromStrict, pack, unpack)

import Database.Persist.Sql hiding (get)
import Database.Persist.Postgresql (withPostgresqlPool)

import Prelude.Unicode

import System.Environment (getArgs)

import Web.PathPieces
import Web.Scotty.Trans

import qualified Model as M

newtype App conn m a = App {unApp ∷ ReaderT (Pool conn) m a}
                     deriving (Functor, Monad, Applicative,
                               MonadReader (Pool conn))

deriving instance MonadIO m ⇒ MonadIO (App conn m)

type AppM       conn m = ScottyT Text (App conn m)
type AppActionM conn m = ActionT Text (App conn m)

runApp ∷ Monad m ⇒ Pool conn → App conn m a → m a
runApp pool = flip runReaderT pool ∘ unApp

data Shoe = Shoe {description ∷ Text, color ∷ Text, size ∷ Text, photo ∷ Text}
            deriving (Eq, Show)
$(deriveFromJSON defaultOptions ''Shoe)

main ∷ IO ()
main = do
  port : connStr : _ ← getArgs
  withPostgresqlPool (BS.pack connStr) 10 $ \pool → do
    scottyT (read port) (runApp pool) (runApp pool) $ do
      post "/shoes"    $ makeShoes pool
      get "/shoes/:id" $ showShoes pool
      get "/shoes"     $ listShoes pool

prepare ∷ Shoe → IO M.Shoe
prepare shoe = do
  putStrLn $ "saving photo: " ++ unpack (photo shoe)
  let p = "/path/to/photo"
  return $ M.Shoe (description shoe) (color shoe) (read ∘ unpack $ size shoe) p

makeShoes ∷ Pool Connection → AppActionM Connection IO ()
makeShoes pool = do
  shoe ← jsonData
  shoe' ← liftIO $ prepare shoe
  id ← liftIO $ unKey <$> (flip runSqlPersistMPool pool $ insert shoe')
  redirect $ "/shoes/" `append` (fromStrict $ toPathPiece id)

showShoes ∷ Pool Connection → AppActionM Connection IO ()
showShoes pool = return ()

listShoes ∷ Pool Connection → AppActionM Connection IO ()
listShoes pool = return ()
