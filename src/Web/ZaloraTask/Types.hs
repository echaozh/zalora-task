{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.ZaloraTask.Types where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Pool
import Data.Text.Lazy (pack)

import Network.HTTP.Types.Status

import Prelude.Unicode

import Web.Scotty.Trans

newtype App conn m a = App {unApp ∷ ReaderT (Pool conn) m a}
                     deriving (Functor, Monad, Applicative, MonadIO,
                               MonadReader (Pool conn))

instance ScottyError Status where
  stringError = toEnum ∘ read
  showError   = pack ∘ show

type AppM       conn m = ScottyT Status (App conn m)
type AppActionM conn m = ActionT Status (App conn m)

runApp ∷ Monad m ⇒ Pool conn → App conn m a → m a
runApp pool = flip runReaderT pool ∘ unApp

