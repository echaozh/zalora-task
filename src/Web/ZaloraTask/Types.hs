{-# LANGUAGE GeneralizedNewtypeDeriving, IncoherentInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Web.ZaloraTask.Types where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Pool
import Data.Text.Lazy (pack)

import Network.HTTP.Types.Status

import Prelude.Unicode

import Web.Scotty.Trans

-- config is read-only, state is read-write
data AppConfig conn = AppConfig {_photoDir ∷ FilePath, _pool ∷ Pool conn}
$(makeLenses ''AppConfig)

newtype App conn m a = App {unApp ∷ ReaderT (AppConfig conn) m a}
                     deriving (Functor, Monad, Applicative, MonadIO,
                               MonadReader (AppConfig conn))

instance ScottyError Status where
  stringError = toEnum ∘ read
  showError   = pack ∘ show

type AppM       conn m = ScottyT Status (App conn m)
type AppActionM conn m = ActionT Status (App conn m)

runApp ∷ Monad m ⇒ FilePath → Pool conn → App conn m a → m a
runApp dir pool = flip runReaderT (AppConfig (dir ++ "/") pool) ∘ unApp

getPool ∷ (MonadTrans t, Monad m) ⇒ t (App conn m) (Pool conn)
getPool = lift ∘ view $ pool

getPhotoDir ∷ (MonadTrans t, Monad m) ⇒ t (App conn m) FilePath
getPhotoDir = lift ∘ view $ photoDir
