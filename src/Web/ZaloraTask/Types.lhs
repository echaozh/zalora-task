  <!--

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

  -->

> module Web.ZaloraTask.Types where

[Scotty](http://hackage.haskell.org/package/scotty) is an easy to use web
micro-framework. However, it doesn't come with user accessible configurations or
states, so we have to make our own. Fortunately, Scotty lets us define our own
monad and wrap it up in `ScottyT`. Because we're not doing sessions here, so a
`MonadRead` instance for read-only database connection pool and serveral startup
settings access is more than enough. For clearance, I call read-only data
"configurations", and read-write data "states".

  <!--

> import Control.Applicative
> import Control.Lens
> import Control.Monad.IO.Class
> import Control.Monad.Reader
>
> import Data.Pool

  -->

Lazy `Text` is used throughout the Scotty library, so I treat it as the default
string type, and import its operations which don't conflict with `Prelude`
directly into our namespace. `ByteString`s' and strict `Text`'s modules are
always imported qualified.

> import Data.Text.Lazy (pack)

  <!--

> import Network.HTTP.Types.Status

> import Web.Scotty.Trans

  -->

By my convention, the directory setting and connection pool are gathered into a
record type called `AppConfig`. I am trying out `lenses`, so I'll make some.

> -- config is read-only, state is read-write
> data AppConfig conn = AppConfig {_photoDir :: FilePath, _pool :: Pool conn,
>                                  _pageSize :: Int}
> $(makeLenses ''AppConfig)

The `App` monad carries the configurations around.

> newtype App conn m a = App {unApp :: ReaderT (AppConfig conn) m a}
>                      deriving (Functor, Monad, Applicative, MonadIO,
>                                MonadReader (AppConfig conn))

I'd like to `raise` HTTP status codes as `ScottyError`s so actions can early exit
with an abnormal status code. So let's make `Status` as an instance of the
`ScottyError` class. I am using `readEither` from Scotty in places where `read`
may fail, glad it comes free with the framework.

For now, unless you go to wild pages out of control of the `Controller` and land
on a Scotty generated 404 page, you will only see the status code with the
default reason message.

> instance ScottyError Status where
>   -- for now, Scotty only `raise`s when input is bad
>   stringError = either (const badRequest400) id . (toEnum<$>) . readEither
>                 . pack
>   showError   = pack . show

The wrapped application and action monads. Notice the `Status` as the error type
in `ScottyT`s type parameters.

We also define the `runApp` function to strip the outer monads and go back to the
base monad, which may, most likely, be the IO monad.

> type AppM       conn m = ScottyT Status (App conn m)
> type AppActionM conn m = ActionT Status (App conn m)
>
> runApp :: Monad m => AppConfig conn -> App conn m a -> m a
> runApp config = flip runReaderT (config & photoDir %~ (++"/")) . unApp

Configuratin accessors for web actions.

> getPool :: (MonadTrans t, Monad m) => t (App conn m) (Pool conn)
> getPool = lift . view $ pool
>
> getPhotoDir :: (MonadTrans t, Monad m) => t (App conn m) FilePath
> getPhotoDir = lift . view $ photoDir
>
> getPageSize :: (MonadTrans t, Monad m) => t (App conn m) Int
> getPageSize = lift . view $ pageSize
