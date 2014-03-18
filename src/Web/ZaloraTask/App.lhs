  <!--

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

  -->

> module Web.ZaloraTask.App where

[Scotty](http://hackage.haskell.org/package/scotty) is an easy to use web
micro-framework. However, it doesn't come with user accessible configurations or
states, so we have to make our own. Fortunately, Scotty lets us define our own
monad and wrap it up in `ScottyT`. Because we're not doing sessions here, so a
`MonadRead` instance for read-only database connection pool and serveral startup
settings access is more than enough. For clearance, I call read-only data
"configurations", and read-write data "states".

  <!--

> import Control.Lens
>
> import Data.Default
> import Data.Functor
> import Data.Monoid
> import Data.Pool

  -->

[scottish](https://github.com/echaozh/scottish) is a library I roll myself, to
make writing `scotty` apps with configs and states easier. It also provides
helpers for database querying (for now), etc (in the future).

> import Web.Scottish
> import Web.Scottish.Database

By my convention, the directory setting and connection pool are gathered into a
record type called `AppConfig`. I am trying out `lenses`, so I'll make some.

Also make the configuration data type an instance of `HasDataConnectionPool`
from `scottish`, which makes running `persistent` transaction easier.

> -- config is read-only, state is read-write
> data AppConfig conn = AppConfig {_photoDir :: FilePath, _pool :: Pool conn,
>                                  _pageSize :: Int}
> $(makeLenses ''AppConfig)
>
> instance HasDatabaseConnectionPool conn (AppConfig conn) where
>   poolLens = pool
> instance Default (AppConfig conn) where
>   def = AppConfig undefined undefined undefined

The following monads are from `scottish` too. They use `Status` instead of the
default `Text` as the error type. Raising `Status` from `AppActionM` will be
automatically handled by setting the status of the response.

> type AppM       conn a = ScottishM'       (AppConfig conn) () a
> type AppActionM conn a = ScottishActionM' (AppConfig conn) () a

Configuratin accessors for web actions.

> getPhotoDir :: AppActionM conn FilePath
> getPhotoDir = (^.photoDir) <$> getConfig
>
> getPageSize :: AppActionM conn Int
> getPageSize = (^.pageSize) <$> getConfig
>
> setPhotoDir :: FilePath -> AppM conn ()
> setPhotoDir = modifyConfig . set photoDir . (<>"/")
>
> setPageSize :: Int -> AppM conn ()
> setPageSize = modifyConfig . set pageSize
