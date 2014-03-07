  <!--

{-# LANGUAGE StandaloneDeriving #-}

  -->

> module Web.ZaloraTask.Controller (routes) where

All business logic resides here, as the controller part of MVC. `routes` is the
only function this module need to export for the server program. However, for
testing purpose, everything is exported.

If the web interface grows large, and more controllers are to be added, the
`routes` function is the only API they need to implement. The server runner
will include all routes.

  <!--

> import Control.Exception
> import Control.Monad.Reader
>
> import Crypto.Hash.MD5
>
> import Data.Aeson.TH
> import qualified Data.ByteString.Base64.Lazy as B64

  -->

As usual, lazy `Text` is the default string type, and `ByteString`s are imported
qualified.

> import qualified Data.ByteString.Lazy as BS
> import qualified Data.ByteString.Char8 as BSC

  <!--

> import Data.Functor
> import Data.Hex
> import Data.Monoid
> import Data.Ratio ((%))

  -->

> import Data.Text.Lazy (Text, fromStrict, pack, toStrict)
> import Data.Text.Lazy.Encoding

[Esqueleto](http://hackage.haskell.org/package/esqueleto) is a marvelous library,
making Haskell code looking more like SQL which most web programmers are
familiar with, greatly lowering the bar to web programming in Haskell.

Esqueleto depends on Persistent, so we use that too.

> import Database.Esqueleto hiding ((%), get)
> import qualified Database.Persist.Sql as P

  <!--

> import Network.HTTP.Types.Status
>
> import Web.PathPieces
> import Web.Scotty.Trans
>
> import qualified Web.ZaloraTask.Model as M
> import Web.ZaloraTask.Types hiding (pool, photoDir)
> import Web.ZaloraTask.View

  -->

Data type for the `POST` request. Note that `size` here is `Text` instead of
`Int` due to the mandated JSON format. And the semantic of the `photo` field is
also different, so I define another type rather than reuse the type from
`Model`.

> data Shoe = Shoe {description :: Text, color :: Text, size :: Text,
>                   photo :: Text}
>           deriving (Eq, Show)
> $(deriveFromJSON defaultOptions ''Shoe)

The controller interface. Notice there are 2 `get "/shoes"` routes, with the
first handling the paged listing, and the second as a fallthrough for the
unpaged listing.

Notice that the Haskell server doesn't serve static files or handle the `/shoes/`
(with a trailing slash) path. I use nginx to do both. I am familiar with nginx,
so I go with it for now. I guess [keter](https://github.com/snoyberg/keter) may
be able to do this and more, and I would like to try it out sometime.

You will find the sample `nginx.conf` at the root of the repo.

> routes :: AppM Connection IO ()
> routes = do
>   post "/shoes"     makeShoes
>   get  "/shoes/:id" showShoes
>   get  "/shoes"     listShoes
>   get  "/shoes"     listAllShoes

A helper function to wrap the database access which may throw `IOException`s.
Haskell exceptions are not type safe, as the code without exception handling can
compile but may fail at runtime. This is bad because you don't always know what
may get thrown at your face, and the documents are not always helpful in this
matter.

`IOExcetion`s are always internal server errors, so we raise them directly.

> runSqlM :: ConnectionPool -> SqlPersistM a -> AppActionM Connection IO a
> runSqlM pool sql = do
>   r <- liftIO $ handle (\e -> return (e::IOException) >> return Nothing)
>        $  (Just <$>) $ runSqlPersistMPool sql pool
>   maybe (raise internalServerError500) return r

When dealing with string types, treating them all as `Monoid` instances makes
the code polymorphic and better tolerate underlying implemention changes. No
matter what you're using, `String`, `ByteString`s, or `Text`s, `<>` will always
work, and can be imported from the same old module.

Scotyy handles parsing of the JSON data from the request body, but it may throw
when the parsing fails. However, all errors are automatically translated into
bad request errors, so we don't need to handle them here.

> makeShoes :: AppActionM Connection IO ()
> makeShoes = do
>   shoe <- jsonData
>   pool <- getPool
>   shoeId <- prepare shoe >>= runSqlM pool . insert
>   redirect $ "/shoes/" <> (fromStrict . toPathPiece . unKey $ shoeId)
>  where
>   prepare shoe = do
>     photo' <- either (const $ raise badRequest400) return
>               $ B64.decode . encodeUtf8 $ photo shoe
>     dir <- getPhotoDir
>     let name = BSC.unpack $ hex $ hash $ BS.toStrict photo'
>     let path = dir ++ name ++ ".jpg"
>     liftIO $ BS.writeFile path photo'
>     shoeSize <- either (const $ raise badRequest400) return $ readEither
>                 $ size shoe
>     return $ M.Shoe (description shoe) (color shoe) shoeSize (pack name)

> showShoes :: AppActionM Connection IO ()
> showShoes = do
>   shoeId' <- param "id"
>   pool <- getPool
>   shoeId <- maybe (raise notFound404) return $ fromPathPiece $ toStrict shoeId'
>   runSqlM pool (P.get shoeId)
>     >>= maybe (raise notFound404) (html . showPage shoeId')
>
> listShoes :: AppActionM Connection IO ()
> listShoes = do
>   page' <- param "p" `rescue` const next
>   page <- either (const $ raise badRequest400) return $ readEither page'
>   pool <- getPool
>   pgSize <- getPageSize
>   let intConv = fromInteger . toInteger
>   total <- runSqlM pool $ P.count ([]::[P.Filter (M.ShoeGeneric backend)])
>   shoes <- if page <= 0 || (pgSize * (page - 1) > total
>                             && (total /= 0 || page /= 1))
>            then raise notFound404
>            else runSqlM pool (select
>                               $ from $ \shoe -> do
>                                 orderBy [asc $ shoe ^. M.ShoeId]
>                                 offset $ intConv $ pgSize * (page - 1)
>                                 limit $ intConv pgSize
>                                 return shoe)
>   html $ (listLayout (navPartial page (ceiling $ total % pgSize))
>           $ listPartial shoes)
>
> listAllShoes :: AppActionM Connection IO ()
> listAllShoes = do
>   pool <- getPool
>   shoes <- runSqlM pool (select
>                          $ from $ \shoe -> do
>                            orderBy [asc $ shoe ^. M.ShoeId]
>                            return shoe)
>   html $ listLayout (return ()) (listPartial shoes)
