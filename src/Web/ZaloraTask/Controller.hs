{-# LANGUAGE StandaloneDeriving #-}

module Web.ZaloraTask.Controller where

import Control.Exception
import Control.Monad.Reader

import Crypto.Hash.MD5

import Data.Aeson.TH
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Functor
import Data.Hex
import qualified Data.Text as TS
import Data.Text.Lazy (Text, append, fromStrict, pack, unpack)
import Data.Text.Lazy.Encoding

import Database.Persist.Sql hiding (get)
import qualified Database.Persist.Sql as P

import Network.HTTP.Types.Status

import Prelude.Unicode

import Text.Blaze.Html5 ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

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
  let name = BSC.unpack $ hex $ hash $ BS.toStrict photo'
  let path = dir ++ name ++ ".jpg"
  liftIO $ BS.writeFile path photo'
  return $ M.Shoe (description shoe) (color shoe) (read ∘ unpack $ size shoe)
    $ pack name

makeShoes ∷ AppActionM Connection IO ()
makeShoes = do
  pool ← getPool
  shoe ← jsonData `rescue` \_ → raise badRequest400
  shoe' ← prepare shoe
  shoeId ← liftIO $ handle (\e → return (e∷IOException) >> return Nothing)
           $ Just ∘ fromStrict ∘ toPathPiece ∘ unKey
           <$> flip runSqlPersistMPool pool (insert shoe')
  maybe (raise internalServerError500) (redirect ∘ ("/shoes/"`append`)) shoeId

showShoes ∷ AppActionM Connection IO ()
showShoes = do
  pool ← getPool
  shoeId' ← param "id"
  shoeId ← maybe (raise notFound404) return $ (fromPathPiece ∘ TS.pack ∘ show
                                               $ (shoeId'∷Int))
  shoe ← liftIO $ handle (\e → do
                             void $ return (e∷IOException)
                             return $ Left internalServerError500)
         $ ((flip runSqlPersistMPool pool (P.get shoeId) ∷ IO (Maybe M.Shoe))
            >>= return ∘ maybe (Left notFound404) Right)
  either raise (html ∘ display shoeId') shoe
  where
    display shoeId shoe = renderHtml $ H.docTypeHtml $ do
      let title = toHtml $ "Shoe #" ++ show shoeId
      H.head $ do
        H.title $ title
        H.style ! A.type_ "text/css" $ "label {margin-right: 20px}"
      H.body $ do
        H.h1 title
        H.p $ H.img ! A.id "photo"
          ! A.src (toValue ("/" `append` M.shoePhoto shoe `append` ".jpg"))
        forM_ [("description", M.shoeDescription), ("color", M.shoeColor),
               ("size", pack ∘ show ∘ M.shoeSize)]
          $ \(s, f) → H.p $ do
          H.label ! A.for (toValue s) $ H.strong $ toHtml
            $ toUpper (head s) : map toLower (tail s) ++ ":"
          H.span ! A.id (toValue s) $ toHtml $ f shoe


listShoes ∷ AppActionM Connection IO ()
listShoes = return ()

handleAppError ∷ Monad m ⇒ AppM conn m ()
handleAppError = defaultHandler $ \e → status e
