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
import Data.Text.Lazy (Text, append, fromStrict, pack, toStrict, unpack)
import Data.Text.Lazy.Encoding

import Database.Esqueleto hiding (get)
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
  get  "/shoes"     listAllShoes

prepare ∷ Shoe → AppActionM Connection IO M.Shoe
prepare shoe = do
  photo' <- either (const $ raise badRequest400) return $ B64.decode $ encodeUtf8
            $ photo shoe
  dir <- getPhotoDir
  let name = BSC.unpack $ hex $ hash $ BS.toStrict photo'
  let path = dir ++ name ++ ".jpg"
  liftIO $ BS.writeFile path photo'
  shoeSize <- either (const $ raise badRequest400) return $ readEither
              $ size shoe
  return $ M.Shoe (description shoe) (color shoe) shoeSize (pack name)

runSqlM ∷ ConnectionPool → SqlPersistM a → AppActionM Connection IO a
runSqlM pool sql = do
  r ← liftIO $ handle (\e → return (e∷IOException) >> return Nothing)
      $  (Just <$>) $ runSqlPersistMPool sql pool
  maybe (raise internalServerError500) return r

makeShoes ∷ AppActionM Connection IO ()
makeShoes = do
  shoe ← jsonData `rescue` const raise badRequest400
  pool ← getPool
  shoeId ← prepare shoe >>= runSqlM pool ∘ insert
  redirect $ "/shoes/" `append` (fromStrict ∘ toPathPiece ∘ unKey $ shoeId)

showShoes ∷ AppActionM Connection IO ()
showShoes = do
  shoeId' ← param "id"
  pool ← getPool
  shoeId ← maybe (raise notFound404) return $ fromPathPiece $ toStrict shoeId'
  runSqlM pool (P.get shoeId)
    >>= maybe (raise notFound404) (html ∘ renderHtml ∘ display shoeId')
  where
    display shoeId shoe = H.docTypeHtml $ do
      let title = toHtml $ "Shoe #" `append` shoeId
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

listPage nav shoes = html ∘ renderHtml ∘ H.docTypeHtml $ do
  let title = "Shoe Listing"
  H.head $ do
    H.title title
    H.style ! A.type_ "text/css"
      $ ".shoeBox {border: 1px solid; padding: 10px; display: inline;}"
    H.style ! A.type_ "text/css" $ ".photo {height: 200px; width: 200px;}"
  H.body $ do
    H.h1 title
    nav
    shoes

displayNav page pages = H.nav $ do
  when (page > 1) $ (H.a ! A.id "prev"
                     ! A.href (toValue $ "/shoes?p=" ++ show (page - 1))
                     $ "Previous Page")
  when (page < pages) $ (H.a ! A.id "next"
                         ! A.href (toValue $ "/shoes?p=" ++ show (page + 1))
                         $ "Next Page")

displayList shoes = H.div $ forM_ shoes $ \(Entity key shoe) →
  let appendKey = (`TS.append` toPathPiece (unKey key))
  in H.a ! A.class_ "link" ! A.href (toValue $ appendKey "/shoes/")
     $ H.figure ! A.class_ "shoeBox" $ do
    H.img ! A.class_ "photo"
      ! A.src (toValue ("/" `append` M.shoePhoto shoe `append` ".jpg"))
    H.figcaption ! A.class_ "shoe" $ toHtml $ appendKey "Shoe #"

listShoes ∷ AppActionM Connection IO ()
listShoes = do
  page ← param "p"
  liftIO $ print page
  pool ← getPool
  psize ← getPageSize
  let intConv = fromInteger ∘ toInteger
  shoes ← runSqlM pool (select
                        $ from $ \shoe → do
                          orderBy [asc $ shoe ^. M.ShoeId]
                          offset $ intConv $ psize * (page - 1)
                          limit $ intConv psize
                          return shoe)
  liftIO $ print shoes
  total ← runSqlM pool $ P.count ([]∷[f (M.ShoeGeneric b)])
  liftIO $ print total
  listPage (displayNav page (total `div` psize)) (displayList shoes)

listAllShoes ∷ AppActionM Connection IO ()
listAllShoes = return ()

handleAppError ∷ Monad m ⇒ AppM conn m ()
handleAppError = defaultHandler $ \e → status e
