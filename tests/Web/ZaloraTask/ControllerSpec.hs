module Web.ZaloraTask.ControllerSpec (spec) where

import Control.Applicative
import Control.Arrow hiding (app)
import Control.Arrow.Unicode
import Control.Exception
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text.Lazy (pack)

import Database.Persist.Sqlite hiding (get, migrate)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import Prelude.Unicode

import System.Directory

import Test.Hspec

import Text.HandsomeSoup
import Text.XML.HXT.Core hiding (app)

import Web.Scotty.Trans (scottyAppT, get, post)

import Web.ZaloraTask.Controller hiding (Shoe)
import Web.ZaloraTask.Model
import Web.ZaloraTask.Types hiding (photoDir, pool)

withPool ∷ (ConnectionPool → IO a) -> (ConnectionPool → IO ()) -> IO ()
withPool bef action =
  withSqlitePool ":memory:" 1 $ bef &&& action ⋙ arr (uncurry (>>))

defaultShoe ∷ Shoe
defaultShoe = Shoe "default shoes" "black" 43 $ pack photoName

fixture ∷ ConnectionPool → IO ()
fixture = runSqlPersistMPool $ do
  runMigration migrate
  replicateM_ 5 $ insert defaultShoe

dropTable ∷ ConnectionPool → IO ()
dropTable = runSqlPersistMPool $ rawExecute "drop table shoe" []

photoDir ∷ FilePath
photoName ∷ FilePath
photoDir = "/tmp/zalora-task/"
photoName = "E7DF7CD2CA07F4F1AB415D457A6E1C13"

photoRaw ∷ String
photoEncoded ∷ String
photoRaw = "1234\n"
photoEncoded = "MTIzNAo="

spec ∷ Spec
spec =
  before (createDirectoryIfMissing True photoDir)
  $ after_ (removeDirectoryRecursive photoDir
            `catch` \e → void $ return (e∷IOException))
  $ around (withPool fixture)
  $ makeShoesSpec >> showShoesSpec >> listShoesSpec

run ∷ AppM Connection IO () → SRequest → ConnectionPool → IO SResponse
run act req p = runSession (srequest req) =<< app
  where
    app = scottyAppT (runApp config) (runApp config) $ handleAppError >> act
    config = AppConfig photoDir p 2

reqFor ∷ ByteString → SRequest
reqFor = flip SRequest "" ∘ setPath defaultRequest

makeShoesSpec ∷ SpecWith ConnectionPool
makeShoesSpec = describe "makeShoes" $ do
  let run'    = run $ post "/" makeShoes
      goodReq = SRequest defaultRequest{requestMethod="POST"}
                $ BS.pack ("{\"description\":\"just shoes\",\"color\":\"red\","
                           ++ "\"size\":\"35\",\"photo\":\"" ++ photoEncoded
                           ++ "\"}")
      badReq  = SRequest defaultRequest{requestMethod="POST"} ""

  context "When everything is fine" $ do
      it "redirects to showShoes" $ \pool →
        ((simpleStatus &&& (lookup hLocation ∘ simpleHeaders))
         <$> run' goodReq pool)
        `shouldReturn` (found302, Just "/shoes/6")

      it "saves the photo to disk base 64 decoded" $ \pool → do
        void $ run' goodReq pool
        readFile (photoDir ++ photoName ++ ".jpg") `shouldReturn` photoRaw

  context "When provided with invalid input" $ do
    it "reports bad request" $ \pool →
      simpleStatus <$> run' badReq pool `shouldReturn` badRequest400

  context "When db fails" $ do
    it "reports internal server error" $ \pool → do
      dropTable pool
      simpleStatus <$> run' goodReq pool `shouldReturn` internalServerError500

cssText ∷ String → LA XmlTree String
cssText = css ⋙ (//>getText)

cssAttr ∷ String → String → LA XmlTree String
cssAttr sel = (css sel ⋙) ∘ getAttrValue

runAndHXT ∷ (SRequest → ConnectionPool → IO SResponse)
          → SRequest → ConnectionPool → LA String b -> IO [b]
runAndHXT run' req pool hxt = do
  html' ← simpleBody <$> run' req pool
  return $ runLA hxt $ BS.unpack html'

showShoesSpec ∷ SpecWith ConnectionPool
showShoesSpec = describe "showShoes" $ do
  let run'    = run $ get "/:id" showShoes

  context "When requesting existing shoes" $ do
    it "reports ok" $ \pool →
      simpleStatus <$> run' (reqFor "/1") pool `shouldReturn` ok200
    it "returns html with shoe data" $ \pool →
      (runAndHXT run' (reqFor "/1") pool
       $ hreadDoc ⋙ (cssText "#description" &&& cssText "#color"
                     &&& cssText "#size" &&& cssAttr "#photo" "src"))
      `shouldReturn` [("default shoes", ("black",
                                         ("43", "/" ++ photoName ++ ".jpg")))]

  context "When requesting non-existent shoes" $ do
    it "reports not found" $ \pool →
      simpleStatus <$> run' (reqFor "/100") pool `shouldReturn` notFound404

  context "When requesting with an improper id" $ do
    it "reports not found" $ \pool →
      simpleStatus <$> run' (reqFor "/NaN") pool `shouldReturn` notFound404

shoeList ∷ Int → Int → [(String, (String, String))]
shoeList from to = [("Shoe #" ++ show n, ("/shoes/" ++ show n,
                                          "/" ++ photoName ++ ".jpg"))
                    | n ← [from .. to]]

listShoesSpec ∷ SpecWith ConnectionPool
listShoesSpec = describe "listShoes" $ do
  context "Without paging" $ do
    let run'    = run $ get "/" listAllShoes

    it "links to all shoes" $ \pool →
      (runAndHXT run' (reqFor "/") pool
       $ hreadDoc ⋙ (cssText ".shoe" &&& cssAttr ".link" "href"
                     &&& cssAttr ".photo" "src"))
      `shouldReturn` shoeList 1 5

  context "With paging" $ do
    let run'    = run $ get "/" listShoes

    it "links to shoes on current page" $ \pool → do
      run' (reqFor "/?p=2") pool >>= print
      (runAndHXT run' (reqFor "/?p=2") pool
       $ hreadDoc ⋙ (cssText ".shoe" &&& cssAttr ".link" "href"
                     &&& cssAttr ".photo" "src"))
        `shouldReturn` shoeList 3 4
      (runAndHXT run' (reqFor "/?p=3") pool
       $ hreadDoc ⋙ (cssText ".shoe" &&& cssAttr ".link" "href"
                     &&& cssAttr ".photo" "src"))
        `shouldReturn` shoeList 5 5

    context "and page number is not right" $
      it "reports not found" $ \pool →
      simpleStatus <$> run' (reqFor "/?=NaN") pool `shouldReturn` notFound404

    context "and a previous page" $
      it "links to that" $ \pool → do
        (runAndHXT run' (reqFor "/?p=2") pool
         $ hreadDoc ⋙ cssAttr "#prev" "href")
          `shouldReturn` ["/shoes?p=1"]
        (runAndHXT run' (reqFor "/?p=3") pool
         $ hreadDoc ⋙ cssAttr "#prev" "href")
          `shouldReturn` ["/shoes?p=2"]

    context "and a next page" $
      it "links to next page if there is one" $ \pool → do
        (runAndHXT run' (reqFor "/?p=1") pool
         $ hreadDoc ⋙ cssAttr "#next" "href")
          `shouldReturn` ["/shoes?p=2"]
        (runAndHXT run' (reqFor "/?p=2") pool
         $ hreadDoc ⋙ cssAttr "#next" "href")
          `shouldReturn` ["/shoes?p=3"]

    context "and no previous page" $
      it "doesn't have previous link" $ \pool →
      (runAndHXT run' (reqFor "/?p=1") pool $ hreadDoc
       ⋙ cssAttr "#prev" "href")
      `shouldReturn` []

    context "and no next page" $
      it "doesn't have next link" $ \pool →
      (runAndHXT run' (reqFor "/?p=3") pool $ hreadDoc
       ⋙ cssAttr "#next" "href")
      `shouldReturn` []
