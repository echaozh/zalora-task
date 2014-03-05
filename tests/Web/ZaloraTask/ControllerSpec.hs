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
  void $ insert defaultShoe

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
  where app = scottyAppT (runApp photoDir p) (runApp photoDir p)
              $ handleAppError >> act

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
        `shouldReturn` (found302, Just "/shoes/2")

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

showShoesSpec ∷ SpecWith ConnectionPool
showShoesSpec = describe "showShoes" $ do
  let run'    = run $ get "/:id" showShoes

  context "When requesting existing shoes" $ do
    it "reports ok" $ \pool →
      simpleStatus <$> run' (reqFor "/1") pool `shouldReturn` ok200
    it "returns html with shoe data" $ \pool → do
      html ← simpleBody <$> run' (reqFor "/1") pool
      let cssText = css ⋙ (//>getText)
          cssAttr sel = (css sel ⋙) ∘ getAttrValue
      runLA (hreadDoc ⋙ (cssText "#description" &&& cssText "#color"
                         &&& cssText "#size" &&& cssAttr "#photo" "src"))
        (BS.unpack html)
        `shouldBe` [("default shoes", ("black", ("43",
                                                 "/" ++ photoName ++ ".jpg")))]

  context "When requesting non-existent shoes" $ do
    it "reports not found" $ \pool →
      simpleStatus <$> run' (reqFor "/100") pool `shouldReturn` notFound404

  context "When requesting with an improper id" $ do
    it "reports not found" $ \pool →
      simpleStatus <$> run' (reqFor "/NaN") pool `shouldReturn` notFound404

listShoesSpec ∷ SpecWith ConnectionPool
listShoesSpec = return ()
