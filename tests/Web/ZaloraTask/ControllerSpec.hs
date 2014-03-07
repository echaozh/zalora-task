module Web.ZaloraTask.ControllerSpec (spec) where

import Control.Applicative
import Control.Arrow hiding (app)
import Control.Arrow.Unicode
import Control.Exception
import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Composition
import Data.List (transpose)
import Data.Monoid
import Data.String
import Data.Text.Lazy (Text, pack)

import Database.Persist.Sqlite hiding (get, migrate)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import System.Directory

import Test.Hspec

import Text.HandsomeSoup
import Text.XML.HXT.Core hiding (app)

import Web.Scotty.Trans (scottyAppT)

import Web.ZaloraTask (handleAppError)
import Web.ZaloraTask.Controller
import Web.ZaloraTask.Model
import Web.ZaloraTask.Types hiding (photoDir, pool)

spec :: Spec
spec =
  before (createDirectoryIfMissing True photoDir)
  $ after_ (removeDirectoryRecursive photoDir
            `catch` \e -> void $ return (e::IOException))
  $ around (withPool fixture)
  $ makeShoesSpec >> showShoesSpec >> listShoesSpec

makeShoesSpec :: SpecWith ConnectionPool
makeShoesSpec = describe "makeShoes" $ do
  let goodReq = SRequest (setPath defaultRequest{requestMethod="POST"} "/shoes")
                $ BSC.pack ("{\"description\":\"just shoes\",\"color\":\"red\","
                           ++ "\"size\":\"35\",\"photo\":\"" ++ photoEncoded
                           ++ "\"}")
      badReq  = SRequest (setPath defaultRequest{requestMethod="POST"} "/shoes")
                $ ""

  context "When everything is fine" $ do
      it "redirects to showShoes" $ \pool -> do
        resp <- run goodReq pool
        simpleStatus resp `shouldBe` found302
        lookup hLocation (simpleHeaders resp) `shouldBe` Just "/shoes/6"

      it "saves the photo to disk base 64 decoded" $ \pool -> do
        void $ run goodReq pool
        readFile (photoDir <> photoPathFor photoName) `shouldReturn` photoRaw

  context "When provided with invalid input" $ do
    it "reports bad request" $ \pool ->
      statusFor badReq pool `shouldReturn` badRequest400

  context "When db fails" $ do
    it "reports internal server error" $ \pool -> do
      dropTable pool
      statusFor goodReq pool `shouldReturn` internalServerError500

showShoesSpec :: SpecWith ConnectionPool
showShoesSpec = describe "showShoes" $ do
  context "When requesting existing shoes" $ do
    it "reports ok" $ \pool ->
      statusFor' "/1" pool `shouldReturn` ok200

    it "returns html with shoe data" $ \pool ->
      map head <$> sequence (map (raht "/1" pool) ["#description", "#color",
                                                   "#size"]
                             ++ [raha "/1" pool "#photo" "src"])
         `shouldReturn` parsedShoe

  context "When requesting non-existent shoes" $ do
    it "reports not found" $ \pool ->
      statusFor' "/100" pool `shouldReturn` notFound404

  context "When requesting with an improper id" $ do
    it "reports not found" $ \pool ->
      statusFor' "/NaN" pool `shouldReturn` notFound404

listShoesSpec :: SpecWith ConnectionPool
listShoesSpec = describe "listShoes" $ do
  let pathFor = ("?p="<>) . (show::Int -> String)

  context "Without paging" $ do
    it "links to all shoes" $ \pool ->
      extractShoeList "" pool `shouldReturn` shoeList 1 5

  context "With paging" $ do
    it "links to shoes on current page" $ \pool -> do
      extractShoeList (pathFor 2) pool `shouldReturn` shoeList 3 4
      extractShoeList (pathFor 3) pool `shouldReturn` shoeList 5 5

    context "and page number is not right" $ do
      it "reports not found for non-existent page" $ \pool ->
        statusFor' (pathFor 100) pool `shouldReturn` notFound404

      it "reports bad request for invalid page number" $ \pool ->
        statusFor' "?p=NaN" pool `shouldReturn` badRequest400

    context "and a previous page" $
      it "links to that" $ \pool -> do
        raha (pathFor 2) pool "#prev" "href" `shouldReturn` ["/shoes?p=1"]
        raha (pathFor 3) pool "#prev" "href" `shouldReturn` ["/shoes?p=2"]

    context "and a next page" $
      it "links to next page if there is one" $ \pool -> do
        raha (pathFor 1) pool "#next" "href" `shouldReturn` ["/shoes?p=2"]
        raha (pathFor 2) pool "#next" "href" `shouldReturn` ["/shoes?p=3"]

    context "and no previous page" $
      it "doesn't have previous link" $ \pool ->
      raha (pathFor 1) pool "#prev" "href" `shouldReturn` []

    context "and no next page" $
      it "doesn't have next link" $ \pool ->
      raha (pathFor 3) pool "#next" "href" `shouldReturn` []

defaultShoe :: Shoe
defaultShoe = Shoe "default shoes" "black" 43 (pack photoName)

parsedShoe :: [Text]
parsedShoe = map ($ defaultShoe) [shoeDescription, shoeColor,
                                  pack . show . shoeSize,
                                  photoPathFor . shoePhoto]

photoDir :: FilePath
photoName :: String
photoDir = "/tmp/zalora-task/"
photoName = "E7DF7CD2CA07F4F1AB415D457A6E1C13"

photoPathFor :: (IsString s, Monoid s) => s -> s
photoPathFor s = "/" <> s <> ".jpg"

photoRaw :: String
photoEncoded :: String
photoRaw = "1234\n"
photoEncoded = "MTIzNAo="

withPool :: (ConnectionPool -> IO a) -> (ConnectionPool -> IO ()) -> IO ()
withPool bef action =
  withSqlitePool ":memory:" 1 $ bef &&& action ⋙ arr (uncurry (>>))

fixture :: ConnectionPool -> IO ()
fixture = runSqlPersistMPool $ do
  runMigration migrate
  replicateM_ 5 $ insert defaultShoe

dropTable :: ConnectionPool -> IO ()
dropTable = runSqlPersistMPool $ rawExecute "drop table shoe" []

run :: SRequest -> ConnectionPool -> IO SResponse
run req p = runSession (srequest req) =<< app
  where
    app = scottyAppT (runApp config) (runApp config) $ handleAppError >> routes
    config = AppConfig photoDir p 2

statusFor :: SRequest -> ConnectionPool -> IO Status
statusFor = (simpleStatus<$>) .: run

statusFor' :: String -> ConnectionPool -> IO Status
statusFor' path = (simpleStatus<$>) . run (reqFor path)

reqFor :: String -> SRequest
reqFor = flip SRequest "" . setPath defaultRequest . BS.pack . ("/shoes"<>)

cssText :: String -> LA XmlTree String
cssText = css >>> (//>getText)

cssAttr :: String -> String -> LA XmlTree String
cssAttr sel = (css sel >>>) . getAttrValue

runAndHXT :: String -> ConnectionPool -> LA XmlTree String -> IO [Text]
runAndHXT req pool hxt = do
  html' <- simpleBody <$> run (reqFor req) pool
  return . map pack $ runLA (hreadDoc >>> hxt) $ BSC.unpack html'

raht :: String -> ConnectionPool -> String -> IO [Text]
raht req pool = runAndHXT req pool .  cssText

raha :: String -> ConnectionPool -> String -> String -> IO [Text]
raha req pool = runAndHXT req pool .: cssAttr

shoeList :: Int -> Int -> [[Text]]
shoeList from to = transpose [map pack ["Shoe #" <> show n, "/shoes/" ++ show n,
                                        "/" ++ photoName ++ ".jpg"]
                             | n <- [from .. to]]

extractShoeList :: String -> ConnectionPool -> IO [[Text]]
extractShoeList path pool =
  sequence (raht path pool ".shoe" : (map (uncurry $ raha path pool)
                                      [(".link", "href"), (".photo", "src")]))
