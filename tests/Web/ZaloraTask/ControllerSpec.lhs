> module Web.ZaloraTask.ControllerSpec (spec) where

I'm using [Hspec](http://hspec.github.io/) for unit testing. IMHO, It helps
efine more readable test cases.

Here I define tests for the `Controller` module, where all business logic
resides.

[HXT](http://www.fh-wedel.de/~si/HXmlToolbox/)
& [HandsomeSoup](http://egonschiele.github.io/HandsomeSoup/) are used to parse
and extract data from the HTML returned by the server app.

  <!--

> import Control.Applicative
> import Control.Arrow hiding (app)
> import Control.Exception
> import Control.Monad
>
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.ByteString.Lazy.Char8 as BSC
> import Data.Composition
> import Data.List (transpose)
> import Data.Monoid
> import Data.String
> import Data.Text.Lazy (Text, pack)
>
> import Database.Persist.Sqlite hiding (get, migrate)
>
> import Network.HTTP.Types.Header
> import Network.HTTP.Types.Status
> import Network.Wai
> import Network.Wai.Test
>
> import System.Directory
>
> import Test.Hspec

  -->

> import Text.HandsomeSoup
> import Text.XML.HXT.Core hiding (app)

  <!--

> import Web.Scottish
> import Web.Scottish.Database
>
> import Web.ZaloraTask.App hiding (photoDir, pool)
> import Web.ZaloraTask.Controller
> import Web.ZaloraTask.Model

  -->

For all cases, a photo directory is created and then deleted. Also, a database
connection pool is first created and then automatically released. Fixture data
is imported before each action. Since the database is an in-memory Sqlite
database, fixture data don't need to be explicitly deleted. Thanks to Hspec 2.0,
`before` and `around` can now inject resources into the test cases.

> spec :: Spec
> spec =
>   before (createDirectoryIfMissing True photoDir)
>   $ after_ (removeDirectoryRecursive photoDir
>             `catch` \e -> void $ return (e::IOException))
>   $ around (withPool fixture)
>   $ makeShoesSpec >> showShoesSpec >> listShoesSpec
>
> makeShoesSpec :: SpecWith ConnectionPool
> makeShoesSpec = describe "makeShoes" $ do
>   let rawReq = setPath defaultRequest{requestMethod="POST"} "/shoes"
>       goodReq = SRequest rawReq
>                 $ ("{\"description\":\"just shoes\",\"color\":\"red\","
>                    <> "\"size\":\"35\",\"photo\":\"" <> photoEncoded <> "\"}")
>       badReq  = SRequest rawReq ""
>
>   context "When everything is fine" $ beforeWith (run goodReq) $ do
>       it "redirects to showShoes" $ \resp -> do
>         simpleStatus resp `shouldBe` found302
>         lookup hLocation (simpleHeaders resp) `shouldBe` Just "/shoes/6"
>
>       it "saves the photo to disk base 64 decoded" $ \_ -> do
>         readFile (photoDir <> photoPathFor photoName) `shouldReturn` photoRaw
>
>   context "When provided with invalid input" $ do
>     it "reports bad request" $ \pool ->
>       statusFor badReq pool `shouldReturn` badRequest400
>
> showShoesSpec :: SpecWith ConnectionPool
> showShoesSpec = describe "showShoes" $ do
>   context "When requesting existing shoes" $ do
>     it "reports ok" $ \pool ->
>       statusFor' "/1" pool `shouldReturn` ok200
>
>     it "returns html with shoe data" $ \pool ->
>       map head <$> sequence (map (raht "/1" pool) ["#description", "#color",
>                                                    "#size"]
>                              ++ [raha "/1" pool "#photo" "src"])
>          `shouldReturn` parsedShoe
>
>   context "When requesting non-existent shoes" $ do
>     it "reports not found" $ \pool ->
>       statusFor' "/100" pool `shouldReturn` notFound404
>
>   context "When requesting with an improper id" $ do
>     it "reports not found" $ \pool ->
>       statusFor' "/NaN" pool `shouldReturn` notFound404
>
> listShoesSpec :: SpecWith ConnectionPool
> listShoesSpec = describe "listShoes" $ do
>   let pathFor = ("?p="<>) . (show::Int -> String)
>
>   describe "listing" $ beforeWith (return . flip extractShoeList) $ do
>     context "Without paging" $ do
>       it "links to all shoes" $ \extract' ->
>         extract' ""  `shouldReturn` shoeList 1 5
>
>     context "With paging" $ do
>       it "links to shoes on current page" $ \extract' -> do
>         extract' (pathFor 2) `shouldReturn` shoeList 3 4
>         extract' (pathFor 3) `shouldReturn` shoeList 5 5
>
>   describe "paging" $ beforeWith (return . flip statusFor') $ do
>     context "and page number is not right" $ do
>       it "reports not found for non-existent page" $ \statusFor'' ->
>         statusFor'' (pathFor 100) `shouldReturn` notFound404
>
>       it "reports bad request for invalid page number" $ \statusFor'' ->
>         statusFor'' "?p=NaN" `shouldReturn` badRequest400
>
>   describe "navigation"
>     $ beforeWith (\pool -> return (\page sel ->
>                                     raha (pathFor page) pool sel "href")) $ do
>       context "and a previous page" $
>         it "links to that" $ \test -> do
>           test 2 "#prev" `shouldReturn` ["/shoes?p=1"]
>           test 3 "#prev" `shouldReturn` ["/shoes?p=2"]
>
>       context "and a next page" $
>         it "links to next page if there is one" $ \test -> do
>           test 1 "#next" `shouldReturn` ["/shoes?p=2"]
>           test 2 "#next" `shouldReturn` ["/shoes?p=3"]
>
>       context "and no previous page" $
>         it "doesn't have previous link" $ \test ->
>         test 1 "#prev" `shouldReturn` []
>
>       context "and no next page" $
>         it "doesn't have next link" $ \test ->
>         test 3 "#next" `shouldReturn` []

Fixture data to be imported into the database.

> defaultShoe :: Shoe
> defaultShoe = Shoe "default shoes" "black" 43 (pack photoName)
>
> parsedShoe :: [Text]
> parsedShoe = [shoeDescription, shoeColor, pack . show . shoeSize,
>               photoPathFor . shoePhoto] <*> [defaultShoe]

Reference data to compare data extracted from app response with.

> shoeList :: Int -> Int -> [[Text]]
> shoeList from to = transpose [map pack ["Shoe #" <> show n,
>                                         "/shoes/" ++ show n,
>                                         "/" ++ photoName ++ ".jpg"]
>                              | n <- [from .. to]]

Convenience to work with photo, the most complicated field here.

> photoDir :: FilePath
> photoName :: String
> photoDir = "/tmp/zalora-task/"
> photoName = "E7DF7CD2CA07F4F1AB415D457A6E1C13"
>
> photoRaw :: String
> photoEncoded :: BSC.ByteString
> photoRaw = "1234\n"
> photoEncoded = "MTIzNAo="
>
> photoPathFor :: (IsString s, Monoid s) => s -> s
> photoPathFor s = "/" <> s <> ".jpg"

Around filter to create/release database connection pool and import fixture data.

> withPool :: (ConnectionPool -> IO a) -> (ConnectionPool -> IO ()) -> IO ()
> withPool bef action =
>   withSqlitePool ":memory:" 1 $ bef &&& action >>> arr (uncurry (>>))
>
> fixture :: ConnectionPool -> IO ()
> fixture = runSqlPersistMPool $ do
>   runMigration migrate
>   replicateM_ 5 $ insert defaultShoe

Helpers to run test sessions to the app. We will not run a full-blown server
handling network traffic; rather, we just run the app and feed it requests in
memory.

Status codes are often checked, so let's make it less tedious.

> run :: SRequest -> ConnectionPool -> IO SResponse
> run req p = runSession (srequest req) =<< app
>   where
>     app = scottishApp' $ do
>       setPool $ return p
>       setPhotoDir photoDir
>       setPageSize 2
>
>       routes
>
> statusFor :: SRequest -> ConnectionPool -> IO Status
> statusFor = (simpleStatus<$>) .: run
>
> statusFor' :: String -> ConnectionPool -> IO Status
> statusFor' path = (simpleStatus<$>) . run (reqFor path)
>
> reqFor :: String -> SRequest
> reqFor = flip SRequest "" . setPath defaultRequest . BS.pack . ("/shoes"<>)

Helpers to check HTML in response.

> cssText :: String -> LA XmlTree String
> cssText = css >>> (//>getText)
>
> cssAttr :: String -> String -> LA XmlTree String
> cssAttr sel = (css sel >>>) . getAttrValue
>
> runAndHXT :: String -> ConnectionPool -> LA XmlTree String -> IO [Text]
> runAndHXT req pool hxt = do
>   html' <- simpleBody <$> run (reqFor req) pool
>   return . map pack $ runLA (hreadDoc >>> hxt) $ BSC.unpack html'
>
> -- short for: runAndHXT(Extract)Text
> raht :: String -> ConnectionPool -> String -> IO [Text]
> raht req pool = runAndHXT req pool .  cssText
>
> -- short for: runAndHXT(Extract)Attribute
> raha :: String -> ConnectionPool -> String -> String -> IO [Text]
> raha req pool = runAndHXT req pool .: cssAttr
>
> extractShoeList :: String -> ConnectionPool -> IO [[Text]]
> extractShoeList path pool =
>   sequence (raht path pool ".shoe" : (map (uncurry $ raha path pool)
>                                       [(".link", "href"), (".photo", "src")]))
