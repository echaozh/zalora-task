module Web.ZaloraTask.ControllerSpec (spec) where

import Control.Applicative
import Control.Arrow hiding (app)
import Control.Arrow.Unicode
import Control.Exception
import Control.Monad

import Data.ByteString.Lazy.Char8 (pack)

import Database.Persist.Sqlite hiding (migrate)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import Prelude.Unicode

import System.Directory

import Test.Hspec

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Model
import Web.ZaloraTask.Types

withPool ∷ (ConnectionPool → IO a) -> (ConnectionPool → IO ()) -> IO ()
withPool bef action =
  withSqlitePool ":memory:" 1 $ bef &&& action ⋙ arr (uncurry (>>))

makeTable ∷ ConnectionPool → IO ()
makeTable = runSqlPersistMPool $ runMigration migrate

dropTable ∷ ConnectionPool → IO ()
dropTable = runSqlPersistMPool $ rawExecute "drop table shoe" []

spec ∷ Spec
spec =
  after_ (removeFile "e7df7cd2ca07f4f1ab415d457a6e1c13"
          `catch` \e → void $ return (e∷IOException))
  $ around (withPool makeTable)
  $ makeShoesSpec >> showShoesSpec >> listShoesSpec

makeShoesSpec ∷ SpecWith ConnectionPool
makeShoesSpec = describe "makeShoes" $ do
  let app p     = scottyAppT (runApp p) (runApp p)
                  $ handleAppError >> post "/" makeShoes
      run p req = runSession (srequest req) =<< (app p)
      goodReq   = SRequest defaultRequest{requestMethod="POST"}
                  $ pack ("{\"description\":\"just shoes\",\"color\":\"red\","
                          ++ "\"size\":\"35\",\"photo\":\"MTIzNAo=\"}")
      badReq    = SRequest defaultRequest{requestMethod="POST"} ""

  context "When everything is fine" $ do
      it "redirects to showShoes" $ \pool → do
        ((simpleStatus &&& (lookup hLocation ∘ simpleHeaders))
         <$> run pool goodReq) `shouldReturn` (found302, Just "/shoes/1")

      it "saves the photo to disk base 64 decoded" $ \pool → do
        void $ run pool goodReq
        readFile "e7df7cd2ca07f4f1ab415d457a6e1c13" `shouldReturn` "1234\n"

  context "When provided with invalid input" $ do
    it "reports bad request" $ \pool → do
      simpleStatus <$> run pool badReq `shouldReturn` badRequest400

  context "When db fails" $ do
    it "reports internal server error" $ \pool → do
      dropTable pool
      simpleStatus <$> run pool goodReq `shouldReturn` internalServerError500

showShoesSpec ∷ SpecWith ConnectionPool
showShoesSpec = return ()

listShoesSpec ∷ SpecWith ConnectionPool
listShoesSpec = return ()
