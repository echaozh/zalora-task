module Web.ZaloraTask.ControllerSpec (spec) where

import Control.Applicative

import Data.ByteString.Lazy.Char8 (pack)

import Database.Persist.Sqlite hiding (migrate)

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import Test.Hspec

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Model
import Web.ZaloraTask.Types

makePool ∷ IO ConnectionPool
makePool = createSqlitePool ":memory:" 1

makeTable ∷ ConnectionPool → IO ()
makeTable = runSqlPersistMPool $ runMigration migrate

spec ∷ Spec
spec = makeShoesSpec >> showShoesSpec >> listShoesSpec

makeShoesSpec ∷ Spec
makeShoesSpec = describe "makeShoes" $ before makePool $ do
  let app p     = scottyAppT (runApp p) (runApp p) $ post "/" makeShoes
      run p req = runSession (srequest req) =<< (app p)

  context "When everything is fine" $ do
      let req = SRequest defaultRequest{requestMethod="POST"}
                $ pack ("{\"description\":\"just shoes\",\"color\":\"red\","
                        ++ "\"size\":\"35\",\"photo\":\"MTIzNAo=\"}")
      it "redirects to showShoes" $ \pool → do
        makeTable pool
        simpleStatus <$> run pool req `shouldReturn` found302

  context "When provided with invalid input" $ do
    it "reports bad request" $ \pool → do
      let req = SRequest defaultRequest{requestMethod="POST"} ""
      simpleStatus <$> run pool req `shouldReturn` badRequest400

showShoesSpec ∷ Spec
showShoesSpec = return ()

listShoesSpec ∷ Spec
listShoesSpec = return ()
