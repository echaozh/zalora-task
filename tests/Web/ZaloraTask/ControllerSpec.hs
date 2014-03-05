module Web.ZaloraTask.ControllerSpec (spec) where

import Control.Applicative
import Control.Arrow hiding (app)
import Control.Monad

import Data.ByteString.Lazy.Char8 (pack)

import Database.Persist.Sqlite hiding (migrate)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import Prelude.Unicode

import Test.Hspec

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Model
import Web.ZaloraTask.Types

makePool ∷ IO ConnectionPool
makePool = createSqlitePool ":memory:" 1

makeTable ∷ ConnectionPool → IO ConnectionPool
makeTable pool = do
  flip runSqlPersistMPool pool $ runMigration migrate
  return pool

spec ∷ Spec
spec = makeShoesSpec >> showShoesSpec >> listShoesSpec

makeShoesSpec ∷ Spec
makeShoesSpec = describe "makeShoes" $ before (makePool >>= makeTable) $ do
  let app p     = scottyAppT (runApp p) (runApp p) $ post "/" makeShoes
      run p req = runSession (srequest req) =<< (app p)

  context "When everything is fine" $ do
      let req = SRequest defaultRequest{requestMethod="POST"}
                $ pack ("{\"description\":\"just shoes\",\"color\":\"red\","
                        ++ "\"size\":\"35\",\"photo\":\"MTIzNAo=\"}")
      it "redirects to showShoes" $ \pool → do
        ((simpleStatus &&& (lookup hLocation ∘ simpleHeaders))
         <$> run pool req) `shouldReturn` (found302, Just "/shoes/1")

      it "saves the photo to disk base 64 decoded" $ \pool → do
        void $ run pool req
        readFile "e7df7cd2ca07f4f1ab415d457a6e1c13" `shouldReturn` "1234\n"

  context "When provided with invalid input" $ do
    it "reports bad request" $ \pool → do
      let req = SRequest defaultRequest{requestMethod="POST"} ""
      simpleStatus <$> run pool req `shouldReturn` badRequest400

  -- flip runSqlPersistMPool pool $ rawExecute "drop table shoe" []

showShoesSpec ∷ Spec
showShoesSpec = return ()

listShoesSpec ∷ Spec
listShoesSpec = return ()
