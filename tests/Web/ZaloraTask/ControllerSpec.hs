module Web.ZaloraTask.ControllerSpec (spec) where

import Data.Functor

import Database.Persist.Postgresql

--import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Test

import Prelude.Unicode

import Test.Hspec

import Web.Scotty.Trans

import Web.ZaloraTask.Controller
import Web.ZaloraTask.Types

makePool ∷ IO ConnectionPool
makePool = createPostgresqlPool "dbname=zalora_test" 1

spec ∷ Spec
spec = do
  describe "makeShoes" $ before makePool $ do
    context "When provided with invalid input" $ do
      it "reports 400" $ \pool → do
        let req = SRequest defaultRequest{requestMethod="POST"} ""
            app = handleAppError >> post "/" makeShoes
        statusCode ∘ simpleStatus
          <$> (scottyAppT (runApp pool) (runApp pool) app
               >>= runSession (srequest req))
          `shouldReturn` 400
