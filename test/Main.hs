module Main (main) where

import Api (app)
import Config (AdminPwd (..))
import Database qualified as DB
import Network.HTTP.Client (Request (redirectCount), Response (responseHeaders, responseStatus), defaultManagerSettings, httpNoBody, newManager, parseRequest)
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (status303)
import Network.Wai.Handler.Warp (testWithApplication)
import Session qualified
import Test.Hspec (describe, hspec, it, shouldBe)
import Util.Redirect (Path (Login), compile)

main :: IO ()
main = do
  dbEnv <- DB.connectAndMigrate
  sessionEnv <- Session.Env <$> Session.generateSecretKey <*> pure (AdminPwd "password")
  hspec $ do
    describe "Root Page Redirection" $ do
      it "redirects to login page when accessing root without authentication" $ do
        testWithApplication (pure $ app dbEnv sessionEnv) $ \port -> do
          manager <- newManager defaultManagerSettings
          request <- parseRequest $ "http://localhost:" ++ show port ++ "/"
          response <- httpNoBody request {redirectCount = 0} manager
          responseStatus response `shouldBe` status303
          let locationHeader = lookup hLocation (responseHeaders response)
          locationHeader `shouldBe` Just (compile $ Login Nothing)