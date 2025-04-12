-- {-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Api (app)
import Config (AdminPwd (..))
import Data.ByteString.Char8 qualified as BS
import Database qualified as DB
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (hLocation, hContentType)
import Network.HTTP.Types.Status (status200, status303, status401)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test qualified as Wai.Test
import Session qualified
import Test.Hspec (describe, hspec, it, shouldBe)
import Util.Redirect (Path (Login), compile)
import Debug.Trace qualified as Debug
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  dbEnv <- DB.connectAndMigrate
  sessionEnv <- Session.Env <$> Session.generateSecretKey <*> pure (AdminPwd "testPassword")
  let pureApp = pure
        $ app dbEnv sessionEnv
  hspec $ do
    describe "Root Page Redirection" $ do
      it "redirects to login page when accessing root without authentication" $ do
        testWithApplication pureApp $ \port -> do
          manager <- HTTP.newManager HTTP.defaultManagerSettings
          request <- HTTP.parseRequest $ "http://localhost:" ++ show port ++ "/"
          response <- HTTP.httpNoBody request {HTTP.redirectCount = 0} manager
          HTTP.responseStatus response `shouldBe` status303
          let locationHeader = lookup hLocation (HTTP.responseHeaders response)
          locationHeader `shouldBe` Just (compile $ Login Nothing)

    describe "Login Functionality" $ do
      it "allows a user to log in with valid credentials" $ do
        testWithApplication pureApp $ \port -> do
          response <- Wai.Test.runSession (simulateLogin "admin" "testPassword" port) =<< pureApp
          Wai.Test.simpleStatus response `shouldBe` status200
          BS.isInfixOf "Greetings" (BS.toStrict $ Wai.Test.simpleBody response) `shouldBe` True

      it "rejects a user with invalid credentials" $ do
        testWithApplication pureApp $ \port -> do
          response <- Wai.Test.runSession (simulateLogin "invalidUser" "wrongPassword" port) =<< pureApp
          Wai.Test.simpleStatus response `shouldBe` status401
          BS.isInfixOf "Invalid credentials" (BS.toStrict $ Wai.Test.simpleBody response) `shouldBe` True

simulateLogin :: BS.ByteString -> BS.ByteString -> Int -> Wai.Test.Session Wai.Test.SResponse
simulateLogin username password _port = do
  let loginPath = "/login"
  let loginBody = "username=" <> username <> "&password=" <> password
  bodyRef <- liftIO $ newIORef (Just loginBody)
  let loginRequest = Wai.Test.setPath Wai.Test.defaultRequest
        { Wai.requestMethod = methodPost
        , Wai.requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        } loginPath
  response <- Wai.Test.request $ Debug.traceShowId $ Wai.setRequestBodyChunks (bodyChunks bodyRef) loginRequest
  Debug.traceShowM response
  -- Follow the redirect if status is 303
  if Wai.Test.simpleStatus response == status303
    then do
      let redirectPath = extractRedirectPath response
      followRedirect redirectPath
    else pure response

bodyChunks :: IORef (Maybe BS.ByteString) -> IO BS.ByteString
bodyChunks ref = do
  mbody <- readIORef ref
  case mbody of
    Just b -> do
      writeIORef ref Nothing
      return b
    Nothing -> return BS.empty

extractRedirectPath :: Wai.Test.SResponse -> BS.ByteString
extractRedirectPath response =
  case lookup "Location" (Wai.Test.simpleHeaders response) of
    Just location -> location
    Nothing -> error "No Location header found in redirect response"

followRedirect :: BS.ByteString -> Wai.Test.Session Wai.Test.SResponse
followRedirect path = do
  let redirectRequest = Wai.Test.setPath Wai.Test.defaultRequest path
  Wai.Test.request redirectRequest