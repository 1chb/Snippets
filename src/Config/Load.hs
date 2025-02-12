module Config.Load (get) where

import Config (AdminPwd (..), Environment (Env))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Base64 (decodeBase64Untyped)
import Session (generateSecretKey)
import System.Environment (getEnv)

get :: (MonadIO m, MonadFail m) => m Environment
get = Env <$> generateSecretKey <*> getAdminPwd
  where
    getAdminPwd = do
      pwd <- liftIO $ getEnv "ADMIN_PASSWORD"
      either (fail . T.unpack) pure . decodeAdminPwd $ T.pack pwd

decodeAdminPwd :: Text -> Either Text AdminPwd
decodeAdminPwd = fmap AdminPwd . decodeBase64Untyped
