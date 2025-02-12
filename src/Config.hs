module Config (Environment (..), SecretKey, AdminPwd (..)) where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Text (Text)

data Environment = Env
  { secretKey :: SecretKey,
    adminPwd :: AdminPwd
  }

instance Show Environment where
  show Env {secretKey = key} = show $ encodeBase64 key

type SecretKey = BS.ByteString

newtype AdminPwd = AdminPwd Text
