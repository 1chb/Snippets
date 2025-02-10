module Login.Session (Environment, generateEnv, generateJWT) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Base64.Types (Alphabet (StdPadded), Base64, extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Text (Text)
import Data.Text qualified as T
import Login.User (User)
import System.Random
import Web.JWT (JWTClaimsSet (iss, sub), encodeSigned, hmacSecret, stringOrURI)

newtype Environment = Env {secretKey :: SecretKey} deriving stock (Show)

type SecretKey = Base64 'StdPadded Text

generateEnv :: (MonadIO m) => m Environment
generateEnv = Env <$> generateSecretKey

generateSecretKey :: (MonadIO m) => m SecretKey
generateSecretKey =
  encodeBase64 . BS.pack <$> replicateM 32 (getStdRandom genWord8)

generateJWT :: Environment -> User -> Text
generateJWT env user =
  let -- Create a claims set with the username as the issuer
      claimsSet =
        mempty
          { iss = stringOrURI "wv2.hopTo.org",
            sub = stringOrURI $ T.show user
          }
      -- Create the signing key
      key = hmacSecret . extractBase64 $ secretKey env
   in -- Encode the claims set into a JWT
      encodeSigned key mempty claimsSet
