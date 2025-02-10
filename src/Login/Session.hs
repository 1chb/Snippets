module Login.Session (Environment, generateEnv, generateJWT) where

-- import Crypto.Random (getRandomBytes)

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Base64.Types (Alphabet (StdPadded), Base64, extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Map qualified as Map
import Data.Text (Text)
import System.Random
import Web.JWT (ClaimsMap (..), JWTClaimsSet (iss, unregisteredClaims), encodeSigned, hmacSecret, stringOrURI)

newtype Environment = Env {secretKey :: SecretKey} deriving stock (Show)

type SecretKey = Base64 'StdPadded Text

generateEnv :: MonadIO m => m Environment
generateEnv = Env <$> generateSecretKey

generateSecretKey :: (MonadIO m) => m SecretKey
generateSecretKey =
  encodeBase64 . BS.pack <$> replicateM 32 (getStdRandom genWord8)

generateJWT :: Environment -> Text -> Text
generateJWT env username =
  let -- Create a claims set with the username as the issuer
      claimsSet =
        mempty
          { iss = stringOrURI username,
            unregisteredClaims = ClaimsMap $ Map.fromList [("http://example.com/is_user", Aeson.Bool True)]
          }
      -- Create the signing key
      key = hmacSecret . extractBase64 $ secretKey env
   in -- Encode the claims set into a JWT
      encodeSigned key mempty claimsSet
