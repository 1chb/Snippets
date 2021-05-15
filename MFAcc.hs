-- ghc -O2 MFAcc -main-is MFAcc.main && time ./MFAcc 1000000
module MFAcc where

import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import System.Environment (getArgs)

newtype MFAcc a = MFAcc { un :: a -> a }

instance Semigroup (MFAcc a) where
  MFAcc f <> MFAcc g = MFAcc (f . g)

instance Monoid (MFAcc a) where
  mempty = MFAcc id

instance (Monoid a, Show a) => Show (MFAcc a) where
  showsPrec k (MFAcc f) = showParen (k>10) $ ("MFAcc (" ++) . shows (f mempty) . ("<>)"++)

main :: IO ()
main = do
  [n] <- getArgs
  let acc = mconcat $ map (MFAcc . shows) [1..read n]
  print $ length $ un acc "."
  print $ Just (mempty :: MFAcc ())
  print (mempty :: MFAcc Ordering, mempty :: MFAcc (Maybe String))
  print $ MFAcc ("APA"++) <| MFAcc (shows "BEPA") :| []
