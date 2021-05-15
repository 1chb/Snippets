-- ghc -O2 MFAcc -main-is MFAcc.main && time ./MFAcc 1000000
module MFAcc where

import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import System.Environment (getArgs)

newtype MFAcc = MFAcc { un :: String -> String }

instance Semigroup MFAcc where
  MFAcc f <> MFAcc g = MFAcc (f . g)

instance Monoid MFAcc where
  mempty = MFAcc id

instance Show MFAcc where
  showsPrec k (MFAcc f) = showParen (k>10) $ ("MFAcc (" ++) . shows (f "") . ("++)"++)

main :: IO ()
main = do
  [n] <- getArgs
  let acc = mconcat $ map (MFAcc . shows) [1..read n]
  print $ length $ un acc "."
  print $ Just (mempty :: MFAcc)
  print (mempty :: MFAcc, mempty :: MFAcc)
  print $ MFAcc ("APA"++) <| MFAcc (shows "BEPA") :| []
