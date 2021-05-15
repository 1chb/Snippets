-- ghc -O2 MSAcc -main-is MSAcc.main && time ./MSAcc 1000000
module MSAcc where

import System.Environment (getArgs)

main :: IO ()
main = do
  [n] <- getArgs
  let acc = mconcat $ map show [1..read n]
  print $ length $ acc
