-- module MSAck where

import System.Environment (getArgs)

main :: IO ()
main = do
  [n] <- getArgs
  let acc = foldr (<>) mempty $ map show [1..read n]
  print $ length $ acc
