{-# LANGUAGE UndecidableInstances #-}
module EnumTable where

import Data.Bifunctor (bimap, first)
import Data.Char (chr, ord)

class EnumTable k where
  genET :: k -> [([Int], [Int])]

instance Enum b => EnumTable [b] where
  genET bs = [([], fromEnum <$> bs)]

instance (Bounded a, Enum a, EnumTable b) => EnumTable (a -> b) where
  genET f = concatMap halfET [minBound .. maxBound] where
    halfET a = fmap (first (fromEnum a:)) $ genET $ f a

drawET :: [([Int], [Int])] -> [String]
drawET tt
  = (' ' : sp (take ni ['a'..]) ++ "| " ++ sp (take no ['A'..]))
  : ('-' : replicate (2*ni) '-' ++ "+-" ++ replicate (2*no) '-')
  : fmap line tt
  where
    (ni, no) = case tt of
      l1 : _ -> bimap length length l1
      [] -> (0,0)
    line (is, os) = ' ' : sp (fmap showE is) ++ "| " ++ sp (fmap showE os)
    sp = foldr (\a b -> a : ' ' : b) ""
    showE x = case x of
      k | k < 10 -> chr $ ord '0' + k
        | otherwise -> chr $ ord 'a' + k - 10

printET :: [([Int], [Int])] -> IO ()
printET = mapM_ putStrLn . drawET
