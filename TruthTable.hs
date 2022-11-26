{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TruthTable where

import Data.Bifunctor (bimap, first)
import Data.Char (chr, ord)

class TruthTable a where
  genTT :: a -> TT

type TT = [([Bool], [Bool])]

instance TruthTable [Bool] where
  genTT bs = [([], bs)]

instance TruthTable Bool where
  genTT b = genTT [b]

instance TruthTable [r] => TruthTable [Bool -> r] where
  genTT fs = concatMap halfTT [minBound .. maxBound] where
    halfTT b = fmap (first (b:)) $ genTT $ ($ b) <$> fs

instance (TruthTable a) => TruthTable (Bool -> a) where
  genTT f = concatMap halfTT [minBound .. maxBound] where
    halfTT b = fmap (first (b:)) $ genTT $ f b

class ShowE e where
  showE :: e -> Char

instance {-# OVERLAPPABLE #-} Enum a => ShowE a where
  showE x = case fromEnum x of
    k | k < 10 -> chr $ ord '0' + k
      | otherwise -> chr $ ord 'a' + k - 10

instance {-# OVERLAPPING #-} ShowE Char where
  showE c = c

drawTT :: TT -> [String]
drawTT tt
  = (' ' : sp (take ni ['a'..]) ++ "| " ++ sp (take no ['A'..]))
  : ('-' : replicate (2*ni) '-' ++ "+-" ++ replicate (2*no) '-')
  : fmap line tt
  where
    (ni, no) = case tt of
      l1 : _ -> bimap length length l1
      [] -> (0,0)
    line (is, os) = ' ' : sp (fmap showE is) ++ "| " ++ sp (fmap showE os)
    sp = foldr (\a b -> a : ' ' : b) ""

printTT :: TT -> IO ()
printTT = mapM_ putStrLn . drawTT
