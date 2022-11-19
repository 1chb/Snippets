{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
module TruthTable where

import Data.Bifunctor (bimap, first)
import Data.Char (chr, ord)

class TruthTable k a b | k -> a b where
  genTT :: k -> TT a b

type TT a b = [([a], [b])]

instance TruthTable [b] b b where
  genTT bs = [([], bs)]

-- instance TruthTable b a b where
--   genTT b = genTT [b]

-- instance (Bounded a, Enum a, TruthTable [r] a b) => TruthTable [a -> r] a b where
--   genTT fs = concatMap halfTT [minBound .. maxBound] where
--     halfTT b = fmap (first (b:)) $ genTT $ ($ b) <$> fs

instance (Bounded a, Enum a, TruthTable t a b) => TruthTable (a -> t) a b where
  genTT f = concatMap halfTT [minBound .. maxBound] where
    halfTT b = fmap (first (b:)) $ genTT $ f b

drawTT :: (Enum a, Enum b) => TT a b -> [String]
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

printTT :: (Enum a, Enum b) => TT a b -> IO ()
printTT = mapM_ putStrLn . drawTT

class ShowE e where
  showE :: e -> Char

instance Enum a => ShowE a where
  showE x = case fromEnum x of
    k | k < 10 -> chr $ ord '0' + k
      | otherwise -> chr $ ord 'a' + k - 10
