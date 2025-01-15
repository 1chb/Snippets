{-# OPTIONS_GHC -Wno-orphans #-}

module FunEqOrd () where

instance (Bounded a, Enum a, Eq b) => Eq (a -> b) where
  f == g = all (\x -> f x == g x) [minBound .. maxBound]

instance (Bounded a, Enum a, Ord b) => Ord (a -> b) where
  compare f g = foldMap (\x -> compare (f x) (g x)) [minBound .. maxBound]
