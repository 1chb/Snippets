module CPS
  ( cps
  ) where

cps :: (a -> b) -> a -> (b -> c) -> c
cps = flip . flip (.)
