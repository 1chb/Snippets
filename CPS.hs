module CPS
  ( cps
  ) where

cps :: (b1 -> b2) -> b1 -> (b2 -> c) -> c
cps = flip . flip (.)
