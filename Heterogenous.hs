{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

class C a where
  m :: a -> Bool
  n :: Bool -> a

data D = forall a. (C a, Show a) => D a

deriving instance Show D

instance C () where
  m = const True
  n = const ()

instance C Int where
  m = odd
  n False = 0
  n True = 1

instance C a => C (Maybe a) where
  m (Just a) = m a
  m Nothing = False
  n False = Nothing
  n True = Just $ n True

instance C D where
  m (D a) = m a
  n b = if b then D () else D (1 :: Int)

f :: D -> String
f (D a) = if m a then show a else "FALSE"

main :: IO ()
main = do
  let d3 = D (3 :: Int)
  let d4 = D (4 :: Int)
  let l = [D (), D (2 :: Int), D (3 :: Int), D (Just ()), d4, d3, D d3, D d4, D (D d3), D (Just d3)]
  print l
  print $ f <$> l
  print $ f d4
  print $ D d4
  print $ f . n <$> [False, True]
