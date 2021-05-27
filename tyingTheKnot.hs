import Prelude hiding (last)

data Node a = Node
  { prev  :: Maybe (Node a)
  , value :: a
  , next  :: Maybe (Node a)
  }

data List a = List
  { first :: Maybe (Node a)
  , last :: Maybe (Node a)
  }

--      +---->|a|-->|b|-->|c|-->|d|-->|e|--0
-- List-|  0--|a|<--|b|<--|c|<--|d|<--|e|--\
--      +----------------------------------/

buildList :: [a] -> List a
buildList []     = List Nothing Nothing
buildList [x] = List node node where
  node = Just $ Node Nothing x Nothing
buildList (x:xs) = List first last
  where  first = Just $ Node Nothing x next
         (next, last) = go1 xs first
         go1 [x] prev = (node, node)
           where node = Just $ Node prev x Nothing
         go1 (x:xs) prev = (node, last)
           where  node = Just $ Node prev x next
                  (next, last) = go1 xs node

plist :: (Show a) => List a -> IO ()
plist list = do
  ffw $ first list
  bwd $ last list
  where
    ffw Nothing = putStrLn "ffw stop"
    ffw (Just node) = print (value node) >> ffw (next node)
    bwd Nothing = putStrLn "bwd stop"
    bwd (Just node) = print (value node) >> bwd (prev node)

main :: IO ()
main = do
  plist $ buildList ""
  plist $ buildList "a"
  plist $ buildList "abcde"
