import Prelude hiding (last)

data Node a = Node
  { prev  :: Node a
  , value :: Maybe a
  , next  :: Node a
  }

buildList :: [a] -> Node a
buildList [] = head where head = Node head Nothing head
buildList xs = head where
  head = Node { prev = last, value = Nothing, next = first }
  (last, first) = go1 xs head
  go1 [x] prev = (node, node) where
    node = Node { prev = prev, value = Just x, next = head }
  go1 (x:xs) prev = (last, node) where
    node = Node { prev = prev, value = Just x, next = next }
    (last, next) = go1 xs node

plist :: (Show a) => Node a -> IO ()
plist Node { prev=last, value=Nothing, next=first } = do
  ffw first
  bwd last
  where
    ffw :: Show a => Node a -> IO ()
    ffw Node { value = Nothing } = putStrLn "ffw stop"
    ffw Node { value = Just x, next = n } = print x >> ffw n
    bwd Node { value = Nothing } = putStrLn "bwd stop"
    bwd Node { value = Just x, prev = p } = print x >> bwd p

main :: IO ()
main = do
  plist $ buildList ""
  plist $ buildList "a"
  plist $ buildList "ab"
  plist $ buildList "abcde"
