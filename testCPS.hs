c :: (a -> b) -> a -> (b -> c) -> c
c = flip . flip (.)

d :: Int -> (Int -> c) -> c
d = c (+1)

e' :: (Int -> c) -> c
e' = d 0 d d d d

e :: (Int -> Int) -> Int
e = e' d d d d d d d d d d d d d d d d d d d d d d d d d

main = print $ e id
