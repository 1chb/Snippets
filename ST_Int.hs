type State = Int

newtype ST a = S (State -> (a,State))

runST :: State -> ST a -> a
runST s (S st) = fst (st s)

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f (S st) = S (\s -> let (x,s') = st s
                             in (f x, s'))

instance Applicative ST where
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    S af <*> S ax = S (\s -> let (f, s') = af s
                                 (x, s'') = ax s'
                             in (f x, s''))
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

relabelAp :: Tree a -> ST (Tree Int)
relabelAp (Leaf a) = Leaf <$> fresh
relabelAp (Node l r) = Node <$> relabelAp l <*> relabelAp r

fresh :: ST Int
fresh = S (\n -> (n, n+1))

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    mx >>= f = S (\s -> let (x, s') = app mx s
                        in app (f x) s')

app :: ST a -> State -> (a, State)
app (S st) s = st s

get :: ST State
get = S (\s -> (s,s))

set :: State -> ST ()
set s = S (\_ -> ((), s))

relabelM :: Tree a -> ST (Tree Int)
relabelM (Leaf a) = do n <- fresh
                       return (Leaf n)
relabelM (Node l r) = do l' <- relabelM l
                         r' <- relabelM r
                         return (Node l' r')

main :: IO ()
main = do
    let stA = fmap (+1) (pure 75 :: ST Int)
    print (runST 100 stA)
    let stF = pure show :: ST (Int -> String)
    let stB = stF <*> stA
    print (runST 200 stB)
    let stC = stB >>= \x -> return (length x)
    print (runST 300 stC)
    let t = Node (Node (Leaf 'A') (Leaf 'B')) (Leaf 'C')
    print t
    print $ runST 1 $ relabelAp t
    print $ runST 1 $ relabelM t
    print $ runST 1 (relabelM t) == runST 1 (relabelAp t)
    print $ runST 500 $ do
        n <- get
        set $ n `div` 2
        relabelM t
