import Data.List (nub)

-- Check that there are no more than k distinct digits in the number n
-- and the d least significant digits of n².
chk :: Int -> Int -> Int -> Bool
chk k d n =
  let n² = toInteger n^2
  in length (nub $ show n ++ take d (reverse $ show n²)) <= k
 

t :: [Int]
t = foldl extend [0] [0..u-1] where
  -- Extend each number from the previous step with all possible digits, confirming additional digits at each step 
  extend s d =
    [n | b <- [0..9], m <- s, let n=b*10^d+m, chk k (d+1) n]
 
-- Ensure that the full set of digits (between n and n^2) does not exceed k digits, now without restricting to the least u digits. 
ss :: [Int]
ss = filter (chk k maxBound) t

k :: Int
k = 4

u :: Int
u = 13 -- 0 < n < 10^u

main:: IO ()
main = do
  print $ length ss
  --mapM_ (\n -> print (n, n^2)) ss
  -- k=4, u=12 => 10165
