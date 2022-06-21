gcd1 :: Int -> Int -> Int 
gcd1 m n = if (m `mod` n) == 0 then n else gcd1 n (m `mod` n)

gcd2 :: Int -> Int -> Int 
gcd2 m 0 = 0 
gcd2 m n = if m < n then gcd n m else (if m `mod` n == 0 then n else gcd2 n (m `mod` n))

evefile :: [Int] -> [Int]
evefile xs = [x | x <- xs, x `mod` 2 == 0]

factors :: Int -> [Int] 
factors n = [x | x <- [1..n], x `mod` n == 0]

primal :: Int -> Bool 
primal n = if factors n == [1,n] then True else False 

primalfac :: Int -> [Int]
primalfac n = [x | x <- factors n, primal x == True] 
 
{- a mod b := to compute m%n -}

