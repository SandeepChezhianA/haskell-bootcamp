{- List is not a type class . It is a polymorphic type-}


check :: Eq a => a -> a -> Int
check a b = if (a == b) then (1 :: Int) else 0


{- Define the following functions 
1. addition 
2. multiplication 
3. greatest element among 3
4. Max of a list 
5. Sum of a list -}

add1 :: Num a => a -> a -> a 
add1 a b = a + b 

mul1 :: Num a => a -> a -> a 
mul1 a b = a * b 

gre3 :: Ord a => a -> a -> a -> a 
gre3 a b c = if (a >= b) then (if a >= c then a else c) else (if b >= c then b else c)

maxli :: (Ord a) => [a] -> a
maxli [x] = x
maxli (x:y:xs) = if (x > y) then maxli (x : xs) else maxli (y : xs)

sumli :: Num a => [a] -> a
sumli [] = 0 
sumli (x:xs) = x + sumli(xs)  
