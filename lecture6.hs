-- Base case : factorial 0 = 1 (Mathematical definition)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple.Utils (xargs)
{-# HLINT ignore "Use foldr" #-}


factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n - 1)


{-
The next two functions will provide us with various versions of the logical operator functions that we use very often. 
We have included the logical operations for AND, NOT and OR 
-}


and1 :: Bool -> Bool -> Bool 
and1 True True = True 
and1 _ _ = False 


and2 :: Bool -> Bool -> Bool 
and2 x y = if x == y then x else False 


and3 :: Bool -> Bool -> Bool 
and3 x y | x == y = x | otherwise False


or1 :: Bool -> Bool -> Bool 
or1 False False = False 
or1 _ _ = True 


or2 :: Bool -> Bool -> Bool 
or2 x y | x == y = x | otherwise True 


not1 :: Bool -> Bool 
not1 False = True 
not1 True = False 


{-
 The next function we are going to code is the drop function which takes 2 arguments, number of elements and a list . 
It drops n elements from the list xs. 
Base case : drop n [] = [], drop 0 [] = [], drop 0 xs = xs , drop n (x:xs) = drop n-1 xs
 -}  


dropit :: Int -> [a] -> [a]
dropit n [] = []
dropit 0 xs = xs 
dropit n (x:xs) = dropit (n - 1) xs


{-
The next function will primarily focus on appending a list to another list. 
For examples append [1,2,4,5,6] [13,93,28,3] will result in a final output of [1,2,4,5,6,13,93,28,3]
Base case : append [] ys = ys, append ys [] = ys, append (x:xs) ys = x : append xs ys
-}


appendi :: [Int] -> [Int] -> [Int]
appendi [] ys = ys 
appendi (x:xs) ys = [x] ++ (appendi xs ys)


{-
The next function will focus on sorting a list of elements following the insertion sort algorithm. It's a pretty cool recursive algorithm and has 
a running time of O(n) which is a very nice algorithm. We need to write two functions. One for ensuring that the insertion happens correctly and the 
other one for compiling all the insertions and finally produce the sorted list 
-}


