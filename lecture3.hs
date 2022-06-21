import Data.Char 

{-Focus mainly on higher order functions rather than other lame stuff-}

factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n - 1)

sof :: Int -> Int
sof 0 = 1 
sof n = factorial (n) + sof (n - 1)

{-Sum of factorial n = factorial n + sof n 
Must be defined for any arbitrary function -}

{-We will now generalise it to a larger set of functions and not just the factorial sum 
We will handle this using higher order functions and it will be damn fun -}

squar :: Int -> Int 
squar x = x * x 

sumoffunc :: (Int -> Int) -> Int -> Int
sumoffunc f 0 = f 0   
sumoffunc f x = f (x) + sumoffunc f (x-1)

{-Test using the factorial function and you should be good brother-}

{-squar :: Int -> Int 
squar x = x * x-}

productoffunc :: (Int -> Int) -> Int -> Int 
productoffunc f 1 = f 1 
productoffunc f x = f (x) * productoffunc f (x-1)

sumoffsquar = sumoffunc (squar)

mod1 :: Int -> Int
mod1 x = if x >= 0 then x else (-x)

{-mod2 :: Int -> Int 
mod2 x 
	|x >= 0 = x 
	|otherwise -x  -}

{-Also would like to study a lot more on list comprehension and get comfortable with it ASAP
Head of a list 
Tail of a list 
List membership 
List building 
Appending & Deletion of list elements -}

{-Coding the caesars cipher
Written under the rule of a right shift of 3-}

upperconv :: [Char] -> [Char] 
upperconv x = [toUpper k | k <- x]

ordconv :: [Char] -> [Int]
ordconv x = [ord k | k <- x]

shiftnum :: Int -> Int -> Int 
shiftnum l n = if l == 32 then 32 else (if (l + n) > 90 then (((l + n) `mod` 90) + 64) else l + n)

finshift :: [Int] -> Int -> [Int] 
finshift x n = [shiftnum k n| k <- x]

caesar :: [Int] -> [Char] 
caesar x = [chr k | k <- x]

fincaesar :: [Char] -> Int -> [Char]
fincaesar x n = caesar(finshift (ordconv(upperconv(x))) n)


{- Lame decrypt function -}
 
decode :: [Char] -> Int -> [Char] 
decode x n = fincaesar x (-n) 

{- Cool & Probabilistic decrypt function -}

supplylist :: [Float]
supplylist = [8.34,1.54,2.73,4.14,12.60,2.03,1.92,6.11,6.71,0.23,0.87,4.24,2.53,6.80,7.70,1.66,0.09,5.68,6.11,9.37,2.85,1.06,2.34,0.20,2.04,0.06]
 
percent :: Int -> Int -> Float 
percent x y = (fromIntegral x / fromIntegral y) * 100 

counti :: [Char] -> Char -> Int 
counti x n = length[1 | k <- x, k == n] 

freqli :: [Char] -> [Float] 
freqli str = [percent (counti str x) (length str) | x <- ['a'..'z']] 

rotates :: Int -> [a] -> [a]
rotates n xs = drop n xs ++ take n xs 

chisqr :: [Float] -> [Float] -> Float 
chisqr os supplylist = sum [ ((o - e) ** 2.0) / e | (o,e) <- zip os supplylist]

chitable :: [Char] -> [Float]
chitable xs = [chisqr (rotates n (freqli xs)) supplylist | n <- [0..25]]

minval :: [Float] -> Float 
minval [x] = x 
minval (x:y:xs) = if x > y then minval(y : xs) else minval(x : xs)

positions :: Eq a => [a] -> a -> Int
positions xs x = head ([i | (x',i) <- zip xs [0..],x==x'])

crack 





