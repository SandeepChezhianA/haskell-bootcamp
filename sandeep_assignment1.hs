import Data.Char

-- Question 1
m :: Integer -> Integer
m n = if n > 100 then n - 10 else m (m (n + 11))

{- 
Case 1 : 3 Values above 100 
Input 1: 120 ; Output 1 : 110 
Input 2: 130 ; Output 2 : 120 
Input 3: 9820 ; Output 3 : 9810 

Case 2: 5 Values less than 100
Input 1 : -300 ; Output 1 : 91 
Input 2 : 40; Output 2 : 91 
Input 3 : 0 ; Output 3 : 91 
Input 4 : 10 ; Output 4 : 91 
Input 5 : 9 ; Output 9 : 91 
-}

-- Question 2

pythogorean :: Integer -> Integer -> Integer -> Bool
pythogorean a b c = ((a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (c^2 + a^2 == b^2))

{-
Input 1 : 3 4 5 ; Output 1 : True 
Input 2 : 40 9 91 ; Output 2 : True 
Input 3 : 29 7 25 ; Output 3 : False
Input 4 : 23 7 25; Output 4 : False
Input 5 : 0 0 0 ; Output 5 : True
-}

-- Question 3

chartoint :: [Char] -> [Int]
chartoint a = [ord b | b <- a]

shiftn :: Int -> Int -> Int
shiftn a b | ((a > 64 && a < 91 && (a + b) > 64 && (a+b) < 91) || ( a > 96 && a < 123 && (a + b) > 96 && (a + b) < 123)) = (a + b) 
           | ((a > 64 && a < 91 && (a + b) <= 65)) = 91 - (65 `mod` (a + b))
           | ((a > 64 && a < 91 && (a + b) >= 91)) = 64 + ((a + b) `mod` 90)
           | ((a > 96 && a < 123 && (a + b) <= 97)) = 123 - (97 `mod` (a+b))
           | ((a > 96 && a < 123 && (a + b) >= 123)) = 96 + ((a + b) `mod` 122)
           | (a == 32)  = 32

finalshft ::[Int] -> Int -> [Int]
finalshft a b = [shiftn c b| c <- a]

inttochar :: [Int] -> [Char]
inttochar a = [chr b | b <- a]

encode :: [Char] -> Int -> [Char]
encode a b = inttochar(finalshft (chartoint(a)) b )

percent :: Int -> Int -> Float
percent n m = (fromIntegral n/ fromIntegral m) * 100

count1 :: String -> Char -> Int 
count1 [] y = 0
count1 (x:xs) y | x == y = (count1 xs y)+1
                | otherwise = (count1 xs y)

lowers :: String -> Int
lowers xs = length [x | x<- xs, (ord x >=97 && ord x <= 122) || (ord x >= 65 && ord x <= 97)]

freq :: String -> [Float]
freq str = [percent (count1 str x) (lowers(str))| x <- ['a'..'z']]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

chisqr :: [Float] -> [Float] -> Float
chisqr ob es = sum [((o-e)**2.0)/e | (o,e) <- zip ob es]

min1 :: Ord a => [a] -> a
min1 [x] = x
min1 (x:y:xs) = if x < y then min1(x:xs) else min1(y:xs)

positions :: Eq a => [a] -> a -> Int
positions xs x = head ([i | (x',i) <- zip xs [0..],x==x'])

expfreq :: [Float]
expfreq = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

chitab :: String -> [Float]
chitab xs = [chisqr (rotate n (freq xs)) expfreq | n <-[0..25] ]

crack :: String -> String
crack xs = encode xs (-(positions (chitab xs) (min1 (chitab xs))))

{-
encode "My name is Sandeep" 27 = "Nz obnf jt Tboeffq" 
crack "Nz obnf jt Tboeffq" = "My name is Sandeep"
encode "What did you eat" = "Ufyr bgb wms cyr"
crack "Ufyr bgb wms cyr" = "What did you eat"
encode "hello Sandeep" 6 = "nkrru Ygtjkkv"
-}

-- Question 4

luhnDouble :: Int -> Int
luhnDouble a = if 2 * a > 9 then  2 * a - 9 else 2 * a

{-
luhnDouble 6 = 3
luhnDouble 4 = 8
luhnDouble 5 = 1 
luhnDouble 102 = 195
luhnDouble 2 = 4
-}

{- Types checked -}

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble(a) + luhnDouble(c) + b + d) `mod` 10 == 0

{-
luhn 1 7 8 4 = True 
luhn 4 7 8 3 = False 
luhn 1 1 6 4 = True 
luhn 0 0 9 1 = True
luhn 0 0 0 1 = False
-}

--Question 5 

lastEl :: [a] -> a 
lastEl [x] = x
lastEl (x:xs) = lastEl xs

stripper :: [a] -> [a]
stripper [x] = []
stripper (x:xs) = [x] ++ stripper xs 


halves :: Num a => [a] -> ([a],[a])
halves [] = ([],[])
halves [x] = error "odd number of elements"
halves (xs) = ([head xs]++xk, yk ++ [lastEl(xs)]) where (xk,yk) = halves(stripper(tail xs))

{-
halves [1,2,3,4] = ([1,2],[3,4])
halves [1,2,3,4,5] = ([1,2*** Exception: odd number of elements CallStack (from HasCallStack): error, called at sandeep_assignment1.hs:134:14 in main:Main
halves [1,2,4,6] = ([1,2],[4,6])
halves [1,2,3,2,4,6] = ([1,2,3],[4,5,6])
halves [] = ([],[])
-}

-- Question 6 

expo :: Integer -> (Integer -> Integer) 
expo a = f where f x = x^a

{-
(expo 3) 4 = 64 
(expo 0) 1 = 1
(expo (-1)) 1 = *** Exception Negative exponent
(expo 64) 2 = 18446744073709551616
(expo 2) 2 = 4
-}

-- Question 7

double :: Num a => a -> a
double x = x * 2

{-
double has been written as a helper function
-}

doubleList :: Num a => [a] -> [a]
doubleList = map double

{-
doubleList [1,2,3,4] = [2,4,6,8]
doubleList [0,0,0,0] = [0,0,0,0]
doubleList [] = []
doubleList [900] = [1800]
doubleList [123,234,456] = [246,468,912]
-}

-- Question 8

grid :: Int -> Int -> [(Int,Int)]
grid a b = [(k,l) | k <- [0..a], l <- [0..b]]

{-
grid 1 2 = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
grid 3 4 = [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),(2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4)]
grid 0 0 = [(0,0)]
grid 0 (-2) = []
grid (-1) (-1) = []
-}

-- Question 9

(%) :: [a] -> Integer -> a
(x:xs) % 0 = x
(x:xs) % n = xs % (n - 1)
[] % n = error "out of range"

{-
[1,22,45,39,28,19] % 4 = 28 
[1,22,45,39,28,19] % 9 = Exception : out of range
[12,32,10,102,19] % (-1) = Exception : out of range 
[] % 2 = Exception : out of range 
[13832,198,129,19] % 1 = 198 
-}

-- Question 10 

scalp :: Num a => [a] -> [a] -> a
scalp [] [] = 0 
scalp xs [] = error "Cannot compute scalar product"
scalp [] xs = error "Cannot compute scalar product"
scalp xs ys = ((head xs) * (head ys)) + scalp (tail xs) (tail ys)

{-
scalp [1] [] = *** Exception: Cannot compute scalar product
               CallStack (from HasCallStack):
               error, called at q10.hs:5:15 in main:Main
scalp [1,3,4,5] [5,6,7,9] = 96 
scalp [12] [192,10] = *** Exception: Cannot compute scalar product
                      CallStack (from HasCallStack):
                      error, called at q10.hs:5:15 in main:Main
scalp [] [] = 0 
scalp [12] [12] = 144
-}


transp :: Num a => [[a]] -> [[a]]
transp ([]:xs) = [] 
transp [] = []
transp (xs) = [map (head) xs] ++ transp (map (tail) (xs))

--transp xs = if ((head xs == [] && tail xs == [[]]) || (xs == [])) then [] else [map (head) xs] ++ transp(map (tail) (xs))

{-
transp [[1,2,3],[4,5,6]] = [[1,4],[2,5],[3,6]]
transp [[1,4],[2,5],[3,6]] = [[1,2,3],[4,5,6]]
transp [[]] = []
transp [[1],[2]] = [[1,2]] 
transp [[1,2,3],[1,2,3,4]] = [[1,1],[2,2],[3,3],[*** Exception: Prelude.head: empty list   (Expected since rows cannot have unequal number of elements.)
transp [[1,2,3,4],[1,2,3]] = [[1,1],[2,2],[3,3],[4,*** Exception: Prelude.head: empty list
-}

lintransp :: Num a => [[a]] -> [[a]] -> [[a]]
lintransp [] ys = []
lintransp xs ys = [[scalp (head xs) (head(transp ys))]] ++ lintransp (tail xs) (ys)

{- 
lintransp [[1,2,3]] [[1],[2],[3]] = [[14]]
lintransp [[1,2,3]] [[2],[3],[4],[5]] = [[*** Exception: Cannot compute scalar product CallStack (from HasCallStack) : error, called at q10.hs:6:15 in main:Main
lintransp [[3,2]] [[2]]  = [[*** Exception: Cannot compute scalar product CallStack (from HasCallStack): error, called at q10.hs:5:15 in main:Main
lintransp [[1],[2]] [[3]] = [[3],[6]]
lintransp [[4,5,6,7,8],[9,10,11,12,13],[12,39,29,22,2]] [[2],[3],[4],[5],[6]] = [[130],[230],[379]]
-}

matMult :: Num a => [[a]] -> [[a]] -> [[a]]
matMult [] [] = []
matMult [] ys = []
matMult (x:xs) ys = transp((lintransp (transp ys) (transp [x]))) ++ matMult xs ys

{-
matMult [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]
matMult [[]] [[]] = [] 
matMult [[1]] [[2]] = [[2]]
matMult [[2,3],[4,5]] [[3]] = [[*** Exception: Cannot compute scalar product CallStack (from HasCallStack): error, called at q10.hs:5:15 in main:Main
matMult [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]] = [[30,36,42],[66,81,96],[102,126,150]]
-}

