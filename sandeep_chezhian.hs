-- Section 1 
import Test.QuickCheck

-- Question 1 
-- Subsection a 

concatInt :: [Int] -> [Int] -> [Int] 
concatInt (xs) (ys) = if xs == [] then ys else [head xs] ++ concatInt (tail xs) (ys)

--QuickCheck for concat

quicon :: [Int] -> [Int] -> Bool 
quicon xs ks = concat[xs,ks] == concatInt (xs) ks

-- Subsection b 

revFloat :: [Float] -> [Float] 
revFloat (xs) = if xs == [] then [] else (revFloat (tail xs)) ++ [head xs]

revche :: [Float] -> Bool 
revche xs = revFloat(revFloat xs) == xs

-- Subsection c 

lengthString :: [Char] -> Int 
lengthString = foldr (\_ n -> n + 1) 0 

-- Question 2 
-- Subsection a 

iter :: Int -> (a -> a) -> a -> a 
iter n f x = if (n == 0 || n <0 ) then x else (if (n == 1) then f x else iter (n - 1) (f) ((f x)))

pow1 :: Int -> Int -> Int 
pow1 x k = ((iter (k-1) (*x) x)) 

powche :: Int -> Int -> Bool 
powche x k = (x^k) == (pow1 x k) 

drop1 :: [Int] -> Int -> [Int]
drop1 (xs) k = iter (k-1) (tail) xs

replicate1 :: Int -> a -> [a]
replicate1 x k = iter (x-1) (++ [k]) [k]

-- Dummy test

sq :: Int -> Int 
sq x = x * x

-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Section 2 

type ChurchNum a = (a -> a) -> a -> a

--Subsection a

fromInt :: Int -> ChurchNum a 
fromInt 0 f x = x
fromInt k f x = f (fromInt (k - 1) f x)

--Subsection b

toInt :: ChurchNum Int -> Int 
toInt n = n (+1) 0 

-- Subsection c

succ :: ChurchNum a -> ChurchNum a 
succ n f x = f (n f x)

-- Subsection d

plus :: ChurchNum a -> ChurchNum a -> ChurchNum a
plus l m f x = l f (m f x)

-- Subsection e

mult :: ChurchNum a -> ChurchNum a -> ChurchNum a 
mult l m f x = l (m f) x

----------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Section 3 

-- Subsection a

data States = Fast | Normal | Slow | Rest
 deriving (Eq, Ord, Show, Read) 

-- Subsection b 

data Sensor = None | Back | Front | Both 
 deriving (Eq,Ord, Show, Read) 

--Subsection c

type SensorData = [Sensor]

-- Subsection d 

autoGear :: Sensor -> States -> States
autoGear Back Rest = Normal 
autoGear Back _ = Fast 
autoGear Both _ = Rest 
autoGear Front Rest = Rest 
autoGear Front _ = Slow 
autoGear None Normal = Fast
autoGear None Fast = Fast 
autoGear None _ = Normal 

-- Subsection e 

automatonhelp :: States -> SensorData -> States
automatonhelp xs [] = xs 
automatonhelp xs ks = automatonhelp (autoGear (head ks) xs) (tail ks)

automaton :: SensorData -> States
automaton xs =  automatonhelp Rest xs

reverser :: SensorData -> SensorData
reverser xs = if xs == [] then [] else reverser(tail xs) ++ [head xs]

statadata :: SensorData -> [States]
statadata xs = if xs == [] then [] else [automaton(reverser(reverser(xs)))] ++ statadata (reverser (tail (reverser xs)))

stataDataval :: [States] -> Float
stataDataval (Normal:xs) = 0.1 + stataDataval(xs)
stataDataval (Rest:xs) =  0 + stataDataval (xs)
stataDataval (Fast:xs) = 0.3 + stataDataval (xs)
stataDataval (Slow:xs) = 0.05 + stataDataval (xs)
stataDataval [] = 0

-- Subsection f 

simDistance :: SensorData -> Float
simDistance xs = stataDataval(statadata xs)

-- Subsection g 

isSuccess :: SensorData -> Bool 
isSuccess xs = if simDistance(xs) >= 10 then True else False 

-- Subsection h 

genAutomatonhelp ::(Sensor-> States -> States) -> SensorData -> States -> States
genAutomatonhelp f [] k = k 
genAutomatonhelp f xs k = genAutomatonhelp f (tail xs) (f (head xs) k)

genAutomaton :: (Sensor-> States -> States) -> SensorData -> States
genAutomaton f x = genAutomatonhelp f x Rest 

-- Subsection i 

------------------------------------------------------------------------------------------------------------------------------------------------------

--Section 4 

-- Write a code to add matrices

-- Checking if a number is a perfect square using lazy evaluation
matadd :: [[Int]] -> [[Int]] -> [[Int]]
matadd (x:xs) (y:ys) = map (rowad xs) ys

rowad :: [Int] -> [Int] -> [Int]
rowad (xs) (ys) = if (xs == [] && ys == []) then [] else  [ head xs + head ys ] ++ (rowad(tail xs) (tail ys)) 
{-
One of the most interesting topics I enjoyed learning was the ability to recursively define data classes. 
For example, building the natural numbers using Peano's axioms was one of the most interesting concepts. 
-}

data Nat = Zero | Succ Nat

tointnat :: Nat -> Int
tointnat Zero = 0
tointnat (Succ n) = 1 + tointnat n

tonatint :: Int -> Nat
tonatint 0 = Zero
tonatint (n) = Succ(tonatint (n-1))