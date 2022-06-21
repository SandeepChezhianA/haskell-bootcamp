type StdMr = (String, Int)
type Pos = (Int,Int)
type Board = [Pos]

data Boolean = False2 | True2 deriving(Eq, Ord, Show, Read)

no :: Int -> Boolean
no a = if a == 0 then True2 else False2

multiply :: Int -> (Int -> Int)
multiply = \x -> (\y -> x * y)
data Move = Upi | Downi | Lefti | Righti 

mover :: Move -> Pos -> Pos 
mover Lefti (x,y) = (x-1, y)
mover Righti (x,y) = (x+1,y)
mover Upi (x,y) = (x, y+1)
mover Downi (x,y) = (x, y-1)


doubleMap :: (a -> a) -> a -> a
doubleMap f a = f(f a) 

add :: Int -> (Int -> Int) 
add = \x -> (\y -> x + y)



boar :: Int -> Int -> Pos
boar a b = (a,b)

boarder :: Pos -> Board
boarder (a,b) = [boar k m | k <- [0..a] , m <- [0..b]]


type Dicti k v = [(k,v)]

ker :: a -> i -> Dicti a i
ker b l = [(b,l)]


keyfind :: Eq k => k -> Dicti k v -> v 
keyfind a (x:xs) = if fst(x) == a then snd(x) else keyfind a (xs)
keyfind a [] = error "Key not found"




marker :: StdMr -> String
marker cs =  show(snd(cs))

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
power :: Integer -> Integer -> Integer
power x 0 = 1 
power x y = x * power x  (y-1)

eqch :: Eq a => a -> a -> Bool
eqch a b = if a == b then True else False 


circ :: Floating a => a -> a
circ x = 2 * pi * x

passlist :: [Int] -> [Bool] 
passlist = map (\x -> (x > 50))


equality :: Eq a => a -> (a -> Bool)
equality = \x -> (\y -> (x == y))

factorial :: Int -> Int 
factorial =  (\x -> if x == 0 then 1 else x * factorial (x - 1))

sumoffunc :: (Int -> Int) -> (Int -> Int)
sumoffunc = \f -> (\x -> if x == 0 then f(0) else (f(x) + sumoffunc f (x-1)))


constant :: a -> b -> a 
constant x _ = x 


expo :: Integer -> (Integer -> Integer)
expo = \x -> (\y -> x + y)

sumi :: [Int] -> Int 
sumi = foldr (+) 0 

lengthi :: [Int] -> Int 
lengthi = foldr (\_ n -> 1 + n) 0 


snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverser :: [a] -> [a]
reverser = foldr (snoc) []

gel :: Integer -> Integer 
gel b = b + 2 

hi a b = gel((expo a) b)



