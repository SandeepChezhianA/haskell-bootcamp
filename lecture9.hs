{-boolpair :: [(Int,Int)] -> [Bool]
boolpair xs = [x <= y | (x,y) <- xs]

pairs :: [Int] -> [(Int,Int)]
pairs xs = zip xs (tail xs)

sorted :: [Int] -> Bool 
sorte-}


positiontuple :: [a] -> [(a,a)]
positiontuple xs = zip xs ([0..(length(xs) - 1)])

positions :: [a] -> a -> [Int]
positions xs x = [y | (x,y) <- (positiontuple xs)]




