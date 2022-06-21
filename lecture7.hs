replicate1 :: a -> Int -> [a] 
replicate1 a 0 = [] 
replicate1 a n = [a] ++ replicate1 a (n-1)

insertEnd :: a -> [a] -> [a]
insertEnd a [] = [a] 
insertEnd a (x:xs) = x : (insertEnd a xs)

maxi :: [Int] -> Int 
maxi [x] = x 
maxi (x:xs) = if x > maxi xs then x else maxi xs

zipp :: [a] -> [b] -> [(a,b)]
zipp _ [] = [] 
zipp [] _ = [] 
zipp (x:xs) (y:ys) = (x,y) : zipp xs ys 


