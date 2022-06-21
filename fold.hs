sumi :: [Int] -> Int
sumi = foldr (+) 0 

lengthi :: [Int] -> Int 
lengthi = foldr (\_ n -> n + 1) 0 

