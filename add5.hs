add5 :: Num a => a -> a
add5 x = x + 5

add5list :: Num a => [a] -> [a]
add5list xs = map (add5) xs

-- Written using lambda calculus 

add5list1 :: Num a => [a] -> [a]
add5list1 xs = map (\lambda x -> x + 5) xs


