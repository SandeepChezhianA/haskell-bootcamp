addi :: Int -> (Int -> Int)
addi = \x -> (\y -> x + y)