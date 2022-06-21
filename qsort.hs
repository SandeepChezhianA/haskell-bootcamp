qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = lessList ++ [x] ++ moreList
        where 
        lessList = qsort [y | y <- xs, y <= x]
        moreList = qsort [y | y <- xs, y > x]
