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