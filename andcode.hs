
and :: Bool -> Bool -> Bool 

and x y = if (x == True) then (if (y == True) then True else False) else False 

and2 :: Bool -> Bool -> Bool
and2 True True = True
and2 _ _ = False 


