{- Float is a single precision floating point number
inverse proportionally depends on the size of the number. 

Double uses twice as much as the memory as that of a single precision floating point number. 
What do these types have in common ? 
 1. All Numerical values can only be stored in these types  
 2. Represented by digits between 0 to 9. 
 3. Commonality in terms of arithmetic operation. 

-}


squaree :: Num a => a -> a
squaree x = x * x 

{-absolute1 :: Num a => a -> a
absolute1 x = if x >= 0 then x else (-x)

signumm :: Num a => a -> a 
signumm x = if x > 0 then 1 else (if x == 0 then 0 else -1)
-}

circare :: Floating a => a -> a 
circare r = pi * (squaree r)

circumfer :: Floating a => a -> a 
circumfer r = 2 * pi * r 

tane :: Floating a => a -> a
tane h = (sin h) / (cos h) 

cote :: Floating a => a -> a
cote h = 1 / (tane h)

{- Logarithm for any base -}

logab :: Floating a => a -> a -> a
logab a b = (log a) / (log b)


