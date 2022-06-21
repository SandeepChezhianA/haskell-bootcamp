data Mover = Lefti | Righti | Upi | Downi 
 deriving(Show, Ord, Eq)

data Shape = Circle Float | Rectangle Float Float


type Pos = (Int,Int)

mover :: Mover -> Pos -> Pos 
mover Lefti (x,y) = (x-1,y)
mover Righti (x,y) = (x+1,y)
mover Upi (x,y) = (x,y+1) 
mover Downi (x,y) = (x,y-1) 

flipper :: Mover -> Mover 
flipper Lefti = Righti 
flipper Righti = Lefti 
flipper Upi = Downi 
flipper Downi = Upi 


square :: Float -> Shape 
square n = Rectangle n n 

areaw :: Shape -> Float 
areaw (Circle r) = pi * (r * r)
