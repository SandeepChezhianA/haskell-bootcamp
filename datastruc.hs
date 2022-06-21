import Distribution.Simple (VersionInterval)
type Pos = (Int, Int)
-- Pos is the data type we declare to initialise the coordinates on the X-Y plane. For example x :: Pos = (2,3); would refer to 2nd row and 3rd column.

type Board = [Pos] 
-- Board is the data type we declare as a list of all the positional coordinates. 
-- Tyoe declarations cannot be recursive. 

type Association k v = [(k,v)]
-- Couples k and v together. 

find :: Eq k => k -> Association k v -> v 
find k xs = if fst(head xs) == k then snd(head xs) else find k (tail xs)

