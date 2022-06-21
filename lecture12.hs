-- Lecture 12 
-- Focus on creating our own types 

type Grid = (Int, Int)

translate :: Grid -> Integer -> Grid 
translate (x,y) 2 = (x + 2, y + 2)


type StudentRollNo = ([Char],Int)


