type Row a = [a]
type Matrix a = [Row a]
type Digit = Char
type Grid = Matrix Digit 

digits :: [Char]
digits = ['1'..'9']

blank :: Digit -> Bool 
blank a = ( a == '0')  -- Establishes that cells which have '0' as a character are expected to be an empty space in the sudoku. 

choice :: Digit -> [Digit] 
choice d = if blank d then digits else [d] 

choices = map (map choice) 

