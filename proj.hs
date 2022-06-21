type Values = Char 
type Row a = [a]
type Matrix a = [Row a] 
type Grid = Matrix Values

easy :: Grid 
easy = ["2....1.38","........5",".7...6..."]

--Empty Grid 

blank :: Grid 
blank = replicate 9 (replicate 9 '.') 

-- Encoding the rules to be satisfied by a solution to a sudoku grid 

{- 
There must only be one instance of a given number between 1 and 9 in each column in the sudoku grid. 
There must only be one instance of a given number between 1 and 9 in each row in the sudoku grid. 
There must only be one instance of a given number between 1 and 9 in each 3 x 3 square withing the grid. 
-} 

rows :: Matrix a -> [Row a] 
rows ma = ma 

-- Alternate way of defining rows would be to use the identity function ('id') in Haskell. This would be listed as a topic of new learning. 

rows1 :: Matrix a -> [Row a]
rows1 = id 

-- Property to be noted : rows (rows ma) = ma 
-- Extracting the columns from the matrix is essentially equivalent to computing the transpose of the matrix. 

cols :: Matrix a -> [Row a] 
cols ([]:xs) = [] 
cols [] = [] 
cols xs = [map (head) xs] ++ cols (map (tail) xs) 

-- Property to be noted : cols (cols ma) = ma 
-- (AT)T = A . Transpose of a transpose returns the original matrix. 
group :: [a] -> [[a]]
group [] = [] 
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a] 
ungroup = concat 

boxes :: Matrix a -> [Row a] 
boxes = map ungroup . ungroup . map cols . group . map group 

-- Extracts all the boxes in the sudoku.  

valid :: Grid -> Bool 
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g)

-- nodups ensures that there are no duplicates within a list.

nodups :: Eq a => [a] -> Bool 
nodups [] = True 
nodups (x:xs) = all (/=x) xs && nodups xs 

-- With this line of code, we have encoded the required checks into the sudoku solver.
-- Valid ensures that there are no duplicates in the boxes, cols and rows. 
-- all is a library function in haskell which ensures that all the elements in a list satisfy a certain property. 

-- Now we implement the basic sudoku solver 
-- In this function, we replace each blank with all possible values between 1 and 9. 

type Choices = [Values] 

-- choice essentially checks if an element in the string is a '.' and if it is a dot, it replaces the '.' with "123456789" which is essentially all the possible choices it can take. 

choice :: Char -> [Char] 
choice v = if v == '.' then ['1'..'9'] else [v] 

choices :: Grid -> Matrix Choices 
choices ma = map (map choice) ma
  
-- Carries out the choice function on every single blank element in the sudoku grid. 
 
cartp :: [[a]] -> [[a]]
cartp [] = [[]] 
cartp (xs:xss) = [y:ys | y <- xs , ys <- cartp (xss)] 

-- the base case for the cp function should return a list containing an empty list. 

collapse :: Matrix [a] -> [Matrix a]
collapse m = cartp (map cartp m) 

-- cartp m produces the product list along the rows
-- The outer cartp produces the product list alone the columns. 

solve :: Grid -> [Grid]  
solve ma = filter (valid) (collapse (choices ma))

-- Upto this we are working with a solution which works in terms of correctness but doesn't produce any results because the computations are a lot for us to make. !!
-- The search base is too big and that's why it works in principle but not in practice. 

-- Now , we improve the efficiency of the already defined algorithm by introducing a prune function. 

prune :: Matrix Choices -> Matrix Choices 
prune = pruneBy boxes . pruneBy cols . pruneBy rows 
            where pruneBy f = f . map pruneRow . f


pruneRow :: Row [Values] -> Row [Values]
pruneRow row = map (remove fixed) row
                where fixed = [d | [d] <- row]


remove :: [Values] -> [Values] -> [Values]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

