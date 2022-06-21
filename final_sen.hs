data Parties = Starks | Lannisters | Arryns
 deriving(Ord,Eq,Show,Read)
-- Defines the parties data type 

type Votes = [Parties] 
-- Defines the number of votes 

counti :: Eq a => a -> [a] -> Int 
counti x xs = if xs == [] then 0 else (if (head xs == x) then (1 + counti x (tail xs)) else (counti x (tail xs)))
 
tupler :: [Parties] -> [(Parties,Int)] 
tupler xs = [(k, counti k xs) | k <- [Starks,Lannisters,Arryns]]

removedup :: [(Parties,Int)] -> [(Parties,Int)]
removedup (x:xs) = x : (filter (/= x) xs)  

