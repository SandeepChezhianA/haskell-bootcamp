{- Assignment 3 -}

altmap :: Eq a => (a -> b) -> (a -> b) -> [a] -> [b]
altmap f1 f2 [] = [] 
altmap f1 f2 [x] = [f1(x)]
altmap f1 f2 [x,y] = [f1(x),f2(y)]
altmap f1 f2 (x:y:xs) = [f1(x),f2(y)] ++ (altmap f1 f2 (xs))





iter1 :: (a -> a) -> a -> [a]
iter1 f x = [x] ++ iter1 f (f(x))

{- Question 9 -}



{- Question 10 -}

data States = Closed | Open
 deriving(Eq,Ord,Show,Read)

data Sensors = Neither | Front | Rear | Both 
 deriving(Eq,Ord,Show,Read)

type Events = [Sensors]

action :: States -> Sensors -> States 
action xs ks = if xs == Closed then (if (ks == Front) then Open else Closed) else (if xs == Open then (if ks == Neither then Closed else Open) else Open)

finalstate :: States -> Events -> States 
finalstate xs [] = xs 
finalstate xs ks = finalstate (action xs (head ks)) (tail ks) 

{- Long question Practice -}

type Rule = (Char,String)
type RewriteRules = [Rule]
type Lsystem = (String,RewriteRules)

counti :: RewriteRules -> Char -> Int 
counti xs k = if xs == [] then 0 else (if fst(head xs) == k then 1 + (counti (tail xs) k) else counti (tail xs) k )

countbool :: RewriteRules -> Char -> Bool
countbool xs k = (counti xs k == 1)

valid  :: RewriteRules -> Bool
valid xs = and [countbool xs (fst(k))| k <- xs]

findRule :: Char -> RewriteRules -> Rule
findRule k xs = if (valid xs) then (if xs == [] then error "Not found in the list" else (if fst(head xs) == k then head xs else findRule k (tail xs))) else error "Not a valid Rewrite Rule"

rewrite :: RewriteRules -> String -> String 
rewrite xs k = concat [snd(findRule l xs) | l <- k]

charmult :: Char -> Int -> String 
charmult k 1 = [k]
charmult k l = [k] ++ charmult (k) (l - 1)

stringmult :: [Char] -> Int -> String 
stringmult xs k = concat [charmult m k | m <- xs]

nrewrite :: RewriteRules -> String -> Int -> String 
nrewrite xs l 1 = rewrite xs l 
nrewrite xs l m = nrewrite xs (rewrite xs l) (m-1)

ms :: Lsystem  
ms = ("bab",[('b',"ba"),('a',"ab")])

ks :: Lsystem
ks = ("ban", [('b',"ab"),('n',"ca"),('a',"aab")])

evaluate :: Lsystem -> [String]
evaluate xs = [nrewrite (snd(xs)) (fst(xs)) k | k <- [1..]]