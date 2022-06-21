type String1 = [Char]
-- Type synonym where you basically can interchange between using two types. For example, one may choose to use String1 or String or [Char] to represent a sequence of characters in double quotes. 

type Position = (Int, Int)
type Board = [Position]

type Associa k v = [(k,v)] 

findi :: Eq k => k -> Associa k v -> (k,v)
findi k xs = if fst(head xs) == k then head xs else findi k (tail xs)

data Booli = False1 | True1  
 deriving(Ord,Eq,Show, Read) 
 
andi :: Booli -> Booli -> Booli 
andi xs ys = if (xs == ys) then xs else False1 

ori :: Booli -> Booli -> Booli 
ori True1 _ = True1 
ori _ True1 = True1
ori False1 False1 = False1 
 
noti :: Booli -> Booli 
noti True1 = False1 
noti False1 = True1 

data Nat = Zero | Succ Nat 
 deriving(Ord,Eq,Show,Read)
nat2int :: Nat -> Int 
nat2int Zero = 0 
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat (n) = Succ(int2nat (n-1))  


