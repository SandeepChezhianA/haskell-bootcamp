import Test.QuickCheck
import Data.Array.Base (bOOL_SCALE)
data Prop = Const Bool| Var Char | Not Prop| And Prop Prop | Or Prop Prop

--Q1) represent the propositions (a) X /\ y , (b) x \/ y , (c) x \/ (~ y /\ z)

q1a :: Prop
q1a = And (Var 'X') (Var 'y')
-- This is not a tautology since this returns a value of False unless and until both X and Y assume a value of True. 

q1b :: Prop
q1b = Or (Var 'x') (Var 'y')
--  This is not a tautology since when X and Y assume the value of False, then OR (False False) will return False hence this must return False.

q1c :: Prop
q1c = Or (Var 'x') (And (Not (Var 'y')) (Var 'z'))
-- This is not a tautology since it doesn't return a value of True for all combinations of X, Y and Z.

q14 :: Prop 
q14 = Not (And (Var 'A') (Not (Var 'A')))
-- This is a tautology since it returns a value of True for any value that A assumes. 

--Q2) Type substitution contains a list of tuples, where each tuple has a variable as its first entry, and the value true or false as its second entry. This corresponds to whether the proposition denoted by a particular variable is true or false. For instance [(P, True), (Q, False), (R, True)] means P is True, Q is False and R is True. 

type Substitution = [(Char,Bool)]

--Q3) Write a function find x s, which takes an element x and a list of tuples (a, b), and returns the first tuple (a,b) where a==x.

find:: Char -> [(Char,Bool)] -> (Char,Bool)
find x xs = if fst(head(xs)) == x then head(xs) else find x (tail xs)


--Q4) Write an eval function, which takes a Substitution(as defined in Q2), and a Proposition, and returns whether the Proposition is True or False under the given substitution. [Use find defined in Q3 in this]

eval :: Substitution -> Prop -> Bool 
eval _ (Const a) = a
eval a (Var k) = snd(find (k)(a))
eval a (And x y) = (eval a x) && (eval a y)
eval a (Not k) = not(eval a k)
eval a (Or x y) = (eval a x) || (eval a y)

--Q5) Write a function vars, which takes a Proposition and returns the list of variables in the proposition, without repetition. [You may need to write a helper function for this]

vars :: Prop -> [Char]
vars (Const _) = []  --Base Case
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y ) = vars x ++ vars y
vars (Or x y) = vars x ++ vars y

-- removedup is the helper function which has been written in order to ensure that the duplicates have been removed. 

removedup :: Eq a => [a] -> [a]
removedup [] = []
removedup (x:xs) = [x] ++ filter (/= x) (removedup xs)

--Q6) Write a function bools, which takes an integer (n) and returns a list constaining all the possible lists of length (n) , with boolean values. 
--For instance
--bools 2 = [[True, True], [True, False], [False, True], [False, True]]
 
--Hint: You can use map and concat

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n =  map ([False] ++) xs ++ map ([True] ++) xs where xs = bools (n - 1)

prop_bools :: Int -> Bool 
prop_bools n = (length (bools n) == (2 ^ n))


--Q7) Write a function substs, which takes a Proposition and returns a list of all possible substitions of variables in the Proposition. 
--Hint: Use functions vars and bools, defined above and try to use map and zip

subst :: Prop -> [Substitution]
subst x = map (zip (removedup(vars x))) (bools(length(removedup(vars x))))

--Q8) Use the above function to write a Tautkya function, which takes a Proposition and checks if it is a tautology. That is, if it evaluates to True under all possible Substitution. 
--Hint: Use the inbuilt and function for lists, eval and list comprehension. 
tautkya :: Prop -> Bool
tautkya p =  and[eval x p | x <- subst p]


--Q9) Write three checks for Q8.

--Test cases to be run for Q8 : 
    -- tautkya q1a (Must return False)
    -- tautkya q1b (Must return False)
    -- tautkya q14 (Must return True)