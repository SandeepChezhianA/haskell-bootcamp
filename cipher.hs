import Test.QuickCheck
import Data.Char

chartoint :: [Char] -> [Int]
chartoint a = [ord b | b <- a]

shiftn :: Int-> Int 
shiftn a b | (a > 64 && a < 91 && (a + b) > 64 && (a + b) < 91) || (a > 96 && a < 123 && (a + b) > 96 && (a + b) < 123) = a + b
    | (a > 64 && a < 91 && (a + b) < 65) = 


finalshft :: [Int] -> Int -> [Int]
finalshft a b = [shiftn c b| c <- a]

inttochar :: [Int] -> [Char]
inttochar a = [chr b | b <- a]

encode :: [Char] -> Int -> [Char]
encode a b = inttochar(finalshft(chartoint(a)) b )

percent :: Int -> Int -> Float
percent n m = (fromIntegral n/ fromIntegral m) * 100

count1 :: String -> Char -> Int 
count1 [] y = 0
count1 (x:xs) y | x == y = (count1 xs y)+1
                | otherwise = (count1 xs y)

lowers :: String -> Int
lowers xs = length [x | x<- xs, ord x >=97 && ord x <= 122]

{-uppers :: String -> Int -}

freq :: String -> [Float]
freq str = [percent (count1 str x) (lowers(str))| (x <- (['a'..'z'] ++ ['A'..'Z'])]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

chisqr :: [Float] -> [Float] -> Float
chisqr ob es = sum [((o-e)**2.0)/e | (o,e) <- zip ob es]

min1 :: Ord a => [a] -> a
min1 [x] = x
min1 (x:y:xs) = if x < y then min1(x:xs) else min1(y:xs)

positions :: Eq a => [a] -> a -> Int
positions xs x = head ([i | (x',i) <- zip xs [0..],x==x'])

expfreq :: [Float]
expfreq = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

chitab :: String -> [Float]
chitab xs = [chisqr (rotate n (freq xs)) expfreq | n <-[0..25] ]

crack :: String -> String
crack xs = encode xs (-(positions (chitab xs) (min1 (chitab xs))))

crackmod :: String -> Int -> Bool 
crackmod xs n = crack (encode xs n) == xs 


