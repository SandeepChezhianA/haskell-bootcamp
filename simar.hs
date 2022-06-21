factorial :: Integer -> Integer 
factorial n = product[1..n]

sfactorial :: Integer -> Integer
sfactorial 1 = 1  
sfactorial n = factorial n + sfactorial (n - 1)

squares :: Integer -> Integer
squares n = n * n 

ssquares :: (Integer -> Integer) -> Integer -> Integer 
ssquares f 1 = 1
ssquares f n = f n + ssquares f (n - 1)

tsquares :: Integer -> Integer
tsquares n = ssquares squares n


absolute n = (if n >= 0 then n else (-n) )
signum n = 