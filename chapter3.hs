import Text.XHtml (yellow)
add' :: Int -> (Int -> Int)
add' x y = x + y 

add1 :: (Int,Int) -> Int
add1 (x,y) = x + y

-- Curried functions let you skip the bracket. 

