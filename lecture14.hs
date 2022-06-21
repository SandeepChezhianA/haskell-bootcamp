--  All p xs = {checks if all the elements of a list satisfy p}
-- Phone number check - All p xs = {Checks }

--Any1 p xs = {Checks if any element satisfies p}

-- None p xs = if none of the members of xs satisfy p.


all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = ((length(filter (p) (xs))) == (length(xs)))

any1 :: (a -> Bool) -> [a] -> Bool
any1 p xs = (length(filter (p) (xs))) > 0 

none1 :: (a -> Bool) -> [a] -> Bool
none1 p xs = not (any1 p xs)


product1 ::


