halve :: [a] -> ([a], [a])
halve (x:xs) | even (length (x:xs)) = (take (length(x:xs) `div` 2) (x:xs), drop (length(x:xs) `div` 2) (x:xs))

-- Define a function third :: [a] -> a that returns the third element in a list that contains
-- at least this many elements using 
-- head and tail
third :: [a] -> a
--third (x:xs) | length (x:xs) >= 3 = head (tail (xs))

-- list indexing (!!)
--third (x:xs) | length (x:xs) >= 3 = (x:xs) !! 2

-- pattern matching
third (_:_:a:_) = a

-- Consider a function safetail :: [a] -> a that behaves in the same way as tail
-- except that it maps the empty list to itself rather than producting an error. 
-- Using tail and the function null :: [a] -> Bool that decides if a list is empty
-- or not, define safetail using
-- A conditional expression
--safetail xs = if xs == [] then [] else tail xs
-- guarded equations
-- safetail xs | xs == [] = []
--             | otherwise = tail xs
--pattern matching
safetail [] = []
safetail (_:xs) = xs

-- Without using any other library functions or operators, show how the meaning
-- of the following pattern matching definition for logical conjunction && an be
-- formalised using conditional expressions:
myAnd :: Bool -> Bool -> Bool
myAnd x y = if x then if y then True else False else False

-- Do the same for the following alternative definition, and note the difference in the
-- number of conditional expressions that are required:
-- True && b = b
-- False && _ = False
myAndTwo :: Bool -> Bool -> Bool
myAndTwo x y = if x == True then y else False

-- Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts
-- 9 if the result greater than 9.
luhnDouble :: Int -> Int
luhnDouble x | 2 * x > 9 = (2 * x) - 9
             | otherwise = 2 * x

-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank
-- card number is valid
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0