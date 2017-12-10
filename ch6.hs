-- Modify the factorial function to return 0 if given a negative number as an argument
factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial (n - 1)
            | n < 0 = 0

-- Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from 
-- a given number down to zero
sumdown :: Int -> Int
sumdown n | n <= 0 = 0
          | otherwise = n + sumdown (n-1)

-- Define the exponentiation operator ^ for non-negtive integers using the same pattern of recursion as
-- the multiplication operator *.
raise :: Int -> Int -> Int
raise _ 0 = 1
raise x y = x * (raise x (y-1))

-- Define a recursive function euclid :: Int -> Int -> Int that implements euclids algorithm for calculating
-- the greatest common divisor of two non-negative integers. If the two numbers are equal, this number is the result.
-- Otherwise, the smaller number is subtracted from the larger and the same process is repeated.
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | otherwise = euclid (min x y) ((max x y) - (min x y))

-- Define the following definitions from the standard prelude, using recursion.
-- AND
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) | x == True = myAnd xs
             | otherwise = False

-- CONCAT
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ (myConcat xss)

-- REPLICATE
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x

-- (!!)
myIndex :: [a] -> Int -> a
myIndex (x:_) 0 = x 
myIndex (x:xs) n = myIndex xs (n-1)

-- Decide if a value is an element of a list
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) | a == x = True
              | otherwise = myElem a xs

-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists
-- are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list
-- separately
-- Hint: first define a function halve :: [a] -> ([a], [a]) that splits a list into two halves whose lengths differ by at most 1
halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort x = merge (msort (fst (halve x))) (msort (snd (halve x)))

-- Using the 5-step process, construct the library functions that:
-- A: calculate the sum of a list of numbers
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum(xs)

-- B: Take a given number of elements from the start of a list
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 x = []
myTake n (x:xs) = x : myTake (n-1) xs

-- C: Select the last element of a non-empty list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs