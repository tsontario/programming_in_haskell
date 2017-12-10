-- concat function using list comprehension
myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

-- Find all factors of a number
myFactors :: Int -> [Int]
myFactors n = [x | x <- [1..(n `div` 2)] ++ [n], n `mod` x == 0]

-- Number is prime?
myPrime :: Int -> Bool
myPrime n = myFactors n == [1,n]

-- Primes up to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2..n], myPrime x]

-- Using zip, list all the positions of a given value in a list
positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (x, i) <- (zip xs [0..(length xs)-1] ), x == n]

-- Count the number of lower case chars in a string
lowers :: String -> Int
lowers s = sum [1 | x <- s, x >= 'a', x <= 'z']

-- Using a list comprehension, give an expression that calculates the sum x^2 for x in range 1..100
sumSquares :: Int
sumSquares = sum [x^2 | x <- [1..100]]

-- Suppose that a coordinate grid of size m x n is given by the list of all pairs (x,y) of integers
-- such that 0<=x<=m and 0<=y<=n. Using a list comprehension, define a function grid ::Int -> Int -> [(Int, Int)]
-- that returns a coordinate grid of a given size.

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Using a list comprehension and the function grid above, define a function square :: Int -> [(Int, Int)]
-- that returns a coordinate square of size n, excluding the diagonal from (0, 0) to (n, n)
square :: Int -> [(Int, Int)]
square n = [(x,y) | x <- [0..n], y <- [0..n], x /= y]

-- In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces
-- a list of identical elements can be defined using a list comprehension
myReplicate :: Int -> a -> [a]
myReplicate n x = [x | n <- [1..n]]

-- A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation x^2 + y^2 = z^2.
-- Using a list comprehension with three generators, define a function pyths :: Int -> [(Int, Int, Int)]
-- that returns the list of all such triples whose components are at most a given limit.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]

-- A positive integer is perfect if it equals the sum of all its factors, excluding the number itself.
-- Using a list comprehension and the function factors, define a function perfects :: Int -> [Int]
-- that returns the list of all perfect numbers up to a given limit
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (myFactors x) == 2*x]


-- can be re-expressed using two comprehensions with single generators.
[x | x <- [([1..2], y<-[3,4])]]

-- Positions: list all the positions of a given value in a list
-- Redefine the function positions using the function find
find :: Eq a => a -> [(a,b)] -> [b] 
find k t = [v | (k', v) <- t, k==k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = [b | (a,b) <- zip xs [0..], (find x [(a,b)] /= [])]

-- The scalar product of two lists of integers xs and ys of length n is given by the su of the products of the
-- corresponding intergers. In a similar manner to chi-square, show how a list comprehension can be used to
-- define a function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists.
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y)<- zip xs ys]

-- Modify the caesar cipher program to also handle upper-case letters
import Data.Char

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2let ((let2int c + n) `mod` 26 - 32)
          | otherwise = c 
            
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]