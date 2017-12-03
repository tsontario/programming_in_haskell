
seqn [] = return []
seqn (act:acts) = do 
    x <- act 
    xs <- acts 
    return (x:xs)

doubleMe x = x + x
doubleUs x y = 2*x + 2*y
doubleSmallNumber x = if x > 100
                        then x
                        else x + x

filterToCaps :: [Char] -> [Char] 
filterToCaps st = [c | c <- st, c `elem` ['A'..'Z']]

mySum [] = 0
mySum (x:xs) = x + mySum xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

revQsort [] = []
revQsort (x:xs) = revQsort larger ++ [x] ++ revQsort smaller
                    where larger = [a | a <- xs, a > x]
                          smaller = [b | b <- xs, b < x]

double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
avg xs = sum xs `div` length xs

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]