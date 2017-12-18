import Data.Char
sumsquareeven :: Int -> Int
sumsquareeven n =sum (map (^2) (filter even [1..n]))

snoc x xs = xs ++ [x]

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)
-- Binary to Integer converter 

type Bit = Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> [Bit]
parity xs | odd bits = 1 : xs
          | otherwise = 0 : xs  
          where bits = length (filter (==1) xs)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = (parity (take 8 xs)) : chop8 (drop 8 xs)

channel :: a -> a
channel = id
badChannel = tail

transmit = decode . channel . encode
badTransmit = decode . badChannel . encode


parityCheck (1:xs) | odd (length (filter (==1) xs)) && (length xs == 8) = xs
                   | otherwise = error "Bad parity check!"
parityCheck (0:xs) | even (length (filter (==1) xs))  && (length xs == 8) = xs
                   | otherwise = error "Bad parity check!"

decode :: [Bit] -> String
decode = map (chr . bin2int . parityCheck) . chop8

-- Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter
mapAndFilter f p xs = map f (filter p xs) 

-- Define the following higher-order functions

-- All
all' :: (a->Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

-- Any
-- any' :: (a->Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any' p xs

-- takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x /= True = []
                    | otherwise = x : takeWhile' p xs

-- dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x == True = dropWhile' p xs
                    | otherwise = (x:xs)

-- Redefine map f and filter p using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y-> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if (p x) then x : y  else y) [] 
                                  
-- Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer
-- For example: dec2int [2,3,4,5] -> 2345

dec2int xs = foldl (\x y -> x*10+y) 0 xs


curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y 

-- Using unfold, redefine chop8, map f, and iterate f
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

-- map'' :: (a->b) -> [a] -> [b]
map'' f = unfold (==[]) (f . head) tail

-- iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its two argument 
-- functions to successive elements in a list in turn about order.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 (x:xs) = altMap' f1 f2 (x:xs)

altMap' _ _ [] = []
altMap' f1 f2 (x:xs) = f1 x : altMap'' f1 f2 xs

altMap'' _ _ [] = []
altMap'' f1 f2 (x:xs) = f2 x : altMap' f1 f2 xs

-- Using altMap, define a function luhn :: [Int] -> Bool that implements the Luhn algorithm from the exercises in chapter 4 for
-- bank card numbers of any length. Test your new functionusing your own bank card.
-- luhn :: [Int] -> Bool
subtract9 :: Int -> Int
subtract9 x | x > 9 = x - 9
            | otherwise = x 

luhnPreProcess = map (subtract9) . (altMap id (*2))
luhn xs = sum (luhnPreProcess (reverse xs)) `mod` 10 == 0