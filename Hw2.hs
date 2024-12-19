module Hw2 where
import Data.Char (digitToInt)
import Data.Char (toUpper)
{-
Name    : Tanush Yarram
UIN     : 133000642
email   : t.yarram@tamu.edu
-} 


-- Q1 -- GCD
gcd' :: Int -> Int -> Int
gcd' x y
    | x<0 || y<0 = error "Exception: One of the inputs is negative." 
    | x == y    = x
    | x > y     = gcd' (x - y) y
    | otherwise = gcd' x (y - x)





-- Q2 -- unwrapList
unwrapList  :: [Maybe Int] -> [Int]
unwrapList  xs = [x | Just x <- xs]




-- Q3 -- drop nth
dropNth :: Int -> [a] -> [a]
dropNth x y = helper x y 1
  where
    helper :: Int -> [a] -> Int -> [a]
    helper _ [] _ = []
    helper z (x:xs) i
        | i `mod` z == 0 = helper z xs (i + 1)
        | otherwise      = x : helper z xs (i + 1)


-- Q4 -- asInt. Initial code copy given
asInt :: String -> Int
asInt ('-':xs) = - (foldl (\prev curr -> prev * 10 + digitToInt curr) 0 xs)
asInt xs       = foldl (\prev curr -> prev * 10 + digitToInt curr) 0 xs


-- Q4 -- average of a List
average :: [Int] -> Double
average x = fromIntegral (sum x) / fromIntegral (length x)

-- Q5 -- mergesort
halve :: [a] -> ([a], [a])
halve xs =  splitAt mid xs
  where mid = length xs `div` 2 + length xs `mod` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


mergesort :: (Ord a) => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort ys) (mergesort zs)
  where
    (ys, zs) = halve xs



-- Q6 -- capitalize
capitalize :: [String] -> [String]
capitalize xs = map (map toUpper) xs