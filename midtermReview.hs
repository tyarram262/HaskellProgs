import Data.List (find)
import Data.List (findIndex)


{-
evenlistAverage :: [Int] -> Either String Integer
-- evenListAverage [] = Left "No input"
evenlistAverage xs
    | null (helperavg xs) [] = Left "No input"
    | otherwise = Right toInteger(sum (helperavg xs))
        

helperavg :: [Double] -> [Double]
helperavg ys = x
    where 
        x = [a | a <- ys, a`mod` 2 == 0]

evenlistAverage :: [Double] -> Either String Int
evenlistAverage xs
    | null evens = Left "No Input"  -- Return "No Input" if there are no even numbers
    | otherwise  = Right (sum evens / fromIntegral (length evens))
    where
        evens = filter isEven xs

-- Helper function to check if a Double is an even integer
isEven :: Double -> Bool
isEven x = x == fromIntegral (round x) && round x `mod` 2 == 0

-}


data Cat = Cat {
    catName :: String,
    catID :: Int
} deriving (Show, Eq)


findCatIndex :: [Cat] -> String -> Int -> Maybe Int
findCatIndex cats fname fid = fmap (+1) $ findIndex (\cat -> catName cat == fname && catID cat == fid) cats

getCatByIndex :: [Cat] -> Maybe Int -> Maybe Cat
getCatByIndex cats (Just fid) = find(\x -> catID x == fid) cats


listInsert :: Int -> [Int] -> [Int]
listInsert x xs
    | x `elem` xs = xs
    | otherwise = ys ++ [x] ++ zs
            where
                ys = [a | a <- xs, a < x]
                zs = [a | a <- xs, a > x]




{-data Random a b = RandomX a b | RandomY a | RandomZ b
    deriving (Show)
-}

{-
Product data type definition
Point = Point a b
-}

{-
Short answer 1:
h :: String -> Show a => a -> String

Short answer 2:
foldl :: (a->b->a) -> a -> [b] -> a 
f is of type (a->b->a)
a is string due to "" and b is type Bool due to the type definition
therefore f is of type (String -> Bool -> String)

Short answer 3:
Because the function filter returns a list of type a and the head function takes a value of type a which may or may not be able to be added to x since we dont check if the head of the list (x) is a digit or not
The add function uses the + operator and therefore both arguments passed must be of type num. In function xyz, x is extracted from list pattern (x:xs) therefore allowing x to be of any type
The function xyz will face a type error since the isDigit function is of type Char -> Bool and therefore filtering isDigit from xs will give a list of chars, thereby making the head of this list a char and not a num so when you perform add, it will give an error for using type char and possibly also because x is unknown and may not be num either.

Short answer 4:
trace :: String -> a -> a
should only be used for debugging or monitering execution
outputs the trace message given before returning the second argument as its result
The main difference between trace and traceshow in haskell is that traceshow derives the show function and therefore is able to print the result at the end whereas trace isnt.
trace :: String -> a -> a
traceShow :: Show a => a -> b -> b
traceShow makes it convenient to print the values of interesting values inside of a function by using show on the argument to convert it to string

Short answer 5: 
In order to define an instance of the bounded typeclass the minimum set of functions you must implement would be minBound and maxBound that represent the lowest
and highest possible value of the type respectively



-}
{- g :: [Bool] 
g a = foldl  f  ""  a  -}
