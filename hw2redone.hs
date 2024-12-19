-- Q1 -- GCD
gcd' :: Int -> Int -> Int
gcd' x y
    | x<0 || y<0 = error "no"
    | x > y = gcd'(x-y) y
    | x < y = gcd' x (y-x)
    | x == y = x



-- Q2 -- unwrapList
unwrapList :: [Maybe Int] -> [Int]
unwrapList x = [y | Just y <- x]

{-
-- Q3 -- drop nth 
dropNth :: Int -> [Int] -> [Int]
dropNth _[]_ = []
dropNth x y = helper x y 1

helper _[]_ = []
helper x (y:ys) z
    | z `mod` x == 0 = helper x ys (z+1)
    | otherwise = y: helper x y (z+1)
-}

dropNth :: Int -> [Int] -> [Int]
dropNth _ [] = []
dropNth x y = take (x-1) y ++ dropNth x (drop x y)


{-
-- Q4 -- asInt. Initial code copy given

asInt xs = loop 0 xs
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs


asInt xs = foldl digitToInt ' ' xs

-- Q4 -- average of a List
average x = x       -- Replace identity placeholder with your code.


-- Q5 -- mergesort
halve x = x         -- Replace  placeholder with your code.

merge x y = x       -- Replace  placeholder with your code.

mergesort x = x      -- Replace  placeholder with your code.


-- Q6 -- capitalize
capitalize   x = x    -- Replace placeholder with your code.
-}