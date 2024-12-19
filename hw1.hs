{-
Name    : Tanush Yarram
UIN     : 133000642
email   :t.yarram@tamu.edu
-} 


-- Q1 -- Fixing quicksort
quicksort :: [Int] -> [Int]
quicksort  []     = []
quicksort  (x:y) = quicksort  ys ++ [x] ++ quicksort zs
           where
             ys = [a | a <- y, a > x]
             zs = [b | b <- y, b <= x]



-- Q2 -- randomCalculation. Revise number systems if needed
randomCalc x = x + (x*11) + (x*111)



-- Q3 -- nth Lucus number
lucas :: Int->Int
lucas 0 = 2
lucas 1 = 1
lucas x = lucas(x-1) + lucas (x-2)


-- Q4 -- average of a List
average :: [Int] -> Double
average x = fromIntegral (sum x) / fromIntegral (length x)


-- Q5 -- Grade calculation type
data Scores a b c d = Scores { classAverage :: a  
                     , hwAverage :: b  
                     , midterm :: c
                     , final :: d   
                     } deriving (Show)  




-- Q6 -- total score calculation for grade
totalPoints a = (hwAverage a * 0.45) + (midterm a * 0.25) + (final a * 0.25) + (classAverage a * 0.05)


-- Q7 -- Polynomial calculator
polyCalcFactory :: [Double] -> Double -> Double
polyCalcFactory a x = (a!!0 * x*x) + (a!!1 *x) + a!!2