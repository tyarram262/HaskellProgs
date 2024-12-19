randomCalc n = n + (n*11)+ (n*111)


lucas:: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n = lucas(n-1) + lucas(n-2)


average :: [Double] -> Double
average xs = (foldl (+) 0 xs) / fromIntegral(length xs)


-- type definition for a scores
data Scores a b c d = Scores{classAverage :: a,
                                hwAverage :: b,
                                midterm::c,
                                final::d}

totalPoints a = (hwAverage a * 0.45) + (midterm a * 0.25) + (final a * 0.25) + (classAverage a * 0.05)

polyCalcFactory :: [Double] -> Double -> Double
polyCalcFactory xs x = (xs!!0 * x*x) + (xs!!1 * x) + xs!!2