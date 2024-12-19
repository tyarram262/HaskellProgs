-- import Data.char(toLower)

{-

-}
{-
--Q1
data Tree = Node Char [Tree]
    | Leaf Char
    | Empty

instance Eq Tree where
    Empty == _ = False
    (Leaf c1) == (Leaf c2) = toLower c1 == toLower c2
    (Node c1 x:xs) === (Node c2 y:ys) = (x == y) && (Node xs == Node ys)
    (Node c1 xs) == (Node c2 ys) = toLower c1 == toLower c2 && xs == ys



-- Q2

circle:: IO()
circle = do
    putStrLn "Enter the radius of the circle:"
    radius <- getLine
    let area = pi * radius * radius
    let circumference = 2 * pi * radius
    putStrLn "Area of the circle:" area
    putStrLn "Cicumference of the circle:" circumference

rectangle :: IO()
rectangle = do
    putStrLn "Enter the length of the rectangle:"
    leninp <- getLine
    putStrLn "Enter the width of the rectangle:"
    widinp <- getLine
    let area = widinp * leninp
    let perimeter = (leninp*2) + (widinp * 2)
    putStrLn "Area of the rectangle:" area
    putStrLn "Perimeter of the rectangle:" perimeter

main :: IO ()
main = do
    putStrLn "Select shape (1 for circle, 2 for rectangle)"
    inp <- getLine
    case inp of
        1 -> circle
        2 -> rectangle
        _ -> putStrLn "Invalid Shape"



--Q3
factorial 0 = 1
factorial n = n * factorial (n-1)

-- sint x i = (((-1)*(-1)) `div` factorial (2*i + 1)) * x*xs

-}
sint x n = do
    (i,acc) <- get
    if i<name
    then return (evalState (sine x n) (i+1,acc+ (sint) x i))
    else return acc
{-
Q3 Short answers:
The type of sine function is
1. sine:: Num a => a -> Int -> Int

2.  get:: IO

3. The initial state for using sin should be 

4. sine :: Int -> State Int Int
sine = 

5.every time the state action, get, runs i increases by 1 (1,5) (2, 5)....


-}