module Main where
{-
Name    : Tanush Yarram
UIN     : 133000642
email   : t.yarram@tamu.edu
-} 


-- Q1 -- CSV file. Grade claculations.

{-
gave it my best shot but csv processing is hard man...
-}

import System.Environment (getArgs)
import Data.List (intercalate)

procStudent :: [String] -> [String]
procStudent (name:hw1:hw2:hw3:[]) =
    let scores = map read [hw1, hw2, hw3] :: [Int]
        total = sum scores
    in [name, hw1, hw2, hw3, show total]
procStudent row = row

procCSV :: [[String]] -> [[String]]
procCSV [] = []  -- Handle empty files gracefully
procCSV (header:rows) =
    let updatedHeader = header ++ ["Total"]
        processedRows = map procStudent rows
    in updatedHeader : processedRows

splitOnComma :: String -> [String]
splitOnComma [] = [""]
splitOnComma (c:cs)
    | c == ','  = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOnComma cs

main :: IO ()
main = do
    args <- getArgs
    let inputFile = "input.csv"
    let outputFile = "output.csv"
    content <- readFile inputFile
    let lined = lines content
        csvData = map splitOnComma lined
        procData = procCSV csvData
        outputContent = unlines (map (intercalate ",") procData)
    writeFile outputFile outputContent

{-
main :: IO ()
main = do
    args <- getArgs
    let inp = args !! 0
    let outp = "output.csv"
    inpCont <- readFile inp
    let modifiedOut = processCSV (lines inpCont)
    writeFile outp (unlines modifiedOut)
-}


{-
hwavgcalculator :: [Int] -> Float
hwavgcalculator x = (fromIntegral (sum x)) / (fromIntegral (length x))
-}



-- Q2 : matrix Multiplication
-- instance Num a :: Num (Matrix a) where
data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance Num a => Num (Matrix a) where
    Matrix a * Matrix b = Matrix [[sum $ zipWith (*) row col | col <- (cols b)] | row <- a]
      where
        cols m = map (\i -> map (!! i) m) [0..(length (head m) - 1)]
    
    (+) = undefined
    (-) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined


-- Q3 tree traversals
data Tree a b = Leaf a | Branch b (Tree a b) (Tree a b)

-- takes two parameters to create a heterogeneous tree where data stored in leaves can be different from those stored in the branches

-- Pre-order traversal
preorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder l br (Leaf a) = [l a]
preorder l br (Branch b a c) = [br b] ++ preorder l br a ++ preorder l br c

-- In-order traversal
inorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder l br (Leaf a) = [l a]
inorder l br (Branch b a c) = inorder l br a ++ [br b] ++ inorder l br c

-- Post-order traversal
postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder l br (Leaf a) = [l a]
postorder l br (Branch b a c) = postorder l br a ++ postorder l br c ++ [br b]


{-
main :: IO ()
main = return ()
-}