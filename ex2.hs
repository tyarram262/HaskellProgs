import System.Environment (getArgs)
import Data.List (intercalate)

-- Split a CSV line by commas
splitCSV :: String -> [String]
splitCSV [] = []
splitCSV xs =
    let (first, rest) = span (/= ',') xs
    in first : case rest of
                  []      -> []
                  (_:rs)  -> splitCSV rs

-- Convert a list of strings (grades) to a list of Ints
convertToInts :: [String] -> [Int]
convertToInts = map read

-- Calculate the total score of a student
totalScore :: [Int] -> Int
totalScore = sum


-- Process the entire CSV file content
processCSV :: [String] -> [String]
processCSV (header:rows) = 
    let newHeader = header ++ ",Total"
        processedRows = map (intercalate "," . processStudent . splitCSV) rows
    in newHeader : processedRows

-- Main function to read, process, and write CSV
main :: IO ()
main = do
    args <- getArgs
    let inputFile = args !! 0
        outputFile = args !! 1
    content <- readFile inputFile
    let csvLines = lines content
    let processedCSV = processCSV csvLines
    writeFile outputFile (unlines processedCSV)
