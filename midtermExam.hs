{-
Short answer 1(a):
foldr is of type (a->b->b) -> b -> [a] ->b
meaning that function f is of type {a->b->b}
where a is type string beacuse of the type definition and b is a list of doubles also because of the type definition so therefore this would make our f function into type Double -> String -> String

Short answer 1(b):
this type definition can be rewritten in the following fashion:
data Table = Table rows cols tableData

rows :: Table -> Int
cols :: Table -> Int
tableData :: Table -> [[Double]]

Short answer 1(c):
alternate :: (a->Maybe) -> (a->Maybe) -> Show a => a -> String

Short answer 1(d):
myfilter :: (a->Bool) ->[a] ->[a]
myfilter _ [] = []
myfilter f (x:xs)
    | f x 
    | otherwise = myfilter f xs

Short answer 2(a):
insertValue t x = preorder t x
    | preorder t x == [] = []
    | otherwise = 

--Preorder traverser

preorder :: a -> b -> Tree a -> [c]
preorder l (Leaf a) = [l a]
preorder l (Node a) = [br b] ++ preorder l br a ++ preorder l br c

Short answer 2(b):
lookupTree t x = preorder t x
    | preorder t x == t = t
    | otherwise = Nothing



 
-}

-- data Table = Table rows cols tableData
-- data Point = Point a b




-- Lambda function: (/x <- x+1)

{-
-- Q3 List average
listAverage :: [Double] -> Either String Double
listAverage xs
    | null (helperavg xs) = Left error "No Input"
    | otherwise = Right (sum (helperavg xs)) `div` length(helperavg xs)

helperavg :: [Double] -> [Double]
helperavg ys = x
    where 
        x = [a | a <- ys, a<=100 && a>=0]

-}