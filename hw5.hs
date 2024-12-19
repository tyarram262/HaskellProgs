import Control.Monad.State
import qualified Data.Map as Map
{-
Name    : Tanush Yarram
UIN     : 133000642
email   : t.yarram@tamu.edu
-} 

data Clist a = End a
            |  Node a (Clist a)

-- Q1 implementations go here

instance Functor Clist where
    fmap f (End n)= End (f n)
    fmap f (Node x xs) = Node (f x) (fmap f xs)

instance Applicative Clist where
    pure a = End a

    (End f) <*> (End x) = End (f x)
    (End f) <*> (Node x xs) = Node(f x) (pure (f x))
    (Node f fs) <*> (Node x xs) = Node (f x) (fs <*> pure x)
    (Node f fs) <*> (Node x xs) = Node (f x) (fs <*> xs)

instance Monad Clist where
    (End x) >>= f = f x
    (Node x xs) >>= f = append (f x) (xs >>= f)
        where
            append (End x) ys = Node x ys
            append (Node x xs) ys = Node x (append xs ys)


squares lst = do
    			x <- lst
    			return (x * x)

-- Q2 a
fib1 0 = 0
fib1 1 = 1
fib1 x = fib1(x-1) + fib1(x-2)



-- Q2b
fib2 :: Int -> State Int Int
fib2 0 = do
    modify (+1)
    return 0
fib2 1 = do
    modify(+1)
    return 1
fib2 n = do
    modify (+1)
    f1 <- fib2 (n-1)
    f2 <- fib2 (n-2)
    return (f1 + f2)

    
-- Q2c
fib3 :: Int -> State (Map.Map Int Int) Int
fib3 0 = return 0
fib3 1 = return 1
fib3 n = do
    cache <- get
    case Map.lookup n cache of
        Just result -> return result
        Nothing -> do
            f1 <- fib3 (n-1)
            f2 <- fib3 (n-2)
            let result = f1 + f2
            modify (Map.insert n result)
            return result