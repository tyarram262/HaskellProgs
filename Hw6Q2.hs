module Hw6Q2 where
-- Basic parser with applicative and monadic combinators.
--
-- Based upon chapter 13 of Hutton's "Programming in Haskell" (2nd ed)
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.

{-
Name    : Tanush Yarram
UIN     : 133000642
email   : t.yarram@tamu.edu
-} 

import Control.Applicative
import Data.Char


newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v,out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                          []        -> parse q inp
                          [(v,out)] -> [(v,out)])

-- Basic building block: item parses a single char from the input.
item :: Parser Char
item = P (\inp -> case inp of
                    []      -> []
                    (x:xs)  -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Token wraps a parser with space-ignoring capabilities.
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- BNF grammar for our language:
--
-- move       ::= command | command move
-- command    ::= "up"    integer
--            |   "down"  integer
--            |   "right" integer
--            |   "left" integer

-- Every non-terminal is a type.
-- Every production for a non terminal is a constructor.
-- Some terminals will disappear and encoded in the tree structure. 

-- ########################### Part A ########################

data Move = Single Command
          | Sequence Command Move
          deriving (Show)

data Command = Up Int
             | Down Int
             | Right Int
             | Left Int
             deriving (Show)


move :: Parser Move
move = do c <- command
          (do m <- move
              return (Sequence c m))
           <|> return (Single c)

command :: Parser Command
command = (do symbol "up"
              n <- integer
              return (Up n))
       <|> (do symbol "down"
               n <- integer
               return (Down n))
       <|> (do symbol "right"
               n <- integer
               return (Right n))
       <|> (do symbol "left"
               n <- integer
               return (Left n))



-- ########################### Part A ########################

-- ########################### Part B ########################

ast1 :: Move
ast1 = Single (Down 20)

ast2 :: Move
ast2 = Sequence (Left 5) (Single (Down 2))

ast3 :: Move
ast3 = Sequence (Right 7) (Sequence (Up 6) (Single (Down 1)))


-- ########################### Part B ########################


-- ########################### Part C ########################

command :: Parser Command
command = (do symbol "up"
              n <- integer
              return (Up n))
       <|> (do symbol "down"
               n <- integer
               return (Down n))
       <|> (do symbol "right"
               n <- integer
               return (Right n))
       <|> (do symbol "left"
               n <- integer
               return (Left n))

move :: Parser Move
move = do c <- command
          (do m <- move
              return (Sequence c m))
           <|> return (Single c)

-- ########################### Part C ########################


-- ########################### Part D ########################
evalCommand :: Command -> (Int, Int) -> (Int, Int)
evalCommand (Up n) (x, y)    = (x, y + n)
evalCommand (Down n) (x, y)  = (x, y - n)
evalCommand (Right n) (x, y) = (x + n, y)
evalCommand (Left n) (x, y)  = (x - n, y)

eval :: Move -> (Int, Int) -> (Int, Int)
eval (Single cmd) loc       = evalCommand cmd loc
eval (Sequence cmd rest) loc = eval rest (evalCommand cmd loc)


-- ########################### Part D ########################

-- tests
prog1 = "up 10"
prog2 = "up 10 down 20"
prog3 = "up 10 down 20 right 10"
prog4 = "up 10 down 20 right 10 left 20"
prog5 = "up 10 down 20 right 10 left 20 down -10 left -10"


