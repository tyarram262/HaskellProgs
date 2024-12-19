module Hw6Q3 where
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
import Debug.Trace (traceShowId)

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
                          [(v,out)]  -> [(v,out)]
                          _         -> parse q inp)


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
-- expr       ::= integer | expr - expr
-- This grammer is left recursive. The grammer is modified to remove left recursion.
-- You can verify that it retains it ambiguity.

-- expr  ::=  <integer> <expr'>
-- expr' ::=  "-" <expr> <expr'> | eps

-- Every non-terminal is a type.
-- Every production for a non terminal is a constructor.
-- Terminals representing language keywords will disappear and encoded in the tree structure. 
data Expr =     Expr  Int   Expr' 
            deriving(Show, Eq)
data Expr' =    Expr' Expr  Expr'
            |   Eps
            deriving(Show, Eq)

-- ###################  Part A ##################################


-- Parse tree 1:
-- Expr 1 (Expr' (Expr 2 (Expr' (Expr 3 Eps))) Eps)
-- Parse tree 2:
-- Expr 1 (Expr' (Expr 2 Eps) (Expr' (Expr 3 Eps)))

ast1 :: Expr
ast1 = Expr 1 (Expr' (Expr 2 (Expr' (Expr 3 Eps))) Eps)

ast2 :: Expr
ast2 = Expr 1 (Expr' (Expr 2 Eps) (Expr' (Expr 3 Eps)))


-- ###################  Part A ##################################

expr :: Parser Expr
expr =   do   i  <-  natural
              e1 <- expr'
              return (Expr i e1)            
expr' =   do  symbol "-"
              e1 <- expr
              e2 <- expr'
              return (Expr' e1 e2)
          <|>
          return Eps         
            
    


-- tests
prog1 = "1 - 2 - 3 - 4 - 5 - 6"

getAST prog = case (parse expr prog) of 
                ((t,""):_) -> t
                otherwise -> error "parse error"


-- ###################  Part B ##################################
eval1 :: Expr -> Int
eval1 (Expr i e) = i - evalExpr' e

evalExpr' :: Expr' -> Int
evalExpr' Eps = 0
evalExpr' (Expr' e e') = eval1 e - evalExpr' e'



eval2 :: Expr -> Int
eval2 (Expr i e) = evalLeftAssoc i e

evalLeftAssoc :: Int -> Expr' -> Int
evalLeftAssoc acc Eps = acc
evalLeftAssoc acc (Expr' (Expr i e) e') = evalLeftAssoc (acc - i) e'




-- ###################  Part B ##################################
