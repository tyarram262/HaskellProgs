module Hw6Q1 where
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
-- expr       ::= ifexpr | cmpexpr
-- ifexpr     ::= 'if' expr 'then' expr 'else' expr
-- cmpexpr    ::= sumexpr '==' cmpexpr | sumexpr
-- sumexpr    ::= term '+' sumexpr | term
-- term       ::= factor '*' term | factor
-- factor     ::= '(' expr ')' | <natural>

-- Every non-terminal is a type.
-- Every production for a non terminal is a constructor.
-- Some terminals will disappear and encoded in the tree structure. 
data Expr =     Expr1 IfExpr
            |   Expr2 CmpExpr 
          deriving(Show)
data IfExpr =   IfExpr1 Expr Expr Expr 
          deriving(Show)
data CmpExpr =  CmpExpr1 SumExpr CmpExpr 
            |   CmpExpr2 SumExpr 
          deriving(Show)
data SumExpr =  SumExpr1 Term
            |   SumExpr2 Term SumExpr 
          deriving(Show)
data Term =     Term1 Factor
            |   Term2 Factor Term 
          deriving(Show)
data Factor =   Factor1 Expr
            |   Factor2 Int 
          deriving(Show)

-- ###################  Part A ##################################

ast1 :: Expr
ast1 = Expr2 (CmpExpr2 (SumExpr1 (Term1 (Factor2 10))))

ast2 :: Expr
ast2 = Expr2 (CmpExpr2 (SumExpr1 (Term2 (Factor2 10) (Term1 (Factor2 2)))))

ast3 :: Expr
ast3 = Expr2 (CmpExpr2 (SumExpr2 (Term2 (Factor2 10) (Term1 (Factor2 2))) (SumExpr1 (Term1 (Factor2 3)))))

ast4 :: Expr
ast4 = Expr2 (CmpExpr2 (SumExpr2 (Term1 (Factor2 3)) (SumExpr1 (Term2 (Factor2 10) (Term1 (Factor2 2))))))


-- ###################  Part B ##################################

expr :: Parser Expr
expr = do 
    x <- ifexpr
    return (Expr1 x)
  <|> do 
    y <- cmpexpr
    return (Expr2 y)

cmpexpr :: Parser CmpExpr
cmpexpr = do x <- sumexpr
             do symbol "=="
                y <- cmpexpr
                return (CmpExpr1 x y)
              <|> return (CmpExpr2 x)

sumexpr :: Parser SumExpr
sumexpr = do x <- term
             do symbol "+"
                y <- sumexpr
                return (SumExpr2 x y)
              <|> return (SumExpr1 x)

term :: Parser Term
term = do x <- factor
          do symbol "*"
             y <- term
             return (Term2 x y)
           <|> return (Term1 x)

factor :: Parser Factor
factor = do symbol "("
            x <- expr
            symbol ")"
            return (Factor1 x)
          <|> do y <- natural
                 return (Factor2 y)

ifexpr :: Parser IfExpr
ifexpr = do symbol "if"
            x <- expr
            symbol "then"
            y <- expr
            symbol "else"
            z <- expr
            return (IfExpr1 x y z)



-- ###################  Part C ##################################
-- Interpreter based on AST
evalFactor :: Factor -> Int
evalFactor = undefined

evalTerm :: Term -> Int
evalTerm = undefined

evalSum :: SumExpr -> Int
evalSum = undefined

evalCmp :: CmpExpr -> Int
evalCmp = undefined

evalIf :: IfExpr -> Int
evalIf = undefined

eval :: Expr -> Int
eval = undefined


-- tests
prog1 = "5"
prog2 = "5 + 3"
prog3 = "5 + 3 + 1"
prog4 = "if 3 == 3 then 5 + 3 else 4"
prog5 = "if 2 == 3 then 5 + 3 else 4"

getAST prog = case (parse expr prog) of 
                ((t,""):_) -> t
                otherwise -> error "parse error"