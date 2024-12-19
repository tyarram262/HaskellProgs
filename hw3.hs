{-
Name    : Tanush Yarram
UIN     : 133000642
email   : t.yarram@tamu.edu
-} 
import Text.Read (readMaybe)

-- Q1 -- Histogram
data BarChart = BarChart {
    labels  :: [String],
    counts  :: [Integer]
}

bar1 = BarChart ["Cat", "Dog", "Others"] [5, 4, 3]

-- Q1 answer goes here

-- Define an instance of show for this histogram
instance Show BarChart where
    show (BarChart labels counts) = concatMap showBar (zip labels counts)
        where
            showBar (label, count) = label ++ replicate (10 - length label) ' ' ++ replicate (fromIntegral count) '*' ++ "\n"



-- Q2 : prefix_calc with exception handling

-- Define your exceptions here
prefix_calc :: String -> Integer -> Integer -> Either String Integer
prefix_calc z x y
    | z == "+" = Right (x+y)
    | z == "-" = Right (x-y)
    | z == "*" = Right (x*y)
    | z == "/" && y == 0 = Left "Exception"
    | z == "/" = Right(x `div` y)
    | otherwise = Left "Invalid operator"

evalq2 :: String -> Integer
evalq2 inp
    | [x, y, z] <- words inp
    , Just yread <- readMaybe y
    , Just zread <- readMaybe z =
        case prefix_calc x yread zread of
            Right result -> result
            Left _ -> 0
    | otherwise = 0



-- Q3 simple type classes

class Animal a where
    speak :: a -> String
    name :: a -> String


data Cat = Cat String
data Dog = Dog String
data Mouse = Mouse String

instance Animal Cat where
    name (Cat x) = x
    speak _ = "Meow"

instance Animal Dog where
    name (Dog x) = x
    speak _ = "Woof"

instance Animal Mouse where
    name (Mouse x) = x
    speak _ = "Squeak"

whatDoAnimalsSay :: Animal a => a -> String
whatDoAnimalsSay x = "I am " ++ name x ++ " and I " ++ speak x



-- Q4 
{--
data MyInt = MyInt Int

-- Instances go here. Do not use deriving for this question.
-- newtype MyInt = MyInt Int

instance Integral MyInt where
    quot (MyInt x) (MyInt y) = MyInt (quot x y)
    rem (MyInt x) (MyInt y) = MyInt (rem x y)
    div (MyInt x) (MyInt y) = MyInt (div x y)
    mod (MyInt x) (MyInt y) = MyInt (mod x y)
    quotRem (MyInt x) (MyInt y) = let (q, r) = quotRem x y in (MyInt q, MyInt r)
    toInteger (MyInt x) = toInteger x

instance Eq MyInt where
    (MyInt x) == (MyInt y) = x==y

instance Ord MyInt where
    compare (MyInt x)(MyInt y) = compare x y

instance Show MyInt where
    show (MyInt x) = show x

instance Num MyInt where
    (MyInt x) + (MyInt y) = MyInt (x+y)
    (MyInt x) - (MyInt y) = MyInt (x-y)
    (MyInt x) * (MyInt y) = MyInt (x*y)
    abs (MyInt x) = MyInt (abs x)
--}
data MyInt = MyInt Int

instance Integral MyInt where
    quot (MyInt x) (MyInt y) = MyInt (quot x y)
    rem (MyInt x) (MyInt y) = MyInt (rem x y)
    div (MyInt x) (MyInt y) = MyInt (div x y)
    mod (MyInt x) (MyInt y) = MyInt (mod x y)
    quotRem (MyInt x) (MyInt y) = let (q, r) = quotRem x y in (MyInt q, MyInt r)
    toInteger (MyInt x) = toInteger x

instance Enum MyInt where
    toEnum x = MyInt (toEnum x)
    fromEnum (MyInt x) = fromEnum x

instance Real MyInt where
    toRational (MyInt x) = toRational x

instance Eq MyInt where
    (MyInt x) == (MyInt y) = x == y

instance Ord MyInt where
    compare (MyInt x) (MyInt y) = compare x y

instance Show MyInt where
    show (MyInt x) = show x

instance Num MyInt where
    (MyInt x) + (MyInt y) = MyInt (x + y)
    (MyInt x) - (MyInt y) = MyInt (x - y)
    (MyInt x) * (MyInt y) = MyInt (x * y)
    abs (MyInt x) = MyInt (abs x)
    signum (MyInt x) = MyInt (signum x)
    fromInteger x = MyInt (fromInteger x)


-- Q5
data Expr  =  Expr (Expr -> Expr -> Expr) Expr Expr
           |  Val Int
           
instance Show Expr where
    show (Val x) = show x

plus (Val x) (Val y) = Val (x + y)
minus :: Expr -> Expr -> Expr
minus (Val x) (Val y) = Val (x - y)
mul (Val x) (Val y) = Val (x * y)
divide (Val x) (Val y) = Val (div x y)

eval e1@(Val x) = e1
eval (Expr op e1 e2) = op (eval e1) (eval e2)

{-
List function calls for expression 1
eval Expr divide (Expr minus (Val 200)(Expr plus (Val 6)(Val 10)))(Expr mul (Val 3)(Val 4))
eval Expr minus(Val 200)(Expr plus(Val 6)(Val 10))
eval (Val 200)
eval(Expr plus(Val 6)(Val 10))
Eval (Val 6)
Eval (Val 10)
plus(Val 6)(Val 10)
minus(Val 200)(Val 6+10)
Eval(Expr mul (Val 3)(Val 4))
Eval (Val 3)
Eval (Val 4)
mul(Val 3)(Val 4)
divide (Val (200-(6+10))) (Val(3*4))
(200-(6+10))/(3*4)
-}

{-
List function calls for expression 2
Eval Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))
Eval (Val 2)
Eval (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))
Eval (Val 6)
Eval (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))
Eval (Val 3)
Eval(Expr divide (Val 4) (Val 2))
Eval (Val 4)
Eval (Val 2)
divide(Val 4)(Val 2)
minus(Val 3)(Val 4/2)
mul(Val 6)(Val 3-(4/2))
plus(Val 2)(Val 6*(3-(4/2)))
(2+(6*(3-(4/2))))
-}


a = Expr divide (Expr minus (Val 200) (Expr plus (Val 6) (Val 10))) (Expr mul (Val 3) (Val 4))
b = Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))