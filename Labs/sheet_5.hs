-- 1. Data Types 
type Name = [Char]
data MyBool = T | F deriving (Show)
data List = Nil | Cons Int List deriving (Show)

--myand T T = T
--myand T F = F
--myand F T = F
--myand F F = F

myand T T = T
--myand x y = F
myand _ _ = F

len Nil = 0
len (Cons _ t) = 1 + len t

myor F F = F
myor x y = T

myOr F F = F
myOr T F = T
myOr F T = T
myOr T T = T

mynot F = T
mynot T = F

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

beats :: RPS -> RPS -> Bool
{-
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats x y = False
-}

beats x y 
	| x == Paper && y == Rock = True
	| x == Rock && y == Scissors = True
	| x == Scissors && y == Paper = True
	| otherwise = False

data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult Zero x = Zero
mult (Succ Zero) x = x
mult (Succ x) y = add (mult x y) y

eq :: Nat -> Nat -> Bool
eq Zero Zero = True
eq Zero x = False
eq x Zero = False 
eq (Succ x) (Succ y) = eq x y

fact :: Integer -> Integer
fact n = product [1..n]
