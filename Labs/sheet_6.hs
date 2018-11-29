-- Lab Sheet 6

-- 1. Higher-Order functions on lists

sumAll :: [Int] -> Int
sumAll [] = 0
sumAll [x] = x
sumAll (h:t) = h + sumAll t 

multAll :: [Int] -> Int
multAll [] = 1
multAll [x] = x
multAll (h:t) = h * multAll t

--fold :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fold f b [] = b
fold f b (h:t) = f h (fold f b t)

len = fold (\_ y -> 1 + y) 0 
 
maxElem = fold max 0 

flatten = fold (++) []

-- 2. Data types

data IntOrBool = In Int | Bo Bool deriving (Show)
mixList :: [IntOrBool]
mixList = [In 0, Bo True, In 1, Bo False]

type Var = Char
data M = V Var | Lambda Var M | App M M deriving (Show)

-- 3. Extras

--fix :: (a -> a) -> a
--fix f = let {x = f x} in x
fix f x = f (fix f x)

--fact :: (Integer -> Integer) -> Integer -> Integer
fact = \rec n -> if n == 0 then 1 else n * rec (n - 1)
