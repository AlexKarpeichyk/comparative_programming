-- 1. Function Arguments

f :: Integer -> Integer -> Integer
f x y = x + y

g :: (Integer, Integer) -> Integer
g (x, y) = x + y

my_curry :: ((a, b) -> c) -> a -> b -> c
my_curry fun x y = fun (x, y)

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry fun (x, y) = fun x y

-- 2. Functions on Lists

first :: Int -> [x] -> [x]
first 0 (h:t) = []
first n (h:t)
	| length (h:t) <= n = (h:t)
	| otherwise = h:(first (n-1) t) 

filt :: Int -> [Int] -> [Int]
filt n [] = []
filt n (h:t)
	| mod h n /= 0 = h:(filt n t)
	| otherwise = filt n t

get_primes [] = []
get_primes (h:t) = h:(get_primes (filt h t))
primes = get_primes [2..]

addOneToAll_1 [] = []
addOneToAll_1 [x] = [x + 1]
addOneToAll_1 (h:t) = (h + 1):(addOneToAll_1 t)

addOneToAll_2 l = [x + 1 | x <- l]

timesTwoToAll l = [x * 2 | x <- l]

addOne x = x + 1
timesTwo x = x * 2 

my_map f [] = []
my_map f [x] = [f x]
my_map f (h:t) = (f h):(my_map f t)

boolToBin [] = []
boolToBin (h:t) = map (\x -> if x == True then 1 else 0) (h:t)

listsToHeads [] = []
listsToHeads (h:t) = map (\x -> head x) (h:t)  

append [] (h:t) = (h:t)
append (h:t) (xh:xt) = h:(append t (xh:xt))

quickSort [] = []
quickSort (h:t) = quickSort [p | p <- t, p <= h] ++ [h] ++ quickSort [i | i <- t, i >= h] 
