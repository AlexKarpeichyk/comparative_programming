-- Lab Sheet 7

-- 1. Types

i :: t -> t
i x = x

k :: t1 -> t -> t1
k x y = x

zero :: t -> t1 -> t1
zero f x = x

one :: (t1 -> t) -> t1 -> t
one f x = f x

two :: (t -> t) -> t -> t
two f x = f (f x)

three :: (t -> t) -> t -> t
three f x = f(f(f x))

s :: (t2 -> t1 -> t) -> (t2 -> t1) -> t2 -> t
s x y z = x z (y z)

w :: (t1 -> t1 -> t) -> t1 -> t
w x y = x y y

--error
--d x y = x x y

newi :: t1 -> t1
newi = s k k

fib :: (Eq a, Num a, Num a1) => a -> a1 -> a1 -> a1
fib n x y = if n == 1 then x else fib (n - 1) (x + y) x

fib2 :: (Eq a, Num t, Num a) => (a, t, t) -> t
fib2 (n,x,y) = if n == 1 then x else fib2 (n - 1, x + y, x)

-- 2. Compatison with java

{-

67^457 

In Java such operation would require at least declaration of a class and a method
that returns the said calculation.

-}


data Shape = Square Float | Circle Float deriving (Show)

area (Square x) = x * x
area (Circle r) = pi * r * r

returnTwo x y = (x, y)

{-

In Java an array of length two would have to be created and returned in a method 

-}
