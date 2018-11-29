double x = x + x
quad x = double (double x)
square_1 x = x * x
square_2 x = x^2
i x = x
firstone x y = x
secondone x y = y
fortytwo x = 42
infinity = infinity + 1
apply x y = x y
twice x y = x (x y)
fib_1 x = if x == 0 || x == 1 then 1 else if x > 1 then fib_1 (x - 1) + fib_1 (x - 2) else -1
fib_2 x
	| x == 1 = 1
	| x == 0 = 1
	| x >  1 = fib_2 (x - 1) + fib_2 (x - 2)
