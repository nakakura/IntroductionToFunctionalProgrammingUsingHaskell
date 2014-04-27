-- on page 11

mycurry	::	((a, b) -> c) -> (a -> b -> c)
mycurry f x y = f(x, y)

multiplec :: Integer -> Integer -> Integer
multiplec = mycurry multiple

myuncurry :: (a -> b -> c) -> ((a, b) -> c)
myuncurry f (x, y) = f x y

multipleuc :: (Integer, Integer) -> Integer
multipleuc = myuncurry multiplec

plus = myuncurry (+)

-- on page 17

smaller :: (Integer, Integer) -> Integer
smaller (x, y) 
	| x <= y = x
	| otherwise = y

smallerif :: (Integer, Integer) -> Integer
smallerif (x, y) = if x < y then x else y

-- on page 19

multi :: (Integer, Integer) -> Integer
multi (x, y) = (a + 1) * (b + 1)
			where a = x + 1; b = y + 1

-- 1.5.1 on page 20

fib :: Integer -> Integer
fib n 	| n < 0 = error "negative"
		| n == 0 = 1
		| n == 1 = 1
		| otherwise = fib(n-1) + fib (n-2)

-- 1.5.2 on page 20

myabs :: Integer -> Integer
myabs x 	| x < 0 = - x
			| otherwise = x
