import Data.Char

-- 2.2.1 on page 37

nextlet :: Char -> Char
nextlet x 	| x == 'z' = 'a'
			| x == 'Z' = 'A'
			| otherwise = chr( ord x + 1 )

-- 2.2.2 on page 37

digitval :: Char -> Int
digitval x 	| a < 0 = error "not a number"
			| a > 9 = error "not a number"
			| otherwise = a
			where a = ord x - ord '0'

-- 2.3.1 on page 40
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
			deriving (Eq, Ord, Enum, Show)

dayBefore :: Day -> Day
dayBefore x = toEnum . mod (fromEnum x + 6) $ (7:: Int)

-- 2.3.2 on page 40

data Direction = North | East | South | West
			deriving (Eq, Ord, Enum, Show)

myreverse :: Direction -> Direction
myreverse x = toEnum . mod (fromEnum x + 2) $ (4:: Int)
