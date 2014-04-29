data Nat = Zero | Succ Nat
			deriving (Eq, Ord, Show)

-- 3.1.1

data PosInt = One | Suc PosInt 
 			deriving (Eq,Ord,Show)

-- 3.1.2

convert :: Nat -> PosInt
convert (Succ Zero) = One
convert (Succ m) = Suc(convert m)

-- 3.1.3

(+) :: PosInt -> PosInt -> PosInt
(+) m One = Suc(m)
(+) m (Suc n) = Suc((Main.+) m n)

(*) :: PosInt -> PosInt -> PosInt
(*) m One = m
(*) m (Suc n) = (Main.+) ((Main.*) m n) m

-- 3.1.4
{-
m * nは、n回展開されるので

3 * 2 = 3 * 1 + 3
	  = 3 * 0 + 3 + 3
	  = 3 + 3
	  = Suc(3 + 2)
	  = Suc(Suc(3 + 1))
	  = Suc(Suc(Suc 3))

m * n = m * (n-1) + m
	  = m * (n-2) + m
	  = ...
	  = m * 0 + m + m + m + ... + m
	  = m + m + m + ... + m (mがn個並ぶ)
という乗算の展開フェーズの評価回数ががn + 1回

1組のm + mの評価回数は、
m + m = Suc(m + (m-1))
	  = Suc(Suc(m + (m-2)))
	  = ...
	  = Suc(Suc...(Suc(m + 0))...)
	  = Sum(Suc...(Suc(m))...)
でm + 1回であるので、n-1組のmの加算回数は
(n - 1) * (m + 1)

合計(n - 1) * (m + 1) + (n + 1) = nm + 2n - m回
-}

-- 3.1.5

(-) :: Nat -> Nat -> Nat
m - Zero = m
Succ m - Succ n = m Main.- n
Zero - Succ m = Zero