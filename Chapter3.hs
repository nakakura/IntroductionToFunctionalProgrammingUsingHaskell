
-- 3.1

data Nat = Zero | Succ Nat
			deriving (Eq, Ord, Show)

plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = Succ(plus m n)

(+)  :: Nat -> Nat -> Nat
m + Zero    = m
m + Succ n  = Succ ( m Main.+ n )

(*) :: Nat -> Nat -> Nat
m * Zero = Zero
m * Succ n = m Main.* n Main.+ m

(-) :: Nat -> Nat -> Nat
m - Zero = m
Succ m - Succ n = m Main.- n
Zero - Succ m = Zero

fact :: Nat -> Nat
fact Zero = Succ Zero
fact (Succ m) = Succ m Main.* fact m

fib :: Nat -> Nat
fib Zero = Zero
fib (Succ Zero) = Succ Zero
fib (Succ m) = fib m Main.+ fib (m Main.- Succ Zero)

