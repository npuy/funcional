-- 1. Considere la siguiente definici ́on de los n ́umeros naturales:
data Nat = Zero | Succ Nat
    deriving Show
-- Ejemplos de naturales son: Zero, Succ Zero, Succ (Succ Zero), etc. De
-- esta forma, la representaci ́on del n - ́esimo natural es de la forma SuccnZero.
-- (a) Defina las siguientes funciones sobre los naturales:
int2nat 0 = Zero
int2nat n = Succ $ int2nat $ n-1

-- nat2int :: Nat →Int convierte un natural en entero
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- duplica :: Nat →Nat doble del argumento
duplica Zero = Zero
duplica (Succ n) = Succ . Succ $ duplica n

-- exp2 :: Nat →Nat exponente en base 2
exp2 Zero = Succ Zero
exp2 (Succ n) = duplica $ exp2 n

-- suma :: Nat →Nat →Nat suma de naturales
-- suma Zero n = n
-- suma (Succ n) m = Succ $ suma n m
-- suma Zero n = n
-- suma (Succ n) m = suma n $ Succ m
suma Zero n = n
suma n Zero = n
suma (Succ n) (Succ m) = Succ . Succ $ suma n m

-- predecesor :: Nat →Nat predecesor (predecesor de cero es cero)
predecesor Zero = Zero
predecesor (Succ n) = n

-- (b) El fold para los naturales se define de la siguiente manera:
foldN :: (a -> a) -> a -> Nat -> a
foldN h e Zero = e
foldN h e (Succ n) = h (foldN h e n)
-- Defina las funciones de la parte a) en funci ́on de foldN .
nat2intfn = foldN (1+) 0

duplicafn = foldN (Succ . Succ) Zero

exp2fn = foldN duplica (Succ Zero)

sumafn n m = foldN Succ m n

-- (c) Defina la funci ́on fib :: Nat → Nat que computa los n ́umeros de fi-
-- bonacci. Una forma de definir esta funci ́on es por la conocida f ́ormula
-- de recurrencia que caracteriza estos n ́umeros. Implem ́entela.
fib Zero = Succ Zero
fib (Succ Zero) = Succ Zero
fib (Succ (Succ n)) = suma (fib $ Succ n) $ fib n

-- Es bien sabido que dicha definici ́on tiene un problema: su compor-
-- tamiento es exponencial. Pruebe aplicarla con valores crecientes de
-- naturales y lo podr ́a verificar. Hay una mejor definici ́on, que es lin-
-- eal, la cual utiliza una forma “iterativa” para computar los n ́umeros
-- de fibonacci. Implemente esta definici ́on alternativa.
fibFast = fst . foldN fn (Succ Zero, Zero)
    where fn (a, b) = (suma a b, a)
