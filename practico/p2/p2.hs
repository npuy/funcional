-- 1. Dada la siguiente funci ́on
-- dup x = (x , x )
-- Explique en que difieren (dup ◦dup) y (dup dup).
-- Difieren en el concepto 
-- dup . dup es la concatenacion de funciones, es aplicar dos veces seguidas dup
-- dup dup es aplicar dup a la funcion dup, es una tupla de funciones
-- dup :: a -> (a, a)
dup x = (x , x)

-- dupdotdup :: a -> ((a, a), (a, a))
dupdotdup x = (dup . dup) x

-- dupdup :: ((a -> (a, a)), (a -> (a, a)))
dupdup = dup dup

-- 2. Dada la funci ́on twice
-- twice f = f ◦f
-- Explique el resultado de hacer twice tail [1, 2, 3, 4]. ¿Es posible hacer
-- twice head [1, 2, 3, 4]? Justifique.

-- twice :: (a -> a) -> a -> a
twice f = f . f

-- Al hacer r = twice tail [1,2,3,4] devuelve [3,4] porque aplica tail dos veces seguidas
-- primero tail de [1,2,3,4] que devuelve [2,3,4] y luego tail a [2,3,4] que devuelve [3,4]

-- No es posible hacer twice head [1, 2, 3, 4] porque el tipo de twice acepta (a -> a)
-- y el tipo de head es ([a] -> a)

-- 3. Sea h x y = f (g x y). ¿Cu ́ales de las siguientes afirmaciones son correctas?
-- (a) h ≡ f . g -- False
-- (b) h x ≡ f . g x -- True
-- (c) h x y ≡ (f . g) x y -- False

-- g :: a -> b -> c
-- f :: c -> d
-- h :: a -> b -> d
-- h x y = f (g x y)

-- h x y == (f . g) x y 
-- ==> 
-- h x y == f (g x) y
-- f (g x) y --> f esta recibientdo algo de tipo b -> c

-- 4. Implemente usando pattern matching una funci ́on sumaPrimeros , que
-- dada una lista de enteros agrega al principio el resultado de sumar sus
-- dos primeros elementos (si tiene). Por ejemplo sumaPrimeros [1, 2, 3, 4]
-- resulta en [3, 1, 2, 3, 4], mientras que sumaPrimeros [1 ] resulta en [1].

sumaPrimeros l@([]) = l
sumaPrimeros l@(x:[]) = l
sumaPrimeros l@(x:y:xs) = x+y:l