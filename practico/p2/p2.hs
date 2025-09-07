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

-- 5. La funci ́on flip tiene el siguiente tipo: (a → b → c) → b → a → c.
-- Observando el tipo, ¿puede determinar qu ́e hace la funci ́on?
-- (a) Implemente la funci ́on flip.
flip1 f b a = f a b

-- (b) Defina una expresi ́on lambda equivalente a la funci ́on flip.
flip2 = \f b a -> f a b

-- 6. Usando secciones y composici ́on de funciones, implemente una funci ́on
-- cuentas :: Integer →Integer , que dado un n ́umero, le sume 3, al resultado
-- lo multiplique por 2, luego le reste 8 y finalmente lo divida por dos.
-- cuentas = ((*) (1/2)) . ((+) (-8)) . ((*) 2) . ((+) 3)
-- cuentas n = flip (/) 2 $ flip (-) 8 $ (*) 2 $ (+) 3 n
cuentas = (flip (/) 2) . (flip (-) 8) . ((*) 2) . ((+) 3)

-- 7. (a) Implemente la funci ́on map usando listas por comprensi ́on.
map1 f l = [f x | x <- l]
-- (b) Implemente la funci ́on filter usando listas por comprensi ́on.
filter1 p l = [x | x <- l, p x]

-- 8. Usando map, defina una funci ́on squares :: [ Int ] → [Int ] que dada una
-- lista de enteros retorne una lista con los cuadrados de los elementos de la
-- lista.
squares :: [Int] -> [Int]
squares = map $ flip (^) 2

-- 9. Defina la funci ́on length en t ́erminos de map y sum .
length1 = sum . map (const 1)

-- 10. Usando filter , defina:
-- (a) Una funci ́on all :: (a →Bool ) →[ a ] →Bool que dada una condici ́on
-- y una lista, verifique si todos los elementos de la lista cumplen con
-- dicha condici ́on. Ejemplos:
-- all (>0) [1, 2, 3] retorna True
-- all (≡’a’) [’a’, ’b’, ’c’] retorna False
all1 :: (a -> Bool) -> [a] -> Bool
all1 p = null . filter (not . p)

-- (b) Una funci ́on elem :: Eq a ⇒ a → [a ] → Bool que determina si un
-- elemento pertenece a una lista. Ejemplos:
-- elem 2 [1, 2, 3] retorna True
-- elem ’a’ [’b’, ’c’] retorna False
elem1 :: Eq a => a -> [a] -> Bool
elem1 x = not . null . filter ((==) x)

-- 11. Indique el tipo y explique lo que hace la siguiente funci ́on:
rara p = filter p . filter (not . p)

-- 12. Indique el tipo y explique lo que hace la siguiente funci ́on:
rara2 = zipWith (.) [length, sum] [drop 4, take 4]
-- Muestre un ejemplo de aplicaci ́on correcta de la expresi ́on (head rara2 ) y
-- su resultado.
-- head rara2 [1..10] -> 6

-- 13. (a) Utilizando flip, mod , length , map y filter , defina una funci ́on que
-- dada una lista de enteros retorne la cantidad de elementos pares que
-- tiene la lista.
cant_pares = length . filter ((==) 0) . map (flip mod $ 2)

-- (b) Haga lo mismo, pero sin usar map.
cant_pares2 = length . filter (((==) 0) . (flip mod $ 2))

-- (c) Haga lo mismo, pero sin usar flip.
cant_pares3 = length . filter (\x -> mod x 2 == 0)

-- 14. La funci ́on filter se puede definir en t ́erminos de concat y map:
-- filter p = concat . map box
-- where box x = ...
-- Dar la definici ́on de box .
filter2 p = concat . map box
    where box x 
            | p x       = [x]
            | otherwise = []

-- 15. Considere el tipo Triangulo definido en el Ejercicio 11 del Pr ́actico 1.
-- data Triangulo = Equi Int |Iso Int Int |Esca Int Int Int
-- Defina una funci ́on isos :: [Triangulo ] → Int , que dada una lista de
-- tri ́angulos retorna la cantidad de ellos que son is ́osceles. Defina isos us-
-- ando:
-- (a) listas por comprensi ́on.
data Triangulo = Equi Int |Iso Int Int |Esca Int Int Int
    deriving Show
triangulos = [(Equi 1), (Equi 1), (Equi 1), (Iso 2 1), (Iso 2 1), (Iso 2 1), (Esca 1 2 3), (Esca 1 2 3)]
isos :: [Triangulo] -> Int
isos l = length [ 1 | (Iso _ _) <- l ]

-- (b) filter
isosf :: [Triangulo] -> Int
isosf = length . filter filtro
    where filtro t = case t of 
                    (Iso _ _) -> True
                    _ -> False 

-- 16. Considere la siguiente representaci ́on de matrices de dos dimensiones en
-- t ́erminos de listas de listas:
-- type Matriz a = [[ a ]]
-- donde vamos a asumir que todas las filas (dadas por las listas de tipo [a])
-- son del mismo tama ̃no. Por ejemplo,
-- m = [[1, 2, 3], [4, 5, 6] ]
-- representa una matriz de 2x3.
-- (a) Usando drop, defina una funci ́on columna :: Int → Matriz a → [ a ]
-- tal que columna i m retorna la i - ́esima columna de la matriz m .
-- (b) Usando columna, defina una funci ́on transpose ::Matriz a →Matriz a
-- que transpone una matriz.
-- Por ejemplo, transpose m retorna [ [1, 4], [2, 5], [3, 6] ]

-- 17. Explique por qu ́e la siguiente definici ́on no es aceptada por el sistema de
-- tipos de Haskell:
-- dobleAp f = (f True, f ’a’)