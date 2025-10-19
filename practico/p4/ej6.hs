-- 6. Se requiere implementar una funci ́on sobrecargada size que computa una
-- noci ́on de tama ̃no.
-- (a) Declare una clase “Sizeable a” con un m ́etodo size ::a →Int . Declare
-- instancias de Sizeable para los tipos Int y Char . El tama ̃no de un
-- entero viene dado por su valor absoluto. El tama ̃no de un car ́acter
-- es 1.
class Sizable a where
    size :: a -> Integer

instance Sizable Integer where
    size = abs

instance Sizable Char where
    size _ = 1

-- (b) Declare instancias de Sizeable para listas y pares. El tama ̃no de una
-- lista es la suma de los tama ̃nos de los valores que contiene. Por
-- ejemplo size [1,2,−3] ≡6. El tama ̃no de los pares es la suma de los
-- tama ̃nos de sus componentes, por ejemplo size (’a’,[ ]) ≡1
instance Sizable a => Sizable [a] where
    -- size = sum . map size
    size [] = 0
    size (x:xs) = size x + size xs

instance (Sizable a, Sizable b) => Sizable (a, b) where
    size (a, b) = size a + size b

-- (c) Declare una instancia de Sizeable para el tipo Tree definido en el ejer-
-- cicio 3. El tama ̃no del  ́arbol viene dado por la suma de los tama ̃nos
-- de los valores que contiene.
data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show

instance Sizable a => Sizable (Tree a) where
    size Empty = 0
    size (Node l a r) = size a + size l + size r

-- (d) Implemente una funci ́on filtLt que dado una lista de valores de un
-- tipo instancia de Sizeable y un entero n , retorna solamente aquellos
-- que tienen tama ̃no menor que n .
filtLt :: Sizable a => [a] -> Integer -> [a]
filtLt [] _ = []
filtLt (x:xs) n
    | size x < n = x : filtLt xs n
    | otherwise = filtLt xs n

-- (e) Implemente una funci ́on isSmaller que dados dos valores de tipos
-- Sizeables (pueden ser tipos distintos), computa un booleano indi-
-- cando si el primero es m ́as peque ̃no que el segundo.
isSmaller :: (Sizable a, Sizable b) => a -> b -> Bool
isSmaller a b
    | size a < size b = True
    | otherwise = False

-- (f) Declare una clase “Enumerate a” como subclase de “Sizeable a” con
-- un m ́etodo enum : Int → [a ]. La funci ́on enum, dado un entero n ,
-- retorna todos los valores del tipo a con tama ̃no menor o igual a n .
-- Declare instancias de Enumerate para Int , Char y pares.
class Sizable a => Enumerate a where
    enum :: Int -> [a]

instance Enumerate Integer where
    enum n = take n [1..]

toChar :: Int -> Char
toChar = toEnum

instance Enumerate Char where
    enum n
        | n >= 1 = map toChar [0..1114111]
        | otherwise = []

enumChar :: Int -> [Char]
enumChar = enum

zipaux l x [] = l
zipaux l x (y:ys) = (x, y) : (zipaux l x ys)

zippro [] _ = []
zippro (x:xs) l = zipaux (zippro xs l) x l

instance (Enumerate a, Enumerate b) => Enumerate (a, b) where
    enum 0 = []
    enum n = concat $ map aux [1..n-1]
        where aux x = zippro (enum x) (enum (n - x))

enumPar :: Int -> [(Integer, Integer)]
enumPar = enum