-- 1. Considere la siguiente definici ́on de los n ́umeros naturales:
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
fibFast = fst . foldN fn (Zero, Succ Zero)
    where fn (a, b) = (b, suma a b)

-- fibFastlazy = 0 : 1 : zipWith (+) fibFastlazy (tail fibFastlazy)

-- fibFastlazytake n = take n fibFastlazy

-- 2. Suponga que definimos los enteros de la siguinte forma:
data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)
data OurInt = IntZero | Pos Nat | Neg Nat
    deriving Show
-- tal que IntZero representa el cero de los enteros, Pos los enteros positivos
-- (1,2,...) y Neg los negativos (-1,-2,...). Por ejemplo, el 2 es dado por
-- Pos (Succ Zero), mientras que el -1 por Neg Zero.
-- (a) Defina la instancia de la clase Num para OurInt .
-- Prec resta n m -> n >= m
resta :: Nat -> Nat -> Nat
resta n Zero = n
resta (Succ n) (Succ m) = resta n m

sumaoi :: OurInt -> OurInt -> OurInt
sumaoi n IntZero = n
sumaoi (Pos n) (Pos m) = (Pos $ Succ $ suma n m)
sumaoi (Neg n) (Neg m) = (Neg $ Succ $ suma n m)
sumaoi (Pos n) (Neg m)
    | n > m = (Pos $ resta n m)
    | m > n = (Neg $ resta m n)
    | otherwise = IntZero
sumaoi n m = sumaoi m n

negateoi :: OurInt -> OurInt
negateoi IntZero = IntZero
negateoi (Pos n) = (Neg n)
negateoi (Neg n) = (Pos n)

muloi :: OurInt -> OurInt -> OurInt
muloi IntZero _ = IntZero
muloi (Pos Zero) m = m
muloi (Pos (Succ n)) m = sumaoi m $ muloi (Pos n) m
muloi (Neg n) m = negateoi $ muloi (Pos n) m

absoi :: OurInt -> OurInt
absoi (Neg n) = (Pos n)
absoi n = n

signumoi :: OurInt -> OurInt
signumoi IntZero = IntZero
signumoi (Pos n) = (Pos Zero)
signumoi (Neg n) = (Neg Zero)

fromIntegeroi :: Integer -> OurInt
fromIntegeroi n 
    | n == 0 = IntZero
    | n > 0 = (Pos $ int2nat n)
    | otherwise = (Neg $ int2nat n)

instance Num OurInt where
    (+) = sumaoi
    (*) = muloi
    abs = absoi
    signum = signumoi
    fromInteger = fromIntegeroi
    negate = negateoi

-- (b) ¿Qu ́e problema tiene esta otra representaci ́on?
-- data OtroInt = OZero |OPos OtroInt |ONeg OtroInt
-- O dicho de otra forma, que propiedad tiene la definici ́on de OurInt
-- que no la tiene OtroInt ?
-- ¿Y esta otra?
-- data OtroInt ′= OPos Nat |ONeg Nat
-- Propiedad de unicidad de reprecentacion

-- 3. Considere la siguiente definici ́on de  ́arbol binario:
data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving Show
-- (a) Defina las recorridas en inorder, preorder y postorder sobre un  ́arbol
-- binario, las cuales listan los elementos del  ́arbol en el respectivo orden.
-- Todas ellas tienen tipo Tree a →[ a ].
extree = (Node (Node Empty 1 Empty) 2 (Node Empty 3 (Node Empty 4 Empty)))

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l n r) = (inorder l) ++ (n : inorder r)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node l n r) = (n : preorder l) ++ (preorder r)

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node l n r) = (postorder l) ++ (postorder r) ++ [n]

-- (b) Defina la funci ́on mkTree :: Ord a ⇒[ a ] →Tree a que construye un
--  ́arbol binario de b ́usqueda a partir de una lista. (El  ́arbol generado
-- no precisa estar balanceado.)
insert :: Ord a => Tree a -> a -> Tree a
insert Empty n = (Node Empty n Empty)
insert (Node l m r) n
    | n > m = (Node l m (insert r n))
    | otherwise = (Node (insert l n) m r)

mkTreeacc :: Ord a => [a] -> Tree a -> Tree a
mkTreeacc [] acc = acc
mkTreeacc (n:r) acc = mkTreeacc r (insert acc n)

mkTree :: Ord a => [a] -> Tree a
mkTree a = mkTreeacc a Empty

mkTree' [] = Empty
mkTree' (x:xs) = Node l x r
    where
        l = mkTree' $ filter (<= x) xs
        r = mkTree' $ filter (> x) xs

-- (c) Que hace la composici ́on inorder ◦mkTree?
-- retorna la lista ordenada (implementa quick sort)

-- 4. Considere el tipo de los  ́arboles binarios con informaci ́on en las hojas:
data BTree a = Leaf a | Fork (BTree a) (BTree a)
    deriving Show
-- (a) Defina la funci ́on depths :: BTree a → BTree Int que reemplaza el
-- valor en cada hoja del  ́arbol por su profundidad en el mismo. La
-- profundidad de la ra ́ız es cero. Por ejemplo:
-- depths (Fork (Fork (Leaf ’a’) (Leaf ’b’)) (Leaf ’c’))
-- =
-- Fork (Fork (Leaf 2) (Leaf 2)) (Leaf 1)
depths :: BTree a -> BTree Int
depthsacc (Leaf _) acc = Leaf acc
depthsacc (Fork l r) acc = (Fork (depthsacc l (acc + 1)) (depthsacc r (acc + 1)))

depths a = depthsacc a 0

-- (b) Defina la funci ́on balanced :: BTree a → Bool que determina si un
--  ́arbol est ́a balanceado. Entendemos por balanceado si el n ́umero de
-- hojas en los sub ́arboles izquierdo y derecho de todo nodo difiere a
-- lo m ́as en uno. Las hojas se consideran balanceadas. Sugerencia:
-- primero defina una funci ́on size que compute el n ́umero de hojas de
-- un  ́arbol.
size :: BTree a -> Int
size (Leaf _) = 1
size (Fork l r) = size(l) + size(r)
balanced :: BTree a -> Bool
balanced (Leaf _) = True
balanced (Fork l r) = (balanced l) && (balanced r) && abs(sl - sr) <= 1
    where 
        sl = size l
        sr = size r

-- (c) Defina la funci ́on mkBTree :: [ a ] → BTree a que convierte una lista
-- no vac ́ıa de valores de tipo a en un  ́arbol balanceado. Sugerencia:
-- primero defina una funci ́on que parta una lista en dos mitades cuyo
-- largo difiera a lo m ́as en uno.
split :: [a] -> ([a],[a])
split [] = ([],[])
split a = ((take half a),(drop half a))
    where 
        la = length a
        half = div la 2

mkBTree :: [a] -> BTree a
mkBTree [x] = (Leaf x)
mkBTree [x,y] = (Fork (Leaf x) (Leaf y))
mkBTree a = (Fork (mkBTree fh) (mkBTree sh))
    where
        (fh, sh) = split a

-- (d) Defina la funci ́on retrieve :: BTree a →Int →a que retorna el valor
-- contenido en la n - ́esima hoja de un  ́arbol contada de izquierda a
-- derecha. El valor n es pasado como uno de los par ́ametros. Las
-- hojas se empiezan a numerar desde 1.
retrieveaux (Leaf a) n acc = (acc + 1, a)
retrieveaux (Fork l r) n acc = if n == nextacc then (lastacc, a) else (lastacc, b)
    where 
        (nextacc, a) = retrieveaux l n acc
        (lastacc, b) = retrieveaux r n nextacc
retrieve :: BTree a -> Int -> a
retrieve arr n = snd $ retrieveaux arr n 0

-- 5. El tipo de los  ́arboles binarios homog ́eneos:
data HTree a = Tip a | Bin (HTree a) a (HTree a)
    deriving Show
-- representa  ́arboles binarios que contienen informaci ́on tanto en sus no-
-- dos como en sus hojas. Dichos  ́arboles est ́an emparentados tanto con los
--  ́arboles de tipo Tree a como con los de tipo BTree a.
-- (a) Defina la correspondiente funci ́on map para este tipo:
mapHT :: (a -> b) -> (HTree a -> HTree b)
mapHT f (Tip a) = Tip $ f a
mapHT f (Bin l a r) = Bin (mapHT f l) (f a) (mapHT f r)

-- (b) Defina una funci ́on subtrees :: BTree a →HTree (BTree a ) que dado
-- un  ́arbol binario con informaci ́on en las hojas computa un  ́arbol ho-
-- mog ́eneo con su misma forma y tal que el valor almacenado en cada
-- nodo contiene el correspondiente  ́arbol binario que tiene ra ́ız en ese
-- nodo en el  ́arbol original. Por ejemplo:
-- subtrees (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))
-- =
-- Bin (Tip (Leaf 2))
-- (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))
-- (Bin (Tip (Leaf 3))
-- (Fork (Leaf 3) (Leaf 4))
-- (Tip (Leaf 4)))

-- data BTree a = Leaf a | Fork (BTree a) (BTree a)

subtrees :: BTree a -> HTree (BTree a)
subtrees l@(Leaf a) = Tip l
subtrees t@(Fork l r) = Bin (subtrees l) t (subtrees r)

-- (c) Defina la funci ́on sizes :: BTree a → HTree Int que dado un  ́arbol
-- binario retorna el  ́arbol homog ́eneo que en cada nodo contiene el
-- n ́umero de hojas que tiene el correspondiente  ́arbol binario con ra ́ız
-- en ese nodo. Por ejemplo:
-- sizes (Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4)))
-- =
-- Bin (Tip 1) 3 (Bin (Tip 1) 2 (Tip 1))
vtree (Tip a) = a
vtree (Bin _ a _) = a

sizes :: BTree a -> HTree Int
sizes (Leaf a) = Tip 1
sizes (Fork l r) = (Bin treesl st treesr)
    where
        treesl = sizes l
        treesr = sizes r
        st = (vtree treesl) + (vtree treesr)

-- (d) Defina la funci ́on sizes ahora usando las partes a) y b).
-- sizes :: BTree a -> HTree Int
-- sizes = mapHT size . subtrees
