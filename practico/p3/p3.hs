-- 1. Explique el tipo de las siguientes funciones:
-- (a) min x y = if x <y then x else y
-- min :: Ord a => a -> a -> a
min1 x y = if x < y then x else y
-- (b) paren x = "("++ show x ++ ")"
-- paren :: Show a => a -> [Char]
paren x = "("++ show x ++ ")"

-- 2. Dada la siguiente definici ́on de tipo:
-- data Semaforo = Verde |Amarillo |Rojo
-- ¿Qu ́e falta para que sea posible hacer (show Verde)?
data Semaforo = Verde | Amarillo | Rojo
    deriving Show

-- 3. Defina las siguientes funciones usando recursi ́on expl ́ıcita.
-- (a) sumSqs :: Num a ⇒[a ] →a
-- Suma los cuadrados de los elementos de una lista.
sumSqs [] = 0
sumSqs (x:xs) = x*x + sumSqs(xs)

-- (b) elem :: Eq a ⇒a →[ a ] →Bool
-- Determina si un elemento pertenece a una lista.
elem1 _ [] = False
elem1 a (x:xs)
    | a == x = True
    | otherwise = elem1 a xs

-- (c) elimDups :: Eq a ⇒[ a ] →[ a ]
-- Elimina los duplicados adyacentes de una lista.
-- Por ejemplo, elimDups [1,2,2,3,4,4,4,3] retorna [1,2,3,4,3].
elimDups [] = []
elimDups [x] = [x]
elimDups (x:xs) = 
    if x == y 
        then ys
        else x:ys
    where ys@(y:_) = elimDups xs
-- elimDups [] = []
-- elimDups [x] = [x]
-- elimDups (x:y:xs)
--     | x == y = elimDups (y:xs)
--     | otherwise = x : elimDups (y:xs)

-- (d) split :: [ a ] →([a ],[a ])
-- Divide una lista en dos listas colocando sus elementos de forma al-
-- ternada. Por ejemplo, split [2,4,6,8,7] retorna ([2,6,7],[4,8]).
split [] = ([],[])
split (x:xs) = (x:as, bs) where (bs, as) = split xs

-- (e) maxInd :: Ord a ⇒[ a ] →(a,Int )
-- Retorna el m ́aximo de una lista no vac ́ıa y el  ́ındice de su primera
-- ocurrencia. Los  ́ındices se comienzan a numerar en 0. Por ejemplo,
-- maxInd [8,10,6,10,10] retorna (10,1).
maxInd [x] = (x, 0)
maxInd (x:xs) = 
    if x >= y 
        then (x, 0) 
        else (y, n + 1)
    where (y, n) = maxInd xs

-- (f) merge :: Ord a ⇒[ a ] →[ a ] →[a ]
-- Mezcla dos listas ordenadas en una nueva lista ordenada.
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

-- 4. Defina las siguientes funciones como foldr :
-- (a) sumSqs :: Num a ⇒[a ] →a
sumSqsF :: Num a => [a] -> a
sumSqsF = foldr func 0
    where func a b = a * a + b

-- (b) elimDups :: Eq a ⇒[ a ] →[ a ]
elimDupsF :: Eq a => [a] -> [a]
elimDupsF = foldr func []
    where func a b = 
            case b of
                b@(x:xs) -> if a == x then b else a:b
                b -> a:b

-- (c) split :: [ a ] →([a ],[ a ])
splitF :: [a] -> ([a],[a])
splitF = foldr func ([],[])
    where func a (bs, as) = (a:as, bs)

-- (d) takeWhile :: (a →Bool ) →[ a ] →[ a ]
takeWhileF p = foldr func []
    where func a b 
            | p a = a:b
            | otherwise = []

-- 5. Defina las siguientes funciones por recursi ́on estructural usando tail-recursion.
-- (a) sumSqs :: Num a ⇒[a ] →a
sumSqsT = sumSqsAcc 0
    where 
        sumSqsAcc acc [] = acc
        sumSqsAcc acc (x:xs) = sumSqsAcc (x*x + acc) xs

-- (b) elem :: Eq a ⇒a →[ a ] →Bool
elemT e = elemAcc False
    where
        elemAcc acc [] = acc
        elemAcc acc (x:xs) 
            | x == e = elemAcc True xs
            | otherwise = elemAcc acc xs

-- (c) elimDups :: Eq a ⇒[ a ] →[ a ]
elimDupsT [] = []
elimDupsT (x:xs) = elimDupsAcc [x] xs
    where 
        elimDupsAcc acc [] = reverse acc
        elimDupsAcc acc@(a:as) (x:xs)
            | a == x = elimDupsAcc acc xs
            | otherwise = elimDupsAcc (x:acc) xs

-- (d) split :: [ a ] →([a ],[a ])
splitT = splitAccA ([],[])
    where
        splitAccA (as,bs) [] = (reverse as,reverse bs)
        splitAccA (as,bs) (x:xs) = splitAccB (x:as, bs) xs
        splitAccB (as,bs) [] = (reverse as,reverse bs)
        splitAccB (as,bs) (x:xs) = splitAccA (as, x:bs) xs

-- (e) maxInd :: Ord a ⇒[ a ] →(a,Int )
-- maxIndT ls@(x:xs) = maxIndAcc (x, len) xs
--     where
--         len = length ls
--         maxIndAcc (x, n) [] = (x, n-1)
--         maxIndAcc (x, n) (y:ys)
--             | x < y = maxIndAcc (y, len) ys
--             | otherwise = maxIndAcc (x, n-1) ys
maxIndT ls@(x:xs) = maxIndAcc (x, 0, 0) xs
    where
        maxIndAcc (x, n, pos) [] = (x, n)
        maxIndAcc (x, n, pos) (y:ys)
            | x < y = maxIndAcc (y, pos + 1, pos + 1) ys
            | otherwise = maxIndAcc (x, n, pos + 1) ys

-- (f) takeWhile :: (a →Bool ) →[ a ] →[ a ]
takeWhileT p = takeWhileAcc []
    where
        takeWhileAcc acc (x:xs)
            | p x = takeWhileAcc (x:acc) xs
            | otherwise = reverse acc

-- (g) dropWhile :: (a →Bool ) →[a ] →[ a ]
-- dropWhileT p = dropWhileAcc
--     where
--         dropWhileAcc (x:xs)
--             | p x = dropWhileAcc xs
--             | otherwise = xs
 
-- 6. Defina la funci ́on sumSqs como foldl .
sumSqsFL :: Num a => [a] -> a
sumSqsFL = foldl (\b a -> b + a*a) 0

-- 7. Sea h x xs = x −sum xs. ¿Cu ́al de las siguientes afirmaciones es correcta?
-- (a) h x xs = foldr (−) x xs
-- (b) h x xs = foldl (−) x xs
-- b es la correcta lo que hace es x - el primer elemento de xs luego se toma el 
-- resultado y se resta al siguiente elemento de xs y asi sucessivamente

-- 8. Una buena implementaci ́on de elem como foldr es m ́as eficiente que una
-- implementaci ́on de elem como foldl . Defina elem como foldr y como
-- foldl . Compare las implementaciones ejecutando con los argumentos 1
-- y [ 1 ..10000000]. Explique por qu ́e una es m ́as eficiente que la otra.
-- elemFR8 x = foldr faux False
--     where
--         faux _ True = True
--         faux a _ = a == x
-- elemFL8 x = foldl faux False
--     where
--         faux True _ = True
--         faux _ a = a == x
elemFR8 x = foldr (\ a b -> b || a == x) False
elemFL8 x = foldl (\ b a -> b || a == x) False

-- 9. Defina la funci ́on maxInd usando foldr y usando foldl .
-- (e) maxInd :: Ord a ⇒[ a ] →(a,Int )
-- Retorna el m ́aximo de una lista no vac ́ıa y el  ́ındice de su primera
-- ocurrencia. Los  ́ındices se comienzan a numerar en 0. Por ejemplo,
-- maxInd [8,10,6,10,10] retorna (10,1).
maxIndFR (x:xs) = fst $ foldr fn ((x, 0), 1) xs
    where fn y ((x, nx), n)
            | y > x = ((y, n), n + 1)
            | otherwise = ((x, nx), n + 1)

maxIndFL (x:xs) = fst $ foldl fn ((x, 0), 1) xs
    where fn ((x, nx), n) y
            | y > x = ((y, n), n + 1)
            | otherwise = ((x, nx), n + 1)

-- 10. Al recorrer una lista las funciones foldr y foldl pueden desarmar y recon-
-- stru ́ır la misma. En el caso de foldl , la reconstrucci ́on invierte la lista.
-- Defina split y elimDups usando foldl y reverse.
-- (d) split :: [ a ] →([a ],[a ])
-- Divide una lista en dos listas colocando sus elementos de forma al-
-- ternada. Por ejemplo, split [2,4,6,8,7] retorna ([2,6,7],[4,8]).
splitfl = (foldl fn ([],[])) . reverse
    where fn (xs, ys) a = (a:ys, xs)

-- (c) elimDups :: Eq a ⇒[ a ] →[ a ]
-- Elimina los duplicados adyacentes de una lista.
-- Por ejemplo, elimDups [1,2,2,3,4,4,4,3] retorna [1,2,3,4,3].
elimDupsfl :: Eq a => [a] -> [a]
elimDupsfl = (foldl fn []) . reverse
    where 
        fn [] a = [a]
        fn ls@(x:xs) a
            | x == a = ls
            | otherwise = a:ls

-- 11. La funci ́on foldl siempre recorre completamente la lista. Defina la funci ́on
-- takeWhile usando foldl . Debe usar el acumulador para saber cu ́ando se
-- obtuvo toda la informaci ́on relevante de la lista.
-- takeWhile :: (a →Bool ) →[ a ] →[ a ]
takeWhilefl p = reverse . fst . foldl fn ([], True)
    where
        fn (acc, continue) a
            | continue && p a = (a:acc, continue)
            | otherwise = (acc, False)

-- 12. Suponga que representamos n ́umeros naturales como listas de d ́ıgitos or-
-- denados de forma descendente seg ́un su significaci ́on. Por ejemplo [1,2,5]
-- representa al n ́umero 125.
-- (a) Defina una funci ́on sucesor :: [Int ] → [ Int ], que dado un natural en
-- esta representaci ́on compute el siguiente natural. ¿Cu ́al estrategia
-- resulta m ́as adecuada para implementar esta funci ́on, la recursi ́on o
-- la recorrida con acumulador? Escriba la funci ́on como/usando foldr
-- o foldl , siguiendo la estrategia m ́as adecuada.
sucesor :: [Int] -> [Int]
sucesor ls = if overflow == 1 then 1:list else list
    where
        (list, overflow) = foldr fn ([], 1) ls
        fn a (ls, 0) = (a:ls, 0)
        fn a (ls, acc) = (((mod (a + acc) 10) : ls), (div (a + acc) 10))

-- (b) Defina una funci ́on decimal ::[ Int ] →Int , que dado un natural en esta
-- representaci ́on compute el entero correspondiente. ¿Cu ́al estrategia
-- resulta m ́as adecuada para implementar esta funci ́on, la recursi ́on o
-- la recorrida con acumulador? Escriba la funci ́on como/usando foldr
-- o foldl , siguiendo la estrategia m ́as adecuada.
decimal :: [Int] -> Int
decimal = foldl (\ b a -> 10*b + a) 0

-- (c) Defina una funci ́on repr :: Int →[Int ], que dado un natural (de tipo
-- Int ) retorna su representaci ́on.
-- repr :: Int -> [Int]
-- repr 0 = []
-- repr n = (mod n 10) : (repr (div n 10))
repr :: Int -> [Int]
repr = repracc []
repracc acc 0 = acc
repracc acc n = repracc ((mod n 10): acc) (div n 10)

-- 13. El algoritmo de Luhn es una f ́ormula de checksum que se usa para validar
-- datos num ́ericos tales como n ́umeros de tarjetas de cr ́edito o n ́umeros de
-- identificaci ́on. Dado un n ́umero natural (de tipo Int ), tal que el d ́ıgito
-- menos significativo corresponde al d ́ıgito de control, el algoritmo realiza
-- los siguientes pasos para validarlo:
-- Se obtiene la lista de d ́ıgitos del n ́umero ordenados de forma descen-
-- dente seg ́un su significaci ́on. Por ejemplo, el n ́umero 125 resulta en
-- [1,2,5]. Para ello podemos usar la funci ́on repr del ejercicio anterior.
-- dobleD :: [ Int ] →[ Int ]
-- Se recorre la lista desde el final hacia adelante y se multiplica por
-- dos cada segundo d ́ıgito. Por ejemplo, dobleD [7,2,4,5] resulta en
-- [14,2,8,5].
-- sumaD :: [ Int ] →Int
-- Se suman las posiciones de la lista; al sumarlas, las posiciones que
-- sean mayores que 9 deben ser ajustadas rest ́andole 9.
-- Por ejemplo, sumaD [14,2,8,5] resulta en (14 −9) + 2 + 8 + 5.
-- validar :: Int →Bool
-- Si el resultado de la suma anterior es m ́ultiplo de 10 entonces el
-- n ́umero original es v ́alido.
-- (a) Implemente las funciones dobleD , sumaD y validar .
-- (b) Defina una funci ́on
-- luhn :: Int →Bool
-- que realice la validaci ́on de un n ́umero mediante la composici ́on de
-- las funciones anteriores.
dobleD :: [ Int ] -> [ Int ]
dobleD = fst . foldr fn ([], 0)
    where
        fn a (acc, 0) = (a:acc, 1)
        fn a (acc, 1) = ((a*2):acc, 0)

sumaD :: [ Int ] -> Int
sumaD = foldl fn 0
    where
        fn b a
            | a > 9 = a - 9 + b
            | otherwise = a + b
            
validar n = (mod n 10) == 0

luhn = validar . sumaD . dobleD . repr