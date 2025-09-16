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
maxIndT ls@(x:xs) = maxIndAcc (x, len) xs
    where
        len = length ls
        maxIndAcc (x, n) [] = (x, n-1)
        maxIndAcc (x, n) (y:ys)
            | x < y = maxIndAcc (y, len) ys
            | otherwise = maxIndAcc (x, n-1) ys

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