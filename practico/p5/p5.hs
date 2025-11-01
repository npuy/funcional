-- 1. Dada la siguiente definici ́on:
-- replicate 0 _ = [ ]
-- replicate n x = x : replicate (n - 1) x
-- Describir las secuencias de reducci ́on en los casos de evaluaci ́on perezosa
-- y por valor para las siguientes expresiones:
-- (a) head (replicate 5 1)
-- lazy evaluation:
-- head (replicate 5 1)
-- = head (1 : replicate 4 1)
-- = 1
-- strict evaluation:
-- head (replicate 5 1)
-- = head (1 : replicate 4 1)
-- = head (1 : 1 : replicate 3 1)
-- = head (1 : 1 : 1 : replicate 2 1)
-- = head (1 : 1 : 1 : 1 : replicate 1 1)
-- = head (1 : 1 : 1 : 1 : 1 : replicate 0 1)
-- = head (1 : 1 : 1 : 1 : 1 : [ ])
-- = 1
-- (b) map (∗2) (replicate 2 2)
-- lazy evaluation:
-- map (∗2) (replicate 2 2)
-- = map (∗2) (2 : replicate 1 2)
-- = (∗2) 2 : map (∗2) (replicate 1 2)
-- = 4 : map (∗2) (replicate 1 2)
-- = 4 : map (∗2) (2 : replicate 0 2)
-- = 4 : (∗2) 2 : map (∗2) (replicate 0 2)
-- = 4 : 4 : map (∗2) (replicate 0 2)
-- = 4 : 4 : map (∗2) [ ]
-- = 4 : 4 : [ ]
-- = [4, 4]
-- strict evaluation:
-- map (∗2) (replicate 2 2)
-- = map (∗2) (2 : replicate 1 2)
-- = map (∗2) (2 : 2 : replicate 0 2)
-- = map (∗2) (2 : 2 : [ ])
-- = (∗2) 2 : map (∗2) (2 : [ ])
-- = 4 : map (∗2) (2 : [ ])
-- = 4 : (∗2) 2 : map (∗2) [ ]
-- = 4 : 4 : map (∗2) [ ]
-- = 4 : 4 : [ ]
-- = [4, 4]
-- (c) length (map (∗2) (replicate 2 2))
-- lazy evaluation:
-- length (map (∗2) (replicate 2 2))
-- = length (map (∗2) (2 : replicate 1 2))
-- = length ((∗2) 2 : map (∗2) (replicate 1 2))
-- = 1 + length (map (∗2) (replicate 1 2))
-- = 1 + length (map (∗2) (2 : replicate 0 2))
-- = 1 + length ((∗2) 2 : map (∗2) (replicate 0 2))
-- = 1 + 1 + length (map (∗2) (replicate 0 2))
-- = 1 + 1 + length (map (∗2) [ ])
-- = 1 + 1 + length [ ]
-- = 1 + 1 + 0
-- = 2
-- strict evaluation:
-- length (map (∗2) (replicate 2 2))
-- = length (map (∗2) (2 : replicate 1 2))
-- = length (map (∗2) (2 : 2 : replicate 0 2))
-- = length (map (∗2) (2 : 2 : [ ]))
-- = length ((∗2) 2 : map (∗2) (2 : [ ]))
-- = length (4 : map (∗2) (2 : [ ]))
-- = length (4 : (∗2) 2 : map (∗2) [ ])
-- = length (4 : 4 : map (∗2 [ ]))
-- = length (4 : 4 : [ ])
-- = 1 + length (4 : [ ])
-- = 1 + 1 + length [ ]
-- = 1 + 1 + 0
-- = 2 + 0
-- = 2

-- 2. Hacer lo mismo que en el ejercicio 1, pero considerando la siguiente definici ́on
-- de replicate:
-- replicate n x = take n (repeat x )

-- (a) head (replicate 5 1)
-- lazy evaluation:
-- head (replicate 5 1)
-- = head (take 5 (repeat 1))
-- = head (1 : take 4 (repeat 1))
-- = 1

-- strict evaluation:
-- head (replicate 5 1)
-- = head (take 5 (repeat 1))
-- = head (take 5 (1 : repeat 1))
-- = head (take 5 (1 : 1 : repeat 1))
-- = head (take 5 (1 : 1 : 1 : repeat 1))
-- = head (take 5 (1 : 1 : 1 : 1 : repeat 1))
-- = ...

-- (b) map (∗2) (replicate 2 2)
-- lazy evaluation:
-- map (∗2) (replicate 2 2)
-- = map (∗2) (take 2 (repeat 2))
-- = map (∗2) (take 2 (2 : repeat 2))
-- = map (∗2) (2 : take 1 (repeat 2))
-- = (∗2) 2 : map (∗2) (take 1 (repeat 2))
-- = 4 : map (∗2) (take 1 (repeat 2))
-- = 4 : map (∗2) (take 1 (2 : repeat 2))
-- = 4 : map (∗2) (2 : take 0 (repeat 2))
-- = 4 : (∗2) 2 : map (∗2) (take 0 (repeat 2))
-- = 4 : 4 : map (∗2) (take 0 (repeat 2))
-- = 4 : 4 : map (∗2) [ ]
-- = 4 : 4 : [ ]
-- = [4, 4]

-- strict evaluation:
-- map (∗2) (replicate 2 2)
-- = map (∗2) (take 2 (repeat 2))
-- = map (∗2) (take 2 (2 : repeat 2))
-- = map (∗2) (take 2 (2 : 2 : repeat 2))
-- = map (∗2) (take 2 (2 : 2 : 2 : repeat 2))
-- = ...

-- (c) length (map (∗2) (replicate 2 2))
-- lazy evaluation:
-- length (map (∗2) (replicate 2 2))
-- = length (map (∗2) (take 2 (repeat 2)))
-- = length (map (∗2) (take 2 (2 : repeat 2)))
-- = length (map (∗2) (2 : take 1 (repeat 2)))
-- = length ((∗2) 2 : map (∗2) (take 1 (repeat 2)))
-- = 1 + length (map (∗2) (take 1 (repeat 2)))
-- = 1 + length (map (∗2) (take 1 (2 : repeat 2)))
-- = 1 + length (map (∗2) (2 : take 0 (repeat 2)))
-- = 1 + length ((∗2) 2 : map (∗2) (take 0 (repeat 2)))
-- = 1 + 1 + length (map (∗2) (take 0 (repeat 2)))
-- = 1 + 1 + length (map (∗2) [ ])
-- = 1 + 1 + length [ ]
-- = 1 + 1 + 0
-- = 2 + 0
-- = 2

-- strict evaluation:
-- length (map (∗2) (replicate 2 2))
-- = length (map (∗2) (take 2 (repeat 2)))
-- = length (map (∗2) (take 2 (2 : repeat 2)))
-- = length (map (∗2) (take 2 (2 : 2 : repeat 2)))
-- = length (map (∗2) (take 2 (2 : 2 : 2 : repeat 2)))
-- = ...

-- 6. Los n ́umeros de Hamming forman una sucesi ́on estrictamente creciente
-- (sin repetidos) de n ́umeros que cumplen las siguientes condiciones:
-- •El n ́umero 1 est ́a en la sucesi ́on
-- •Si x est ́a en la sucesi ́on, entonces tambi ́en est ́an 2 x , 3 x y 5 x
-- •Ning ́un otro n ́umero est ́a en la sucesi ́on.
-- (a) Defina la lista infinita hamming :: [Integer ] de los n ́umeros de Ham-
-- ming.
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))
  where
    merge xs@(x:xt) ys@(y:yt)
      | x < y     = x : merge xt ys
      | x > y     = y : merge xs yt
      | otherwise = x : merge xt yt

-- (b) Defina una funci ́on hammingTo :: Integer →[ Integer ] que retorne los
-- n ́umeros de Hamming menores a un n ́umero dado.
hammingTo :: Integer -> [Integer]
hammingTo n = takeWhile (< n) hamming

-- 7. Mantener una casa en orden es un trabajo que nunca tiene fin. Algunas de
-- las tareas que se deben realizar son: limpiar, cocinar, fregar, lavar ropa,
-- ir de compras... y cuando se termina de hacer todo, hay que volver a
-- empezar!
-- (a) Defina un tipo de datos Tarea que modele las posibles tareas de una
-- casa
data Tarea = Limpiar | Cocinar | Fregar | LavarRopa | Compras
    deriving Show

-- (b) Defina la lista infinita de tareas , que consiste en primero limpiar,
-- despu ́es cocinar, despu ́es fregar, despu ́es lavar ropa, despu ́es ir de
-- compras, despu ́es volver a limpiar, etc.
tareas :: [Tarea]
tareas = Limpiar : Cocinar : Fregar : LavarRopa : Compras : tareas

-- (c) Una pareja moderna divide las tareas en partes iguales. Defina la
-- funci ́on tareasPareja::Int →[Tareas ] →([Tareas ],[Tareas ],[ Tareas ])
-- que dado un n ́umero n de tareas a realizar y la lista (infinita) de tar-
-- eas, retorne una tripla donde el primer y segundo componente con-
-- tienen la divisi ́on de las n tareas y el tercer componente las tareas
-- que restan por hacer.
tareasPareja :: Int -> [Tarea] -> ([Tarea],[Tarea],[Tarea])
tareasPareja n (t:ts) = (t:tareas1, tareas2, trestantes)
    where 
        (tareas2, tareas1, trestantes)
            | n <= 1 = ([],[], ts)
            | otherwise = tareasPareja (n-1) ts

-- (d) Defina la funci ́on planificar :: Int → Int → ([Tareas ],[ Tareas ]), que
-- dadas la cantidad de tareas que se deben realizar por d ́ıa y la cantidad
-- de d ́ıas, retorna las tareas que debe realizar cada miembro de la pareja
-- en el per ́ıodo.
planificar :: Int -> Int -> ([Tarea],[Tarea])
planificar n m = (tareas1, tareas2)
    where
        t = m*n
        (tareas1, tareas2, _) = tareasPareja t (take t tareas)

-- 8. Un juego consiste en largar a una persona con los ojos vendados a caminar
-- por una habitaci ́on llena de obst ́aculos en b ́usqueda de un objeto. Si se
-- da contra un obst ́aculo pierde, si llega a encontrar el objeto gana y puede
-- sacarse la venda.
-- La configuraci ́on en un momento dado del juego se define por el siguiente
-- tipo:
data Juego = Juego Int     -- eje x
                   Int     -- eje y
                   Pos     -- jugador
                   [ Pos ] -- obstaculos
                   Pos     -- objeto
    deriving Show
type Pos = (Int ,Int)
-- Donde un valor de la forma (Juego tamX tamY jug obs obj ) indica los
-- tama ̃nos tamX y tamY de los ejes x e y respectivamente, la posici ́on pos
-- del jugador, la lista de posiciones obs de los obst ́aculos y la posici ́on obj
-- del objeto. Las posiciones se representan como duplas (x ,y ) que van de 0
-- a tamX −1 en el eje de las x y de 0 a tamY −1 en el eje de las y .
-- (a) Implementar la funci ́on iniciar , que dados los componentes de la
-- configuraci ́on de un juego, retorna un valor de tipo juego si todas las
-- posiciones son correctos con respecto al tama ̃no de la habitaci ́on:
iguales :: Pos -> Pos -> Bool
iguales (x1,y1) (x2,y2) = x1==x2 && y1==y2

afuera :: Pos -> Pos -> Bool
afuera (tamX, tamY) (x, y)
    | x < 0 = True
    | x >= tamX = True
    | y < 0 = True
    | y >= tamY = True
    | otherwise = False

iniciarValido :: Int -> Int -> Pos -> [ Pos ] -> Pos -> Bool
iniciarValido tamX tamY jug obs obj
    | afuera (tamX, tamY) jug = False
    | afuera (tamX, tamY) obj = False
    | any (afuera (tamX, tamY)) obs = False
    | any (iguales obj) obs = False
    | otherwise = True

iniciar :: Int -> Int -> Pos -> [ Pos ] -> Pos -> Maybe Juego
iniciar tamX tamY jug obs obj
    | iniciarValido tamX tamY jug obs obj = Just $ Juego tamX tamY jug obs obj
    | otherwise = Nothing

-- (b) Un  ́arbol de juego representa un juego, conteniendo todos los posibles
-- movimientos desde su posici ́on inicial.
data ArbolJuego = Fin Resultado
                | Sigue ArbolJuego -- adelante
                        ArbolJuego -- atras
                        ArbolJuego -- izquierda
                        ArbolJuego -- derecha
data Resultado = Gana | Pierde
    deriving Show
-- Los caminos de la ra ́ız hacia las hojas (Fin) son posibles jugadas
-- completas con un resultado dado (Gana o Pierde).
-- Los movimientos posibles son:
-- •adelante: avanzar en el eje de las x
-- •atr ́as: retroceder en el eje de las x
-- •izquierda: retroceder en el eje de las y
-- •derecha: avanzar en el eje de las y
-- Los movimientos que hacen que la persona “se choque contra la
-- pared” lo dejan en el mismo lugar donde estaba (pero se cuentan
-- como un movimiento).
data Mov = Ade | Atr | Izq | Der
    deriving Show
-- Implementar la funci ́on arbol , que dada una configuraci ́on inicial re-
-- torna el  ́arbol de juego:
moverPos :: Pos -> Mov -> Pos
moverPos (x,y) Ade = (x+1,y)
moverPos (x,y) Atr = (x-1,y)
moverPos (x,y) Izq = (x,y-1)
moverPos (x,y) Der = (x,y+1)

next :: Juego -> Mov -> ArbolJuego
next juego@(Juego tamX tamY jug obs obj) mov
    | afuera (tamX, tamY) nuevaPos = arbol juego
    | otherwise = arbol (Juego tamX tamY nuevaPos obs obj)
        where nuevaPos = moverPos jug mov

arbol :: Juego -> ArbolJuego
arbol juego@(Juego tamX tamY jug obs obj)
    | iguales jug obj = Fin Gana
    | any (iguales jug) obs = Fin Pierde
    | otherwise = 
        Sigue 
            (next juego Ade)
            (next juego Atr)
            (next juego Izq)
            (next juego Der)

-- (c) Implementar la funci ́on mover , que dado un movimiento y un  ́arbol
-- de juego, retorna el sub  ́arbol resultante de realizar dicho movimiento
-- (si es posible):
mover :: Mov -> ArbolJuego -> ArbolJuego
mover _ (Fin a) = (Fin a)
mover Ade (Sigue a _ _ _) = a
mover Atr (Sigue _ a _ _) = a
mover Izq (Sigue _ _ a _) = a
mover Der (Sigue _ _ _ a) = a

-- (d) Implementar la funci ́on jugar , que dada una lista que representa a
-- una secuencia de movimientos y un  ́arbol de juego, retorna el resul-
-- tado de realizar (alg ́un prefijo de) dichos movimientos o Nothing si se
-- queda en un estado en el que el juego no ha terminado (no ha ganado
-- ni perdido):
jugar :: [ Mov ] -> ArbolJuego -> Maybe Resultado
jugar _ (Fin a) = Just a
jugar [] _ = Nothing
jugar (mov:lmov) ar = jugar lmov (mover mov ar)

-- (e) Implementar la funci ́on mejorJugada, que dado un  ́arbol de juego
-- retorna la secuencia de movimientos m ́as corta que resulte en ganar
-- el juego.
-- mejorJugada :: ArbolJuego -> [ Mov ]
