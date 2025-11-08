-- 1. Escriba un programa que encuentre las ra ́ıces de la ecuaci ́on cuadr ́atica
-- ax2 + bx + c = 0, donde a, b y c son valores reales le ́ıdos de la entrada
-- est ́andar, uno por l ́ınea. El programa debe imprimir en la salida est ́andar
-- uno de los siguientes mensajes, seg ́un corresponda:
-- Dos raices reales diferentes: 〈raiz〉 y 〈raiz〉
-- Una raiz doble: 〈raiz〉
-- Dos raices complejas diferentes: 〈real〉 (+/-) i 〈imaginaria〉
baskara :: Double -> Double -> Double -> String
baskara a b c
    | d > 0     = "Dos raices reales diferentes: " ++ show r1 ++ " y " ++ show r2
    | d == 0    = "Una raiz doble: " ++ show r1
    | otherwise = "Dos raices complejas diferentes: " ++ show realPart ++ " +/- i " ++ show imagPart
    where
        d = b^2 - 4*a*c
        r1 = (-b + sqrt d) / (2*a)
        r2 = (-b - sqrt d) / (2*a)
        realPart = -b / (2*a)
        imagPart = sqrt (-d) / (2*a)

interactiveBaskara :: IO ()
interactiveBaskara = do
    putStrLn "Ingrese el valor de a:"
    aInput <- getLine
    putStrLn "Ingrese el valor de b:"
    bInput <- getLine
    putStrLn "Ingrese el valor de c:"
    cInput <- getLine
    let a = read aInput :: Double
        b = read bInput :: Double
        c = read cInput :: Double
    putStrLn $ baskara a b c

-- 2. Escriba un programa que lea de la entrada est ́andar un n ́umero natural n
-- y despliegue en pantalla todos los divisores naturales de n.
divisores :: Int -> Int -> [Int]
divisores n 1 = [1]
divisores n m
    | (mod n m) == 0 = m : divisores n (m-1)
    | otherwise = divisores n (m-1)

listToString :: Show a => [a] -> String
listToString [a] = show a
listToString (a:ls) = show a ++ ", " ++ listToString ls

interactiveDivisores :: IO()
interactiveDivisores = do
    putStrLn "Ingrese n:"
    nInput <- getLine
    let n = read nInput :: Int
    putStrLn $ listToString $ divisores n n

-- 3. Escriba un programa que lea de la entrada est ́andar un n ́umero natural n,
-- mayor que cero. A continuaci ́on, el programa deber ́a leer n enteros, uno
-- por l ́ınea, y luego desplegar en pantalla el mayor de ellos.


-- 4. Escriba un programa que lea de la entrada est ́andar una secuencia de
-- enteros positivos (uno por l ́ınea) hasta encontrar el entero -1 y los imprima
-- en orden inverso al que fueron le ́ıdos, pero sin almacenarlos en una lista
-- intermedia con el fin de aplicar reverse.
printReverse :: IO()
printReverse = do
    nInput <- getLine
    let n = read nInput :: Int
    if n < 0
        then putStrLn " "
        else do
            printReverse
            putStrLn $ show n

-- 5. Escriba un programa que determine y exhiba la desviaci ́on est ́andar de
-- n n ́umeros reales. Los n ́umeros deben ser le ́ıdos de la entrada est ́andar,
-- uno por l ́ınea, hasta encontrar el n ́umero 0 (que ser ́a usado como marca
-- de finalizaci ́on). La desviaci ́on est ́andar de una muestra de n ́umeros
-- x1, x2, ..., xnse define como s =
-- √∑n
-- i (xi− ̄x)2
-- n−1 , donde  ̄x =
-- ∑n
-- i xi
-- n.


-- 6. Escriba un programa rand :: IO Integer que se compute un n ́umero en-
-- tero pseudoaleatorio x tal que 1 6x 620. Para ello, use la funci ́on
-- getMonotonicTimeNSec :: IO Word64 del m ́odulo GHC .Clock que lee un
-- valor del reloj. Convierta el valor le ́ıdo (de tipo Word64 ) al tipo Integer ,
-- y use la suma de sus d ́ıgitos m ́odulo 20 para construir el resultado final.


-- 7. Escriba un programa que implemente el juego guess. El programa sortea
-- un n ́umero entero entre 1 y 20. El jugador ingresa n ́umeros enteros en la
-- entrada est ́andar tratando de adivinar el n ́umero sorteado. Luego de cada
-- intento el programa reporta si el n ́umero ingresado es menor o mayor al
-- sorteado, o si es igual, en el  ́ultimo caso finalizando la ejecuci ́on.
    