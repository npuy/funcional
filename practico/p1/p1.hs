-- 1. Defina una función sumsqrs que tome 3 números y retorne la suma de los cuadrados de los dos mayores.
sumsqrs x y z = 
    if z < x && z < y then 
        x * x + y * y
    else if y < x && y < z then
        x * x + z * z
    else
        y * y + z * z

-- 2. Defina una función analyze :: Int → Int → Int → Bool , que determina si tres enteros positivos son los lados de un triangulo.
analyze :: Int -> Int -> Int -> Bool
analyze x y z = 
    x + y > z && 
    y + z > x && 
    x + z > y

-- 3. Defina and y or usando expresiones condicionales. Haga lo mismo utilizando pattern matching.
my_and :: Bool -> Bool -> Bool
-- my_and a b = a && b
my_and True True = True
my_and _ _ = False

my_or :: Bool -> Bool -> Bool
-- my_or a b = a || b
my_or False False = False
my_or _ _ = True

-- 4. Defina al conectivo lógico implicación como un operador de tipo Bool.
implica :: Bool -> Bool -> Bool
implica False _ = True
implica _ True = True
implica _ _ = False

-- 5. Supongamos que representamos fechas a traves de una tripla de enteros
-- que corresponden a dia, mes y año. Defina una función edad que dada
-- dos fechas, una representando la fecha de nacimiento de una persona, y la
-- otra representando la fecha actual, calcula la edad en años de la persona.
type Fecha = (Int, Int, Int)

edad :: Fecha -> Fecha -> Int
edad (dia_nac, mes_nac, ano_nac) (dia_act, mes_act, ano_act) = 
    if mes_nac > mes_act then
        ano_act - ano_nac - 1
    else if mes_nac == mes_act && dia_nac > dia_act then
        ano_act - ano_nac - 1
    else
        ano_act - ano_nac

-- Se desea procesar información relativa a estudiantes. Cada estudiante est ́a
-- dado por su nombre (cadena de caracteres), CI (entero), a ̃no de ingreso
-- (entero) y lista de cursos aprobados. Cada curso est ́a dado por el nom-
-- bre del curso (cadena de caracteres), código del curso (entero) y nota de
-- aprobación (entero).
-- (a) Represente la información de cada estudiante a traves de tuplas.
estudiante1 = ("Nicolas", 12345678, 2020, [("GAL", 1234, 10), ("CDIV", 4321, 6)])
estudiante2 = ("Nicolas2", 12345678, 2020, [("GAL", 1234, 10), ("CDIV", 4321, 6)])
estudiante3 = ("Nicolas3", 12345678, 2021, [("GAL", 1234, 10), ("CDIV", 4321, 6)])

listEst = [estudiante1, estudiante2, estudiante3]

-- (b) Escriba una función que dado un estudiante retorne su nombre y CI.
nombre_ci (nombre, ci, _, _) = (nombre, ci)

-- (c) Escriba una función que dado un estudiante retorne su a ̃no de ingreso.
ano_ingreso (_, _, ano, _) = ano

-- (d) Escriba una función que dado un estudiante y una nota retorne una
-- lista con los códigos de los cursos que aprobó con esa nota. 
-- (Sugerencia: use comprensión de listas).
cursos_aprobados (_, _, _, lista) nota = [cod | (_, cod, notaCurso) <- lista, nota == notaCurso]

-- (e) Escriba una función que dada una lista de estudiantes retorne una
-- lista de pares (nombre, CI) de aquellos estudiantes ingresados en
-- un determinado a ̃no dado como parámetro. (Sugerencia: use com-
-- prensión de listas).
generacion listaEstudiantes ano = [(nom, ci) | (nom, ci, anoIngreso, _) <- listaEstudiantes, anoIngreso == ano]

-- Rehaga el ejercicio anterior usando ahora tipos de datos algebraicos en
-- lugar de tuplas.

data Nombre = Nombre String
    deriving(Show)
data CI = CI Int
    deriving(Show)
data Ano = Ano Int
    deriving(Show, Eq)
data CursoCodigo = CursoCodigo Int
    deriving(Show)
data Nota = Nota Int
    deriving(Show, Eq)
data Curso = Curso Nombre CursoCodigo Nota
    deriving(Show)
data Cursos = Cursos [Curso]
    deriving(Show)
data Estudiante = Estudiante Nombre CI Ano Cursos
    deriving(Show)

est1 = (Estudiante (Nombre "nico") (CI 12345678) (Ano 2020) (Cursos [(Curso (Nombre "GAL") (CursoCodigo 1234) (Nota 10))]))
est2 = (Estudiante (Nombre "nico") (CI 12345678) (Ano 2020) (Cursos [(Curso (Nombre "GAL") (CursoCodigo 1234) (Nota 10))]))
est3 = (Estudiante (Nombre "nico") (CI 12345678) (Ano 2019) (Cursos [(Curso (Nombre "GAL") (CursoCodigo 1234) (Nota 10))]))

list = [est1, est2, est3]

nombre_ci_alg :: Estudiante -> (Nombre, CI)
nombre_ci_alg (Estudiante nom ci _ _) = (nom, ci)

ano_ingreso_alg :: Estudiante -> Ano
ano_ingreso_alg (Estudiante _ _ ano _) = ano

cursos_aprobados_alg :: Estudiante -> Nota -> [CursoCodigo]
cursos_aprobados_alg (Estudiante _ _ _ (Cursos lista)) nota = [cod | (Curso _ cod notaCurso) <- lista, nota == notaCurso]

generacion_alg :: [Estudiante] -> Ano -> [(Nombre, CI)]
generacion_alg listaEstudiantes ano = [(nom, ci) | (Estudiante nom ci anoIngreso _) <- listaEstudiantes, anoIngreso == ano]

-- Deseamos representar pares internamente ordenados, que son pares de
-- n ́umeros reales (r, s) tales que r <= 6s.
-- (a) Defina el tipo de los pares ordenados
data ParesOrdenados = Par Float Float
    deriving(Show)

-- (b) Defina una funci ́on que dado un par de reales cualesquiera retorna
-- un par internamente ordenado.
ord :: Float -> Float -> ParesOrdenados
ord r s 
    | r <= 6 * s = (Par r s)
    | otherwise  = (Par s r)

-- (c) Defina la operaci ́on de suma de pares internamente ordenados, que
-- suma las correspondientes componentes de dos pares retornando un
-- nuevo par.
suma_ord :: ParesOrdenados -> ParesOrdenados -> ParesOrdenados
suma_ord (Par r1 s1) (Par r2 s2) = (Par (r1+r2) (s1+s2)) 

-- (d) Defina la operaci ́on de multiplicaci ́on por un escalar, que dado un real
-- y un par internamente ordenado multiplica la primera componente
-- del par por el escalar. El resultado debe ser un par internamente or-
-- denado. Si se pierde el orden se deben intercambiar las componentes.
mul_ord :: ParesOrdenados -> Float -> ParesOrdenados
mul_ord (Par r s) scalar = ord (r * scalar) s

-- Todo n ́umero entero x se puede descomponer de manera  ́unica en t ́erminos
-- de dos n ́umeros enteros y y z , tales que:
-- •−5 < y <= 5
-- •x = y + 10 ×z.
-- Defina una funci ́on que dado un entero x devuelve una tupla con los
-- n ́umeros y y z .
term :: Int -> (Int, Int)
term x 
    | (mod x 10) > 5 = (((div x 10) + 1), ((mod x 10) - 10))
    | otherwise    = ((div x 10), (mod x 10))

-- Deseamos representar n ́umeros racionales y operaciones sobre ellos. Los
-- racionales son representados por pares de enteros cuya segunda compo-
-- nente es distinta de cero. Cada racional tiene infinitas representaciones,
-- pero existe la llamada representaci ́on can ́onica en la que la segunda com-
-- ponente del par de enteros es mayor que cero y ambos enteros son primos
-- entre si.
-- (a) Defina el tipo racional
data Rac = Rac Int Int
    deriving(Show)

-- (b) Defina una funci ́on que dado un par de enteros, el segundo de los
-- cuales es distinto de cero, retorne un racional en su representaci ́on
-- can ́onica.
can :: (Int, Int) -> Rac
can (x, y)
    | y < 0     = (Rac ((-1) * div x (gcd x y)) ((-1) * div y (gcd x y)))
    | otherwise = (Rac (div x (gcd x y)) (div y (gcd x y)))

-- (c) Defina las operaciones de suma, resta, multiplicaci ́on, y negaci ́on de
-- racionales, e int2rac, que convierte un entero en un racional. Dichas
-- operaciones deben devolver representaciones can ́onicas como resul-
-- tado.
-- Nota: Puede usar la funci ́on gcd (definida en el Prelude) la cual
-- computa el m ́aximo com ́un divisor de dos n ́umeros.
suma :: Rac -> Rac -> Rac
suma (Rac x1 y1) (Rac x2 y2) = can (x1 * y2 + x2 * y1, y1 * y2)

resta :: Rac -> Rac -> Rac
resta (Rac x1 y1) (Rac x2 y2) = can (x1 * y2 - x2 * y1, y1 * y2)

mul :: Rac -> Rac -> Rac
mul (Rac x1 y1) (Rac x2 y2) = can (x1 * x2, y1 * y2)

neg :: Rac -> Rac
neg (Rac x1 y1) = can ((-1) * x1, y1)

int2rac :: Int -> Rac
int2rac x = (Rac x 1)

-- Dado el siguiente tipo para representar tri ́angulos:
-- data Triangulo = Equi Int |Iso Int Int |Esca Int Int Int
-- Defina la funci ́on mkTriangulo que dados tres enteros positivos, que rep-
-- resentan a los lados de un tri ́angulo v ́alido, retorna un valor de tipo
-- Triangulo.
data Triangulo = Equi Int |Iso Int Int |Esca Int Int Int
    deriving(Show)

mkTriangulo :: Int -> Int -> Int -> Triangulo
mkTriangulo a b c
    | a == b && b == c = (Equi a)
    | a == b    = (Iso a c)
    | b == c    = (Iso b a)
    | c == a    = (Iso c b)
    | otherwise = (Esca a b c)
