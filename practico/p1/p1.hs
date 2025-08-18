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