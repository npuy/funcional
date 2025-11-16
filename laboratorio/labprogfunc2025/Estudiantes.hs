{- Grupo: 3
   Integrante(s):
     Pereira, Nicolas, 51397864
-}

module Estudiantes where

import JSONLibrary
import TypedJSON

---------------------------------------------------------------------------------------
-- Importante:
-- Notar que NO se puede importar el módulo AST, que es interno a la biblioteca.
---------------------------------------------------------------------------------------

-- ! Nota para el lector, cuando empece el archivo no habia visto las clases de Monadas
-- ! por eso hay muchos Case of, para los ajustes use la do notation :)

tyEstudiante =
  TyObject [ ("CI",       TyNum),
             ("apellido", TyString),
             ("cursos",   TyArray tyCurso),
             ("nombre",   TyString)]

  where tyCurso =
          TyObject [("anio",     TyNum),
                    ("codigo",   TyNum),
                    ("nombre",   TyString),
                    ("nota",     TyNum),
                    ("semestre", TyNum)
                    ]

-- Fallo en test: addCurso 1
-- Fallo en test: addCurso 2

resolver :: x -> Maybe x -> x
resolver other mx = 
  case mx of
    Just x -> x
    Nothing -> other

-- Dado 2 cursos retorna true si estan ordenados
ordenado :: JSON -> JSON -> Bool
ordenado x y =
  resolver False (
    do
      janiox <- lookupField x "anio"
      janioy <- lookupField y "anio"
      aniox <- fromJNumber janiox
      anioy <- fromJNumber janioy
      jsemestrex <- lookupField x "semestre"
      jsemestrey <- lookupField y "semestre"
      semestrex <- fromJNumber jsemestrex
      semestrey <- fromJNumber jsemestrey
      jcodigox <- lookupField x "codigo"
      jcodigoy <- lookupField y "codigo"
      codigox <- fromJNumber jcodigox
      codigoy <- fromJNumber jcodigoy
      if aniox > anioy then
        return True
      else if aniox /= anioy then
        return False
      else if semestrex > semestrey then
        return True
      else if semestrex /= semestrey then
        return False
      else if codigox < codigoy then
        return True
      else
        return False
  )

-- Recibe la lista de cursos y retorna true si estan ordenados por anio, semestre y codigo
estanOrdenadosCursos :: [JSON] -> Bool
estanOrdenadosCursos [] = True
estanOrdenadosCursos [_] = True
estanOrdenadosCursos (x:xs@(y:_)) = (ordenado x y) && estanOrdenadosCursos xs

-- decide si un valor que representa un estudiante esta bien formado
estaBienFormadoEstudiante :: JSON -> Bool  
estaBienFormadoEstudiante a = 
  hasType a tyEstudiante &&
  resolver False (
    do
      jcursos <- getCursos a
      cursos <- fromJArray jcursos
      return $ estanOrdenadosCursos cursos
  )

estudianteEjemplo :: JSON
estudianteEjemplo = read "{ \"nombre\" : \"Haskell\", \"apellido\" : \"Curry\", \"CI\" : 12345678, \"cursos\" : [ { \"nombre\"   : \"Calculo DIV\", \"codigo\"   : 123, \"anio\"      : 2019, \"semestre\" : 1, \"nota\"     : 1 }, { \"nombre\"   : \"Calculo DIV\", \"codigo\"   : 123, \"anio\"      : 2019, \"semestre\" : 2, \"nota\"     : 7 } ] }"

cursoEjemplo :: JSON
cursoEjemplo = read "{ \"nombre\"   : \"Programación 2\", \"codigo\"   : 130, \"anio\"      : 2020, \"semestre\" : 1, \"nota\"     : 0 }"

cursoEjemploObj :: Object JSON
cursoEjemploObj = [ ("nombre",   mkJString "Programación 2"),
                    ("codigo",   mkJNumber 130),
                    ("anio",      mkJNumber 2020),
                    ("semestre", mkJNumber 1),
                    ("nota",     mkJNumber 0) ] 

-- getters
getCI :: JSON -> Maybe Integer
getCI estJson = 
  case lookupField estJson "CI" of
    Just ci -> fromJNumber ci
    Nothing -> Nothing

getNombre :: JSON -> Maybe String
getNombre estJson = 
  case lookupField estJson "nombre" of
    Just nombre -> fromJString nombre
    Nothing -> Nothing

getApellido :: JSON -> Maybe String
getApellido estJson = 
  case lookupField estJson "apellido" of
    Just apellido -> fromJString apellido
    Nothing -> Nothing

getCursos :: JSON -> Maybe JSON
getCursos estJson = lookupField estJson "cursos"

notaAprobacion :: Integer
notaAprobacion = 3
-- dado un curso, devuelve si fue aprobado
fueAprobado :: JSON -> Bool
fueAprobado cursoJson = 
  case lookupField cursoJson "nota" of
    Just notaJson -> 
      case fromJNumber notaJson of
        Just notaNum -> notaNum >= notaAprobacion
        Nothing -> False
    Nothing -> False

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados estJson = 
  do 
    estObj <- fromJObject estJson
    cursosJsonArray <- getCursos estJson
    cursosList <- fromJArray cursosJsonArray
    let cursosAprobados = mkJArray $ filter fueAprobado cursosList
    let newCursosObj = [("cursos", cursosAprobados)]
    return $ mkJObject $ rightJoin estObj newCursosObj

-- retorna si el curso fue rendido en un año dado
fueRendidoEnAnio :: Integer -> JSON -> Bool
fueRendidoEnAnio anio cursoJson = 
  case lookupField cursoJson "anio" of
    Just anioJson -> 
      case fromJNumber anioJson of
        Just anioNum -> anioNum == anio
        Nothing -> False
    Nothing -> False

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio anio estJson =
  case getCursos estJson of
    Just cursosJsonArray -> 
      case fromJArray cursosJsonArray of
        Just cursosList -> Just . mkJArray $ filter (fueRendidoEnAnio anio) cursosList
        Nothing -> Nothing
    Nothing -> Nothing

-- listado de notas
listadoNotas :: [JSON] -> [Integer]
listadoNotas = map getNota
  where
    getNota cursoJson =
      case lookupField cursoJson "nota" of
        Just notaJson -> 
          case fromJNumber notaJson of
            Just notaNum -> notaNum
            Nothing -> 0
        Nothing -> 0

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad estJson =
  case getCursos estJson of
    Just cursosJsonArray -> 
      case fromJArray cursosJsonArray of
        Just cursosList -> 
          let notas = listadoNotas cursosList
              totalNotas = sum notas
              cantidadNotas = length notas
          in if cantidadNotas > 0
             then Just $ fromIntegral totalNotas / fromIntegral cantidadNotas
             else Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- agrega curso a lista de cursos de un estudiante
-- addCurso curso estJson =
--   case getCursos estJson of
--     Just cursosJsonArray -> 
--       case fromJArray cursosJsonArray of
--         Just cursosList -> 
--           case fromJObject estJson of 
--             Just objEst ->
--               mkJObject $ leftJoin [("cursos", mkJArray newCursosList)] objEst
--               where newCursosList = (mkJObject curso) : cursosList
--             Nothing -> estJson
--         Nothing -> estJson
--     Nothing -> estJson
addCurso :: Object JSON -> JSON -> JSON
addCurso curso estJson = case (
  do
    objEst <- fromJObject estJson
    cursosJsonArray <- getCursos estJson
    cursosList <- fromJArray cursosJsonArray
    let newCursosList = mkJArray $ addCursoInOrder cursosList $ mkJObject curso
    let newCursosObj = [("cursos", newCursosList)]
    return $ mkJObject $ rightJoin objEst newCursosObj
  ) of
    Just json -> json
    Nothing -> estJson

-- Dado una lista de cursos y un curso retorna la lista de cursos con el nuevo curso insertado en orden
addCursoInOrder :: [JSON] -> JSON -> [JSON]
addCursoInOrder [] curso = [curso]
addCursoInOrder ls@(x:xs) curso =
  if ordenado x curso then
    x : addCursoInOrder xs curso
  else
    curso : ls
