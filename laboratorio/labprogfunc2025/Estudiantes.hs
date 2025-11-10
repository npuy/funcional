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


-- decide si un valor que representa un estudiante esta bien formado
estaBienFormadoEstudiante :: JSON -> Bool  
estaBienFormadoEstudiante a = hasType a tyEstudiante

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
  case getCursos estJson of
    Just cursosJsonArray -> 
      case fromJArray cursosJsonArray of
        Just cursosList -> Just . mkJArray $ filter fueAprobado cursosList
        Nothing -> Nothing
    Nothing -> Nothing

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
addCurso :: Object JSON -> JSON -> JSON
addCurso curso estJson =
  case getCursos estJson of
    Just cursosJsonArray -> 
      case fromJArray cursosJsonArray of
        Just cursosList -> 
          case fromJObject estJson of 
            Just objEst ->
              mkJObject $ leftJoin [("cursos", mkJArray newCursosList)] objEst
              where newCursosList = (mkJObject curso) : cursosList
            Nothing -> estJson
        Nothing -> estJson
    Nothing -> estJson
