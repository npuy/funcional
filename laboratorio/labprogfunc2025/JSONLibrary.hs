{- Grupo: 3
   Integrante(s):
     Pereira, Nicolas, 51397864
-}

module JSONLibrary
 (lookupField,
  lookupFieldObj,
  keysOf,
  valuesOf,
  entriesOf,
  leftJoin,
  rightJoin,
  filterArray,
  insertKV,
  sortKeys,
  mkJString, mkJNumber, mkJBoolean, mkJNull, mkJObject, mkJArray,
  fromJString, fromJNumber, fromJBoolean, fromJObject, fromJArray,
  isJString, isJNumber, isJBoolean, isJNull, isJObject, isJArray,
  JSON(),importJSON,
  Object()
 )
where

import AST


{- lookupField:
 Cuando el primer argumento es un objeto y tiene como clave el valor
 dado como segundo argumento, entonces se retorna el valor JSON
 correspondiente (bajo el constructor {\tt Just}). De lo contrario se
 retorna {\tt Nothing}. Si un objeto tiene claves repetidas, se
 retorna el valor de más a la derecha.
-}
lookupField :: JSON -> Key -> Maybe JSON
lookupField (JObject obj) key = lookupFieldObj obj key
lookupField _ _ = Nothing

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj [] _ = Nothing
lookupFieldObj ((key,val):obj) keyToFind
  | key == keyToFind = Just val
  | otherwise = lookupFieldObj obj keyToFind

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object JSON -> [Key]
keysOf = map fst

-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object JSON -> [JSON]
valuesOf = map snd

-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object JSON -> [(Key,JSON)]
entriesOf obj = obj

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del primer objeto.
leftJoin :: Object a -> Object a -> Object a
leftJoin obj1 obj2 = obj1 ++ filter (\(key,_) -> not (elem key (map fst obj1))) obj2

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del segundo objeto.
rightJoin :: Object a -> Object a -> Object a
rightJoin obj1 obj2 = filter (\(key,_) -> not (elem key (map fst obj2))) obj1 ++ obj2

-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) ->  Array -> Array
filterArray = filter

-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV kv@(key,_) obj = before ++ [kv] ++ after
  where (before, after) = span (\(k,_) -> k < key) obj

-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV = (:)

-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys = foldr insertKV []


-- constructoras
mkJString :: String -> JSON
mkJString = JString

mkJNumber :: Integer -> JSON
mkJNumber = JNumber

mkJBoolean :: Bool -> JSON
mkJBoolean = JBoolean

mkJNull :: () -> JSON
mkJNull _ = JNull

mkJArray :: [JSON] -> JSON
mkJArray = JArray

mkJObject :: [(Key, JSON)] -> JSON
mkJObject = JObject


-- destructoras
fromJString :: JSON -> Maybe String
fromJString (JString s) = Just s
fromJString _ = Nothing

fromJNumber :: JSON -> Maybe Integer
fromJNumber (JNumber n) = Just n
fromJNumber _ = Nothing

fromJBoolean  :: JSON -> Maybe Bool
fromJBoolean (JBoolean b) = Just b
fromJBoolean _ = Nothing

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject (JObject obj) = Just obj
fromJObject _ = Nothing

fromJArray :: JSON -> Maybe [JSON]
fromJArray (JArray arr) = Just arr
fromJArray _ = Nothing


-- predicados
isJNumber :: JSON -> Bool
isJNumber (JNumber _) = True
isJNumber _ = False

isJNull :: JSON -> Bool
isJNull JNull = True
isJNull _ = False

isJString :: JSON -> Bool
isJString (JString _) = True
isJString _ = False

isJObject :: JSON -> Bool
isJObject (JObject _) = True
isJObject _ = False

isJArray :: JSON -> Bool
isJArray (JArray _) = True
isJArray _ = False

isJBoolean :: JSON -> Bool
isJBoolean (JBoolean _) = True
isJBoolean _ = False

