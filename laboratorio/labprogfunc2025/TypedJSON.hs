{- Grupo: 3
   Integrante(s):
     Pereira, Nicolas, 51397864
-}

module TypedJSON where

import AST
import JSONLibrary
import Control.Monad
import Data.List


-- Tipos JSON
data JSONType
  = TyString
  | TyNum
  | TyObject (Object JSONType)
  | TyArray JSONType
  | TyBool
  | TyNull
  deriving (Show, Eq)


-- dado un valor JSON se infiere el tipo. Se devuelve
-- Nothing si el valor está mal tipado
typeOfArray :: Array -> Maybe JSONType -> Bool
typeOfArray _ Nothing = False
typeOfArray [] _ = True
typeOfArray (x:xs) t = typeOf x == t && typeOfArray xs t

typeOf :: JSON -> Maybe JSONType
typeOf (JString _) = Just TyString
typeOf (JNumber _) = Just TyNum
typeOf (JBoolean _) = Just TyBool
typeOf JNull = Just TyNull

typeOf (JArray []) = Nothing
typeOf (JArray (x:xs))
  | typeOfArray xs typeOfX = Just $ TyArray $ (\(Just a) -> a) typeOfX
  | otherwise = Nothing
    where typeOfX = typeOf x

typeOf (JObject obj)
  | hasDuplicates orderedLs = Nothing
  | hasNothing = Nothing
  | otherwise = Just $ TyObject $ map (\(k,Just t) -> (k,t)) types
    where
      orderedLs = sortKeys obj
      types = map (\(k,v) -> (k, typeOf v)) orderedLs
      -- si alguna clave no tiene tipo, Nothing
      hasNothing = any (\(_,mt) -> mt == Nothing) types
      -- si hay claves repetidas, Nothing
      hasDuplicates [] = True
      hasDuplicates [_] = False
      hasDuplicates ((k1,_):rs@((k2,_):_)) = k1 == k2 || hasDuplicates rs

-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf [] = False
objectWf [_] = True
objectWf ((k1,_):rs@((k2,_):_)) = k1 < k2 && objectWf rs

-- decide si todos los tipos objeto contenidos en un tipo JSON
-- están bien formados.
typeWf :: JSONType -> Bool
typeWf TyString = True
typeWf TyNum = True
typeWf TyBool = True
typeWf TyNull = True
typeWf (TyArray t) = typeWf t
typeWf (TyObject obj) = objectWf obj && all (\(_,t) -> typeWf t) obj

-- dado un valor JSON v, y un tipo t, decide si v tiene tipo t.
hasType :: JSON -> JSONType -> Bool
hasType j t =
  | Just tj = tj == t
  | Nothing = False
