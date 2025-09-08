-- 1. Explique el tipo de las siguientes funciones:
-- (a) min x y = if x <y then x else y
-- min :: Ord a => a -> a -> a
min1 x y = if x < y then x else y
-- (b) paren x = "("++ show x ++ ")"
-- paren :: Show a => a -> [Char]
paren x = "("++ show x ++ ")"
