-- 1. Para las siguientes funciones, determine su tipo y pase su implementaci ́on
-- de notaci ́on do a uso de los operadores (>>=) y (>>):
-- (a) foo x y = do a ←getLine
--                  putStrLn (a ++ x)
--                  b ←getLine
--                  putStrLn (b ++ y)
--                  putStrLn (a ++ b)
foo :: String -> String -> IO ()
foo x y = 
    getLine >>= \a -> 
        putStrLn(a ++ x) >> 
        getLine >>= \b -> 
            putStrLn(b ++ y) >> 
            putStrLn(a++b)

-- (b) bar m1 m2 = do m1
--                    x ←m2
--                    y ←m1
--                    return (x ∨y)
bar :: Monad m => m Bool -> m Bool -> m Bool
bar m1 m2 =
    m1 >>
    m2 >>= \x ->
        m1 >>= \y ->
            return (x||y)

-- (c) baz mf my x = do f ←mf
--                      y ←my
--                      Just (f x y)
baz :: Maybe (b -> a -> c) -> Maybe a -> b -> Maybe c
baz mf my x =
    mf >>= \f ->
        my >>= \y ->
            Just (f x y)