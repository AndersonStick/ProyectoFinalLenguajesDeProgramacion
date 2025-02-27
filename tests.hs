module Main where

import Analizador (Query(..), Expr(..), Condition(..), Operator(..))

-- Función para probar consultas de manera manual
runTest :: String -> String -> Query -> IO ()
runTest title description expected = do
    putStrLn ("===== " ++ title ++ " =====")
    putStrLn ("Consulta: " ++ description)
    putStrLn ("Esperado: " ++ show expected)
    putStrLn "--------------------------"

main :: IO ()
main = do
    -- Prueba 1: Consulta simple sin WHERE
    runTest "Prueba 1: Consulta sin WHERE" "SELECT nombre, edad FROM usuarios;"
        (Query ["nombre", "edad"] "usuarios" Nothing)
    
    -- Prueba 2: Consulta con WHERE
    runTest "Prueba 2: Consulta con WHERE" "SELECT nombre FROM usuarios WHERE edad > 30;;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Gt "30"))))
    
    -- Prueba 3: Consulta con AND y OR
    runTest "Prueba 3: WHERE con AND y OR" "SELECT nombre FROM usuarios WHERE edad > 30 AND ciudad = Madrid;"
        (Query ["nombre"] "usuarios" (Just (And (Cond (Condition "edad" Gt "30")) (Cond (Condition "ciudad" Eq "Madrid")))))
    
    -- Prueba 4: Comparadores (=, !=, >=, <=, >, <)
    runTest "Prueba 4.1: WHERE con operador =" "SELECT nombre FROM usuarios WHERE edad = 30;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Eq "30"))))
    runTest "Prueba 4.2: WHERE con operador >=" "SELECT nombre FROM usuarios WHERE edad >= 30;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Ge "30"))))
    runTest "Prueba 4.3: WHERE con operador <=" "SELECT nombre FROM usuarios WHERE edad <= 30;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Le "30"))))
    runTest "Prueba 4.4: WHERE con operador !=" "SELECT nombre FROM usuarios WHERE edad != 30;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Ne "30"))))
    runTest "Prueba 4.5: WHERE con operador <" "SELECT nombre FROM usuarios WHERE edad < 30;"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Lt "30"))))
    runTest "Prueba 4.6: WHERE con operador >" "SELECT nombre FROM usuarios WHERE edad > 30"
        (Query ["nombre"] "usuarios" (Just (Cond (Condition "edad" Gt "30"))))