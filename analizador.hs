module Analizador where

import Text.ParserCombinators.Parsec

-- Definición de los operadores de comparación
-- Eq  -> Igual (=)
-- Gt  -> Mayor que (>).
-- Lt  -> Menor que (<).
-- Ge  -> Mayor o igual (>=).
-- Le  -> Menor o igual (<=).
-- Ne  -> Diferente (!=).
data Operator = Eq | Gt | Lt | Ge | Le | Ne
    deriving (Show, Eq)

-- Representa una condición en la cláusula WHERE
-- Ejemplo: edad > 30
data Condition = Condition {
    column   :: String,
    operator :: Operator,
    value    :: String
} deriving (Show, Eq)

-- Representa expresiones lógicas en WHERE (AND, OR)
data Expr
    = Cond Condition        -- Expresión simple con una condición
    | And Expr Expr         -- Expresión con AND
    | Or Expr Expr          -- Expresión con OR
    deriving (Show, Eq)

-- Representa una consulta completa SQL-like
data Query = Query {
    selectFields :: [String],  -- Lista de columnas seleccionadas
    fromTable    :: String,    -- Tabla de la consulta
    whereClause  :: Maybe Expr -- Condición opcional en WHERE
} deriving (Show, Eq)

-- Parser para los operadores de comparación
operatorParser :: Parser Operator
operatorParser = choice
    [ try (string "=")  >> return Eq
    , try (string ">=") >> return Ge
    , try (string "<=") >> return Le
    , try (string "!=") >> return Ne
    , try (string ">")  >> return Gt
    , try (string "<")  >> return Lt
    ]

-- Parser para una condición individual (columna operador valor)
-- Ejemplo: edad > 30
conditionParser :: Parser Condition
conditionParser = do
    col <- many1 letter
    spaces   -- Captura el nombre de la columna
    op <- operatorParser
    spaces  -- Captura el operador
    val <- many1 (letter <|> digit)
    spaces -- Captura el valor
    return (Condition col op val)

-- Parser para expresiones WHERE con precedencia correcta: AND > OR
exprParser :: Parser Expr
exprParser = orExpr

-- OR tiene menor prioridad, por lo que es la estructura de nivel superior
orExpr :: Parser Expr
orExpr = chainl1 andExpr (try (string "OR") >> spaces >> return Or)

-- AND tiene mayor prioridad, por lo que se evalúa primero
andExpr :: Parser Expr
andExpr = chainl1 term (try (string "AND") >> spaces >> return And)

-- Un término es una condición simple o una expresión entre paréntesis
term :: Parser Expr
term = do
    spaces
    cond <- conditionParser
    spaces
    return (Cond cond)
    <|> between (char '(' >> spaces) (spaces >> char ')') exprParser

-- Parser para palabras clave SQL como SELECT, FROM, WHERE
reserved :: String -> Parser String
reserved word = do
    result <- try (string word)
    spaces
    return result

-- Parser para listas de columnas en SELECT
-- Ejemplo: SELECT nombre, edad
columnsParser :: Parser [String]
columnsParser = do
    cols <- sepBy1 (many1 letter <|> string "*") (do
        char ','
        spaces)
    spaces -- Se asegura de consumir los espacios antes de "FROM"
    return cols

-- Definición manual de optionMaybe para compatibilidad con Hugs
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = do
    result <- option Nothing (do
        val <- p
        return (Just val))
    return result

-- Parser para la consulta completa con SELECT, FROM y WHERE opcional
queryParser :: Parser Query
queryParser = do
    reserved "SELECT"
    cols <- columnsParser
    reserved "FROM"
    table <- many1 letter
    spaces
    wherePart <- optionMaybe (do
        reserved "WHERE"
        exprParser)
    return (Query cols table wherePart)

-- Función para convertir una consulta desglosada a formato DOT
toDot :: Query -> String
toDot (Query fields table whereExpr) =
    unlines (
        ["digraph Query {",
         "  node [shape=box];",
         "  Query -> Table [label=\"FROM\"];",
         "  Table [label=\"" ++ table ++ "\"];"]
        ++ map (\f -> "  Query -> " ++ f ++ " [label=\"SELECT\"];") fields
        ++ whereDot whereExpr
        ++ ["}"])
  where
    whereDot Nothing = []
    whereDot (Just expr) = ["  Query -> Where [label=\"WHERE\"];"] ++ exprToDot "Where" expr

    exprToDot parent (Cond (Condition col op val)) =
        ["  " ++ parent ++ " -> Cond_" ++ col ++ " [label=\"" ++ col ++ " " ++ show op ++ " " ++ val ++ "\"];"]

    exprToDot parent (And l r) =
        let andNode = parent ++ "_AND"
        in  ["  " ++ parent ++ " -> " ++ andNode ++ " [label=\"AND\"];", 
             "  " ++ andNode ++ " [shape=ellipse];"]
            ++ exprToDot andNode l
            ++ exprToDot andNode r

    exprToDot parent (Or l r) =
        let orNode = parent ++ "_OR"
        in  ["  " ++ parent ++ " -> " ++ orNode ++ " [label=\"OR\"];", 
             "  " ++ orNode ++ " [shape=ellipse];"]
            ++ exprToDot orNode l
            ++ exprToDot orNode r


-- Función principal de prueba
main :: IO ()
main = parseTest queryParser "SELECT nombre FROM empleados WHERE edad > 25 AND departamento = Ventas OR salario > 5000 AND experiencia >= 5;"