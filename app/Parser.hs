module Parser where
import           Control.Monad.Except
import           LispVal                       (LispError (Parser),
                                                LispVal (Atom, Bool, DottedList, List, Number, String),
                                                ThrowsError)
import           Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr -- Specialised application of readOrThrow
readExprList = readOrThrow (endBy parseExpr spaces) -- Specialised application of readOrThrow; used to load programs of scheme files
spaces :: Parser()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return (String x)

--parseStringDesugar :: Parser LispVal
--parseStringDesugar = char '"' >> many (noneOf "\"") >>= (\x -> char '"' >> return (String x))

-- Atom is letter or symbol, followed by any number of letters/digits/symbols
-- <|> choice operator; try first parser, if fails then try second
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

-- parseAtomDesugared :: Parser LispVal
-- parseAtomDesugared = (letter <|> symbol) >>= (\first -> many (letter <|> digit <|> symbol) >>= (\rest -> let atom = first:rest
--    in return $ case atom of
--        "#t" -> Bool True
--        "#f" -> Bool False
--        _    -> Atom atom))

parseNumber :: Parser LispVal
parseNumber =  Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces -- Series of expressions separated by whitespace

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)

-- parseDottedListDesugar :: Parser LispVal
-- parseDottedListDesugar = endBy parseExpr spaces >>= (\head -> char '.' >> spaces >> parseExpr >>= (\tail -> return $ DottedList head tail))

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> (char '(' >> (try parseList <|> parseDottedList) >>= (\x -> char ')' >> return x)) -- backtracking in parsing; list and dotted list same up to dot

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
