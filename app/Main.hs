module Main where

import           Control.Monad
import           Control.Monad.Except
import           Data.IORef
-- Use IORef to store stateful variables; allows state in IO monads which we use IO monad anyway for REPL
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec hiding (spaces)

-- Store environment as map of tuples of string "Atoms" to their value; IORef LispVal as LispVal might be another scope which has its own state
type Env = IORef [(String, IORef LispVal)]

-- Helper to create empty env in IO Monad
nullEnv :: IO Env
nullEnv = newIORef []

-- With IO Monad and Error monad, we need a way to combine the functionality of two monads: Monad Transformers
type IOThrowsError = ExceptT LispError IO

-- Need a way to lift a regular ThrowsError into our combined monad
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

-- Need a way to run the IOThrowsError action
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

-- Check variable already exists in env
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef -- Need to lift the IO [(String, IORef LispVal)] env into IOThrowsError combined monad, so use liftIO
    maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env) -- flip used as writeIORef takes Ref then value, whereas usign maybe to apply the result of (lookup var env) to the function; so need to flip the args
    return value -- Returned for convinience

-- Sets value if exists, inits it if doesnt
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    defined <- liftIO $ isBound envRef var
    if defined
        then setVar envRef var value >> return value
        else liftIO $ do -- liftIO to lift IO action into IOThrowsError
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env) -- Overwrites env, with new env consisting of (key, val) `cons` oldEnv
            return value

-- Bind many values at once, as when invoking a function
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where   extendEnv bindings env = liftM (++ env) (mapM addBinding bindings) -- Creates list of current bindings, and appends env onto the end of it
            addBinding (var, value) = do -- Creates new IORef and returns variable with its reference
                ref <- newIORef value
                return (var, ref)


data LispVal = Atom String -- Stores string naming the Atom
             | List [LispVal] -- Stores a list of other LispVals
             | DottedList [LispVal] LispVal -- Scheme form (a b . c); improper list; stores list of all elements except the last, then stores last element as another field
             | Number Integer -- Haskell Integer
             | String String -- Haskel string
             | Bool Bool -- Haskell bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal) -- Basic functions; +, eqn, -...
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env } -- User defined functions
             | IOFunc ([LispVal] -> IOThrowsError LispVal) -- Function that can perform IO
             | Port Handle -- Thing you can read from and write to

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where show = showLispVal
showLispVal :: LispVal -> String
showLispVal (String s) = "\"" ++ s ++ "\""
showLispVal (Atom a) = a
showLispVal (Number n) = show n
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
showLispVal (List contents) = "(" ++ unwordsList contents ++ ")"
showLispVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showLispVal tail ++ ")"
showLispVal (PrimitiveFunc _) = "<primitive>"
showLispVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing  -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showLispVal (IOFunc _) = "<IO Primative>"
showLispVal (Port _) = "<IO Port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- Partially applied to create type constructor which takes a "happy path" argument
-- e.g. ThrowsError String a == ThrowsError LispVal
type ThrowsError = Either LispError



-- Return error as string representation within monad
trapError action = catchError action (return . show)

-- ExtractValue ONLY used after trapError, where we ensure a valid Right constructor. Leaving
-- Left pattern match undefined enforces that this should never be used in a case where Right is invalid.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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


-- parseQuotedDesugar :: Parser LispVal
-- parseQuotedDesugar = char '\'' >> parseExpr >>= (\x -> return $ List [Atom "quote", x])

-- All the ways we can evaluate a LispVal;
-- Parser converts the string of scheme input into various data types/structures
-- then this evaluates those data types and converts them into something Haskell can do computatino with

-- Need to pass env through each eval call as no global vars in Haskell
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             _          -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = -- Variable define
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = -- Function define
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do -- Function evaluation
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- Helper functions to make creating functions easier
makeFunc varargs env params body = return $ Func (map showLispVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showLispVal

-- Passed LispVal representing function; could be primative (from list below) or user-defined
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args -- Primative func below
apply (Func params varargs body closure) args = -- User defined
      if num params /= num args && varargs == Nothing -- Invalid num args
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
            ("-", numericBinop (-)),
            ("*", numericBinop (*)),
            ("/", numericBinop div),
            ("mod", numericBinop mod),
            ("quotient", numericBinop quot),
            ("remainder", numericBinop rem),
            ("=", numBoolBinop (==)), -- Boolean operators; separate for num comparisons, bool comparisons, and str comparisons
            ("<", numBoolBinop (<)),
            (">", numBoolBinop (>)),
            ("/=", numBoolBinop (/=)),
            (">=", numBoolBinop (>=)),
            ("<=", numBoolBinop (<=)),
            ("&&", boolBoolBinop (&&)),
            ("||", boolBoolBinop (||)),
            ("string=?", strBoolBinop (==)),
            ("string<?", strBoolBinop (<)),
            ("string>?", strBoolBinop (>)),
            ("string<=?", strBoolBinop (<=)),
            ("string>=?", strBoolBinop (>=)),
            ("car", car),
            ("cdr", cdr),
            ("cons", cons),
            ("eq?", eqv),
            ("eqv?", eqv)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

-- Primatives need binding to null env so they exist as PrimativeFuncs rather than reading in as a String and looking up in table above
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

-- Wrapper around apply, destructures the argument list into the form apply expects
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

-- Wraps openFile in Haskell; allows reading from and writing to
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- Wraps Haskell hClose
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- Reads line from file, accessed via the Port
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

-- Converts LispVal to string and writes to Port
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- Wrapper around readFile; reads whole file into string in memory
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- Reads everything in file and evaluates each with readExprLst
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- Wraps the above in List constructor
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

-- Generic operator which expects exactly 2 arguments and returns a boolean
-- unpacker determines whether expecting a string, number, or boolean input using helper functions.
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else
                                 do
                                        left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right -- Infix use of operator

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- Extracts number from LispVal
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- Extract string from LispVal
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


-- Extract Bool from LispVal
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- List Primatives:
-- car = head in list
-- cdr = tail in list
-- cons = join two elements into list
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1] -- elem : Nil is a 1 item list
cons [x, List xs]             = return $ List $ x : xs -- elem : xs is elem : (elem2 : (elem3 : [])), so just new list
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast -- Dotted list with improper tail
cons [x1, x2]                 = return $ DottedList [x1] x2 -- Dotted list as cons cell not terminated by Nil
cons badArgList               = throwError $ NumArgs 2 badArgList -- Cons with non-2 args is error


-- If two items in list are equivalent
-- EQUAL? UNIMPLEMENTED CURRENTLY
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2 -- Trivial
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2 -- Trvial
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2 -- Trvial
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2 -- Trivial
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]] -- Checks eq. of the concatenatino of list with single list element [x]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && -- Checks length equal, then zips together both lists. Then runs eqvPair on each and returns "False" if any eqvPair fails ("all")
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of -- Checks equivalence of the two values as a list recursively
                                Left err         -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- REPL code

-- Output and flush stream
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- Prints out prompt and reads line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Now takes env
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

-- Monadic infinite loop
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- runOne evaluates a file
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
