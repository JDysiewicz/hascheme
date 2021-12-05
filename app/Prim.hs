{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Prim where
import           Control.Monad.Except
import           Env                  (bindVars, nullEnv)
import           Eval                 (apply, load)
import           LispVal              (Env, IOThrowsError,
                                       LispError (NumArgs, TypeMismatch),
                                       LispVal (Atom, Bool, DottedList, IOFunc, List, Number, Port, PrimitiveFunc, String),
                                       ThrowsError, liftThrows)
import           Parser               (readExpr)
import           System.IO

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
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2 -- Trivial
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2 -- Trvial
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2 -- Trvial
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2 -- Trivial
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]] -- Checks eq. of the concatenatino of list with single list element [x]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) && -- Checks length equal, then zips together both lists. Then runs eqvPair on each and returns "False" if any eqvPair fails ("all")
                                                             all eqvPair (zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of -- Checks equivalence of the two values as a list recursively
                                Left err         -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList
