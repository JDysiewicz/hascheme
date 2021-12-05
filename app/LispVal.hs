{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LispVal where
import           Control.Monad.Except
import           Data.IORef
import           System.IO
import           Text.Parsec

-- Store environment as map of tuples of string "Atoms" to their value; IORef LispVal as LispVal might be another scope which has its own state
type Env = IORef [(String, IORef LispVal)]

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


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- Error Handling

-- Partially applied to create type constructor which takes a "happy path" argument
-- e.g. ThrowsError String a == ThrowsError LispVal
type ThrowsError = Either LispError
-- With IO Monad and Error monad, we need a way to combine the functionality of two monads: Monad Transformers
type IOThrowsError = ExceptT LispError IO

-- Need a way to lift a regular ThrowsError into our combined monad
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val


-- Need a way to run the IOThrowsError action
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . (\(Right val) -> val)

-- Return error as string representation within monad
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

-- Helpers --
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLispVal
