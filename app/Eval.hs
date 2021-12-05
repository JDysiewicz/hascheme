{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Eval where
import           Control.Monad.Except
import           Env                  (bindVars, defineVar, getVar, setVar)
import           LispVal              (Env, IOThrowsError,
                                       LispError (BadSpecialForm, NumArgs),
                                       LispVal (Atom, Bool, DottedList, Func, IOFunc, List, Number, PrimitiveFunc, String),
                                       liftThrows, showLispVal)
import           Parser               (readExprList)
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
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showLispVal params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
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

-- Reads everything in file and evaluates each with readExprLst
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList
