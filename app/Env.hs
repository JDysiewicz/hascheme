module Env where
import           Control.Monad.Except
import           Data.IORef
import           LispVal              (Env, IOThrowsError,
                                       LispError (UnboundVar), LispVal,
                                       ThrowsError)



-- Helper to create empty env in IO Monad
nullEnv :: IO Env
nullEnv = newIORef []

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

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
