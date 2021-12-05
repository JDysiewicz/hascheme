module Repl where
import           Control.Monad
import           Env           (bindVars)
import           Eval          (eval)
import           LispVal       (Env, LispVal (Atom, List, String), liftThrows,
                                runIOThrows)
import           Parser        (readExpr)
import           Prim          (primitiveBindings)
import           System.IO
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
