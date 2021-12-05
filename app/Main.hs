module Main where
import           Control.Monad.Except
import           LispVal
import           Repl                 (runOne, runRepl)
import           System.Environment


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
