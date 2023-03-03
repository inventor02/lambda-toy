module Main where

import Toy.Language.Runtime
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Control.Monad (when)
import System.Exit (exitSuccess)
import Control.Exception (catch, SomeException (SomeException), ErrorCall (ErrorCall))

main :: IO ()
main = loop

loop :: IO ()
loop = do
  putStr "toy> "
  hFlush stdout
  input <- getLine
  when (input == ":q") $ putStrLn "Bye" *> exitSuccess
  r <- catch
            (return $ eval input)
            (\e -> do 
                    let e' = show (e :: SomeException)
                    hPutStrLn stderr e'
                    return "")
  putStrLn (">>>> " ++ show r)
  loop