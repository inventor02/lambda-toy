module Main where

import Toy.Language.Runtime
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Control.Monad (when)
import System.Exit (exitSuccess)
import Control.Exception (SomeException, catch)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  putStr "toy> "
  hFlush stdout
  input <- getLine
  when (input == ":q") $ putStrLn "Bye" *> exitSuccess
  catch (eval' input)
        (\e -> do let err = show (e :: SomeException)
                  hPutStrLn stderr ("!!!> " ++ err)
                  return ())
  loop

eval' :: String -> IO ()
eval' i = do
  let r = eval i
  putStrLn (">>>> " ++ r)
  return ()