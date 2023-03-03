module Main where

import Toy.Language.Runtime
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ error "need a file name"
  let file = head args
  contents <- readFile file
  let r = eval contents
  print r
  return ()