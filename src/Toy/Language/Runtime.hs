module Toy.Language.Runtime where

import Toy.Language.Lexer
import Toy.Language.Parser
import Toy.Language.Types

stringToExpr :: String -> Expr
stringToExpr = parseToy . alexScanTokens

exprType :: Expr -> ToyType
exprType = typeOf []

eval :: String -> String
eval s = show e ++ " :: " ++ unparseType t
  where e = stringToExpr s
        t = exprType e
