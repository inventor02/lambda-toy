module Toy.Language.Runtime where

import Data.List

import Toy.Language.Lexer
import Toy.Language.Parser
import Toy.Language.Types

type Environment = [(String, CExpr)]
data CExpr = E Expr | C CExpr Environment deriving (Eq, Show)

data Frame  = AppHole CExpr
            | HoleApp CExpr Environment
            | LessHole CExpr
            | HoleLess CExpr
            | AddHole CExpr
            | HoleAdd CExpr
            | IfHoleThenElse CExpr CExpr Environment
            | LetHoleIn CExpr Environment
  deriving (Eq, Show)
type Kontinuation = [Frame]
type Configuration = (CExpr, Environment, Kontinuation)

stringToExpr :: String -> Expr
stringToExpr = parseToy . alexScanTokens

exprType :: Expr -> ToyType
exprType = typeOf []

eval :: String -> String
eval s = show e ++ " :: " ++ unparseType t
  where e = stringToExpr s
        t = exprType e

eval1 :: Configuration -> Configuration

eval1 (E (LessThan (LitInt n) (LitInt m)), e, k) = (E (LitBool (n < m)), e, k)
eval1 (E (LessThan e1@(LitInt n) e2), e, k) = (E e2, e, LessHole (E e1):k)
eval1 (E (LessThan e1 e2), e, k) = (E e1, e, HoleLess (E e2):k)

eval1 (E (Plus (LitInt n) (LitInt m)), e, k) = (E (LitInt (n + m)), e, k)
eval1 (E (Plus e1@(LitInt n) e2), e, k) = (E e2, e, AddHole (E e1):k)
eval1 (E (Plus e1 e2), e, k) = (E e1, e, HoleAdd (E e2):k)

eval1 (E (IfThenElse (LitBool True) e2 _), e, k) = (E e2, e, k)
eval1 (E (IfThenElse (LitBool False) _ e3), e, k) = (E e3, e, k)
eval1 (E (IfThenElse e1 e2 e3), e, k) = (E e1, e, IfHoleThenElse (E e2) (E e3) e:k)

eval1 (E (LetIn x t v@(LitInt _) e2), e, k) = (E e2, (x, C (E v) e):e, k)
eval1 (E (LetIn x t v@(LitBool _) e2), e, k) = (E e2, (x, C (E v) e):e, k)
eval1 (E (LetIn x t v@(Func {}) e2), e, k) = (E e2, (x, C (E v) e):e, k)
eval1 (E (LetIn x t e1 e2), e, k) = (E e1, e, LetHoleIn (E e2) e:k)

eval1 (E (App (Func x _ e1) v@(LitInt _)), e, k) = (E e1, (x, C (E v) e):e, k)
eval1 (E (App (Func x _ e1) v@(LitBool _)), e, k) = (E e1, (x, C (E v) e):e, k)
eval1 (E (App (Func x _ e1) v@(Func {})), e, k) = (E e1, (x, C (E v) e):e, k)
eval1 (E (App f@(Func {}) e2), e, k) = (E e2, e, AppHole (E f):k)
eval1 (E (App e1 e2), e, k) = (E e1, e, HoleApp (E e2) e:k)

eval1 (E (Var x), e, k) = (x', e, k)
  where x' = case lookup x e of
                Just a -> a
                Nothing -> error "evaluation error"

-- eval1 (E (LitInt ))