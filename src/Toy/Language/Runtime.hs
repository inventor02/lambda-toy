module Toy.Language.Runtime where

import Toy.Language.Lexer
import Toy.Language.Parser
import Toy.Language.Types

-- environment
type Environment = [(String, CExpr)]

-- expression type that supports either a wrapped expression or a continuation
data CExpr = E Expr | C String ToyType CExpr Environment deriving (Eq, Show)

-- CEK machine data types
data Frame  = AppHole CExpr
            | HoleApp CExpr Environment
            | LessHole CExpr
            | HoleLess CExpr Environment
            | AddHole CExpr
            | HoleAdd CExpr Environment
            | IfHoleThenElse CExpr CExpr Environment
            | LetHoleIn String ToyType CExpr Environment
  deriving (Eq, Show)
type Kontinuation = [Frame]
type Configuration = (CExpr, Environment, Kontinuation)

-- convert an input string to a \Toy expression
stringToExpr :: String -> Expr
stringToExpr = parseToy . alexScanTokens

-- get the type of an expression
exprType :: Expr -> ToyType
exprType = typeOf []

-- unpack a CExpr back to a normal expression
unpack :: CExpr -> Expr
unpack (C x t e _) = Func x t (unpack e)
unpack (E e) = e

-- evaluate a string and return a "pretty printed" expression
eval :: String -> String
eval s = show (unpack e') ++ " :: " ++ unparseType t
  where e = stringToExpr s
        (e', _, _) = loopEval (E e, [], [])
        t = exprType e

loopEval :: Configuration -> Configuration
loopEval (e, env, k)  | e == e' && isVal e' && null k = (e', [], [])
                      | otherwise = loopEval (e', env', k')
  where (e', env', k') = eval1 (e, env, k)

-- determine if an expression is a terminated value
isVal :: CExpr -> Bool
isVal (C {}) = True
isVal (E (LitInt _)) = True
isVal (E (LitBool _)) = True
isVal _ = False

-- single step CEK evaluation function
eval1 :: Configuration -> Configuration

-- rule for variables
eval1 (E (Var x), e, k) = (x', e, k)
  where x' = case lookup x e of
                Just val -> val
                Nothing -> error "unexpected runtime evaluation error, undefined variable"

-- completed evaluations
eval1 (v, e, []) | isVal v = (v, e, [])

-- rules for addition
eval1 (E (Plus e1 e2), e, k) = (E e1, e, HoleAdd (E e2) e:k)
eval1 (E e1@(LitInt n1), e, HoleAdd (E e2) e':k) = (E e2, e', AddHole (E e1):k)
eval1 (E e2@(LitInt n2), e, AddHole (E (LitInt n1)):k) = (E (LitInt (n1 + n2)), [], k) -- TODO why clear environment here?

-- rules for less than
eval1 (E (LessThan e1 e2), e, k) = (E e1, e, HoleLess (E e2) e:k)
eval1 (E e1@(LitInt n1), e, HoleLess (E e2) e':k) = (E e2, e', LessHole (E e1):k)
eval1 (E e2@(LitInt n2), e, LessHole (E (LitInt n1)):k) = (E (LitBool (n1 < n2)), [], k)

-- rules for if then else
eval1 (E (IfThenElse e1 e2 e3), e, k) = (E e1, e, IfHoleThenElse (E e2) (E e3) e:k)
eval1 (E (LitBool b), e, IfHoleThenElse (E e2) (E e3) e':k) | b = (E e2, e', k)
                                                            | otherwise = (E e3, e', k)

-- rules for let in
eval1 (E (LetIn x t e1 e2), e, k) = (E e1, e, LetHoleIn x t (E e2) e:k)
eval1 (v, e, LetHoleIn x t (E e2) e':k) | isVal v = (E e2, (x, v):e', k)

-- rules for lambda abstraction
eval1 (E (Func x t e1), e, k) = (C x t (E e1) e, [], k)

-- rules for application
eval1 (E (App e1 e2), e, k) = (E e1, e, HoleApp (E e2) e:k)
eval1 (e1, e, HoleApp (E e2) e':k) = (E e2, e', AppHole e1:k)
eval1 (v, e, AppHole (C x t e1 e'):k) | isVal v = (e1, (x, v):e', k)

-- catch-all where we have no cases to execute
-- theoretically, this should not happen
-- well typed programs never go wrong >:)
eval1 _ = error "unexpected runtime evaluation error"
