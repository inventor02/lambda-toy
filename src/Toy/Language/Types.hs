module Toy.Language.Types where

import Toy.Language.Parser ( Expr(..), ToyType(..) )
import Data.List (find)
type TypeEnvironment = [(String, ToyType)]

envAdd :: TypeEnvironment -> String -> ToyType -> TypeEnvironment
envAdd e s t = (s, t):e

envGet :: TypeEnvironment -> String -> Maybe ToyType
envGet e s = snd <$> find (\(s', _) -> s' == s) e

unparseType :: ToyType -> String
unparseType TBool = "Bool"
unparseType TInt = "Int"
unparseType (TFunc t1 t2) = "(" ++ unparseType t1 ++ " -> " ++ unparseType t2 ++ ")"

typeOf :: TypeEnvironment -> Expr -> ToyType
typeOf e (LitInt _) = TInt
typeOf e (LitBool _) = TBool
typeOf e (Var x) = case envGet e x of
                    Nothing -> error ("unknown type for " ++ x)
                    Just t' -> t'
typeOf e (LessThan e1 e2) = case (typeOf e e1, typeOf e e2) of
                              (TInt, TInt) -> TBool
                              (t1, t2) -> error ("expected Int < Int, got " ++ unparseType t1 ++ " < " ++ unparseType t2)
typeOf e (Plus e1 e2) = case (typeOf e e1, typeOf e e2) of
                              (TInt, TInt) -> TInt
                              (t1, t2) -> error ("expected Int + Int, got " ++ unparseType t1 ++ " < " ++ unparseType t2)
typeOf e (IfThenElse e1 e2 e3) | te1 /= TBool = error ("expected condition is Bool, got " ++ unparseType te1)
                               | te2 /= te3 = error ("then and else must have same type, got " ++ unparseType te2 ++ " and " ++ unparseType te3)
  where te1 = typeOf e e1
        te2 = typeOf e e2
        te3 = typeOf e e3
typeOf e (Func x t e1) = TFunc t te1
  where e' = envAdd e x t
        te1 = typeOf e' e1
typeOf e (LetIn x t e1 e2) = te2
  where e' = envAdd e x t
        te2 = typeOf e' e2
typeOf e (App e1 e2) | tf1 /= te2 = error ("mismatched function types, expected " ++ unparseType tf1 ++ " -> " ++ unparseType tf2 ++ ", got " ++ unparseType te2 ++ " -> ?")
                     | otherwise = tf2
  where (tf1, tf2) = case typeOf e e1 of
                      TFunc f1 f2 -> (f1, f2)
                      t -> error ("expected function type, got " ++ unparseType t)
        te2 = typeOf e e2