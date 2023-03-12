{
module Toy.Language.Parser where

import Toy.Language.Lexer
}

%name parseToy
%tokentype {Token}
%error {parseError}

%token
  Int                 {TokenInt _}
  Bool                {TokenBool _}
  lit                 {TokenLit _ $$}
  true                {TokenTrue _}
  false               {TokenFalse _}
  '<'                 {TokenLessThan _}
  '+'                 {TokenPlus _}
  var                 {TokenVar _ $$}
  if                  {TokenIf _}
  then                {TokenThen _}
  else                {TokenElse _}
  '\\'                {TokenBackslash _}
  '('                 {TokenOpenBrack _}
  ')'                 {TokenCloseBrack _}
  ':'                 {TokenColon _}
  let                 {TokenLet _}
  in                  {TokenIn _}
  '='                 {TokenEquals _}
  '->'                {TokenArrow _}
  ','                 {TokenComma _}
  fst                 {TokenFst _}
  snd                 {TokenSnd _}

%right '->'
%right let in
%nonassoc '<' '='
%left '+'
%nonassoc '(' ')' ':' ',' lit true false var if then else fst snd
%left '\\'
%left APP

%%

Expr  : lit                                             {LitInt $1}
      | true                                            {LitBool True}
      | false                                           {LitBool False}
      | Expr '<' Expr                                   {LessThan $1 $3}
      | Expr '+' Expr                                   {Plus $1 $3}
      | var                                             {Var $1}
      | '\\' '(' var ':' Type ')' Expr                  {Func $3 $5 $7}
      | let '(' var ':' Type ')' '=' Expr in Expr       {LetIn $3 $5 $8 $10}
      | Expr Expr %prec APP                             {App $1 $2}
      | if Expr then Expr else Expr                     {IfThenElse $2 $4 $6}
      | '(' Expr ',' Expr ')'                           {Pair $2 $4}
      | fst Expr                                        {Fst $2}
      | snd Expr                                        {Snd $2}

Type  : Int                             {TInt}
      | Bool                            {TBool}
      | Type '->' Type                  {TFunc $1 $3}
      | '(' Type ',' Type ')'           {TPair $2 $4}

{

data ToyType = TInt | TBool | TFunc ToyType ToyType | TPair ToyType ToyType
  deriving (Show, Eq)

data Expr = LitInt Int
          | LitBool Bool
          | LessThan Expr Expr
          | Plus Expr Expr
          | Var String
          | IfThenElse Expr Expr Expr
          | Func String ToyType Expr
          | LetIn String ToyType Expr Expr
          | App Expr Expr
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
  deriving (Eq)

instance Show Expr where
  show (LitInt n) = show n
  show (LitBool b) = show b
  show (LessThan e1 e2) = show e1 ++ " < " ++ show e2
  show (Plus e1 e2) = show e1 ++ " + " ++ show e2
  show (Var x) = x
  show (IfThenElse e1 e2 e3) = "if (" ++ show e1 ++ ") then " ++ show e2 ++ " else " ++ show e3
  show (Func x t e) = "\\(" ++ x ++ " : " ++ show t ++ ") -> " ++ show e
  show (LetIn x t e1 e2) = "let (" ++ x ++ " : " ++ show t ++ ") = " ++ show e1 ++ " in " ++ show e2
  show (App e1 e2) = show e1 ++ " " ++ show e2
  show (Pair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Fst e1) = "fst " ++ show e1
  show (Snd e1) = "snd " ++ show e1

parseError :: [Token] -> a
parseError [] = error "unknown parse error"
parseError (t:_) = error ("parse error at " ++ errloc t)
  where errloc = showPosn.tokenPosn

}