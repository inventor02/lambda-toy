{
module Parser where

import Lexer
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

%right '->'
%right let in
%nonassoc '<' '='
%left '+'
%nonassoc '(' ')' ':' lit true false var if then else
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

Type  : Int                             {TInt}
      | Bool                            {TBool}
      | Type '->' Type                  {TFunc $1 $3}

{

data ToyType = TInt | TBool | TFunc ToyType ToyType
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
  deriving (Show, Eq)

parseError :: [Token] -> a
parseError [] = error "unknown parse error"
parseError (t:_) = error "parse error at " ++ errloc t
  where errloc = showPosn.tokenPosn

}