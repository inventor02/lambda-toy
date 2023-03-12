{
module Toy.Language.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-z]
$varname = [a-zA-Z0-9]

tokens :-

$white+                         ;
Int                             {\p s -> TokenInt p}
Bool                            {\p s -> TokenBool p}
$digit+                         {\p s -> TokenLit p (read s)}
True                            {\p s -> TokenTrue p}
False                           {\p s -> TokenFalse p}
"<"                             {\p s -> TokenLessThan p}
"+"                             {\p s -> TokenPlus p}
if                              {\p s -> TokenIf p}
then                            {\p s -> TokenThen p}
else                            {\p s -> TokenElse p}
\\                              {\p s -> TokenBackslash p}
"("                             {\p s -> TokenOpenBrack p}
")"                             {\p s -> TokenCloseBrack p}
":"                             {\p s -> TokenColon p}
let                             {\p s -> TokenLet p}
in                              {\p s -> TokenIn p}
"="                             {\p s -> TokenEquals p}
"->"                            {\p s -> TokenArrow p}
","                             {\p s -> TokenComma p}
fst                             {\p s -> TokenFst p}
snd                             {\p s -> TokenSnd p}
$alpha$varname*                 {\p s -> TokenVar p s}

{

data Token  = TokenInt AlexPosn
            | TokenBool AlexPosn
            | TokenLit AlexPosn Int
            | TokenTrue AlexPosn
            | TokenFalse AlexPosn
            | TokenLessThan AlexPosn
            | TokenPlus AlexPosn
            | TokenVar AlexPosn String
            | TokenIf AlexPosn
            | TokenThen AlexPosn
            | TokenElse AlexPosn
            | TokenBackslash AlexPosn
            | TokenOpenBrack AlexPosn
            | TokenCloseBrack AlexPosn
            | TokenColon AlexPosn
            | TokenLet AlexPosn
            | TokenIn AlexPosn
            | TokenEquals AlexPosn
            | TokenArrow AlexPosn
            | TokenComma AlexPosn
            | TokenFst AlexPosn
            | TokenSnd AlexPosn
  deriving (Show, Eq)

tokenPosn :: Token -> AlexPosn
tokenPosn t = case t of
                TokenInt p -> p
                TokenBool p -> p
                TokenLit p _ -> p
                TokenTrue p -> p
                TokenFalse p -> p
                TokenLessThan p -> p
                TokenPlus p -> p
                TokenVar p _ -> p
                TokenIf p -> p
                TokenThen p -> p
                TokenElse p -> p
                TokenBackslash p -> p
                TokenOpenBrack p -> p
                TokenCloseBrack p -> p
                TokenColon p -> p
                TokenLet p -> p
                TokenIn p -> p
                TokenEquals p -> p
                TokenArrow p -> p
                TokenComma p -> p
                t -> error ("token not implemented fully " ++ show t)

showPosn :: AlexPosn -> String
showPosn (AlexPn _ l c) = "line " ++ show l ++ ", column " ++ show c

}