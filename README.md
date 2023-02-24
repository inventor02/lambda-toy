# lambda-toy

## Grammar

```
T ::= Int | Bool | T -> T

E ::= $digits | true | false | E < E | E + E | $varname | if E then E else E | \($varname : T) E | let ($varname : T) = E in E | E E
```

## Lexemes

- Int
- Bool
- $digits
- true
- false
- <
- +
- some var name?
- if
- then
- else
- \
- (
- )
- :
- let
- in
- =
- ->