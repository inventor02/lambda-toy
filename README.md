# lambda-toy

A variant of the λToy language from [COMP2212 Programming Language Concepts](https://www.southampton.ac.uk/courses/modules/comp2212), based
heavily on lectures and material from Dr Julian Rathke.

## Grammar

```
T ::= Int | Bool | T -> T | (T, T)

E ::= $digits | true | false | (E, E) | E < E | E + E | $varname | if E then E else E | \($varname : T) E | let ($varname : T) = E in E | E E
```

## Usage

You can either execute the `toyi` binary, or run `toy <filename>` where `<filename>` is a λToy program file.