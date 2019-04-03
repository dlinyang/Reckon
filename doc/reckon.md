# reckon

## syntax

``` bnf
program        ::= statement |statement program;
statement      ::= declare | expression ;
declare        ::= type-declare | function-declare
expression     ::= atom-expression
                 | defines
                 | application 
                 | let defines expression 
                 | lambda  defines expression
                 | pattern expression alternatives
                 | if expr then expr else expr
atom-expression::= variable
                 | value
                 | Type
defines        ::= define | define defines
define         ::= variable = expression
application    ::= expression atom-expression
                 | expression prime-function expression;
comment        ::= line-comment | block-comment;
comment-line   ::= comment-line-start letters;
comment-block  ::= comment-block-start letters comment-block-end；
```

```
lowerLatter ::= a..z
upperLatter ::= A..Z
digital ::= 1..9
latter ::= lowerLatter|upperLatter|symbol
type ::= $upperLatter(latter|digital)
module :: = $upperLetter(latter)
number ::= $digital()()
identifier ::= $lowerLatter(later)
```

## Local definitions

```reckon
f 0 =  1
f x = x * f (x - 1)
main = printLn  f 20
```

## lambda abstractions

```reckon
double = \x.x+x
```

```reckon
double x = x + x
```

## structured data type

```reckon
Nat := N | S Nat
Q := Nat Nat

class monad where
    fmap id =  id
    fmap (f  g) = (fmap f) (fmap g)

instance num Nat where
```

## Representing constructors



## pattern match

```reckon
f [] = []
f x
  | 1 =  x + x
  | otherwise x
```

## type system