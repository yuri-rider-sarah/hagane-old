# Hagane Language Reference

## 1. Tokens
A program consists of a sequence of tokens. Whitespace characters may occur anywhere between tokens, and are used to separate them.

### 1.1. Identifiers
An identifier may consist of characters with Unicode General Category L\* (Letter), M\* (Mark), N\* (Number), Pc (Punctuation, Connector), Pd (Punctuation, Dash), or S\* (Symbol).
The following characters from category Po (Punctuation, Other) are also permitted: `%&*/\؉؊٪‰‱′″‴‵‶‷⁂⁗⁎⁑⁕﹠﹡﹨﹪％＆＊／＼`
However, identifiers may not begin with a numeric character (with General Category N\*).

### 1.2. Keywords
Keywords are distinguished from identifiers by ending in a period. The rules for which characters may occur in a keyword before the period are the same as for identifiers.

### 1.3. Integer literals
Integer literals consist of digits `0`-`9`.

### 1.4. Other tokens
The following characters represent tokens: `(`, `)`, `[`, `]`, `{`, `}`, `'`, `:`, `#`.
They don't need to be separated from other tokens with whitespace.

## 2. Types

### 2.1. Basic types
There are two builtin types: `Int` and `Bool`.
- `Int` represents 64-bit signed integers.
- `Bool` represents true/false values, denoted respectively as `⊤`/`⊥`.

### 2.2. Tuple types
```
##(<type_1> ... <type_n>)
```
This is the type of tuple of n elements having types `<type_1>` through `<type_n>`.

Of particular note is the empty tuple `#()`, also called the unit type, which contains only one value, also written `#()`.
It is used as a return type for functions which do not return a meaningful value.

Nonempty tuples are useless in the current version, as there is no way to deconstruct them.

### 2.3. Function types
```
(<type_1> ... <type_n> : <type_ret>)
```
This is the type of a function taking n arguments of types `<type_1>` through `<type_n>` and returning a value of type `<type_ret>`.

## 3. Expressions

### 3.1. Syntax elements

#### 3.1.1. Expressions
The basic element of Hagane syntax is the expression. Each expression evaluates to a value.

#### 3.1.2. Blocks
Blocks are groupings of expressions. To evaluate a block, each expression is evaluated, and the value of the last expression is returned.

A block with no expressions evaluates to `#()`.

### 3.2. Basic expressions
Identifiers evaluate to the value of the variable they represent.
Integer literals evaluate to the integer they represent.

### 3.3. Function call
```
(<expr_f> <expr_1> ... <expr_n>)
```
The value of `<expr_f>` is called with the values of `<expr_1>` through `<expr_n>`.
The result is the return value of the call.

The function and arguments are evaluated left-to-right.

### 3.4. Tuple construction
```
##(<expr_1> ... <expr_n>)
```
This creates an n-tuple containing the values of `<expr_1>` through `<expr_n>`.

The subexpressions are evaluated left-to-right.

### 3.5. `do.`
```
do. <block>
```
The block `<block>` is evaluated normally.

### 3.6. `let.`
```
let. <identifier> <expr>
```
The variable `<identifier>` is declared and bound to the value of `<expr>`.

The same variable may be declared multiple times, in which case the later declarations shadow earlier ones.

### 3.7. `set.`
```
set. <identifier> <expr>
```
This changes the value of variable `<identifier>` to the value of `<expr>`.

### 3.8. `if.`
```
if. <cond> then. <consequent> else. <alternative>
```
`<cond>` is an expression of type `Bool`, while `<consequent>` and `<alternative>` are blocks of the same type.

First, `<cond>` is evaluated.
If the resulting value is `⊤`, `<consequent>` is evaluated and its value returned.
Otherwise the resulting value is `⊥`, in which case `<alternative>` is evaluated and its value returned.

### 3.9. `while.`
```
while. <cond> <body>
```
`<cond>` is an expression of type `Bool`, and `<body>` is a block.
`<body>` is executed while `<cond>` is `⊤`.
The type of this expression is `#()`.

### 3.10. `λ.`
```
λ.(<var_1> ... <var_n>) <body>
```
This evaluates to an function that takes the parameters `<var_1>` through `<var_n>` and returns the value of the block `<body>`.

Due to limited type inference in the current version all `λ.` expressions must be followed by a type annotation.

### 3.11. Type annotations
```
<expr> '<type>
```
Identifiers, literals, function calls, tuple constructions, `if.` and `λ.` expressions may be followed by a type annotation. It indicates that the type of `<expr>` is `<type>`.

It's intended to be used in situations where the type cannot be inferred automatically and therefore has to be specified manually.

## 4. Builtin functions
Several basic functions are built into the language.

### 4.1. Arithmetic functions
```
+ '(Int Int : Int)
- '(Int Int : Int)
* '(Int Int : Int)
/ '(Int Int : Int)
% '(Int Int : Int)
```
These functions perform basic arithmetic on integers.

### 4.2. Comparison functions
```
= '(Int Int : Bool)
≠ '(Int Int : Bool)
< '(Int Int : Bool)
≤ '(Int Int : Bool)
> '(Int Int : Bool)
≥ '(Int Int : Bool)
```
These functions perform basic integer comparisons.

### 4.3. Printing
```
print '(Int : #())
```
This function prints an integer to the standard output, followed by a newline.
