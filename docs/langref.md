# Hagane Language Reference (version 0.0.2)

## 1. Tokens
A program consists of a sequence of tokens. Whitespace characters may occur anywhere between tokens, and are used to separate them.

### 1.1. Identifiers
An identifier may consist of characters with Unicode General Category L\* (Letter), M\* (Mark), N\* (Number), Pc (Punctuation, Connector), Pd (Punctuation, Dash), or S\* (Symbol).
The following characters from category Po (Punctuation, Other) are also permitted: `%&*/\؉؊٪‰‱′″‴‵‶‷⁂⁗⁎⁑⁕﹠﹡﹨﹪％＆＊／＼`
However, identifiers may not begin with a numeric character (with General Category N\*).

### 1.2. Keywords
Keywords are distinguished from identifiers by ending in a period or a comma.
The rules for which characters may occur in a keyword before the period are the same as for identifiers.

Primary keywords end in a period, while secondary keywords end in a comma.

### 1.3. Integer literals
Integer literals consist of digits `0`-`9`.

### 1.4. Other tokens
The following characters represent tokens: `(`, `)`, `[`, `]`, `{`, `}`, `'`, `:`, `#`.

They don't need to be separated from other tokens with whitespace, except for `#`, which has to be separated from the left.

### 1.5. Comments
Comments are marked with the character `※` and last until the end of the line.

## 2. Types

### 2.1. Basic types
There are two builtin types: `Int` and `Bool`.
- `Int` represents 64-bit signed integers.
- `Bool` represents true/false values, denoted respectively as `⊤`/`⊥`.

### 2.2. Tuple types
```
#(<type_1> ... <type_n>)
```
This is the type of tuple of n elements having types `<type_1>` through `<type_n>`.

Of particular note is the empty tuple `#()`, also called the unit type, which contains only one value, also written `#()`.
It is used as a return type for functions which do not return a meaningful value.

Nonempty tuples are useless in the current version, as there is no way to deconstruct them.

### 2.3. Function types
```
‡(<type_1> ... <type_n> : <type_ret>)
```
This is the type of a function taking n arguments of types `<type_1>` through `<type_n>` and returning a value of type `<type_ret>`.

## 3. Syntax
The basic element of Hagane syntax is the expression. Each expression evaluates to a value.

### 3.1. Basic expressions
Identifiers evaluate to the value of the variable they represent.
Integer literals evaluate to the integer they represent.

### 3.2. Function call
```
<expr_f>(<expr_1> ... <expr_n>)
```
The value of `<expr_f>` is called with the values of `<expr_1>` through `<expr_n>`.
The result is the return value of the call.

The function and arguments are evaluated left-to-right.

### 3.3. Tuple construction
```
#(<expr_1> ... <expr_n>)
```
This creates an n-tuple containing the values of `<expr_1>` through `<expr_n>`.

The subexpressions are evaluated left-to-right.

### 3.4. Special expressions
Special expressions are used for all other expressions. A special expression consists of a primary keyword and a series of clauses, where each clause is either a secondary keyword or a block.

#### 3.4.1. Block syntax
A block consists of a series of expressions, either surrounded by braces or indented. A block consisting of a single expression may also be written as just the expression.

To determine if a line starts an indented block, its indentation level is compared to the indentation level of the line containing the primary keyword of the special expression that would contain the block.

```
<outer context>
    <expr_1>
    ...
    <expr_n>
<outer context>
```
```
{<expr_1> ... <expr_n>}
```
```
<expr>
```

#### 3.4.2. Block evaluation

How a block is interpreted depends on the special expression to which it belongs.
However, many special expressions will use blocks in the same way, which is referred to as evaluating them.

To evaluate a block, each expression is evaluated, and the value of the last expression is returned. A block with no expressions evaluates to `#()`.

#### 3.4.3. Standard special expression syntax
```
<keyword>(<clause_1> ... <clause_n>)
```
This is the standard syntax for special expressions.

#### 3.4.4. Block special expression syntax
```
<keyword> <clause_1> ... <clause_n>
```
A special expression may be written without the parentheses at the top level of a block or program.

The first line after the primary keyword that doesn't continue an earlier clause or start with a secondary keyword is considered to belong to the next expression.

For example, the following consists of two expressions — an `if.` special expression and a call to `print`:
```
if. >(x 0) then,
    set. x -(x 1)
    print(x)
else, print(0)
print(y)
```

#### 3.4.5. Clause special expression syntax
```
<keyword> <clause_1> ... <clause_n>
```
A special expression may also be written without the parentheses when it occurs as a clause belonging to a block special expression or another clause special expression.

The first line after the primary keyword that doesn't continue an earlier clause is considered to belong to the next expression.

Part of the expression may be indented. Lines within the indented part may start with a secondary keyword or continue an eariler clause.
The end of the indented part marks the end of the expression.

For example, the following code contains two `if.` expressions, one nested inside the second block of another:
```
if. =(y 0) then, if. >(x 0)
    then, x
    else, 0
else, y
```

### 3.5. Type annotation
```
<expr> '<type>
```
Most expressions may be followed by a type annotation. It indicates that the type of `<expr>` is `<type>`.

Block and clause special expressions may not have a type annotation.

It's intended to be used in situations where the type cannot be inferred automatically and therefore has to be specified manually.

# 4. Builtin special expressions

Names of blocks are written in triangle brackets. All secondary keywords are optional.

### 4.1. `do.`
```
do. <block>
```
The block `<block>` is evaluated.

### 4.2. `let.`
```
let. <identifier> <expr>
```
`<identifier>` must consist of a single identifier.

The variable `<identifier>` is declared and bound to the value of `<expr>`.

The same variable may be declared multiple times, in which case the later declarations shadow earlier ones.

The type of this expression is `#()`.

### 4.3. `set.`
```
set. <identifier> <expr>
```
`<identifier>` must consist of a single identifier.

This changes the value of variable `<identifier>` to the value of `<expr>`.

The type of this expression is `#()`.

### 4.4. `if.`
```
if. <cond> then, <consequent> else, <alternative>
```
`<cond>` is a block of type `Bool`, while `<consequent>` and `<alternative>` are blocks of the same type.

First, `<cond>` is evaluated.
If the resulting value is `⊤`, `<consequent>` is evaluated and its value returned.
Otherwise the resulting value is `⊥`, in which case `<alternative>` is evaluated and its value returned.

### 4.5. `while.`
```
while. <cond> do, <body>
```
`<cond>` is a block of type `Bool`, and `<body>` is a block of any type.

`<body>` is repeatedly executed as long as `<cond>` evaluates to `⊤`.

The type of this expression is `#()`.

### 4.6. `λ.`
```
λ. <vars> ⇒, <body>
```
The block `<vars>` must consist of variables.

This evaluates to an function that takes as parameters the variables composing `<vars>` and returns the value of the block `<body>`.

Due to limited type inference in the current version all `λ.` expressions must have type annotations on all their parameters and the last expression of the body.

## 5. Builtin functions
Several basic functions are built into the language.

### 5.1. Arithmetic functions
```
+ '(Int Int : Int)
- '(Int Int : Int)
* '(Int Int : Int)
/ '(Int Int : Int)
% '(Int Int : Int)
```
These functions perform basic arithmetic on integers.

### 5.2. Comparison functions
```
= '(Int Int : Bool)
≠ '(Int Int : Bool)
< '(Int Int : Bool)
≤ '(Int Int : Bool)
> '(Int Int : Bool)
≥ '(Int Int : Bool)
```
These functions perform basic integer comparisons.

### 5.3. Input/output
```
print '(Int : #())
read '(: Int)
```

`print` prints an integer to the standard output, followed by a newline.

`read` reads an integer from the standard input. If an integer cannot be read, an error message is printed to the standard error and the program fails.
