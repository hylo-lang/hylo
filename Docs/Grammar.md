# Grammar

This document describes Val's grammar.

## Operators

Whether an operator is interpreter as prefix, an infix or suffix depends on whitespaces that surrounds them.

* Operators with a whitespace on both sides are infix
* Operators with a whitespace only on the left are prefix
* Operators with a whitespace only on the rright are postfix

Most operators except can be overridden in type declarations.
Exceptions are the address-of operator (`&` as a prefix), the optional chaining operator (`?`, which is always postfix) and the forced unwrapping operator (`!` as a postfix).

Not all operators can appear in prefix, infix or postfix position.

* Valid prefix operators are `+` `-` `!` `~` `&`
* Valid infix operators are `=` `+` `-` `*` `/` `%` `<` `>` `<=` `>=` `==` `!=` `&&` `||` `~` `&` `|` `^` `<<` `>>` `...` `..<`
* Valid postfix operators are `!` `?`

Operators are defined as member methods of an operand.
Since postfix operators cannot be overloaded, the number of parameters is sufficient to distinguish between prefix and postfix interpretations, while parameter names are ignored.
For example:

```val
type Foo {
  // A non-mutating prefix operator.
  fun + () -> Foo { ... }

  // A mutating infix operator.
  mut fun = (other: Foo) -> Unit { ... }
}
```
