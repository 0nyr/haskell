# Lambda calculus in Haskell

### fv and bv

In the context of lambda calculus and functional programming, `fv` (free variables) and `bv` (bound variables) are used to analyze and manipulate lambda expressions, which are the basis of many functional programming languages.

1. **Free Variables (`fv`):** Free variables in a lambda expression are those that are not bound by a lambda abstraction in the expression. For example, in the expression λx.yx (where λx. denotes a function of x), y is a free variable because it's not bound by a lambda abstraction. Free variables are important because they represent the inputs or dependencies that a function or expression needs from its surrounding context or environment.
2. **Bound Variables (`bv`):** Bound variables in a lambda expression are those that are declared within the lambda abstraction. In the example λx.yx, x is a bound variable because it is defined as the parameter of the lambda function. Understanding bound variables is important because changing the name of a bound variable doesn't change the behavior of the function or expression (this is known as alpha equivalence). This feature is used in compiler optimizations and when proving properties of programs.

So the purpose of `fv` and `bv` functions is to provide a mechanism to analyze lambda expressions by extracting their free and bound variables respectively. This analysis can be used in various ways, such as optimizing the expression, checking for variable capture (a situation where a free variable becomes bound unexpectedly), performing substitution of variables, and many other tasks involved in interpreting or compiling functional languages.
