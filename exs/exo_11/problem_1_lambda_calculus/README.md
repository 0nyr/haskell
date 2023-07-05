# Lambda calculus

An application (λx . E ) A is evaluated (i.e., a β-reduction is performed) by substituting A for x in E : (λx . E ) A →β E [x := A]. Evaluate the following λ-expressions as far as possible (including the evaluation of arithmetic expressions). Underline the redex of each β-reduction and state the substitution explicitly (i.e., give the intermediate step E [x := A] before carrying out the substitution).
(a) (λf . f (x +1)) (λx . 2·x )
(b) (λy. (λx . x ·y) (y + 1)) x
(c) (λf . f f ) (λx . (λy. x )) (λz . z )

##### Recalls

β-Reduction: (λx.E) A →β E [x := A]

* formal specification of function application

δ-Reduction: Ex.: 5*5 →δ 25

* application of a predeﬁned function (not part of the minimal λ-calculus)

RedEx = Reducible Expression (of the form `(λx. ...) (input)`)

##### name collision in substitution

Lambda calculus substitution is the process of replacing a variable in a lambda expression with another expression. This is a fundamental operation in the lambda calculus, as it is how function application is modeled.

Here's the gist of it:

Consider a simple lambda expression: `λx. x`, this represents an identity function, i.e., it returns whatever we give it.

If we substitute `x` with `y` (perform the operation `(λx. x)[x := y]`), we'll get `y`, as expected.

However, in the lambda calculus, we have to be careful when we perform substitution inside more complex lambda expressions. Let's take a look at a slightly more complex example: `λx. (λy. x y)`. If we want to substitute `x` with `y`, we can't simply replace `x` with `y`, because `y` is bound inside the inner lambda abstraction (`λy. x y`), and this would result in a clash, giving us `λy. (λy. y y)` which is not what we intended.

To avoid this, we must rename the `y` inside the inner abstraction before we perform the substitution, giving us something like: `λz. (λy. z y)`, where we've replaced the `y` in the outer scope with `z`.

### (a) (λf . f (x +1)) (λx . 2·x )

> In Markdown, there is no underline. Need to use HTML.

<p> <u>(λf . f (x +1)) (λx . 2·x )</u> (whole expression, normal order operation) </p>

<p> →β f (x +1) [f := (λx . 2·x )] </p>

<p>= <u>(λx . 2·x )(x + 1)</u> (Redex can still be reduced)</p>

<p>→β 2·x [x := (x + 1) ]</p>

<p>= <u>2·(x + 1)</u></p>

<p>→δ 2·x + 2</p></p>

### (λy. (λx . x ·y) (y + 1)) x

> WARN: The following is wrong ! 

<p><u>(λy. (λx . x·y) (y + 1)) x</u> (we could also have choosen "(λy. <u>(λx . x·y) (y + 1)</u>)) x" but we have prefered to use normal order operation (leftmost-outermost)</p>

<p> →β (λx . x·y) (y + 1) [y := x] </p>

<p> = <u>(λx . x·x) (x + 1)</u> THIS IS WRONG, NAME COLLISION, x is free inside replacement would be confused with bound variable inside expression </p>

<p> →β x·x [x := (x + 1)] </p>

<p> = <u>(x + 1)·(x + 1)</u> </p>

<p> →δ x·x + 2·x + 1 </p>

---

Correction: outermost

<p><u>(λy. (λx . x·y) (y + 1)) x</u></p>

<p> →β (λx . x·y) (y + 1) [y := x] </p>

<p> =α ((λz. z·y) (y + 1) [y := x] </p>

<p> = <u>((λz. z·x) (x + 1)</u></p>

<p> →β (z·x) [z := x + 1] </p>

<p> = (x + 1)·x

---

Correction: innermost

<p>(λy. <u>(λx . x·y) (y + 1)</u>)) x (we use inner-most operation order</p>

<p> →β (λy. (x·y)[x := y + 1]) x </p>

<p> = <u>(λy. (y+1)·y) x</u> </p>

<p> →β (y+1)·y [y:=x] </p>

<p> = (x+1)·x </p>

<p> →δ x·x + 2·x + 1 </p>

### (λf . f f ) (λx . (λy. x )) (λz . z )

> WARN: The following is wrong !

<p> <u>(λf . f f ) (λx . (λy. x )) (λz . z )</u></p>

<p> →β f f [f := (λx . (λy. x )) (λz . z )] </p>

<p> = <u>(λx . (λy. x )) (λz . z )(λx . (λy. x )) (λz . z )</u> </p>

<p> →β λy. x [x := (λz . z )(λx . (λy. x )) (λz . z )] </p>

<p> = <u>λy. ((λz . z )(λx . (λy. x )) (λz . z ))</u></p>

I'm not sure here if I should continue. But it seems to me that the strict application of normal order operation cannot be performed further since there is no more input.

##### questions

> Q? I'm not sure here if the new x is the same as the old x... ? For now, I consider it is...

**Answer:** λ-calculus with nameless (or de Bruijn) terms avoids any name collision issues.

In the context of Haskell or other functional languages that follow lexical scoping rules, the `x` in the expression `(λf . f (x +1)) (λx . 2·x )` would be two different `x`s. The `x` in `f (x + 1)` would be a free variable and won't be replaced when we substitute `f` with `(λx . 2·x )`. I followed this interpretation for the first problem.

