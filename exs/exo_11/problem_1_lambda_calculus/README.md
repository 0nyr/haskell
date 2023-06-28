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

RedEx = Reducible Expression

### (a) (λf . f (x +1)) (λx . 2·x )

> In Markdown, there is no underline. Need to use HTML.

> Q? I'm not sure here if the new x is the same as the old x... ? For now, I consider it is...

<p> <u>(λf . f (x +1)) (λx . 2·x )</u> (whole expression, normal order operation </p>

<p> →β f (x +1) [f := (λx . 2·x )] </p>

<p>= <u>(λx . 2·x )(x + 1)</u></p>

<p>→β 2·x [x := (x + 1) ]</p>

<p>= <u>2·(x + 1)</u></p>

<p>→δ 2·x + 2</p></p>

### (λy. (λx . x ·y) (y + 1)) x

<p><u>(λy. (λx . x·y) (y + 1)) x</u></p>

<p> →β (λx . x·y) (y + 1) [y := x] </p>

<p> = <u>(λx . x·x) (x + 1)</u> </p>

<p> →β x·x [x := (x + 1)] </p>

<p> = <u>(x + 1)·(x + 1)</u> </p>

<p> →δ x·x + 2·x + 1 </p>

### (λf . f f ) (λx . (λy. x )) (λz . z )

<p> <u>(λf . f f ) (λx . (λy. x )) (λz . z )</u></p>

<p> →β f f [f := (λx . (λy. x )) (λz . z )] </p>

<p> = <u>(λx . (λy. x )) (λz . z )(λx . (λy. x )) (λz . z )</u> </p>

<p> →β λy. x [x := (λz . z )(λx . (λy. x )) (λz . z )] </p>

<p> = <u>λy. ((λz . z )(λx . (λy. x )) (λz . z ))</u></p>

I'm not sure here if I should continue. But it seems to me that the strict application of normal order operation cannot be performed further since there is no more input.






