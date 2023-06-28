# Strictness of foldl

### a) foldl testing

Evaluating foldl on + for different possible ranges on my machine gives the following results:

```haskell
ghci> foldl (+) 0 [1..65000]
2112532500
ghci> foldl (+) 0 [1..650000]
211250325000
ghci> foldl (+) 0 [1..6500000]
21125003250000
ghci> foldl (+) 0 [1..65000000]
*** Exception: stack overflow
```

We indeed have a memory problem here. It lies in Haskell's non-strict (lazy) evaluation strategy. Since `foldl` is tail recursive, it doesn't immediately evaluate the accumulated value at each step. Instead, it builds up a large chain of unevaluated thunks. Each step of `foldl (+) x (y:ys)` creates an unevaluated thunk `x + y`, leading to a thunk structure like this for `[1..3]`:

```haskell
(((0 + 1) + 2) + 3)
```

These thunks consume stack space. Only when the very last value is demanded (at the end of the computation) does Haskell actually go ahead and perform the addition operations. At this point, the entire thunk chain must be loaded into memory, and if it's too large (as in your example), you get a stack overflow.

### b) solving the issue using a strict function

In the case of a strict function like `+`, we can use the strict version of `foldl`, called `foldl'` (in `Data.List`), which forces the evaluation of the accumulator at each step, preventing the buildup of thunks:

```haskell
import Data.List (foldl')
foldl' (+) 0 [1..65000000]
```

This successfully compute the sum without a stack overflow, because `foldl'` doesn't build up thunks—it evaluates the sum at each step. We get:

```haskell
ghci> import Data.List (foldl')
ghci> foldl' (+) 0 [1..65000000]
2112500032500000
```

### c)
