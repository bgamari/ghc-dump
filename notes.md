# de bruijn method

de-bruijning two terms will fail in some situations.
I had an example in mind which turned out to be a symptom of the GCP-function.
I guess I'll have to think some more.

# diffing let expressions

using the GCP function from AEATSD isnt great for diffing lets:

```
let
  f = ...
  x = ...
in
  2 * f x

~VS.~

let
  g = ...
  y = ...
  z = 3 * y
in
  2 * g z
```

Diffing these two terms should emphasize the change in the `in`-section; however, diffing lets "naively" doesnt permit this.
We could separate the bindings and the expressions part to make nicer diffs here.
In order to diff this nicely using the nominal unification stuff we need to unify the `in`-section first.
