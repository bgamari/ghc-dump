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

# including top-level binders in the de-bruijn index

isnt as easy as it sounds. consider:

```
y = 42
x = 2 * y
```

```
x = 2 * 42
```

solution: when comparing binder B, create a top-level index of every top-level binder except B.
put them in a list, fill the shorter one up to the longer one, start indexing the bindings to compare from there.
this will probably when comparing the whole module.
another (hacky) solution would be to convert variables that refer to global binders to VarGlobals

> welp, ghc-dump-core does that last thing anyways *shrug*
> that may be kinda shitty, we'll see

# nested function calls

stuff like `GHC.* (Numinstance) Float a b` often gets converted into stuff like `GHC.timesFloat a b`.
The diffs look pretty horrible because no diffs are summarized yet.
See Maths2/cube from 0001 to 0002 for an example.
