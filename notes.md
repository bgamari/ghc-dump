# de bruijn method

de-bruijning two terms will fail in some situations.

Example:

```
Inputs:

\y.(\x.x) (\x.x)
\y.y (\x.x)

De-Bruijn:

\0.(\1.1) (\2.2)
\0.0 (\1.1)
```

These terms should not mark the argument of the function call as different!

TODO: make up modules that exhibit this behavior for tests

## possible solution

- Traverse both terms simultaneously, pair up corresponding binders, translate one of them into the "binder universe" of the other
- diff stuff!
- profit

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

## TODO

- [x] module diffen pairing ausdenken
  - [ ] dont check type equality, check structure-based equality
  - [ ] check term structure if types couldnt be matched
- [x] ghc-dump-util compilen
  - [x] find out what that error means
- [ ] corediff file1:bndr file2:bndr'
- [ ] rausfloaten mal anschauen (expr -> let ... in expr')
- [ ] Constructed Product Result Analysis in GhcDump/Ast unter neuer Version einbauen (eventuell...)
- [x] Coolen erweiterbaren AST einbauen
- [x] Prettyprinten mit Text.PrettyPrint.ANSI. ... (ist es möglich hier iwie `ppr :: Expr -> Reader Opts String` zu produzieren?)
- [x] Alternativen zu De-Bruijn-Indizes testen
  - important and kind of a requirement for a nice pairing algo
- [ ] keinen unterschied zeigen wenn sich bei variablen nur der name geändert hat, hier werden ja die infos auch nich angezeigt
- [x] diffmod Maths/1 und Maths/23 macht Fehler, liegt wahrscheinlich an fehlenden eingebundenen globals. Sollte ohne DB eig nicht auftreten
- [ ] floatIn-Funktion, die ausgewählte Binder aus let-blöcken entfernt und im Body substituiert.
- [ ] diffmod ghc-8.8.3/Flags/0 ghc-8.11.0/Flags/0 zeigt manche binder und typen als unterschiedlich an, woran liegt das?
