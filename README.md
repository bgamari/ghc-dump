# corediff

A tool for diffing GHC Core programs.

## Prerequisites

Install `ghc-dump-core`.
This package enables you to export a consistent Core representation across a variety of GHC versions.
The [Hackage version][1] supports up to GHC 8.6.5.
If you need a more recent version, I've [forked it for support up to GHC 8.11][2] (works as of 2020-05-16).

```bash
# Either:
cabal v1-install ghc-dump-core
# Or, dirty but bleeding edge:
git clone https://github.com/pbrinkmeier/ghc-dump
cd ghc-dump
cabal v1-install -w ${ghc up to 8.11} --allow-newer
```

Install this package in the same way (there is no entry on Hackage yet):

```bash
git clone https://github.com/pbrinkmeier/corediff
cabal v1-install -w ${ghc up to 8.11} --allow-newer
```

## Tests

There are no automatic unit or integration tests yet.
I'm planning to write some.
Until then, you can use the commands described in "Usage" to see if the program works.

## Usage

Dump GHC Core programs using `ghc-dump-core`, e.g.:

```bash
# ./demos contains a few files I use for testing
# $GHC points to the version of GHC you want to use
$GHC -fplugin GhcDump.Plugin -O2 demos/Maths.hs
```

Compare two passes or two completely different files in the top-level binding `square`:

```bash
corediff square demos/Maths.pass-0000.cbor demos/Maths.pass-0019.cbor
```

## Challenges

### Generic programming

### A variety of nested types

A Core expression always has the type `Expr' bndr var` (in `ghc-dump-core`; it's just `Expr` in GHC).
Unlike in section 2 of [An efficient algorithm for type-safe structural diffing][3] (AEATSD), `Expr'`s do not only contain nested `Expr'`s, but also `Type'`s and `Alt'`s.
In order to implement the algorithm as shown in the paper, we need to define a "change" type for each of those nested types.
This means that we need keep track of several different types of subtrees when searching two expressions for values that appear in both of them:
Common sub-`Expr'`s, sub-`Alt'`s, etc.

The paper doesn't need to solve this problem since it uses a generics library to work on types.

[1]: https://hackage.haskell.org/package/ghc-dump-core
[2]: https://github.com/pbrinkmeier/ghc-dump
[3]: https://dl.acm.org/doi/10.1145/3341717
