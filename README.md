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

## Usage

Dump GHC Core programs using `ghc-dump-core`, e.g.:

```bash
# ./demos contains a few files I use for testing
# $GHC points to the version of GHC you want to use
$GHC -fplugin GhcDump.Plugin demos/Maths.hs
```

Compare two passes or two completely different files:

```bash
corediff demos/Maths.pass-0000.cbor demos/Maths.pass-0019.cbor
```

[1]: https://hackage.haskell.org/package/ghc-dump-core
[2]: https://github.com/pbrinkmeier/ghc-dump
