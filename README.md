# corediff

Diff GHC Core programs.

## About

CoreDiff improves upon `diff` by assimilating terms before they are compared.
Alpha-equivalent terms don't show up as differences.
This is useful for finding the differences in Core-to-Core transformations that two versions of GHC performed.

## Usage

CoreDiff expects two CBOR files exported by [ghc-dump][1] as its input:
```
$ mkdir ghcX ghcY
$ ghcX -O2 Fac.hs -fplugin GhcDump.Plugin -dumpdir ghcX
$ ghcY -O2 Fac.hs -fplugin GhcDump.Plugin -dumpdir ghcY
corediff diff ghcX/Fac.pass-0000.cbor ghcY/Fac.pass-0000.cbor
...
```

For playing around, there is a bunch of CBOR files in the `demos` directory of this repo.
