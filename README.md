# sslang

A language built atop the Sparse Synchronous Model

This depends on the Haskell Stack and the SSM library

See <https://docs.haskellstack.org/en/stable/GUIDE/> for Haskell Stack
documentation

Build the compiler (sslc) with

````
stack build
````


Test with, e.g.,

````
cd regression-tests ; ./runtests.sh
stack test sslang:scanner-test
stack test sslang:parser-test
````

## Development Setup

To setup git aliases:

```
git config --local include.path ../.gitconfig
```

To lint:

```
git lint
```

To format:

```
git fmt
```

Run with `--help` for more details.
