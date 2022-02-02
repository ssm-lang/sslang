# sslang

![tests](https://github.com/ssm-lang/ssm-runtime/actions/workflows/test.yml/badge.svg?branch=dev)
[![doc](https://github.com/ssm-lang/ssm-runtime/actions/workflows/doc.yml/badge.svg?branch=dev)](https://ssm-lang.github.io/sslang)

A language built atop the Sparse Synchronous Model.

## Quickstart

A thorough guide for building and contributing to this project can be found in [`CONTRIBUTING.md`](CONTRIBUTING.md). Hyperlinks to that document are provided throughout this brief overview to direct the reader to more details.

This compiler is build using Haskell Stack; the development process depends on a few other tools. If you haven't set these up before, see the [setup instructions](CONTRIBUTING.md#development-environment-setup) for details.

[Build sslc](CONTRIBUTING.md#building-sslc) and its [code documentation](CONTRIBUTING.md#building-code-documentation) using:

```
stack build
stack haddock
```

Documentation is built to the `haddock-out/` directory, and may be viewed using your browser.

[Run and test](CONTRIBUTING.md#running-sslc) sslc using:

```
stack exec sslc <args..>
stack test
```
