# sslang

A language built atop the Sparse Synchronous Model.

## Build and Development Setup

### Requirements

We assume that development will take place in a UNIX-like environment (i.e., macOS, WSL, or some kind of Linux distro). Development in Windows is probably possible but unsupported.

### Setup Build Toolchain

You can install this compiler's build toolchain using [GHCup][ghcup], Haskell's toolchain manager, with the following command:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
  sh
```

This will run a short but interactive script; make sure to let it know where it should add the `PATH` variable. It will also ask if you would like to install [HLS][hls] (Haskell Language Server); you should do so if you would like to set up IDE features with an LSP-compatible text editor.

The GHCup setup script may also detect that dependencies are missing, and ask you to install them; make sure to do so before proceeding. For instance, if you are running Ubuntu 20.10:

```
sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

Package names differ depending on your distro and version, so make sure to read the suggestion.

The GHCup installation script will also install [GHC][ghc] (Haskell compiler), [Cabal][cabal] (Haskell build system), [Stack][stack] (Haskell project manager), and [HLS][hls] (Haskell language server) automatically. You shouldn't need to use GHCup except to upgrade any of these tools.

[haskell]: https://www.haskell.org/
[ghc]: https://www.haskell.org/ghc/
[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/GUIDE/
[hls]: https://github.com/haskell/haskell-language-server
[ghcup]: https://www.haskell.org/ghcup/

### Linting and Formatting

This project uses [HLint][hlint] for linting and [Brittany][brittany] for formatting. To install these tools, run:

```
stack install hlint
stack install brittany
```

[hlint]: https://hackage.haskell.org/package/hlint
[brittany]: https://hackage.haskell.org/package/brittany

Convenience scripts are provided under the [`scripts`](./scripts/) subdirectory, to help you lint and format your code. As long as your current working directory is within this repo, you may invoke these scripts directly,
or you may set up Git aliases for easier access.

To setup git aliases:

```
git config --local include.path ../.gitconfig
```

To lint:

```
git lint                  # lint all files modified since HEAD
git lint [<files..>]      # lint specified files
git lint --since <commit> # lint all files modified since <commit>
git lint --help           # show help menu
```

To format:

```
git fmt                   # format all files modified since HEAD
git fmt [<files..>]       # format specified files
git fmt --since <commit>  # format all files modified since <commit>
git fmt --help            # show help menu
```

## Building and Testing

Build the compiler (sslc) with:

````
stack build
````

## Documentation

Build and view code documentation with

````
stack haddock --open
````

Haddock documentation is placed in, e.g., /mnt/sedwards/group/projects/bdl/sslang/.stack-work/install/x86_64-linux-tinfo6/b3ccde3c8441eeca22b0a903ee50d8515e74811fa5bed474a049e55591dcb5b4/8.10.7/doc/all/index.html

Build the language reference manual (sslang-lrm.pdf) with

````
cd doc
make
````

## Testing

To run all tests:

```
stack test
```

All tests should be passing before merging a PR.

New tests can be declared by adding items to the `tests` section in [`package.yaml`](package.yaml). For instance, the scanner test is declared as:

```yaml
tests:
  scanner-test:     # Name of test suite
    main: Spec.hs   # Name of test entry point module, in source-dirs
    source-dirs:
    - test/scanner  # Source directories to include in test target
    dependencies:
    - sslang        # Allows us to import modules from sslang
    - hspec         # Used for managing unit tests; see below
  # Other tests...
```

To run an individual test, you may specify `sslang:<test-name>` as a parameter to `stack test`. For instance, to run the scanner test:

```
stack test sslang:scanner-test
```

By convention, the `main` module of tests for Haskell projects is typically named `Spec.hs`.

### Regression Tests

Test with, e.g.,

````
cd regression-tests ; ./runtests.sh
````

TODO: Stephen to write more about `runtests.sh`.

### Unit Tests

Unit tests provide access to Haskell modules in a way that is more direct than specifying an additional `--dump` option to the executable CLI, and more persistent than using the GHCi REPL. Note, however, that unit tests do not supplant regression tests; code should not be considered tested until it is covered by some regression test. Rather, unit tests provide a way to run your code before you fully integrate it with the rest of the compiler. They should be used throughout the development process to directly test and document the isolated behavior of specific compiler components.

This project manages unit tests using [Hspec][hspec], which provides a basic test driver interface and a plugin for [automatic test discovery][hspec-discover]. For example, the `scanner-test` example declares an Hspec test. The directory structure under `test/scanner` might look like this:

```
test/
|_  scanner/
    |_  Spec.hs  # main
    |_  Tests/
        |_  ScanBlockSpec.hs
        |_  ScanCommentSpec.hs
        | # etc.
```

`test/scanner/Spec.hs` consists of only the following, which tells the `hspec-discover` plugin to look for tests in all subdirectories:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Tests must always end in `Spec.hs` to be included by `hspec-discover`. For instance, `ScanCommentSpec.hs` might look like:

```haskell
module Tests.ScanCommentsSpec where
import           Test.Hspec                     (Spec(..), it, shouldBe)

-- Imports from sslang:
import           Front.Scanner                  (scanTokenTypes)
import           Front.Token                    (TokenType(..))

spec :: Spec
spec = do
  it "ignores single-line comments" $ do
    scanTokenTypes "// no"      `shouldBe`  Right []

  it "scans tokens before single-line comments" $ do
    scanTokenTypes "42 // no"   `shouldBe`  Right [TInteger 42]
    scanTokenTypes "24// no"    `shouldBe`  Right [TInteger 24]
```

The entry point of each test is the `spec :: Spec` function, which allows individual test cases to be specified in a monadic context. The `it` combinator should be used to provide a succinct description of the behavior in prose (which is reported by the test driver on success), while `shouldBe` tests that its left operand (actual) is equal to its right operand (expected) using their `Eq` instances. If a test fails, Hspec reports the expected and actual outputs using their `Show` instances, and highlights any differences.

You can also run individual test modules or test cases. For instance, to run only the test cases in `ScanCommentsSpec`, run:

```
stack test sslang:scanner-test --ta '--match "/Tests.ScanComments/"'
```

Or to run only a specific test case:

```
stack test sslang:scanner-test --ta '--match "/Tests.ScanComments/ignores single-line comments/"'
```

[hspec]: http://hspec.github.io/
[hspec-discover]: http://hspec.github.io/hspec-discover.html
