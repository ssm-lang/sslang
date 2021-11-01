# sslang

A language built atop the Sparse Synchronous Model.

## Setup

The sslang compiler, sslc, is developed using the [Haskell programming language][haskell], and developed using the following tools:

- [GHC][ghc]: haskell compiler
- [Cabal][cabal]: build system
- [Stack][stack]: project manager
- [Haddock][haddock]: code documentation generator (packaged with Stack)
- [HLS][hls]: [language server][lsp] (optional)
- [Brittany][brittany]: formatter (optional)
- [Hlint][hlint]: linter (optional)

This section will guide you through setting up your development environnment with these tools. We assume that development will take place in a UNIX-like environment (i.e., macOS, WSL, or some kind of Linux distro). Development in Windows is probably possible but unsupported.

[haskell]: https://www.haskell.org/
[ghc]: https://www.haskell.org/ghc/
[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/GUIDE/
[hls]: https://github.com/haskell/haskell-language-server
[hlint]: https://hackage.haskell.org/package/hlint
[brittany]: https://hackage.haskell.org/package/brittany
[lsp]: https://langserver.org/

### Toolchain Setup

You can easily setup most of sslang's project dependencies using [GHCup][ghcup]<sup>[1](#why-ghcup)</sup>, Haskell's toolchain manager. To do so, run the following command:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
  sh
```

This will run a short but interactive script that installs GHC, Cabal, and Stack; make sure to let it know where it should add the `PATH` variable. GHCup will also ask if you would like to install HLS, which you may use to extend your LSP-compatible editor with IDE features (optional).

The GHCup setup script may also detect that dependencies are missing, and ask you to install them; make sure to do so before proceeding. For instance, if you are running Ubuntu 20.10:

```
sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

Package names differ depending on your distro and version, so make sure to read the suggestion.

You can now use Stack to install [HLint][hlint] and [Brittany][brittany] (optional). To do so, run:

```
stack install hlint
stack install brittany
```

<a name="why-ghcup">1</a>: While you can also directly install Stack and use that to manage GHC versions, GHCup is more specialized toward coordinating versioning for just the core components of the toolchain, i.e., GHC, Cabal, Stack, and HLS. You can read more about its rationale [here](https://www.haskell.org/ghcup/about/#faq).

[ghcup]: https://www.haskell.org/ghcup/

### Convenience Git Aliases (optional)

Convenience scripts are provided under the [`scripts`](./scripts/) subdirectory, to help lint, format, and build this respoitory's code. As long as your current working directory is within this repo, you may invoke these scripts directly.

These scripts may be added as Git aliases for even easier access (e.g., to lint your code, just run `git lint`). These aliases are defined in [`.gitconfig`](./.gitconfig), and can be set up by running this command from anywhere within this repo:

```
git config --local include.path ../.gitconfig
```

## Build

### Building sslc

Build the sslang compiler, sslc, with:

````
stack build
````

By default, Stack does not link in the test driver, and needs to recompile everything if you later decide to run tests. To work around this behavior, you can ask Stack to link in tests without running them:

```
stack build --test --no-run-tests
```

If you've set up the convenience Git aliases, then you will be able to use a convenience alias for the above command:

```
git build
```

You may also start a build server to continuously watch your filesystem and build as soon as it detects changes, eliminating the need to later run `build` manually:

```
git build --test --no-run-tests --file-watch
git watch # equivalent
```

You can also continuously build documentation, though that tends to take considerably longer:

```
git build --test --no-run-tests --file-watch --haddock
git watch --haddock # equivalent
```

### Running sslc

You may run sslc using:

```
stack exec sslc <args..>
```

If you would like `sslc` to be available on your `PATH` (i.e., installed in `~/.local/bin/`), you may run:

```
stack install
```

Then you may invoke the executable directly:

```
sslc <args..>
```

### Building Documentation

Build this repo's code documentation using [Haddock][haddock] by running:

```
stack haddock
```

Haddock documentation is placed in `haddock-out/`. You may view this output in your browser, which can be invoked using:

```
stack haddock --open
```

The langauge reference manual (`sslang-lrm.pdf`) can be found in the `doc/` folder. Build it by going there and invoking `make`.

## Testing

All existing tests should be passing before merging a PR, and where appropriate, new tests should be added to demonstrate functionality and correctness of your code. Tests may be run using:

```
stack test
```

New test suites can be declared by adding items to the `tests` section in [`package.yaml`](package.yaml). For instance, the scanner test is declared as:

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

### Linting and Formatting

To keep code on the main branch clean and consistent, you should always make sure to lint (with [Hlint][hlint]) and format (with [Brittany][brittany]) your code before merging any PR. You may invoke Hlint and Brittany manually, but the following Git aliases are provided to help coordinate these tools with your development workflow.

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
