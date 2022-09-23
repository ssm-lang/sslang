# Contributing

## Table of Contents

<!-- vim-markdown-toc GFM -->

* [Contributing Guidelines](#contributing-guidelines)
  * [Rules](#rules)
  * [Recommended Git Workflow](#recommended-git-workflow)
* [Development Environment Setup](#development-environment-setup)
  * [Build Toolchain Setup](#build-toolchain-setup)
  * [Development Tools Setup (optional)](#development-tools-setup-optional)
  * [Git Alias Setup (optional)](#git-alias-setup-optional)
* [Developing `sslc`](#developing-sslc)
  * [Building sslc](#building-sslc)
  * [Running sslc](#running-sslc)
  * [Building Code Documentation](#building-code-documentation)
  * [Adding Test Suites](#adding-test-suites)
  * [Adding Regression Tests](#adding-regression-tests)
  * [Adding New Unit Tests](#adding-new-unit-tests)
  * [Linting and Formatting](#linting-and-formatting)

<!-- vim-markdown-toc -->

## Contributing Guidelines

These guidelines are here to ensure that we maintain a clean, linear Git
history. All contributions should be made in the form of pull requests (PRs).

For quick reference about Git commands and concepts, see John's
[notes on using Git][j-hui-yagg].

[j-hui-yagg]: https://j-hui.com/pages/yagg

### Rules

- Collaborators all have write access, so be mindful of what others are working
  on.

  - Each PR should have at least one approving review before being merged in,
    preferably from someone also responsible for the code that the PR affects
    (though not someone who has also worked on the PR). This repo is configured
    to require one approving review, though anyone may submit a review.

  - The author of the PR should be the one to merge in the PR, unless there they
    explicitly instruct someone else to do it.

  - Do not push directly to `main`. This branch is already configured with push
    protections to prevent this.

- Write sensible, descriptive commit messages.

  - Commit messages on `main` should follow [this guide][git-style]: summarize
    in 50 characters or less, in the imperative tense, but elaborate in the
    message body if necessary. For instance, write "Use name mangling to fix
    name clash" instead of "used name mangling, fixes name clash".

  - Quick and dirty commit messages are fine in PRs, but make sure they are
    squashed by the time they are merged into `main` or anyone else's branch.
    If you squash and merge, GitHub's PR interface should give you an
    opportunity to edit the squashed commit message.

- Keep the `main` commit history clean.

  - Always make sure to look at that your commits don't touch files

  - If your PR contains a sequence of such commits, make sure to "Squash and
    Merge", and provide a good commit message to summarize all the changes made.

  - If you want to land multiple _clean_ commits onto `main`, make sure to clean
    them up locally, then use "Rebase and Merge".

  - Each commit should point to a working tree that is in a sensible state
    (i.e., builds, works (partially), no dead code etc.).

- Follow the recommended code style.

  - For Haskell code, refer to [Kowainik's style guide][kowainik-style],
    format using [Brittany][brittany], and lint using [hlint][hlint].

  - For C code, refer to [Majerle's style guide][majerle-style], format using
    [clang-format][clang-format], and lint using [clang-tidy][clang-tidy].

[git-style]: https://commit.style/
[kowainik-style]: https://kowainik.github.io/posts/2019-02-06-style-guide
[brittany]: https://github.com/lspitzner/brittany
[hlint]: https://hackage.haskell.org/package/hlint
[majerle-style]: https://github.com/MaJerle/c-code-style
[clang-format]: https://clang.llvm.org/docs/ClangFormat.html
[clang-tidy]: https://clang.llvm.org/extra/clang-tidy/

### Recommended Git Workflow

If you've never made a PR before, here's a basic workflow to follow:

1. Start with the `main` branch (or whatever you intend to PR into later), and
   make sure it is up to date: `git checkout main && git pull`.

2. Create a new feature branch: `git checkout -b <branch-name>`.

3. Push your new branch to GitHub: `git push -u origin <branch-name>`. This also
   sets your upstream branch to `origin/<branch-name>`.

4. Optionally, create a draft PR so others can follow your work.

5. Make changes, add them, commit them. If you notice new commits to `main` that
   might conflict, merge them in. Or rebase, but you may have to force push.

6. Create a PR (if you haven't already), and request a review. There should be
   a menu to select reviewers in the top right of the PR page, with some
   suggestions.

7. If changes are requested, make those changes, and re-request a review. Try to
   resolve all conversations.

8. After receiving approval, "Squash and Merge". You may need to first resolve
   any merge conflicts with `main`.

Here's a diagram to help you illustrate where everything should take place:

```
                                                (4) Create Draft PR to main
                                                (6) Create PR to main
                                                (7) Respond to reviews

      main <--------- (8) Squash and Merge --------- my-branch
        |                                               |
        |                                               |
        |                                               |    GitHub.com (origin)
--------|-----------------------------------------------|-----------------------
        |                                               |             local repo
        |                                               |
  (1) git checkout main && git pull            (3) git push -u my-branch
        |                                      (5) git push
        |                                               |
        |                                               |
        v                                               |
      main ----- (2) git checkout -b my-branch ----> my-branch

                                                 (5) git add ...
                                                     git commit ...
```

## Development Environment Setup

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
[haddock]: https://www.haskell.org/haddock/doc/html/index.html
[hls]: https://haskell-language-server.readthedocs.io/en/latest/
[hlint]: https://hackage.haskell.org/package/hlint
[brittany]: https://hackage.haskell.org/package/brittany
[lsp]: https://langserver.org/

### Build Toolchain Setup

You can easily setup most of sslang's project dependencies using [GHCup][ghcup]<sup>[1](#why-ghcup)</sup>, Haskell's toolchain manager. To do so, run the following command:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
  sh
```

This will run a short but interactive script that installs GHC, Cabal, and Stack; make sure to let it know where it should add the `PATH` variable. GHCup will also ask if you would like to install HLS, which you may use to extend your LSP-compatible editor with IDE features (optional).

The GHCup setup script may also detect that dependencies are missing, and ask you to install them; make sure to do so before proceeding. For instance, if you are running Ubuntu 20.10:

```shell
sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
```

Package names differ depending on your distro and version, so make sure to read the suggestion.

<a name="why-ghcup">Footnote 1</a>: _While you can also directly install Stack and use that to manage GHC versions, GHCup is more specialized toward coordinating versioning for just the core components of the toolchain, i.e., GHC, Cabal, Stack, and HLS. You can read more about its rationale [here](https://www.haskell.org/ghcup/about/#faq)._

[ghcup]: https://www.haskell.org/ghcup/

### Development Tools Setup (optional)

If you are helping develop sslc, you may find it helpful to have [HLint][hlint] and [Brittany][brittany] available. You can install these with Stack:

```shell
stack install hlint
stack install brittany
```

### Git Alias Setup (optional)

Convenience scripts are provided under the [`scripts`](./scripts/) subdirectory, to help lint, format, and build this respoitory's code. As long as your current working directory is within this repo, you may invoke these scripts directly.

These scripts may be added as Git aliases for even easier access (e.g., to lint your code, just run `git lint`). They are defined in [`.gitconfig`](./.gitconfig), and can be set up by running the following command inside of this repo:

```shell
git config --local include.path ../.gitconfig
```

Though these convenience aliases are optional, they help outline a recommended command-line workflow that you may wish to follow.

[convenience-aliases]: #git-alias-setup-optional

## Developing `sslc`

### Building sslc

You can build sslc by running:

```shell
stack build
```

By default, Stack does not link in the test driver, and needs to recompile everything if you later decide to run tests. To work around this behavior, you can ask Stack to link in tests without running them:

```shell
stack build --test --no-run-tests
git build # equivalent convenience alias
```

You may also start a build server to continuously watch your filesystem and build as soon as it detects changes, eliminating the need to later run `build` manually:

```shell
git build --test --no-run-tests --file-watch
git watch # equivalent convenience alias
```

You can also continuously build code documentation, though that takes considerably longer, and is not recommended unless you are actively working on documentation:

```shell
git build --test --no-run-tests --file-watch --haddock
git watch --haddock # equivalent convenience alias
```

### Running sslc

You may run sslc using:

```shell
stack exec sslc <args..>
```

You can optionally install `sslc` to `~/.local/bin/`, so that you can invoke it directly (as long as `~/.local/bin/` is in your `PATH`):

```shell
stack install
sslc <args..>
```

All existing tests should be passing before merging a PR, and where appropriate, new tests should be added to demonstrate functionality and correctness of your code. Tests may be run using:

```shell
stack test
```

### Building Code Documentation

Code documentation for this compiler is generated using [Haddock][haddock]. You can build the documentation by running:

```shell
stack haddock
```

This generates the documentation in `haddock-out/`, in the form of a navigable static website (similar to what is found on [Hackage](https://hackage.haskell.org/)). You may view the HTML files in there using your browser.

The langauge reference manual is maintained separately in the [`doc/`](doc) folder; see build instructions there.

### Adding Test Suites

All existing tests should be passing before merging a PR, and where appropriate, new tests should be added to demonstrate functionality and correctness of your code. Tests may be run using:

```shell
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

```shell
stack test sslang:scanner-test
```

By convention, the `main` module of tests for Haskell projects is typically named `Spec.hs`.

### Adding Regression Tests

Test with, e.g.,

````shell
cd regression-tests && ./runtests.sh
````

TODO: Stephen to write more about `runtests.sh`.

### Adding New Unit Tests

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

```shell
stack test sslang:scanner-test --ta '--match "/Tests.ScanComments/"'
```

Or to run only a specific test case:

```shell
stack test sslang:scanner-test --ta '--match "/Tests.ScanComments/ignores single-line comments/"'
```

[hspec]: http://hspec.github.io/
[hspec-discover]: http://hspec.github.io/hspec-discover.html

### Linting and Formatting

To keep code on the main branch clean and consistent, you should always make sure to lint (with [Hlint][hlint]) and format (with [Brittany][brittany]) your code before merging any PR. You may invoke Hlint and Brittany manually, but the following [convenience aliases][convenience-aliases] are provided to coordinate with your development workflow.

To lint:

```shell
git lint                  # lint all files modified since HEAD
git lint [<files..>]      # lint specified files
git lint --since <commit> # lint all files modified since <commit>
git lint --help           # show help menu
```

To format:

```shell
git fmt                   # format all files modified since HEAD
git fmt [<files..>]       # format specified files
git fmt --since <commit>  # format all files modified since <commit>
git fmt --help            # show help menu
```
