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

## Development Environment Setup

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
