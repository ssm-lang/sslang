@page contributing Contributing

@tableofcontents

# Contributing Guidelines

These guidelines are here to ensure that we maintain a clean, linear Git
history. All contributions should be made in the form of pull requests (PRs).

For quick reference about Git commands and concepts, see John's
[notes on using Git][j-hui-yagg].

[j-hui-yagg]: https://j-hui.com/pages/yagg

## Rules

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

  - For C code, refer to [Majerle's style guide][majerle-style], format using
    [clang-format][clang-format], and lint using [clang-tidy][clang-tidy].

[git-style]: https://commit.style/
[kowainik-style]: https://kowainik.github.io/posts/2019-02-06-style-guide
[brittany]: https://github.com/lspitzner/brittany
[hlint]: https://hackage.haskell.org/package/hlint
[majerle-style]: https://github.com/MaJerle/c-code-style
[clang-format]: https://clang.llvm.org/docs/ClangFormat.html
[clang-tidy]: https://clang.llvm.org/extra/clang-tidy/

## Recommended workflow

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
   any merge conflicts with `main`. (To merge the latest commits from `main`:
   `git pull origin main`; this will allow you to manually resolve conflicts.)

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

# Repository Organization

## Top-level files

There are a few directories at the top-level of this repository:

- `src/` contains the source code for the core SSM runtime library. The library "core" here is platform-agnostic, and must be linked against platform-specific bindings.

- `include/` contains header files for the SSM runtime library.

    - `include/ssm.h` defines the user-facing interface, i.e., what applicaton code includes.

    - `include/ssm-internal.h` defines the internal interface, primarily for communicating with platform-specific bindings.

- `doc/` contains additional documentation to supplement the code documentation, as well as configuration files and assets for Doxygen.

- `examples/` contains example applications built using the SSM runtime library.

- `test/` contains source code (`test/*.c`) and expected output (`test/*.out`) for tests.

- `Makefile` builds the SSM runtime library. Note that this Makefile is only used to build the SSM runtime library for the current machine/platform, primarily for testing purposes. It does not support cross-compilation; platform-specific builds are supported via PlatformIO, which ignores this Makefile. For a list of supported targets, run `make help`.

- `build/` is where the `Makefile` places build artifacts. It should never be checked into version control.

- `runtests.sh` is a test script that builds the examples and tests, and compares them against the expected output.

## Continuous integration

Continuous integration is performed via GitHub Actions; the following workflows are specified in `.github/workflows/`:

- `test.yml` runs the full test suite on every pull request to the `main` branch, and prevents failing tests from being merged to `main`.

- `doc.yml` generates code documentation using Doxygen, and prevents broken documentation from being merged into `main`. When new code is committed to main, the documentation is deployed to the `gh-pages` branch, where it is served to <https://ssm-lang.github.io/ssm-runtime>.

- `cov.yml` generates a code coverage report for tests on each PR to `main`, and describes any changes in coverage (with respect to the base branch).
