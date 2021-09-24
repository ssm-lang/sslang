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

  - Follow [this guide][git-style]: summarize in 50 characters or less (but
    elaborate in the message body), in the imperative tense.

  - Quick and dirty commit messages are fine in PRs, but make sure they are
    squashed by the time they are merged into `main` or anyone else's branch.

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

## Recommended workflow

1. Start with the `main` branch (or whatever you intend to PR into later), and
   make sure it is up to date: `git checkout main && git pull`.

2. Create a new feature branch: `git checkout -b <branch-name>`.

3. Push your new branch to GitHub: `git push -u origin <branch-name>`. This also
   sets your upstream branch to `origin/<branch-name>`.

4. Optionally, create a draft PR so others can follow your work.

5. Make changes, add them, commit them. If you notice new commits to `main` that
   might conflict, merge them in. Or rebase, but you may have to force push.

6. Request a review. There should be a menu to select reviewers in the top right
   of the PR page, with some suggestions.

7. If changes are requested, make those changes, and re-request a review. Try to
   resolve all conversations.

8. After receiving approval, "Squash and Merge". You may need to first resolve
   any merge conflicts with `main`.
