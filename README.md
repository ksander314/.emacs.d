.emacs.d
========

My emacs configuration.

## Package management

Packages are installed by [straight.el](https://github.com/radian-software/straight.el).
Every package is pinned to a git SHA in `straight/versions/default.el`,
which is checked into this repo. Startup reads the lockfile — upstream
changes have no effect until you explicitly pull and refreeze.

## Upgrading packages

Manual, deliberate process. Don't run on a schedule.

1. **Pull upstream:**

   ```
   M-x straight-pull-all
   ```

   Fetches and fast-forwards every package's local clone.

2. **Preview what moved** (optional, before restart):

   ```
   M-x my/straight-pending-updates
   ```

   Pops up a buffer listing every package whose HEAD diverged from the
   lockfile, with the new commits inlined per package. Useful for
   deciding which packages deserve attention at step 6.

3. **Restart Emacs and use it normally** for a while.

   A mass rebuild produces a burst of `⛔ Warning (native-compiler)`
   popups. Expected, not a regression — native-comp cannot see through
   `(require ...)` inside a function body or an `fboundp` guard. A real
   breakage looks different: a module fails to load, a command is
   undefined, an error lands in `*Messages*`.

4. **If something broke:**

   ```
   M-x straight-thaw-versions
   ```

   Resets every repo to the SHA in the committed lockfile. Restart. You're
   back where you started — nothing to undo.

   This reads the lockfile *on disk*, so it only works while the lockfile
   is still the old one. If you already froze and committed, first
   `git checkout straight/versions/default.el`, then thaw.

5. **If everything works:**

   ```
   M-x straight-freeze-versions
   ```

   Writes the new SHAs to `straight/versions/default.el`.

   It first tries to push local changes and then asks `The following
   packages were not pushed: … Really write lockfiles?` — answer **yes**.
   Not an error: straight wants every pinned SHA to exist upstream, and
   these are other people's repos with nowhere to push.

6. **Review the diff before committing:**

   ```
   git diff straight/versions/default.el
   ```

   For each package that bumped, read what changed upstream:

   ```
   cd straight/repos/<pkg> && git log <OLDSHA>..<NEWSHA>
   ```

   Scrutinise by privilege — a theme update needs no reading; an
   `agent-shell`, `gptel`, `magit`, or `straight.el` update deserves real
   attention.

7. **Commit:**

   ```
   git add straight/versions/default.el
   git commit -m "Bump <packages>"
   ```

## What the lockfile does and doesn't do

It freezes time. It does **not** validate code.

- It DOES block compromised upstream commits or MELPA recipe changes from
  reaching your Emacs at startup — only the pinned SHA loads.
- It does NOT validate code when you run `straight-pull-all`. If a
  malicious commit is upstream when you pull, you'll get it. The lockfile
  just makes the update a discrete event you can review (step 6).
- It does NOT help if the very first install was already compromised; the
  lockfile pins whatever you first froze.

The defense is the review at step 6, not the lockfile itself.

## Other useful commands

- `M-x straight-pull-package` — update a single package by name.
  `straight-pull-package-and-deps` does the same but drags its
  dependencies along, which is usually what you want.
- `M-x straight-remove-unused-repos` — prune `straight/repos/` after
  removing `use-package` forms from the config. Refreeze afterwards.
- `M-x straight-rebuild-all` — force rebuild of byte-compiled `.elc` if
  they seem stale. `straight-rebuild-package` for just one.

## Adding and removing packages

Adding: write `(use-package foo)` in the relevant `lisp/init-*.el` module
and restart — `straight-use-package-by-default` handles the clone. For a
dependency of a manually cloned external under `~/src/`, call
`(straight-use-package 'dep)` *before* the `(require ...)`.

Never `M-x package-install`. It bypasses the lockfile entirely and taints
the session with package.el.
