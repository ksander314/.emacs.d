.emacs.d
========

My emacs configuration.

## Package management

Packages are installed by [straight.el](https://github.com/radian-software/straight.el).
Every package is pinned to a git SHA in `straight/versions/default.el`,
which is checked into this repo. straight.el reads that lockfile only to
clone a missing repo and in `M-x straight-thaw-versions`; at startup it
builds whatever each repo under `straight/repos/` has checked out. So an
upstream change reaches Emacs once a repo is moved and Emacs restarts.

## Upgrading packages

`bin/straight-update`, by hand, about once a month. A package runs with
your rights — `~/.authinfo`, ssh-agent, kubectl — so new code is read
before Emacs builds or loads it. Never `M-x straight-pull-all` in the
running Emacs: it moves the repos, and the next restart runs code nobody
has read.

1. **Check:**

   ```
   bin/straight-update check
   ```

   Fetches every repo without checking anything out, so no package code
   runs. Picks, per package, the newest commit older than 7 days and
   writes those SHAs into `straight/versions/default.el`: the plan is the
   lockfile's diff. `check 14` waits longer, `check 0` takes everything.
   The 7 days leave others time to notice a compromised commit; a commit
   date can be forged, so they are a filter, not a guarantee.

2. **Review** what `check` printed: per package, the commits taken and
   every added line that spawns a process, opens a URL, evals, reads
   credentials or the environment, or writes and deletes files. A changed
   `Package-Requires` line means a new or newer dependency. Scrutinise by
   privilege — a theme needs no reading; `agent-shell`, `gptel`, `magit`
   and `straight.el` deserve real attention. To leave a package out, drop
   its line from the plan:

   ```
   git checkout -p straight/versions/default.el
   ```

3. **Apply:**

   ```
   bin/straight-update apply
   ```

   Loads `init.el` in `emacs --batch`, moves each repo to its SHA in the
   lockfile, loads it again (straight rebuilds what moved) and prints only
   what got worse: load errors, packages that fail to `require`, new
   byte-compile warnings in this config, a repo cloned without a lockfile
   entry. Ends with a commit message.

4. **Restart Emacs** right away: until then a package loaded for the
   first time comes from the new build, the rest from the old one.

   A mass rebuild produces a burst of `⛔ Warning (native-compiler)`
   popups. Expected, not a regression — native-comp cannot see through
   `(require ...)` inside a function body or an `fboundp` guard. A real
   breakage looks different: a module fails to load, a command is
   undefined, an error lands in `*Messages*`.

5. **Commit** the lockfile with the message `apply` printed. If something
   broke instead:

   ```
   bin/straight-update rollback
   ```

   Restores the committed lockfile and moves the repos back to it.
   Restart. After the bump is committed: `git revert` it, then
   `bin/straight-update apply`.

On another machine, after `git pull`: `bin/straight-update apply`.

## What the lockfile does and doesn't do

It freezes time. It does **not** validate code.

- It DOES keep upstream changes, a compromised commit or a MELPA recipe
  change included, away from your Emacs until you move a repo.
- It does NOT validate code. A malicious commit older than 7 days when
  you run `check` lands in the plan; the review at step 2 is what stops
  it.
- It does NOT help if the very first install was already compromised; the
  lockfile pins whatever you first froze.
- It does NOT cover tree-sitter grammars: `init.el` installs a missing
  one from its repo's branch head and compiles it.

The defense is the review at step 2, not the lockfile itself.

## Other useful commands

- `M-x straight-remove-unused-repos` — prune `straight/repos/` after
  removing `use-package` forms from the config. Refreeze afterwards.
- `M-x straight-rebuild-all` — force rebuild of byte-compiled `.elc` if
  they seem stale. `straight-rebuild-package` for just one.
- `M-x straight-pull-package` updates one package, but its new code runs
  at the next restart unread. To update one package, run `check` and drop
  the other lines from the plan.

## Adding and removing packages

Adding: write `(use-package foo)` in the relevant `lisp/init-*.el` module
and restart — `straight-use-package-by-default` handles the clone. The
clone is upstream's head and runs unread, so add only packages you trust.
For a dependency of a manually cloned external under `~/src/`, call
`(straight-use-package 'dep)` *before* the `(require ...)`.

Then `M-x straight-freeze-versions` and commit the lockfile. It first
tries to push local changes and then asks `The following packages were
not pushed: … Really write lockfiles?` — answer **yes**. Not an error:
straight wants every pinned SHA to exist upstream, and these are other
people's repos with nowhere to push.

Never `M-x package-install`. It bypasses the lockfile entirely and taints
the session with package.el.
