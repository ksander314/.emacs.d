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
   privilege — a theme needs no reading; `agent-shell`, `acp.el`, `magit`
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

A new package runs with your rights just like an upgrade, so it gets
the same 7 days and the same batch load before the running Emacs sees
it:

1. **Pick a commit.** `M-x straight-get-recipe` copies the recipe
   straight will use into the kill ring; the clone URL follows from
   `:fetcher` and `:repo`, or is `:url`. Take the newest first-parent
   commit older than 7 days, as `check` does. A blobless bare clone
   fetches the history and no files; if the recipe names a `:branch`,
   log that one:

   ```
   git clone --bare --filter=blob:none https://codeberg.org/geiser/guile.git /tmp/guile.git
   git -C /tmp/guile.git log --first-parent --format='%H %cs %s' -n 10
   ```

2. **Pin it and declare it.** Add `("guile" . "<sha>")` to
   `straight/versions/default.el`, keeping the alphabetical order. The
   key is the local repo, usually the last part of `:repo`, not the
   package name: `geiser-guile` lives in `"guile"`. straight clones a
   missing repo at its lockfile SHA, so this is the commit that lands.
   Then write `(use-package foo :defer t)` in the relevant
   `lisp/init-*.el` module; a new module also needs its `require` in
   `init.el`.

3. **Load it in batch**, from the repo root. straight clones and builds
   the new repos; the running Emacs stays untouched:

   ```
   emacs -Q --batch -l bin/straight-smoke.el < /dev/null 2>&1 | grep -E '^smoke: |: (Warning|Error): |^Warning \('
   ```

   Read the lines that name the new package or its module.
   `bin/straight-update apply` cannot do this step: it only moves repos
   that already exist.

4. **Pin what it pulled in.** Its dependencies were cloned at head,
   unread. This lists every repo with no lockfile line:

   ```
   for d in straight/repos/*/; do r=$(basename "$d"); grep -qF "(\"$r\" ." straight/versions/default.el || echo "$r"; done
   ```

   If Emacs ships a dependency in a new enough version — see
   `M-: (alist-get 'project package--builtin-versions)` — add it to
   `straight-built-in-pseudo-packages` in `init.el`; otherwise its clone
   shadows the built-in library everywhere. geiser needed `project`,
   which pulled `xref`. Any other dependency gets pinned as in steps 1
   and 2. Either way delete `straight/repos/<repo>` and
   `straight/build/<package>`, then repeat steps 3 and 4 until the list
   is empty.

5. **Restart Emacs** and commit the lockfile with the module.

For a dependency of a manually cloned external under `~/src/`, call
`(straight-use-package 'dep)` *before* the `(require ...)`.

Removing: delete the `use-package` form, run
`M-x straight-remove-unused-repos`, then `M-x straight-freeze-versions`
and commit the lockfile. Freeze first tries to push local changes and
then asks `The following packages were not pushed: … Really write
lockfiles?` — answer **yes**. Not an error: straight wants every pinned
SHA to exist upstream, and these are other people's repos with nowhere
to push.

Never `M-x package-install`. It bypasses the lockfile entirely and taints
the session with package.el.

## Checking Emacs Lisp

Flymake byte-compiles and checkdocs every file in `lisp/` while it is
open (`init.el` gets checkdoc only). Every module is clean, and

```
bin/check-elisp
```

keeps it so: it runs the same checks on `lisp/`, `bin/` and `init.el`,
each file in its own Emacs so one module's load cannot hide another's
missing `require`, prints `FILE:LINE: MESSAGE` and exits 1 on any
finding. As a pre-push hook it checks the tip of each branch being
pushed, only when the push changes a `.el` file, and stops the push on
any finding. Once per clone:

```
ln -s ../../bin/check-elisp .git/hooks/pre-push
```

`git push --no-verify` skips it.
