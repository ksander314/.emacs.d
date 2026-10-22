# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal Emacs configuration. `init.el` is the entry point; it loads feature modules from `lisp/`.

## Applying changes

To test a change without restarting Emacs: `M-x eval-buffer` (current file) or `M-x load-file` on a specific module. To byte-compile a file for error checking: `M-x byte-compile-file`.

Package updates go through `bin/straight-update` (check, review, apply), as README.md describes; never `straight-pull-all` in the running Emacs.

## Architecture

`init.el` handles global settings and the core package stack, then `require`s each module:

```
Module             Purpose
──────────────────────────────────────────────────────────────────────────────
init-utils         File utilities (my/delete-this-file, my/rename-this-file-and-buffer, my/browse-current-file),
                   C-c f opens file:line refs
init-go            Go: eglot + gopls (staticcheck), auto-format/organize-imports on save, my/go-debug-test for dape
init-rust          Rust: rust-mode + eglot + rust-analyzer (clippy), auto-format on save
init-zig           Zig: zig-mode + eglot (zls, if installed), auto-format on save
init-scheme        Guile: geiser + geiser-guile; .scm buffers get geiser-mode (C-c C-z REPL, C-c C-d C-d docs)
init-c++           C++: eglot + clangd (--header-insertion=never)
init-org           Org agenda (d=dashboard, u=unplanned, i=in-progress, r=review open items), capture (t=task,
                   j=jira, p=project), clock automation, project association (my/org-set-project), meeting notes
                   (C-c m), archive with :Project: prompt (C-c X), pomodoro (C-c P), weekly review (C-c w),
                   standup, code review journal (C-c R), decision log (C-c D), 1-on-1 notes (C-c 1), incident
                   log with auto-resolve (C-c !), timesheet (C-c T), energy tracker (C-c E)
init-git           magit (C-c g g), diff-hl
init-agent-shell   agent-shell (C-c A; C-c O for OpenCode) + persistent alert stack; M-RET submits through
                   agent-shell-submit; chat labels show the first name of user-full-name. Sleep during a turn
                   is agent-shell's own agent-shell-inhibit-system-sleep
init-display       Relative line numbers, trailing whitespace highlighting
init-eshell        Custom prompt, per-command history append, C-c C-r for consult-history
init-keystroke-log Records keystrokes to keystroke-log.csv; my/klog-typo-report, my/klog-char-freq-report,
                   my/klog-bigram-speed-report, my/klog-chord-freq-report, my/klog-mode-distribution-report,
                   my/klog-layout-distribution-report for analysis. An input source switch that Hammerspoon
                   pushes (my/klog-set-layout, macOS only) is a row of its own, event "layout"
init-focus-shield  Interruption shield: C-c z saves window layout and point and pauses the clocked task,
                   C-c Z restores them and resumes it; interruptions are logged to ~/src/org/interruptions.org,
                   my/focus-report shows the stats
init-kube          TRAMP method /kube:NAMESPACE.CONTEXT@POD:/path over kubectl exec (bin/kube-tramp);
                   my/kube-find-file picks context, namespace and pod, then a file on it
init-tatr          tatr task tracker over tasks/<HUID>/TASK.md folders. C-c n is the prefix: n new task,
                   t turn the TODO under point into one, f find by HUID, r referers, y copy the HUID.
                   tatr.el itself is vendored from rexim's dotfiles and kept unmodified, so local
                   defaults and keys go in init-tatr.el and a refresh is a plain overwrite
init-jira          Jira over jira.el; the site address and the token both come from the *.atlassian.net
                   machine in ~/.authinfo, so no address is in git; without that machine (home) jira.el is not
                   installed and no key is bound. C-c j: my/jira-dashboard, leaf issues only (no parents
                   of subtasks), each with a time bar. Top list, under the active sprint's name, day and goal
                   (tab line; the issues in Develop or In Progress precede it, and head the list in
                   bold): assigned to me in the current sprint, ordered by whose move it is
                   (my/jira-moves: returned, no reviewer, in work, paused, not started — then the ones
                   waiting for others, longest standing first). Bottom list: waiting for my review (Reviewers = customfield_10093), by
                   board column (my/jira-board-columns, hand-kept) then priority, with a count of returns.
                   In both lists: i starts/stops the org clock on the issue (today's entry under today's
                   heading of work.org, :JIRA: key, INPROCESS/PAUSE, so standup, timesheet and the Focus
                   Shield see it); w sends the time not yet sent (:JIRA_SENT:) as a Jira worklog; r moves the
                   issue to review with that time in the same request (the review transition demands it).
                   a (in both, in C-c J and in an issue card): my/jira-agent-shell,
                   a fresh Claude agent-shell in ~/src/backend-dashboard (C-u: pick the directory) with the
                   issue's key, summary and link waiting in the prompt; one shell per issue, a second a returns
                   to it. C-c J: jira.el's team list; its filter is a transient saved
                   value in transient/values.el (gitignored), set from the menu: l, C-x C-s.
                   my/jira-issues-clear-loading-flag clears jira.el's stuck in-flight flag on every refresh
```

## Key conventions

- All modules use the standard Emacs Lisp file layout: the `;;; filename.el --- Description -*- lexical-binding: t -*-` header, `;;; Commentary:` (the file's notes, may be empty), `;;; Code:`, and at the end `(provide 'module-name)` followed by `;;; filename.el ends here`.
- Flymake (byte-compile and checkdoc) reports nothing in any module; keep it that way. A module declares what it uses: `require` for libraries that are always installed and cheap, `declare-function` and a bare `defvar` for ones that load later. A declaration does not load anything, so a call to a declared function needs something that loads its library first. `bin/check-elisp` runs the same checks on `lisp/`, `bin/` and `init.el` and is the pre-push hook (`ln -s ../../bin/check-elisp .git/hooks/pre-push`, once per clone); run it before committing elisp.
- Custom functions are prefixed `my/`. Interactive commands use `(interactive)`.
- Language setup functions follow the pattern `my/LANG-setup`, added via `add-hook` to both classic and `-ts-` mode hooks (e.g., `go-mode-hook` and `go-ts-mode-hook`).
- LSP is via built-in **eglot** (not lsp-mode). Eglot keybindings are set globally in `init.el`: `C-c C-g` (definitions), `C-c C-r` (references), `C-c C-a` (code actions), `C-c C-n` (rename).
- Completion stack: **vertico** (minibuffer) + **orderless** (matching) + **marginalia** (annotations) + **consult** (search/navigation) + **corfu** (in-buffer).
- Tree-sitter via **treesit-auto** (`treesit-auto-install nil`, so nothing is installed on demand); an idle timer on startup installs the go/c/cpp/rust/zig/python/yaml/toml/json/bash grammars when they are missing.
- Auth credentials read from `~/.authinfo` via `auth-source`.
- External packages loaded conditionally from `~/src/`: carp/lisp/agent.el, eshboard (on `C-c k`), kbdtr/kbdtr.el (keyboard cheat sheet: `C-c K` finds how to type a char, `C-c L` shows the layers).
- Input method: `cyrillic-dvorak-programming` (defined in `lisp/cyrillic-dvorak-programming.el`), with **reverse-im** so shortcuts work regardless of active input method.
