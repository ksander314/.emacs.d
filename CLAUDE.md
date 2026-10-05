# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal Emacs configuration. `init.el` is the entry point; it loads feature modules from `lisp/`.

## Applying changes

To test a change without restarting Emacs: `M-x eval-buffer` (current file) or `M-x load-file` on a specific module. To byte-compile a file for error checking: `M-x byte-compile-file`.

## Architecture

`init.el` handles global settings and the core package stack, then `require`s each module:

```
Module             Purpose
──────────────────────────────────────────────────────────────────────────────
init-utils         File utilities (my/delete-this-file, my/rename-this-file-and-buffer, my/browse-current-file),
                   C-c f opens file:line refs, my/straight-pending-updates previews package upgrades
init-go            Go: eglot + gopls (staticcheck), auto-format/organize-imports on save, my/go-debug-test for dape
init-rust          Rust: rust-mode + eglot + rust-analyzer (clippy), auto-format on save
init-zig           Zig: zig-mode + eglot (zls, if installed), auto-format on save
init-c++           C++: eglot + clangd (--header-insertion=never)
init-org           Org agenda (d=dashboard, u=unplanned, i=in-progress, r=review open items), capture (t=task,
                   j=jira, p=project), clock automation, project association (my/org-set-project), meeting notes
                   (C-c m), archive with :Project: prompt (C-c X), pomodoro (C-c P), weekly review (C-c w),
                   standup, code review journal (C-c R), decision log (C-c D), 1-on-1 notes (C-c 1), incident
                   log with auto-resolve (C-c !), timesheet (C-c T), energy tracker (C-c E)
init-git           magit (C-c g g), diff-hl
init-gptel         gptel with OpenAI + Gemini backends; system prompt tuned for Go backend engineering
init-agent-shell   agent-shell (C-c A; C-c O for OpenCode) + persistent alert stack; holds a macOS no-sleep
                   assertion (caffeinate) while a turn runs
init-display       Relative line numbers, trailing whitespace highlighting
init-eshell        Custom prompt, per-command history append, C-c C-r for consult-history
init-keystroke-log Records keystrokes to keystroke-log.csv; my/klog-typo-report, my/klog-char-freq-report,
                   my/klog-bigram-speed-report, my/klog-chord-freq-report, my/klog-mode-distribution-report,
                   my/klog-layout-distribution-report for analysis
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
                   machine in ~/.authinfo, so no address is in git. C-c j: my/jira-dashboard, two lists — assigned
                   to me, waiting for my review (Reviewers = customfield_10093) — leaf issues only, ordered by
                   board column (my/jira-board-columns, hand-kept) then priority, with a time bar and, for
                   reviews, a count of returns. a (in both, in C-c J and in an issue card): my/jira-agent-shell,
                   a fresh Claude agent-shell in ~/src/backend-dashboard (C-u: pick the directory) with the
                   issue's key, summary and link waiting in the prompt; one shell per issue, a second a returns
                   to it. C-c J: jira.el's team list; its filter is a transient saved
                   value in transient/values.el (gitignored), set from the menu: l, C-x C-s.
                   my/jira-issues-clear-loading-flag clears jira.el's stuck in-flight flag on every refresh
```

## Key conventions

- All modules use `;;; filename.el --- Description -*- lexical-binding: t -*-` header and end with `(provide 'module-name)`.
- Custom functions are prefixed `my/`. Interactive commands use `(interactive)`.
- Language setup functions follow the pattern `my/LANG-setup`, added via `add-hook` to both classic and `-ts-` mode hooks (e.g., `go-mode-hook` and `go-ts-mode-hook`).
- LSP is via built-in **eglot** (not lsp-mode). Eglot keybindings are set globally in `init.el`: `C-c C-g` (definitions), `C-c C-r` (references), `C-c C-a` (code actions), `C-c C-n` (rename).
- Completion stack: **vertico** (minibuffer) + **orderless** (matching) + **marginalia** (annotations) + **consult** (search/navigation) + **corfu** (in-buffer).
- Tree-sitter via **treesit-auto** (`treesit-auto-install t`); grammars for go/c/cpp/rust/zig/python/yaml/toml/json/bash are ensured via idle timer on startup.
- Auth credentials read from `~/.authinfo` via `auth-source`.
- External packages loaded conditionally from `~/src/`: carp/lisp/agent.el, eshboard (on `C-c k`).
- Input method: `cyrillic-dvorak-programming` (defined in `lisp/cyrillic-dvorak-programming.el`), with **reverse-im** so shortcuts work regardless of active input method.
