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
init-utils         File utilities (my/delete-this-file, my/rename-this-file-and-buffer), C-c f opens file:line refs
init-go            Go: eglot + gopls (staticcheck), auto-format/organize-imports on save, my/go-debug-test for dape
init-rust          Rust: rust-mode + eglot + rust-analyzer (clippy), auto-format on save
init-haskell       Haskell: haskell-mode + eglot, cabal build
init-erl           Erlang: erlang-mode + eglot
init-python        Python: eglot only
init-c++           C++: eglot + clangd (--header-insertion=never)
init-org           Org agenda (d=dashboard, u=unplanned, i=in-progress, r=review open items), capture (t=task,
                   j=jira, p=project), clock automation, project association (my/org-set-project), meeting notes
                   (C-c m), archive with :Project: prompt (C-c A), pomodoro (C-c P), weekly review (C-c w),
                   standup, code review journal (C-c R), decision log (C-c D), 1-on-1 notes (C-c 1), incident
                   log with auto-resolve (C-c !), timesheet (C-c T), energy tracker (C-c E), org-modern visuals
init-git           magit (C-c g g), diff-hl, git-messenger, gptel-magit for AI commit messages
init-gptel         gptel with OpenAI + Gemini backends; system prompt tuned for Go backend engineering
init-agent-shell   agent-shell (C-c a) + knockknock notifications
init-copilot       Copilot (loaded only if ~/src/copilot.el exists); C-c C-e to trigger, C-c a to accept
init-display       Relative line numbers, trailing whitespace highlighting
init-eshell        Custom prompt, per-command history append, C-c C-r for consult-history
init-keystroke-log Records keystrokes to keystroke-log.csv; my/klog-typo-report, my/klog-char-freq-report,
                   my/klog-bigram-speed-report for analysis
init-focus-shield  Distraction blocking during focused work
```

## Key conventions

- All modules use `;;; filename.el --- Description -*- lexical-binding: t -*-` header and end with `(provide 'module-name)`.
- Custom functions are prefixed `my/`. Interactive commands use `(interactive)`.
- Language setup functions follow the pattern `my/LANG-setup`, added via `add-hook` to both classic and `-ts-` mode hooks (e.g., `go-mode-hook` and `go-ts-mode-hook`).
- LSP is via built-in **eglot** (not lsp-mode). Eglot keybindings are set globally in `init.el`: `C-c C-g` (definitions), `C-c C-r` (references), `C-c C-a` (code actions), `C-c C-n` (rename).
- Completion stack: **vertico** (minibuffer) + **orderless** (matching) + **marginalia** (annotations) + **consult** (search/navigation) + **corfu** (in-buffer).
- Tree-sitter via **treesit-auto** (`treesit-auto-install t`); grammars for go/c/cpp/rust/python/yaml/toml/json/bash are ensured via idle timer on startup.
- Auth credentials read from `~/.authinfo` via `auth-source`.
- JIRA integration is conditional on `user-mail-address` matching `astepanenko@tradingview.com`.
- External packages loaded conditionally from `~/src/`: copilot.el, jira.el, carp/lisp/agent.el.
- Input method: `cyrillic-dvorak-programming` (defined in `lisp/cyrillic-dvorak-programming.el`), with **reverse-im** so shortcuts work regardless of active input method.
