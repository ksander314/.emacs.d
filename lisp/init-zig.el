;;; init-zig.el --- Zig configuration -*- lexical-binding: t -*-
(use-package zig-mode
  :ensure t :defer t
  :custom
  (zig-format-on-save nil))

(use-package zig-ts-mode
  :ensure t :defer t
  :straight (zig-ts-mode :type git :host codeberg :repo "meow_king/zig-ts-mode"))

;; zig-ts-mode font-lock queries target the tree-sitter-grammars/tree-sitter-zig
;; grammar; override the recipe that treesit-auto registers (maxxnino's fork,
;; which lacks node types like `payload`) so a future idle-timer reinstall does
;; not regress to the incompatible grammar.
(with-eval-after-load 'treesit
  (setf (alist-get 'zig treesit-language-source-alist)
        '("https://github.com/tree-sitter-grammars/tree-sitter-zig")))

(defun my/zig-before-save ()
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))))

(defun my/zig-eglot-managed ()
  (when (and (derived-mode-p 'zig-mode 'zig-ts-mode) (eglot-managed-p))
    (ignore-errors (eglot-inlay-hints-mode))))

;; zig-mode's `zig-run'/`zig-test-buffer' invoke `zig run/test <file>', which
;; compile a single file in isolation. That fails on multi-module projects
;; (e.g. `@import("hello_zig")' is a named module declared only in build.zig,
;; invisible to a lone `zig run'). Drive the build system instead so the module
;; graph is wired up. Run from the build.zig root so error paths resolve, and
;; force a pipe (clean ASCII output -- see `my/zig-compile-via-pipe' below).
(defun my/zig--build (subcmd)
  (let ((default-directory (or (locate-dominating-file
                                (or (buffer-file-name) default-directory)
                                "build.zig")
                               default-directory))
        (process-connection-type nil))
    (compile (concat "zig build " subcmd))))

(defun my/zig-build-run ()
  "Run the project via `zig build run' from the build.zig root."
  (interactive)
  (my/zig--build "run"))

(defun my/zig-build-test ()
  "Test the project via `zig build test' from the build.zig root."
  (interactive)
  (my/zig--build "test"))

(defun my/zig-setup ()
  (require 'zig-mode)
  (when (executable-find "zls")
    (condition-case err (eglot-ensure)
      (error (message "eglot-ensure failed in zig: %s" err))))
  (hl-line-mode 1)
  (subword-mode 1)
  (setq-local indent-tabs-mode nil)
  (add-hook 'before-save-hook #'my/zig-before-save nil t)
  (local-set-key (kbd "C-c c") 'zig-compile)        ; zig build
  (local-set-key (kbd "C-c r") 'my/zig-build-run)   ; zig build run
  (local-set-key (kbd "C-c t") 'my/zig-build-test)) ; zig build test

;; zig-mode registers only `(zig-mode . ("zls"))'; the buffer opens in
;; zig-ts-mode (treesit-auto remap), so teach eglot to launch zls there too.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((zig-ts-mode zig-mode) . ("zls"))))

;; `compile' connects the subprocess through a pty, so `zig build' detects a
;; terminal and renders a live progress bar: synchronized-output escapes
;; (ESC[?2026h), DEC line-drawing charset for the step tree (ESC(0...ESC(B) and
;; ANSI colors, none of which compilation-mode interprets -- the buffer fills
;; with raw escape garbage. Piped, zig emits clean ASCII tree output and no
;; escapes, so force a pipe for the zig build commands.
(defun my/zig-compile-via-pipe (orig &rest args)
  (let ((process-connection-type nil))
    (apply orig args)))
(with-eval-after-load 'zig-mode
  (dolist (fn '(zig-compile zig-run zig-test-buffer))
    (advice-add fn :around #'my/zig-compile-via-pipe)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'my/zig-eglot-managed))

(dolist (hook '(zig-mode-hook zig-ts-mode-hook))
  (add-hook hook #'my/zig-setup))

(provide 'init-zig)
