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

(defun my/zig-setup ()
  (require 'zig-mode)
  (when (executable-find "zls")
    (condition-case err (eglot-ensure)
      (error (message "eglot-ensure failed in zig: %s" err))))
  (hl-line-mode 1)
  (subword-mode 1)
  (setq-local indent-tabs-mode nil)
  (add-hook 'before-save-hook #'my/zig-before-save nil t)
  (local-set-key (kbd "C-c c") 'zig-compile)
  (local-set-key (kbd "C-c r") 'zig-run)
  (local-set-key (kbd "C-c t") 'zig-test-buffer))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'my/zig-eglot-managed))

(dolist (hook '(zig-mode-hook zig-ts-mode-hook))
  (add-hook hook #'my/zig-setup))

(provide 'init-zig)
