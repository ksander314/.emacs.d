;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-
(use-package rust-mode
  :ensure t :defer t)

(use-package cargo :ensure t :defer t)
(use-package toml-mode :ensure t :defer t)

(defun my/rust-before-save ()
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))))

(defun my/rust-setup ()
  (condition-case err (eglot-ensure)
    (error (message "eglot-ensure failed in rust: %s" err)))
  (ignore-errors (eglot-inlay-hints-mode))
  (require 'rust-cargo)
  (cargo-minor-mode 1)
  (hl-line-mode 1)
  (subword-mode 1)
  (setq-local indent-tabs-mode nil)
  (add-hook 'before-save-hook #'my/rust-before-save nil t)
  (local-set-key (kbd "C-c c") 'rust-compile)
  (local-set-key (kbd "C-c r") 'rust-run)
  (local-set-key (kbd "C-c l") 'rust-run-clippy)
  (local-set-key (kbd "C-c t") 'cargo-process-current-test))

(dolist (hook '(rust-mode-hook rust-ts-mode-hook))
  (add-hook hook #'my/rust-setup))

(provide 'init-rust)
