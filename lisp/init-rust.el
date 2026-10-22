;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-
(declare-function cargo-minor-mode "cargo")
(declare-function eglot-format-buffer "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(declare-function eglot-managed-p "eglot")

(use-package rust-mode
  :ensure t :defer t)

(use-package cargo :ensure t :defer t)

(defun my/rust-before-save ()
  "Format the buffer through eglot before saving, when eglot manages it."
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))))

(defun my/rust-setup ()
  "Set up a Rust buffer: eglot, inlay hints, cargo, editing modes, keys."
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
