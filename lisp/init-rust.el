;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-
(use-package rust-mode
  :ensure t :defer t
  :config (setq rust-format-on-save t))

(use-package cargo :ensure t :defer t)
(use-package toml-mode :ensure t :defer t)

(defun my/rust-setup ()
  (ignore-errors (eglot-ensure))
  (eglot-inlay-hints-mode)
  (cargo-minor-mode 1)
  (hl-line-mode 1)
  (subword-mode 1)
  (setq-local indent-tabs-mode nil)
  (local-set-key (kbd "C-c c") 'rust-compile)
  (local-set-key (kbd "C-c r") 'rust-run)
  (local-set-key (kbd "C-c l") 'rust-run-clippy)
  (local-set-key (kbd "C-c t") 'cargo-process-current-test))

(dolist (hook '(rust-mode-hook rust-ts-mode-hook))
  (add-hook hook #'my/rust-setup))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'rust-ts-mode 'rust-mode)
                (font-lock-ensure)))))

(provide 'init-rust)
