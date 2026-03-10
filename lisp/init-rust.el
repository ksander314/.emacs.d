(use-package rust-mode
  :defer t
  :config (setq rust-format-on-save t))

(use-package cargo :defer t)
(use-package toml-mode :defer t)

(defun my/rust-setup ()
  (eglot-ensure)
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

(provide 'init-rust)
