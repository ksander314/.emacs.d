(require-package 'rust-mode)
(require-package 'cargo)
(require-package 'toml-mode)
(require-package 'flycheck-rust)

(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-rust-analyzer-server-display-inlay-hints t)

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-p") 'projectile-commander)
  (define-key rust-mode-map (kbd "C-c c") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c r") 'rust-run)
  (define-key rust-mode-map (kbd "C-c l") 'rust-run-clippy)
  (define-key rust-mode-map (kbd "C-c t") 'cargo-process-current-test))

(defun my-rust-mode-setup ()
  (lsp-deferred)
  (setq lsp-ui-doc-enable nil)
  (projectile-mode 1)
  (cargo-minor-mode 1)
  (hl-line-mode 1)
  (setq rust-format-on-save t)
  (setq indent-tabs-mode nil))

(add-hook 'rust-mode-hook 'my-rust-mode-setup)

(provide 'init-rust)
