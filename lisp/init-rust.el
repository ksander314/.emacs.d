(require-package 'rust-mode)
(require-package 'projectile)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'cargo)
(require-package 'toml-mode)
(require-package 'flycheck-rust)

(require 'rust-mode)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'cargo)
(require 'flycheck-rust)

;; Define a function to encapsulate all rust-mode hooks and settings
(defun my-rust-mode-setup ()
  "Custom configurations and keybindings for rust-mode."
  ;; Enable LSP mode and set configuration
  (lsp)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-idle-delay 0.7)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  
  ;; Disable LSP UI documentation
  (setq lsp-ui-doc-enable nil)
  
  ;; Enable necessary minor modes
  (projectile-mode 1)
  (cargo-minor-mode 1)
  (hl-line-mode 1)

  ;; Enable formatted code on save
  (setq rust-format-on-save t)

  ;; Set indentation to spaces
  (setq indent-tabs-mode nil)

  ;; Local keybindings for Rust development
  (define-key rust-mode-map (kbd "C-c C-p") 'projectile-commander)
  (define-key rust-mode-map (kbd "C-c c") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c r") 'rust-run)
  (define-key rust-mode-map (kbd "C-c l") 'rust-run-clippy)
  (define-key rust-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key rust-mode-map (kbd "C-c C-i") 'lsp-find-implementation)
  (define-key rust-mode-map (kbd "C-c C-g") 'lsp-find-definition)
  (define-key rust-mode-map (kbd "C-c C-f") 'pop-tag-mark)
  (define-key rust-mode-map (kbd "C-c C-t") 'lsp-find-type-definition)
  (define-key rust-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key rust-mode-map (kbd "C-c t") 'cargo-process-current-test))

;; Add hook to rust-mode
(add-hook 'rust-mode-hook 'my-rust-mode-setup)

(provide 'init-rust)
