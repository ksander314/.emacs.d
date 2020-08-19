(require-package 'rust-mode)
(require 'rust-mode)
(require-package 'lsp-mode)
(require 'lsp-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c c") 'rust-run)
(define-key rust-mode-map (kbd "C-c l") 'rust-run-clippy)

(require-package 'toml-mode)
(require 'toml-mode)
(add-hook 'rust-mode-hook
          (lambda () (lsp)))
(require-package 'cargo)
(require 'cargo)
(add-hook 'rust-mode-hook
          (lambda () (cargo-minor-mode)))

(require-package 'flycheck-rust)
(require 'flycheck-rust)
(add-hook 'rust-mode-hook
          (lambda () (flycheck-mode-hook)))
(require-package 'lsp-ui)
(require 'lsp-ui)
(require-package 'lsp-treemacs)
(require 'lsp-treemacs)
(add-hook 'rust-mode-hook #'lsp)
(provide 'init-rust)
