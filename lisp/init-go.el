(require-package 'go-mode)
(require-package 'dap-mode)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'company)
(require-package 'gotest)
(require-package 'projectile)
(require-package 'flycheck-golangci-lint)
(defun my-go-mode-hook ()
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c t") 'go-test-current-test)
  (local-set-key (kbd "C-c C-g") 'lsp-find-definition)
  (local-set-key (kbd "C-c C-t") 'lsp-find-type-definition)
  (local-set-key (kbd "C-c C-d") 'godef-describe)
  (local-set-key (kbd "C-c C-r") 'lsp-find-references)
  (local-set-key (kbd "C-c C-i") 'lsp-find-implementation)
  (local-set-key (kbd "C-c C-f") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-s") 'dap-debug)
  (local-set-key (kbd "C-c C-b") 'dap-breakpoint-toggle)
  (local-set-key (kbd "C-c C-p") 'projectile-commander)
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (setq lsp-file-watch-threshold '10000)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t
      lsp-enable-snippet nil)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
  (setq flycheck-golangci-lint-config "~/.golangci.yml")
  (flycheck-add-next-checker 'lsp 'golangci-lint)
  )
(add-hook 'go-mode-hook 'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook 'lsp-ui-mode)
(add-hook 'go-mode-hook (lambda () (auto-complete-mode -1)))
(add-hook 'go-mode-hook 'hl-line-mode)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'which-function-mode)
(add-hook 'go-mode-hook 'projectile-mode)
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail tabs))
(add-hook 'go-mode-hook 'whitespace-mode)
(add-hook 'go-mode-hook 'lsp-deferred)
(require 'dap-go)
(add-hook 'go-mode-hook (lambda () (setq show-trailing-whitespace t)))
(provide 'init-go)
