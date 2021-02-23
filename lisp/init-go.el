(require-package 'go-mode)
(require-package 'dap-mode)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'company)
(require-package 'company-lsp)
(defun my-go-mode-hook ()
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c C-g") 'godef-jump)
  (local-set-key (kbd "C-c C-d") 'godef-describe)
  (local-set-key (kbd "C-c C-r") 'lsp-find-references)
  (local-set-key (kbd "C-c C-i") 'lsp-find-implementation)
  (local-set-key (kbd "C-c C-f") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-s") 'dap-debug)
  (local-set-key (kbd "C-c C-b") 'dap-breakpoint-toggle)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )
(add-hook 'go-mode-hook 'lsp-deferred)
(setq lsp-gopls-staticcheck t)
(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)
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
(require 'whitespace)
(setq whitespace-line-column 80)
(add-hook 'go-mode-hook 'lsp-deferred)
(require 'dap-go)
(provide 'init-go)
