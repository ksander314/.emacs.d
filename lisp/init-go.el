(require-package 'go-mode)
(require-package 'dap-mode)
(require-package 'gotest)
(require-package 'flycheck-golangci-lint)

(setq lsp-gopls-staticcheck t)
(setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

(defvar-local my-flycheck-local-cache nil)
(defun my-flycheck-local-checker-get (fn checker property)
  (if (eq checker 'lsp)
      (or (alist-get property my-flycheck-local-cache)
          (funcall fn checker property))
    (funcall fn checker property)))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup)
  (advice-add 'flycheck-checker-get :around 'my-flycheck-local-checker-get)
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'go-mode)
                (setq my-flycheck-local-cache '((next-checkers . (golangci-lint))))))))
(setq flycheck-golangci-lint-config "~/.golangci.yml")

(defun my-go-mode-hook ()
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
  (local-set-key (kbd "C-c c") 'projectile-compile-project)
  (local-set-key (kbd "C-c t") 'go-test-current-test)
  (local-set-key (kbd "C-c C-s") 'dap-debug)
  (local-set-key (kbd "C-c C-b") 'dap-breakpoint-toggle)
  (local-set-key (kbd "C-c C-p") 'projectile-commander)
  (local-set-key (kbd "C-c C-c") 'comment-region)

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook 'lsp-ui-mode)
(add-hook 'go-mode-hook 'hl-line-mode)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'projectile-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local whitespace-line-column 100)
            (setq-local whitespace-style '(face lines-tail))
            (whitespace-mode 1)))

(with-eval-after-load 'dap-mode
  (require 'dap-dlv-go))

(provide 'init-go)
