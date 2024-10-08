(require-package 'go-mode)
(require-package 'dap-mode)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'gotest)
(require-package 'projectile)
(require-package 'flycheck-golangci-lint)
(defun my-go-mode-hook ()
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
  (local-set-key (kbd "C-c c") 'projectile-compile-project)
  (local-set-key (kbd "C-c t") 'go-test-current-test)
  (local-set-key (kbd "C-c C-g") 'lsp-find-definition)
  (local-set-key (kbd "C-c C-t") 'lsp-find-type-definition)
  (local-set-key (kbd "C-c C-d") 'lsp-describe-thing-at-point)
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
  (setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t
      lsp-enable-snippet nil)
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup)

        ;; Add buffer local Flycheck checkers after LSP for different major modes.
    (defvar-local my-flycheck-local-cache nil)
    (defun my-flycheck-local-checker-get (fn checker property)
      ;; Only check the buffer local cache for the LSP checker, otherwise we get
      ;; infinite loops.
      (if (eq checker 'lsp)
          (or (alist-get property my-flycheck-local-cache)
              (funcall fn checker property))
        (funcall fn checker property)))
    (advice-add 'flycheck-checker-get
                :around 'my-flycheck-local-checker-get)
    (add-hook 'lsp-managed-mode-hook
              (lambda ()
                (when (derived-mode-p 'go-mode)
                  (setq my-flycheck-local-cache '((next-checkers . (golangci-lint)))))))
    )
  (setq flycheck-golangci-lint-config "~/.golangci.yml")
  )
(add-hook 'go-mode-hook 'lsp-deferred)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook 'lsp-ui-mode)
;;(add-hook 'go-mode-hook (lambda () (auto-complete-mode -1)))
(add-hook 'go-mode-hook 'hl-line-mode)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'projectile-mode)
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))
(add-hook 'go-mode-hook 'whitespace-mode)
(require 'dap-dlv-go)
(add-hook 'go-mode-hook (lambda () (setq show-trailing-whitespace t)))
(provide 'init-go)
