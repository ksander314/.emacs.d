(use-package go-mode :defer t)
(use-package gotest :defer t)

(defun my/go-eglot-organize-imports ()
  (when (eglot-managed-p)
    (ignore-errors
      (eglot-code-action-organize-imports (point-min) (point-max)))))

(defun my/go-before-save ()
  (when (eglot-managed-p)
    (ignore-errors (eglot-format-buffer))
    (my/go-eglot-organize-imports)))

(defun my/go-test-name-at-point ()
  "Return the name of the Go test function at or before point."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^func \\(Test[a-zA-Z0-9_]+\\)" nil t)
      (match-string-no-properties 1))))

(defun my/go-debug-test ()
  "Debug the Go test function at point with dape."
  (interactive)
  (let ((test-name (my/go-test-name-at-point))
        (dir (file-name-directory (buffer-file-name))))
    (unless test-name
      (user-error "No test function found at point"))
    (setq-local dape-command
                `(dlv :mode "test"
                      :cwd ,dir
                      :program ,dir
                      :args ["-test.run" ,(concat "^" test-name "$")]))
    (call-interactively 'dape)))

(defun my/go-setup ()
  (eglot-ensure)
  (eglot-inlay-hints-mode)
  (which-function-mode)
  (subword-mode)
  (hl-line-mode)
  (local-set-key (kbd "C-c c") 'project-compile)
  (local-set-key (kbd "C-c t") 'go-test-current-test)
  (local-set-key (kbd "C-c C-s") 'dape)
  (local-set-key (kbd "C-c C-b") 'dape-breakpoint-toggle)
  (local-set-key (kbd "C-c C-t") 'my/go-debug-test)
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (add-hook 'before-save-hook #'my/go-before-save nil t)
  (unless (string-match-p "go" (or compile-command ""))
    (setq-local compile-command "go build -v && go test -v && go vet"))
  (setq-local whitespace-line-column 100)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(dolist (hook '(go-mode-hook go-ts-mode-hook))
  (add-hook hook #'my/go-setup))

(provide 'init-go)
