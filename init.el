(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'init-package)
(require 'init-claude)
(require 'init-go)
(require 'init-utils)
(require 'init-flymake)
(require 'init-c++)
(require 'init-haskell)
(require 'init-locale)
(require 'init-theme)
(require 'init-org)
(require 'init-git)
(require 'init-rust)
(require 'init-erl)
(require 'init-elisp)
(require 'init-gptel)
(require 'init-font)
(require 'init-display)
(require 'init-eshell)
(require 'init-python)
(require 'init-copilot)

(desktop-save-mode t)

(require 'server)
(unless (server-running-p)
  (server-start))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-full-name
      (string-trim (shell-command-to-string "git config --global user.name")))
(setq user-mail-address
      (string-trim (shell-command-to-string "git config --global user.email")))

(defalias 'list-buffers 'ibuffer)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq grep-command "grep -IHrn -e \"\\([^[:alnum:]_]\\|^\\)\\([^[:alnum:]_]\\|$\\)\"")
(add-hook 'grep-mode-hook 'hl-line-mode)

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq dired-use-ls-dired nil)

(windmove-default-keybindings 'meta)

(require-package 'yaml-mode)
(require-package 'json-mode)
(add-to-list 'image-types 'svg)

(require-package 'projectile)
(require-package 'lsp-mode)
(require-package 'lsp-ui)

(with-eval-after-load 'lsp-mode
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-enable-snippet nil)
  (define-key lsp-mode-map (kbd "C-c C-g") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c C-i") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c C-t") 'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c C-f") 'pop-tag-mark))

(when (string= user-mail-address "astepanenko@tradingview.com")
  (add-to-list 'load-path "~/src/jira.el")
  (require 'jira)
  (use-package jira
    :config
    (setq jira-base-url "https://tradingview-air.atlassian.net")
    (setq jira-token-is-personal-access-token nil)
    (setq jira-api-version 3))

  (add-hook 'jira-issues-mode-hook 'hl-line-mode)

  (setq jira-issues-table-fields '(:key :issue-type-name :status-name :assignee-name :summary)))

(require-package 'multiple-cursors)
(global-set-key (kbd "C-c C-*") 'mc/mark-all-in-region-regexp)

(let ((f (expand-file-name "~/src/carp/lisp/agent.el")))
  (when (file-exists-p f) (load-file f)))
