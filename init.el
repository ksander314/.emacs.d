(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq use-package-always-ensure t)

;; Auth
(setq auth-sources '("~/.authinfo"))

;; Modules
(require 'init-utils)
(require 'init-go)
(require 'init-c++)
(require 'init-haskell)
(require 'init-org)
(require 'init-git)
(require 'init-rust)
(require 'init-erl)
(require 'init-gptel)
(require 'init-display)
(require 'init-eshell)
(require 'init-python)
(require 'init-copilot)
(require 'init-agent-shell)
(require 'init-keystroke-log)

(desktop-save-mode t)

(require 'server)
(unless (server-running-p)
  (server-start))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defalias 'list-buffers 'ibuffer)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq large-file-warning-threshold nil)
(setq project-vc-ignores '("vendor/" "node_modules/"))
(add-hook 'grep-mode-hook 'hl-line-mode)
(global-set-key (kbd "C-c c") 'compile)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(setq user-full-name
      (with-temp-buffer
        (call-process "git" nil t nil "config" "--global" "user.name")
        (string-trim (buffer-string))))
(setq user-mail-address
      (with-temp-buffer
        (call-process "git" nil t nil "config" "--global" "user.email")
        (string-trim (buffer-string))))

(setq dired-use-ls-dired nil)
(windmove-default-keybindings 'meta)
(add-to-list 'image-types 'svg)

(savehist-mode 1)

;; Completion framework: vertico + orderless + marginalia + consult
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)))

;; Inline completion
(use-package corfu
  :init (global-corfu-mode))

;; Tree-sitter
(setq treesit-font-lock-level 4)
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Flymake (built-in)
(global-set-key (kbd "C-c e") 'flymake-goto-next-error)
(global-set-key (kbd "C-c o") 'flymake-goto-prev-error)

;; Eglot (built-in)
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c C-g") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c C-r") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c C-d") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c C-n") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c C-f") 'xref-go-back)
  (define-key eglot-mode-map (kbd "C-c C-i") 'eglot-find-implementation)
  (push '((c++-mode c++-ts-mode c-mode c-ts-mode)
          "clangd" "--header-insertion=never")
        eglot-server-programs))

(setq eglot-events-buffer-config '(:size 0))

(setq-default eglot-workspace-configuration
              '(:gopls (:staticcheck t)
                :rust-analyzer (:check (:command "clippy"))))

;; Theme & Font
(use-package color-theme-sanityinc-solarized
  :config (load-theme 'sanityinc-solarized-light t))
(set-face-attribute 'default nil :font "Inconsolata" :height 180)

;; Locale
(require 'cyrillic-dvorak-programming)
(setq default-input-method "cyrillic-dvorak-programming")
(use-package reverse-im
  :config
  (reverse-im-activate "cyrillic-dvorak-programming"))

;; Misc
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)
(use-package editorconfig :config (editorconfig-mode 1))
(use-package multiple-cursors
  :bind ("C-c C-*" . mc/mark-all-in-region-regexp))
(use-package dape
  :commands (dape dape-breakpoint-toggle)
  :config
  (setq dape-request-timeout 30))

;; JIRA (conditional)
(when (string= user-mail-address "astepanenko@tradingview.com")
  (add-to-list 'load-path "~/src/jira.el")
  (require 'jira)
  (setq jira-base-url "https://tradingview-air.atlassian.net"
        jira-token-is-personal-access-token nil
        jira-api-version 3)
  (add-hook 'jira-issues-mode-hook 'hl-line-mode)
  (setq jira-issues-table-fields '(:key :issue-type-name :status-name :assignee-name :summary)))

;; External
(let ((f (expand-file-name "~/src/carp/lisp/agent.el")))
  (when (file-exists-p f) (load-file f)))
