;;; init.el --- Main init -*- lexical-binding: t -*-
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(let ((archive (expand-file-name "archives/melpa/archive-contents" package-user-dir)))
  (when (or (not (file-exists-p archive))
            (time-less-p (days-to-time 1)
                         (time-since (file-attribute-modification-time
                                      (file-attributes archive)))))
    (package-refresh-contents)))
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
(require 'init-focus-shield)
(require 'init-kube)
(global-set-key (kbd "C-c z") #'my/focus-freeze)
(global-set-key (kbd "C-c Z") #'my/focus-thaw)

;; Org keybindings — global (work from anywhere)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c t") #'my/org-quick-task)
(global-set-key (kbd "C-c u") #'my/org-urgent-task)
(global-set-key (kbd "C-c s") #'my/org-standup)
(global-set-key (kbd "C-c m") #'my/org-meeting)
(global-set-key (kbd "C-c w") #'my/org-weekly-review)
(global-set-key (kbd "C-c R") #'my/org-capture-code-review)
(global-set-key (kbd "C-c D") #'my/org-capture-decision)
(global-set-key (kbd "C-c 1") #'my/org-one-on-one)
(global-set-key (kbd "C-c !") #'my/org-capture-incident)
(global-set-key (kbd "C-c T") #'my/org-timesheet)
(global-set-key (kbd "C-c E") #'my/org-energy-check)

;; Org keybindings — org-mode only (need cursor on a heading)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r") #'my/org-refile-to-today)
  (define-key org-mode-map (kbd "C-c d") #'my/org-done)
  (define-key org-mode-map (kbd "C-c i") #'my/org-start)
  (define-key org-mode-map (kbd "C-c p") #'my/org-pause)
  (define-key org-mode-map (kbd "C-c l") #'my/org-link-task)
  (define-key org-mode-map (kbd "C-c P") #'my/org-pomodoro)
  (define-key org-mode-map (kbd "C-c A") #'my/org-archive-done))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c r") #'my/org-agenda-refile-to-today)
  (define-key org-agenda-mode-map (kbd "C-c d")
    (lambda () (interactive) (org-agenda-todo "DONE")))
  (define-key org-agenda-mode-map (kbd "C-c i")
    (lambda () (interactive) (org-agenda-todo "INPROCESS")))
  (define-key org-agenda-mode-map (kbd "C-c p")
    (lambda () (interactive) (org-agenda-todo "PAUSE"))))

(desktop-save-mode t)
(with-eval-after-load 'eglot
  (add-to-list 'desktop-minor-mode-table '(eglot--managed-mode nil)))

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
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :init (global-corfu-mode))

;; Tree-sitter
(setq treesit-font-lock-level 4)
(use-package treesit-auto
  :config
  (setq treesit-auto-install nil)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  (run-with-idle-timer 2 nil
    (lambda ()
      (dolist (lang '(go c cpp rust python yaml toml json bash))
        (unless (treesit-language-available-p lang)
          (treesit-install-language-grammar lang))))))

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

;; Force font-lock refresh after eglot connects (fixes missing syntax highlight)
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'font-lock-ensure))

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

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)
            (when (display-graphic-p)
              (run-with-idle-timer 1 nil (lambda () (org-agenda nil "d"))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((agent-shell-knockknock :url
			     "https://github.com/xenodium/agent-shell-knockknock")
     (knockknock :url "https://github.com/xenodium/knockknock"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
