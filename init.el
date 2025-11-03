(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'init-package)
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

(desktop-save-mode t)

(require 'server)
(unless (server-running-p)
  (server-start))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq user-mail-address "olexander314@gmail.com")
(setq user-login-name "Alexander Stepanenko")

(defalias 'list-buffers 'ibuffer)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq grep-command "grep -IHrn -e \"\\([^[:alnum:]_]\\|^\\)\\([^[:alnum:]_]\\|$\\)\"")
(add-hook 'grep-mode-hook 'hl-line-mode)

(custom-set-variables
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-pylint-executable "python3")
 '(flycheck-python-pycompile-executable "python3"))
(add-hook 'python-mode-hook 'flycheck-mode)
(setq gud-pdb-command-name "python -m pdb")

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(windmove-default-keybindings 'meta)

(require-package 'yaml-mode)
(require-package 'json-mode)
(add-to-list 'image-types 'svg)

(use-package exec-path-from-shell
  :ensure t
  :config
  ;; This will pull the PATH (and other variables, if needed) from your shell.
  (exec-path-from-shell-initialize))

;; https://github.com/copilot-emacs/copilot.el
(add-to-list 'load-path "~/src/copilot.el")
(require 'copilot)
(defun my-copilot-setup ()
  (setq copilot-idle-delay nil)
  (local-set-key (kbd "C-c a") 'copilot-accept-completion)
  (local-set-key (kbd "C-c C-e") 'copilot-complete)
  )
(add-hook 'copilot-mode-hook 'my-copilot-setup)

(setq user-full-name
      (string-trim (shell-command-to-string "git config --global user.name")))

(setq user-mail-address
      (string-trim (shell-command-to-string "git config --global user.email")))

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

(require-package 'jira)
(use-package jira
  :config
  (setq jira-base-url "https://tradingview-air.atlassian.net")
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3))
