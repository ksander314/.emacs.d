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
(require 'init-ruby)
(require 'init-erl)
(require 'init-elisp)

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

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(windmove-default-keybindings 'meta)

(require-package 'yaml-mode)
(require-package 'json-mode)
