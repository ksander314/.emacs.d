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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-c a" . copilot-accept-completion))
  :ensure t)

(require 'auth-source)
(setq auth-sources '((:type entry :file "~/.authinfo.gpg" :secret t))
