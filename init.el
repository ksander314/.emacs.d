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

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(require-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(windmove-default-keybindings 'meta)

(require-package 'yaml-mode)
(require-package 'json-mode)
(add-to-list 'image-types 'svg)

(defun org-calculate-totals-for-day ()
  "Calculate total effort and clocked time for the current Org heading."
  (interactive)
  (save-excursion
    (let ((total-effort 0)
          (total-clocked (org-clock-sum-current-item)))
      (org-map-entries
       (lambda ()
         (let ((effort (org-entry-get (point) "Effort")))
           (message "DEBUG: Effort=%s, Clocked=%s" effort total-clocked)
           (message "DEBUG: Total-Effort=%s, Total-Clocked=%s" total-effort total-clocked)
           ;; Add Effort if exists
           (when effort
             (let ((effort-minutes (org-duration-to-minutes effort)))
               (setq total-effort (+ total-effort (truncate effort-minutes)))))
           ;; Add Clocked time if exists
           ))
       nil 'tree)
      ;; Update totals in the main heading
      (org-back-to-heading t)
      (org-set-property "TotalEffort" (org-minutes-to-hh:mm (truncate total-effort)))
      (org-set-property "TotalClocked" (org-minutes-to-hh:mm (truncate total-clocked)))
      (message "Totals updated: Effort=%s, Clocked=%s"
               (org-minutes-to-hh:mm (truncate total-effort))
               (org-minutes-to-hh:mm (truncate total-clocked))))))

(defun org-minutes-to-hh:mm (minutes)
  "Convert minutes to HH:MM format."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))
