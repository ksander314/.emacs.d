;;; init-copilot.el --- Copilot configuration -*- lexical-binding: t -*-
;; https://github.com/copilot-emacs/copilot.el
(let ((copilot-dir (expand-file-name "~/src/copilot.el")))
  (when (file-exists-p copilot-dir)
    (dolist (dep '(f editorconfig track-changes))
      (unless (package-installed-p dep)
        (package-install dep)))
    (add-to-list 'load-path copilot-dir)
    (require 'copilot)
    (defun my/copilot-setup ()
      (setq copilot-idle-delay nil)
      (local-set-key (kbd "C-c a") 'copilot-accept-completion)
      (local-set-key (kbd "C-c C-e") 'copilot-complete))
    (add-hook 'copilot-mode-hook 'my/copilot-setup)))

(provide 'init-copilot)
