(require-package 'git-gutter)
(global-git-gutter-mode +1)
(require-package 'gptel-magit)

(require-package 'magit)
(global-set-key "\C-cgg" 'magit-status)

(require-package 'git-messenger)
(require 'git-messenger)
(global-set-key (kbd "C-c g p") 'git-messenger:popup-message)
(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

(defun gptel-magit-setup ()
  "Setup gptel-magit for Magit."
  (gptel-magit-install))

(add-hook 'magit-mode-hook 'gptel-magit-setup)

(provide 'init-git)
