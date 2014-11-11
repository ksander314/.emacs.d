(require-package 'git-gutter)
(global-git-gutter-mode +1)

(require-package 'magit)
(global-set-key "\C-cgg" 'magit-status)

(require-package 'git-messenger)

(provide 'init-git)
