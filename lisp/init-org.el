(require-package 'gnuplot)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (gnuplot . t)))

(add-hook 'org-mode-hook (lambda () (setq org-enforce-todo-dependencies t)))

(provide 'init-org)
