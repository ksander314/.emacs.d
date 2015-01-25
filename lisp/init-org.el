(require-package 'gnuplot)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (gnuplot . t)))

(provide 'init-org)
