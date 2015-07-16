(require 'flymake)
(provide 'init-flymake)

(global-set-key (kbd "C-c e") 'flymake-goto-next-error)
(global-set-key (kbd "C-c o") 'flymake-goto-prev-error)
