(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'projectile)
(require-package 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(defun my-ruby-mode-hook ()
  (local-set-key (kbd "C-c C-g") 'lsp-find-definition)
  (local-set-key (kbd "C-c C-t") 'lsp-find-type-definition)
  (local-set-key (kbd "C-c C-r") 'lsp-find-references)
  (local-set-key (kbd "C-c C-i") 'lsp-find-implementation)
  (local-set-key (kbd "C-c C-p") 'projectile-commander)
  )
(add-hook 'ruby-mode-hook 'lsp-deferred)
(add-hook 'ruby-mode-hook 'lsp-ui-mode)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'projectile-mode)
(provide 'init-ruby)
