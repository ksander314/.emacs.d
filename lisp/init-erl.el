(require-package 'erlang)
(require-package 'projectile)
(defun my-erl-mode-hook ()
  (local-set-key (kbd "C-c C-p") 'projectile-commander)
  )
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))
(add-hook 'erlang-mode-hook 'whitespace-mode)
(add-hook 'erlang-mode-hook 'hl-line-mode)
(add-hook 'erlang-mode-hook 'subword-mode)
(add-hook 'erlang-mode-hook 'my-erl-mode-hook)
(add-hook 'erlang-mode-hook 'projectile-mode)
(add-hook 'erlang-mode-hook (lambda () (setq show-trailing-whitespace t)))
(provide 'init-erl)
