(setq-default c-basic-offset 4)

(defun my/c++-setup ()
  (eglot-ensure)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'block-open 0)
  (c-set-offset 'brace-list-open 0)
  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(dolist (hook '(c++-mode-hook c++-ts-mode-hook))
  (add-hook hook #'my/c++-setup)
  (add-hook hook 'subword-mode)
  (add-hook hook 'which-function-mode)
  (add-hook hook 'hl-line-mode)
  (add-hook hook 'hs-minor-mode))

(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c-ts-mode-hook 'hs-minor-mode)

(provide 'init-c++)
