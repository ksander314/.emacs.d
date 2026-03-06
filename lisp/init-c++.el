(global-set-key (kbd "C-c c") 'compile)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(defun my-c++-mode-setup ()
  (lsp-deferred)
  (setq lsp-clients-clangd-args '("--header-insertion=never"))

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'block-open 0)
  (c-set-offset 'brace-list-open 0)

  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(add-hook 'c++-mode-hook 'my-c++-mode-setup)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'which-function-mode)
(add-hook 'c++-mode-hook 'hl-line-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)

(provide 'init-c++)
