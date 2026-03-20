;;; init-haskell.el --- Haskell configuration -*- lexical-binding: t -*-
(use-package haskell-mode
  :mode "\\.ghci\\'"
  :hook ((haskell-mode . turn-on-haskell-doc-mode)
         (haskell-mode . turn-on-haskell-indent)
         (haskell-mode . subword-mode)
         (haskell-mode . hl-line-mode)
         (inferior-haskell-mode . turn-on-haskell-doc-mode))
  :bind (:map haskell-mode-map
         ("C-c h" . hoogle)
         ("C-c C-c" . haskell-compile)
         ("C-c C-l" . haskell-process-load-file)
         ("C-c C-b" . haskell-interactive-switch)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info))
  :custom
  (haskell-tags-on-save t)
  (haskell-compile-cabal-build-command
   "cd %s && ~/.cabal/bin/cabal build -j8 --ghc-option=-ferror-spans")
  (haskell-process-path-cabal "~/.cabal/bin/cabal"))

(use-package ghci-completion
  :hook (inferior-haskell-mode . turn-on-ghci-completion))

(with-eval-after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*l?hs\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

(defun my/haskell-setup ()
  (eglot-ensure)
  (setq-local whitespace-line-column 80)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(add-hook 'haskell-mode-hook #'my/haskell-setup)

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(provide 'init-haskell)
