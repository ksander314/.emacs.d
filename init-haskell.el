(require-package 'haskell-mode)

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook (lambda () (subword-mode +1)))

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle))

(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

(require-package 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook #'flymake-haskell-multi-load)

(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*l?hs\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail tabs))
(add-hook 'haskell-mode-hook 'whitespace-mode)

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)))

(setq haskell-compile-cabal-build-command "cd %s && ~/.cabal/bin/cabal build -j8 --ghc-option=-ferror-spans")
(setq haskell-process-path-cabal "~/.cabal/bin/cabal")

(provide 'init-haskell)
