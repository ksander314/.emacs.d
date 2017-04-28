(defun flymake-cpp-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-pipe" "-std=c++14" "-Wall" "-Wextra" "-Weffc++" "-Wold-style-cast" "-Wsign-promo" "-fsyntax-only" local-file))))
(setq flymake-allowed-file-name-masks
      (cons '(".+\\.[ch]\\(pp\\)?"
              flymake-cpp-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))
(setq flymake-err-line-patterns
      (cons '("\\(.*\\) at \\([^ \n]\\) line \\([0-9]+\\)[,.\n]"
              2 3 nil 1)
            flymake-err-line-patterns))
(add-hook 'c++-mode-hook 'flymake-mode)
(global-set-key (kbd "C-c f") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c c") 'compile)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(defun indentation-c++-mode-hook ()
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'innamespace '0)
  (c-set-offset 'inline-open '0)
  (c-set-offset 'block-open '0)
  (c-set-offset 'brace-list-open '0))
(add-hook 'c++-mode-hook 'indentation-c++-mode-hook)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'which-function-mode)
(add-hook 'c++-mode-hook 'hl-line-mode)
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail tabs))
(add-hook 'c++-mode-hook 'whitespace-mode)
(require-package 'helm-gtags)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(custom-set-variables
 '(helm-gtags-auto-update t))
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "C-c g t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "C-c g r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "C-c g s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "C-c C-g") 'helm-gtags-find-tag-from-here)
     (define-key helm-gtags-mode-map (kbd "C-c g f") 'helm-gtags-find-files)
     (define-key helm-gtags-mode-map (kbd "C-c C-f") 'helm-gtags-previous-history)))
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook (lambda () (setq show-trailing-whitespace t)))
(provide 'init-c++)
