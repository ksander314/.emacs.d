;;; init-erl.el --- Erlang configuration -*- lexical-binding: t -*-
(use-package erlang :ensure t :defer t)

(defun my/erl-setup ()
  (condition-case err (eglot-ensure)
    (error (message "eglot-ensure failed in erlang: %s" err)))
  (subword-mode)
  (hl-line-mode)
  (setq-local whitespace-line-column 100)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(add-hook 'erlang-mode-hook #'my/erl-setup)

(provide 'init-erl)
