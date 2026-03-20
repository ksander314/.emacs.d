;;; init-erl.el --- Erlang configuration -*- lexical-binding: t -*-
(use-package erlang :defer t)

(defun my/erl-setup ()
  (eglot-ensure)
  (subword-mode)
  (hl-line-mode)
  (setq-local whitespace-line-column 100)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(add-hook 'erlang-mode-hook #'my/erl-setup)

(provide 'init-erl)
