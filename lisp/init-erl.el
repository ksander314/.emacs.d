;;; init-erl.el --- Erlang configuration -*- lexical-binding: t -*-
(use-package erlang :ensure t :defer t)

(defun my/erl-setup ()
  (ignore-errors (eglot-ensure))
  (subword-mode)
  (hl-line-mode)
  (setq-local whitespace-line-column 100)
  (setq-local whitespace-style '(face lines-tail))
  (whitespace-mode 1))

(add-hook 'erlang-mode-hook #'my/erl-setup)

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'erlang-mode)
                (font-lock-ensure)))))

(provide 'init-erl)
