;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
(defun my/python-setup ()
  (ignore-errors (eglot-ensure)))

(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'my/python-setup))

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-ts-mode 'python-mode)
                (font-lock-ensure)))))

(setq gud-pdb-command-name "python -m pdb")

(provide 'init-python)
