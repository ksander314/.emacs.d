;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
(defun my/python-setup ()
  (condition-case err (eglot-ensure)
    (error (message "eglot-ensure failed in python: %s" err))))

(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'my/python-setup))

(setq gud-pdb-command-name "python -m pdb")

(provide 'init-python)
