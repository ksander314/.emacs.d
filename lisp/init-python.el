(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(setq gud-pdb-command-name "python -m pdb")

(provide 'init-python)
