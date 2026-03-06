(add-hook 'python-mode-hook 'flycheck-mode)
(setq gud-pdb-command-name "python -m pdb")

(provide 'init-python)
