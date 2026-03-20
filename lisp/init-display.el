(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq-default show-trailing-whitespace t)
(column-number-mode t)

(defun disable-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode -1))

(defun disable-trailing-whitespace ()
  "Disable trailing whitespace highlighting in the current buffer."
  (setq show-trailing-whitespace nil))

(dolist (hook '(term-mode-hook gptel-mode-hook agent-shell-mode-hook))
  (add-hook hook 'disable-line-numbers)
  (add-hook hook 'disable-trailing-whitespace))

(add-hook 'eshell-mode-hook 'disable-trailing-whitespace)

(provide 'init-display)
