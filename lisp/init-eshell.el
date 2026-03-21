;;; init-eshell.el --- Eshell configuration -*- lexical-binding: t -*-
(use-package eshell-syntax-highlighting
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(defface my/eshell-prompt-dir-face
  '((t :foreground "#0a64f5" :weight bold))
  "Face for directory in eshell prompt.")

(defface my/eshell-prompt-tail-face
  '((t :foreground "#ad071a" :weight bold))
  "Face for tail of eshell prompt.")

(defun my/eshell-current-dir-name ()
  "Return the basename of `eshell/pwd` (handles ~ and / correctly)."
  (let ((p (abbreviate-file-name (eshell/pwd))))
    (file-name-nondirectory (directory-file-name p))))

(defun my/eshell-prompt ()
  "Prompt: [basename] $ "
  (let* ((dir (propertize (my/eshell-current-dir-name) 'face 'my/eshell-prompt-dir-face))
         (tail (propertize "➜" 'face 'my/eshell-prompt-tail-face)))
    (concat "[" dir "] " tail " ")))

(setq eshell-prompt-function #'my/eshell-prompt)
(setq eshell-prompt-regexp "^\\[.*\\] ➜ ")
(setq eshell-highlight-prompt nil)

(add-hook 'eshell-mode-hook
          (lambda () (local-set-key (kbd "C-c C-r") #'consult-history)))

(setq-default eshell-history-size 99999)

(defun my/eshell-append-to-history ()
  "Append the most recent command from eshell history to the history file."
  (when (and eshell-history-file-name (ring-p eshell-history-ring))
    (let* ((last-command (ring-ref eshell-history-ring 0))
           (command-with-newline (concat last-command "\n")))
      (append-to-file command-with-newline nil eshell-history-file-name))))

(with-eval-after-load 'eshell
  (remove-hook 'eshell-post-command-hook #'eshell-save-history)
  (add-hook 'eshell-post-command-hook #'my/eshell-append-to-history))

(provide 'init-eshell)
