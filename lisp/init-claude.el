(use-package vterm :demand t)
(use-package claude-code :demand t)

;; Send Shift+Enter via CSI u encoding, bypassing libvterm's key handling.
(defvar vterm--process)
(defun my/claude-code-send-newline ()
  "Send Shift+Enter to Claude Code to insert a newline."
  (interactive)
  (when (and (eq major-mode 'claude-code-vterm-mode) vterm--process)
    (process-send-string vterm--process "\e[13;2u")))

(define-key claude-code-vterm-mode-map (kbd "C-c C-j") #'my/claude-code-send-newline)


(provide 'init-claude)
