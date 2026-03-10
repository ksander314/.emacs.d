(use-package vterm :demand t)
(use-package claude-code :demand t)

(defun my/claude-code-new (name)
  "Start a named Claude Code session in current project."
  (interactive "sSession name: ")
  (let* ((project-root (claude-code-normalize-project-root
                        (if-let ((proj (project-current)))
                            (project-root proj)
                          default-directory)))
         (default-directory project-root)
         (buffer-name (format "*claude:%s:%s*" project-root name))
         (buf (get-buffer-create buffer-name))
         (vterm-shell claude-code-executable))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-vterm-mode)
        (claude-code-vterm-mode)))
    (switch-to-buffer-other-window buffer-name)))

;; Send Shift+Enter via CSI u encoding, bypassing libvterm's key handling.
(defvar vterm--process)
(defun my/claude-code-send-newline ()
  "Send Shift+Enter to Claude Code to insert a newline."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda ()
     (process-send-string vterm--process "\e[13;2u"))))

(define-key claude-code-vterm-mode-map (kbd "C-c C-j") #'my/claude-code-send-newline)

;; Fix display: remove fringes/margins
(add-hook 'claude-code-vterm-mode-hook
          (lambda ()
            (set-window-fringes (selected-window) 0 0)
            (set-window-margins (selected-window) 0 0)))

;; Reduce terminal width by 2 in claude-code buffers
(defun my/vterm-shrink-width-for-claude (orig-fn process windows)
  (let ((result (funcall orig-fn process windows)))
    (when result
      (if (eq (buffer-local-value 'major-mode (process-buffer process))
              'claude-code-vterm-mode)
          (cons (- (car result) 2) (cdr result))
        result))))

(advice-add 'vterm--window-adjust-process-window-size :around
            #'my/vterm-shrink-width-for-claude)

;; Fix pixel-level line jumping from braille spinner chars
(set-fontset-font t '(#x2500 . #x259F) (face-attribute 'default :family))
(set-fontset-font t '(#x2800 . #x28FF) (face-attribute 'default :family))

(provide 'init-claude)
