(require 'vterm)
(require-package 'claude-code)
(require 'claude-code)

(defun my/claude-code-new (name)
  "Start a named Claude Code session in current project."
  (interactive "sSession name: ")
  (let* ((project-root (claude-code-normalize-project-root (projectile-project-root)))
         (default-directory project-root)
         (buffer-name (format "*claude:%s:%s*" project-root name))
         (buf (get-buffer-create buffer-name))
         (vterm-shell claude-code-executable))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-vterm-mode)
        (claude-code-vterm-mode)))
    (switch-to-buffer-other-window buffer-name)))

;; Send Shift+Enter via CSI u encoding, bypassing libvterm's key handling.
;; This sends \e[13;2u directly to the process stdin, which Ink interprets
;; as Shift+Enter = insert newline (not submit).
(defvar vterm--process)
(defun my/claude-code-send-newline ()
  "Send Shift+Enter to Claude Code to insert a newline."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda ()
     (process-send-string vterm--process "\e[13;2u"))))

(define-key claude-code-vterm-mode-map (kbd "C-c C-j") #'my/claude-code-send-newline)

;; Fix display: remove fringes/margins (fixes text clipping)
(add-hook 'claude-code-vterm-mode-hook
          (lambda ()
            (set-window-fringes (selected-window) 0 0)
            (set-window-margins (selected-window) 0 0)))

;; Make vterm report 1 fewer column for claude-code buffers.
;; Prevents lines from hitting the terminal width boundary.
(defun my/vterm-shrink-width-for-claude (orig-fn process windows)
  "Reduce terminal width by 1 in claude-code buffers."
  (let ((result (funcall orig-fn process windows)))
    (when result
      (if (eq (buffer-local-value 'major-mode (process-buffer process))
              'claude-code-vterm-mode)
          (cons (- (car result) 2) (cdr result))
        result))))

(advice-add 'vterm--window-adjust-process-window-size :around
            #'my/vterm-shrink-width-for-claude)

;; Fix pixel-level line jumping during Claude Code "thinking" phase.
;; Root cause: braille spinner chars (⠋⠙⠹ etc.) use a fallback font with
;; different ascent/descent than the default font, making the spinner line
;; a few pixels taller/shorter each frame. vterm issue #563.
;; Fix: force braille + box-drawing chars to use the default font.
(set-fontset-font t '(#x2500 . #x259F) (face-attribute 'default :family))  ; box drawing + block elements
(set-fontset-font t '(#x2800 . #x28FF) (face-attribute 'default :family))  ; braille patterns

(provide 'init-claude)
