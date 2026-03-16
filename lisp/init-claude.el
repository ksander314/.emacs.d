(use-package vterm :demand t)

;; Shim: replace projectile functions with project.el equivalents
;; so claude-code works without projectile
(unless (fboundp 'projectile-project-root)
  (defun projectile-project-root (&optional dir)
    (when-let ((proj (project-current nil dir)))
      (project-root proj)))

  (defun projectile-project-files (&optional dir)
    (when-let ((proj (project-current nil dir)))
      (mapcar (lambda (f) (file-relative-name f (project-root proj)))
              (project-files proj))))

  (defun projectile-current-project-files ()
    (projectile-project-files))

  (defun projectile-project-name ()
    (if-let ((root (projectile-project-root)))
        (file-name-nondirectory (directory-file-name root))
      "-"))

  (defun projectile-project-type () 'generic)

  (provide 'projectile))

(use-package claude-code :demand t)

;; Send Shift+Enter via CSI u encoding, bypassing libvterm's key handling.
(defvar vterm--process)
(defun my/claude-code-send-newline ()
  "Send Shift+Enter to Claude Code to insert a newline."
  (interactive)
  (when (and (eq major-mode 'claude-code-vterm-mode) vterm--process)
    (process-send-string vterm--process "\e[13;2u")))

(define-key claude-code-vterm-mode-map (kbd "C-c C-j") #'my/claude-code-send-newline)

(defun my/claude-open-file-at-point ()
  "Open file path at point in Claude Code buffer.
Supports: file.go:42, file.go:42:5, @file.go#L42, @file.go#L42-50"
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (pos (- (point) (line-beginning-position)))
         ;; Find file-like path around point
         (path nil) (lineno nil) (col nil))
    ;; Try to match around cursor position
    (when (string-match
           "\\(?:@\\)?\\([a-zA-Z0-9_./-]+\\.[a-zA-Z]+\\)\\(?:#L\\([0-9]+\\)\\|:\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?"
           line)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        ;; Find the match closest to cursor
        (setq path (match-string 1 line)
              lineno (or (match-string 2 line) (match-string 3 line))
              col (match-string 4 line))
        ;; If cursor isn't near this match, scan for more
        (when (or (> pos (+ match-end 5)) (< pos (- match-start 5)))
          (let ((search-start pos))
            (when (string-match
                   "\\(?:@\\)?\\([a-zA-Z0-9_./-]+\\.[a-zA-Z]+\\)\\(?:#L\\([0-9]+\\)\\|:\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?"
                   line search-start)
              (setq path (match-string 1 line)
                    lineno (or (match-string 2 line) (match-string 3 line))
                    col (match-string 4 line)))))))
    (unless path
      (user-error "No file path found at point"))
    ;; Resolve path relative to project root
    (let* ((project-root (or (when-let ((proj (project-current)))
                               (project-root proj))
                             default-directory))
           (full-path (if (file-name-absolute-p path)
                          path
                        (expand-file-name path project-root))))
      (unless (file-exists-p full-path)
        (user-error "File not found: %s" full-path))
      (find-file-other-window full-path)
      (when lineno
        (goto-char (point-min))
        (forward-line (1- (string-to-number lineno)))
        (when col
          (forward-char (1- (string-to-number col))))))))

(defun my/claude-goto-file-reference ()
  "Find all file references in Claude Code buffer and open selected one."
  (interactive)
  (let ((refs nil)
        (pattern "\\(?:@\\)?\\([a-zA-Z0-9_./-]+\\.[a-zA-Z]+\\)\\(?:#L\\([0-9]+\\)\\|:\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)")
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((path (match-string 1))
               (lineno (or (match-string 2) (match-string 3)))
               (display (if lineno
                            (format "%s:%s" path lineno)
                          path)))
          (unless (or (member display refs)
                      (string-match-p "\\`\\." path)) ; skip dotfiles
            (push display refs)))))
    (unless refs
      (user-error "No file references found"))
    (let* ((selected (completing-read "Open file: " (nreverse refs) nil t))
           (parts (split-string selected ":"))
           (path (car parts))
           (lineno (cadr parts))
           (project-root (or (when-let ((proj (project-current)))
                               (project-root proj))
                             default-directory))
           (full-path (if (file-name-absolute-p path)
                          path
                        (expand-file-name path project-root))))
      ;; If not found at project root, search in project
      (unless (file-exists-p full-path)
        (let* ((basename (file-name-nondirectory path))
               (candidates (directory-files-recursively project-root
                             (concat "\\`" (regexp-quote basename) "\\'"))))
          (setq full-path
                (cond
                 ((null candidates) nil)
                 ((= (length candidates) 1) (car candidates))
                 (t (completing-read "Multiple matches: " candidates nil t))))))
      (unless full-path
        (user-error "File not found: %s" path))
      (find-file-other-window full-path)
      (when lineno
        (goto-char (point-min))
        (forward-line (1- (string-to-number lineno)))))))

(defun my/claude-set-local-keys ()
  "Set claude-code local keybindings."
  (local-set-key (kbd "C-c C-f") #'my/claude-open-file-at-point)
  (local-set-key (kbd "C-c C-o") #'my/claude-goto-file-reference)
  (local-set-key (kbd "C-c C-v") #'my/claude-toggle-copy-mode))

(defun my/claude-toggle-copy-mode ()
  "Toggle vterm-copy-mode with visible cursor."
  (interactive)
  (vterm-copy-mode (if vterm-copy-mode -1 1))
  (setq-local cursor-type (if vterm-copy-mode 'box nil))
  (unless vterm-copy-mode
    (my/claude-set-local-keys)))

;; Fix vterm width: account for fringes that vterm ignores
(defun my/vterm-get-margin-width-with-fringes ()
  "Get margin width including fringes."
  (let ((width 2)
        (max-line-num (+ (frame-height) vterm-max-scrollback)))
    (when (bound-and-true-p display-line-numbers)
      (setq width (+ width 4
                     (string-width (number-to-string max-line-num)))))
    width))

(advice-add 'vterm--get-margin-width :override #'my/vterm-get-margin-width-with-fringes)

(add-hook 'claude-code-vterm-mode-hook #'my/claude-set-local-keys)

(with-eval-after-load 'vterm
  (define-key vterm-copy-mode-map (kbd "C-c C-v") #'my/claude-toggle-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-c C-f") #'my/claude-open-file-at-point)
  (define-key vterm-copy-mode-map (kbd "C-c C-o") #'my/claude-goto-file-reference))

(global-set-key (kbd "C-c l l") 'claude-code-run)
(global-set-key (kbd "C-c l t") 'claude-code-transient)
(global-set-key (kbd "C-c l s") 'claude-code-insert-current-file-path-to-session)
(global-set-key (kbd "C-c l r") 'claude-code-send-region)


(provide 'init-claude)
