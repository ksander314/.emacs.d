(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun my/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun my/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))

(defun my/ffap-with-line ()
  "Like `ffap' but handles file:line format."
  (interactive)
  (let ((thing (ffap-string-at-point)))
    (if (and thing (string-match "\\([^:]+\\):\\([0-9]+\\)" thing))
        (let ((file (match-string 1 thing))
              (line (string-to-number (match-string 2 thing))))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line)))
      (ffap))))

(global-set-key (kbd "C-c f") 'my/ffap-with-line)

(provide 'init-utils)
