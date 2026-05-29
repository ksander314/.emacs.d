;;; init-utils.el --- Utility functions -*- lexical-binding: t -*-
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

(defun my/minutes-to-hh:mm (minutes)
  "Convert MINUTES to HH:MM format string."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(defun my/straight-pending-updates ()
  "Show packages whose HEAD differs from the straight.el lockfile.
For each diverged repo, list the new commits not yet frozen.
Run this after `straight-pull-all' and before restart to preview
what would be picked up by `straight-freeze-versions'."
  (interactive)
  (let* ((lock-file (expand-file-name "straight/versions/default.el"
                                      user-emacs-directory))
         (repos-dir (expand-file-name "straight/repos/" user-emacs-directory))
         (locked (with-temp-buffer
                   (insert-file-contents lock-file)
                   (read (current-buffer))))
         (results nil))
    (dolist (entry locked)
      (let* ((pkg (car entry))
             (old-sha (cdr entry))
             (repo (expand-file-name pkg repos-dir)))
        (when (file-directory-p (expand-file-name ".git" repo))
          (let* ((default-directory (file-name-as-directory repo))
                 (head (string-trim
                        (shell-command-to-string "git rev-parse HEAD"))))
            (unless (or (string-empty-p head) (string= head old-sha))
              (push (list pkg old-sha head
                          (shell-command-to-string
                           (format "git log --oneline %s..%s"
                                   (shell-quote-argument old-sha)
                                   (shell-quote-argument head))))
                    results))))))
    (with-current-buffer (get-buffer-create "*straight pending updates*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null results)
            (insert "No pending updates — every repo matches the lockfile.\n")
          (insert (format "%d package(s) ahead of the lockfile:\n\n"
                          (length results)))
          (dolist (r (nreverse results))
            (insert (format "=== %s  %s..%s ===\n%s\n"
                            (nth 0 r)
                            (substring (nth 1 r) 0 7)
                            (substring (nth 2 r) 0 7)
                            (nth 3 r)))))
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))))

(provide 'init-utils)
