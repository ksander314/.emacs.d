(require-package 'popup)
(defun current-line ()
       "Print the current line number (in the buffer) of point."
       (interactive)
       (save-restriction
         (widen)
         (save-excursion
           (beginning-of-line)
           (format "%d"
                   (1+ (count-lines 1 (point)))))))
(defun hg-summary-current-line ()
  "Pop up tip with current line commit summary."
  (interactive)
  (popup-tip
   (shell-command-to-string
    (concat "hg log --template '{desc}\n' -r `hg annotate -l " buffer-file-name " | sed -n -e" (current-line) "p | sed 's/\\(^[[:digit:]]\\+\\):.*/\\1/'`"))))
(global-set-key (kbd "C-c C-h s") 'hg-summary-current-line)
(provide 'init-hg-commit-message)
