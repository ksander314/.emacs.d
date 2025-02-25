(require-package 'gnuplot)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (gnuplot . t)))

(add-hook 'org-mode-hook (lambda () (setq org-enforce-todo-dependencies t)))

(setq org-todo-keywords
      '((sequence "TODO" "INPROCESS" "|" "DONE" "CANCELED" "DELEGATED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("INPROCESS" . "orange")
        ("DELEGATED" . "green")))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(defun org-calculate-totals-for-day ()
  "Calculate total effort and clocked time for the current Org heading."
  (interactive)
  (save-excursion
    (let ((total-effort 0)
          (total-clocked (org-clock-sum-current-item)))
      (org-map-entries
       (lambda ()
         (let ((effort (org-entry-get (point) "Effort")))
           ;; Add Effort if exists
           (when effort
             (let ((effort-minutes (org-duration-to-minutes effort)))
               (setq total-effort (+ total-effort (truncate effort-minutes)))))
           ;; Add Clocked time if exists
           ))
       nil 'tree)
      ;; Update totals in the main heading
      (org-back-to-heading t)
      (org-set-property "TotalEffort" (org-minutes-to-hh:mm (truncate total-effort)))
      (org-set-property "TotalClocked" (org-minutes-to-hh:mm (truncate total-clocked)))
      (message "Totals updated: Effort=%s, Clocked=%s"
               (org-minutes-to-hh:mm (truncate total-effort))
               (org-minutes-to-hh:mm (truncate total-clocked))))))

(defun org-minutes-to-hh:mm (minutes)
  "Convert minutes to HH:MM format."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

(setq org-capture-templates
      '(("d" "Daily Work Log"
         entry (file (lambda () (format "~/org/%d.org" (string-to-number (format-time-string "%V")))))
         "\n* %(format-time-string \"%Y-%m-%d\") [/]\n** TODO Slack Later :CALL:\n:PROPERTIES:\n:Effort:   0.5h\n:END:\n** TODO Daily Call :CALL:\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %H:%M>\" (encode-time 0 30 12 (string-to-number (format-time-string \"%d\")) (string-to-number (format-time-string \"%m\")) (string-to-number (format-time-string \"%Y\"))))\n:PROPERTIES:\n:Effort:   0.5h\n:END:\n"
)))

(provide 'init-org)
