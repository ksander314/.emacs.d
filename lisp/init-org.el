(require-package 'gnuplot)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (gnuplot . t)))

(add-hook 'org-mode-hook (lambda () (setq org-enforce-todo-dependencies t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PAUSE(p)" "INPROCESS(s)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("INPROCESS" . "orange")
        ("CANCELED" . "green")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)

(defun my-org-auto-clock-on-state-change ()
  "Auto-start clock when state changes to INPROCESS and auto-stop when state changes to PAUSE."
  (cond
   ((string= org-state "INPROCESS")
    ;; Only clock in if no clock is already running.
    (unless org-clock-current-task
      (org-clock-in)))
   ((string= org-state "PAUSE")
    ;; Only clock out if a clock is running.
    (when org-clock-current-task
      (org-clock-out)))))

(add-hook 'org-after-todo-state-change-hook #'my-org-auto-clock-on-state-change)

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

(load "../jira-secrets.el")

(use-package org-jira
  :ensure t
  :config
  (setq org-jira-file "~/org/jira.org")
  (setq jiralib-url jiralib-url)
  (setq jiralib-username jiralib-username)
  (setq jiralib-password jiralib-password)
  (defconst jiralib-token jiralib-token)
  (setq org-jira-custom-jqls
  '(
    (:jql " project IN (WTSN) and Sprint in openSprints() order by created DESC "
          :limit 100
          :filename "current-sprint")
    (:jql " project IN (WTSN) and Sprint in openSprints() and assignee = currentUser() order by created DESC "
          :limit 100
          :filename "my-current-tasks")
    ))
  )

(use-package org-alert
  :ensure t
  :after org
  :config
  ;; Set how often (in seconds) to check for upcoming deadlines
  (setq org-alert-interval 60)
  (setq org-alert-notify-cutoff 5)
  (setq org-alert-notify-event-cutoff 5)
  ;; Optionally, set the notification command (Linux example)
  (setq org-alert-notification-command "notify-send")
  (setq alert-default-style 'libnotify)
  (org-alert-enable))

(provide 'init-org)
