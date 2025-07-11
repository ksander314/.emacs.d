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
         entry (file (lambda ()
                       (format "~/org/%d.org"
                               (string-to-number (format-time-string "%V")))))
         "\n* %(format-time-string \"%Y-%m-%d\") [/]
:PROPERTIES:
:StartWorkingAt: %(format-time-string \"<%Y-%m-%d %H:%M>\" (current-time))
:FinishWorkingAt: %(format-time-string \"<%Y-%m-%d %H:%M>\" (time-add (current-time) (seconds-to-time (* 8.5 3600))))
:END:

** TODO Slack Later :INVISIBLE:
:PROPERTIES:
:Effort:   0.5h
:END:

** TODO Daily Call :CALL:
SCHEDULED: %(format-time-string \"<%Y-%m-%d %H:%M>\" (encode-time 0 30 12 (string-to-number (format-time-string \"%d\")) (string-to-number (format-time-string \"%m\")) (string-to-number (format-time-string \"%Y\"))))
:PROPERTIES:
:Effort:   0.5h
:END:

** TODO Dinner :INVISIBLE:
:PROPERTIES:
:Effort: 1h
:END:
"
)))

(load "../jira-secrets.el")

(use-package org-jira
  :ensure t
  :config
  (setq org-jira-file "~/org/jira.org")
  (setq jiralib-url my-jiralib-url)
  (setq jiralib-token my-jiralib-token)
  (setq jiralib-user my-jiralib-username)
  (setq org-jira-custom-jqls
  '(
    (:jql " project IN (WTSN) and Sprint in openSprints() order by created DESC "
          :limit 100
          :filename "current-sprint")
    ))
  )

(use-package org-alert
  :ensure t
  :after org
  :config
  ;; Set how often (in seconds) to check for upcoming deadlines
  (setq org-alert-interval 60)
  (setq org-alert-notify-cutoff 5)
  (setq org-alert-notify-after-event-cutoff 5)
  ;; Optionally, set the notification command (Linux example)
  (setq org-alert-notification-command "notify-send")
  (setq alert-default-style 'libnotify)
  (org-alert-enable))

(defun my/org-read-duration-property (prop)
  "Read a duration property PROP from the current headline and convert it to minutes."
  (let ((val (org-entry-get nil prop)))
    (when val
      (org-duration-to-minutes val))))

(defun my/org-calc-workday-stats ()
  "Calculate workday statistics from the current Org buffer.
Expected properties:
- TotalEffort (in format like '8:30')
- TotalClocked (in format like '3:55')
And clocked times in LOGBOOK entries for tasks tagged with \"INVISIBLE\" or \"CALL\"."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((total-effort 0)
          (total-clocked 0)
          (invisible-clocked 0)
          (call-clocked 0))
      ;; Read top-level properties from the first headline (assumed to be the workday header)
      (when (re-search-forward ":TotalEffort:[ \t]*\\([^:\n]+\\)" nil t)
        (setq total-effort (org-duration-to-minutes (match-string 1))))
      (when (re-search-forward ":TotalClocked:[ \t]*\\([^:\n]+\\)" nil t)
        (setq total-clocked (org-duration-to-minutes (match-string 1))))
      ;; Iterate over all headlines in the file
      (org-map-entries
       (lambda ()
         (let* ((tags (org-get-tags))
                ;; Sum clocked time in the current subtree using org-clock-sum.
                (clocked (org-clock-sum nil 'value)))
           (when (member "INVISIBLE" tags)
             (setq invisible-clocked (+ invisible-clocked clocked)))
           (when (member "CALL" tags)
             (setq call-clocked (+ call-clocked clocked)))))
       nil 'file)
      ;; Calculate unclocked time as difference between total effort and total clocked
      (let ((unclocked (max 0 (- total-effort total-clocked))))
        (message "TasksClocked: %s, InvisibleClocked: %s, CallClocked: %s, UnclockedBetweenStartAndFinish: %s"
                 (org-minutes-to-duration total-clocked)
                 (org-minutes-to-duration invisible-clocked)
                 (org-minutes-to-duration call-clocked)
                 (org-minutes-to-duration unclocked))))))

(provide 'init-org)
