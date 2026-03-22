;;; init-org.el --- Org-mode configuration -*- lexical-binding: t -*-
(use-package gnuplot :ensure t :defer t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (gnuplot . t)))

  (setq org-enforce-todo-dependencies t)
  (setq org-startup-indented t)
  (setq org-ellipsis " ⤵")

  (setq org-agenda-files '("~/src/org/inbox.org"
                           "~/src/org/work.org"
                           "~/src/org/projects.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PAUSE(p)" "INPROCESS(s)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-warning) ("INPROCESS" . "orange")
          ("CANCELED" . "green")))

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Refile targets: daily headings in work.org, project headings in projects.org
  (setq org-refile-targets '(("~/src/org/work.org" :level . 1)
                              ("~/src/org/projects.org" :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        '(("t" "Task" entry (file "~/src/org/inbox.org")
           "* TODO %?\n")
          ("j" "JIRA Task" entry (file "~/src/org/inbox.org")
           "* TODO %^{JIRA Key} %?\n:PROPERTIES:\n:JIRA: %\\1\n:END:\n")))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "INPROCESS"
                  ((org-agenda-overriding-header "В работе")))
            (tags-todo "+TODO=\"TODO\""
                       ((org-agenda-files '("~/src/org/work.org"))
                        (org-agenda-overriding-header "Задачи в work.org")))))
          ("u" "Unplanned" tags "+unplanned"
           ((org-agenda-overriding-header "Незапланированные задачи")))
          ("i" "In Progress" todo "INPROCESS"
           ((org-agenda-overriding-header "Сейчас в работе")))))

  (setq org-archive-location "~/src/org/work.org_archive::datetree/")

  (add-hook 'org-after-todo-state-change-hook #'my/org-auto-clock-on-state-change))

(defun my/org-auto-clock-on-state-change ()
  "Auto-start clock when state changes to INPROCESS and auto-stop when state changes to PAUSE."
  (cond
   ((string= org-state "INPROCESS")
    (org-clock-in))
   ((string= org-state "PAUSE")
    (when org-clock-current-task
      (org-clock-out)))))

;;; Task state shortcuts (no arrow keys needed)

(defun my/org-start ()
  "Set current heading to INPROCESS (clock starts automatically)."
  (interactive)
  (org-todo "INPROCESS"))

(defun my/org-done ()
  "Set current heading to DONE (clock stops automatically)."
  (interactive)
  (org-todo "DONE"))

(defun my/org-pause ()
  "Set current heading to PAUSE (clock stops automatically)."
  (interactive)
  (org-todo "PAUSE"))

;;; Link task to external system (JIRA, GitHub, GitLab)

(defvar my/org-link-types
  '(("j" . ("JIRA"   . "https://tradingview-air.atlassian.net/browse/%s"))
    ("g" . ("GitHub"  . nil))
    ("l" . ("GitLab"  . nil)))
  "Link types: key → (name . url-template-or-nil).
When url-template is non-nil, the input is a ticket key expanded into a full URL.
When nil, the input is a full URL.")

(defun my/org-link-task ()
  "Attach a link (JIRA/GitHub/GitLab) to the current org heading.
Stores as a property and as a clickable org-link in the body."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((choice (read-char-choice "[j]ira [g]ithub [l]ab: " '(?j ?g ?l)))
         (entry (alist-get (char-to-string choice) my/org-link-types nil nil #'string=))
         (name (car entry))
         (template (cdr entry))
         (input (read-string (format "%s key/URL: " name)))
         (url (if template
                  (format template input)
                input))
         (prop-name name))
    (org-set-property prop-name url)
    (save-excursion
      (org-end-of-meta-data t)
      (insert (format "[[%s][%s: %s]]\n" url name
                      (if template input (file-name-nondirectory url)))))
    (message "Linked: %s → %s" name url)))

;;; Urgent/unplanned task — straight to today, tagged

(defun my/org-urgent-task ()
  "Add an unplanned task directly to today in work.org with :unplanned: tag.
Optionally start working on it immediately."
  (interactive)
  (let* ((task (read-string "Urgent task: "))
         (start-now (y-or-n-p "Start now? ")))
    (when (string-empty-p (string-trim task))
      (user-error "Empty task"))
    (let ((marker (my/org-ensure-daily-heading))
          heading-pos)
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char (marker-position marker))
         (org-end-of-subtree t)
         (unless (bolp) (insert "\n"))
         (setq heading-pos (point-marker))
         (insert (format "** TODO %s :unplanned:\n" task)))
        (save-buffer))
      (when start-now
        (switch-to-buffer (marker-buffer heading-pos))
        (goto-char heading-pos)
        (org-todo "INPROCESS")))))

;;; Meeting notes — quick start, write during the call

(defun my/org-meeting ()
  "Start a meeting: create heading under today in work.org, clock in, take notes."
  (interactive)
  (let ((topic (read-string "Meeting topic: ")))
    (when (string-empty-p (string-trim topic))
      (user-error "Empty topic"))
    (let ((marker (my/org-ensure-daily-heading))
          heading-pos)
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char (marker-position marker))
         (org-end-of-subtree t)
         (unless (bolp) (insert "\n"))
         (setq heading-pos (point-marker))
         (insert (format "** INPROCESS %s :meeting:\n" topic))
         (insert ":PROPERTIES:\n")
         (insert (format ":MeetingTime: %s\n" (format-time-string "%H:%M")))
         (insert ":END:\n"))
        (save-buffer))
      (switch-to-buffer (marker-buffer heading-pos))
      (goto-char heading-pos)
      (org-end-of-meta-data t)
      (open-line 1)
      (org-clock-in)
      (message "Meeting started: %s — C-c d to finish" topic))))

;;; Quick task capture (no template selection)

(defun my/org-quick-task ()
  "Add a task to inbox.org with one prompt.  No template selection."
  (interactive)
  (let ((task (read-string "Task: ")))
    (when (string-empty-p (string-trim task))
      (user-error "Empty task"))
    (with-current-buffer (find-file-noselect "~/src/org/inbox.org")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO %s\n" task))
      (save-buffer))
    (message "Added: %s" task)))

;;; Daily heading in work.org

(defun my/org-find-date-heading (date-str file)
  "Find heading for DATE-STR in FILE.  Return marker or nil."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward (format "^\\* %s$" (regexp-quote date-str)) nil t)
       (point-marker)))))

(defun my/org-daily-heading ()
  "Find today's heading in work.org.  Return marker or nil."
  (my/org-find-date-heading (format-time-string "%Y-%m-%d")
                            (expand-file-name "~/src/org/work.org")))

(defun my/org-ensure-daily-heading ()
  "Find or create today's heading in work.org.  Return the marker."
  (or (my/org-daily-heading)
      (let ((work-file (expand-file-name "~/src/org/work.org")))
        (with-current-buffer (find-file-noselect work-file)
          (org-with-wide-buffer
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert (format "* %s\n" (format-time-string "%Y-%m-%d")))
           (save-buffer)
           (point-marker))))))

(defun my/org-refile-to-today ()
  "Refile current entry to today's heading in work.org."
  (interactive)
  (let ((marker (my/org-ensure-daily-heading)))
    (org-refile nil nil
               (list nil
                     (buffer-file-name (marker-buffer marker))
                     nil
                     (marker-position marker)))
    (message "Refiled to today.")))

;;; Standup report

(defun my/org-standup--last-workday ()
  "Return the date string of the last workday (skip weekends)."
  (let* ((days-back (pcase (format-time-string "%u")  ; 1=Mon ... 7=Sun
                      ("1" 3)   ; Monday → Friday
                      ("7" 2)   ; Sunday → Friday
                      (_ 1)))   ; otherwise → yesterday
         (date (time-subtract (current-time) (days-to-time days-back))))
    (format-time-string "%Y-%m-%d" date)))

(defun my/org-standup ()
  "Generate standup report from last workday's clocked entries.  Copy to kill-ring."
  (interactive)
  (require 'org-clock)
  (let* ((date-str (my/org-standup--last-workday))
         (today-str (format-time-string "%Y-%m-%d"))
         (work-file (expand-file-name "~/src/org/work.org"))
         (inbox-file (expand-file-name "~/src/org/inbox.org"))
         (yesterday-entries nil)
         (today-tasks nil)
         (report ""))
    ;; Collect last workday's clocked entries from work.org
    (with-current-buffer (find-file-noselect work-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward (format "^\\* %s$" (regexp-quote date-str)) nil t)
         (let ((end (save-excursion (org-end-of-subtree t) (point))))
           (while (re-search-forward "^\\*\\* " end t)
             (let* ((heading (org-get-heading t t t t))
                    (todo-state (org-get-todo-state))
                    (clocked (org-clock-sum-current-item))
                    (jira (org-entry-get (point) "JIRA"))
                    (unplanned (member "unplanned" (org-get-tags))))
               (when (and todo-state (> clocked 0))
                 (push (format "- [%s] %s%s%s"
                               (my/minutes-to-hh:mm clocked)
                               heading
                               (if jira (format " (%s)" jira) "")
                               (if unplanned " [unplanned]" ""))
                       yesterday-entries))))))))
    ;; Collect today's TODO tasks
    (with-current-buffer (find-file-noselect work-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward (format "^\\* %s$" (regexp-quote today-str)) nil t)
         (let ((end (save-excursion (org-end-of-subtree t) (point))))
           (while (re-search-forward "^\\*\\* \\(TODO\\|INPROCESS\\) " end t)
             (push (format "- %s" (org-get-heading t t t t)) today-tasks))))))
    ;; Also grab inbox tasks as today candidates
    (with-current-buffer (find-file-noselect inbox-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\* TODO " nil t)
         (push (format "- %s (inbox)" (org-get-heading t t t t)) today-tasks))))
    ;; Build report
    (setq report
          (concat (format "%s:\n" date-str)
                  (if yesterday-entries
                      (mapconcat #'identity (nreverse yesterday-entries) "\n")
                    "- (ничего не залочено)")
                  "\n\nСегодня:\n"
                  (if today-tasks
                      (mapconcat #'identity (nreverse today-tasks) "\n")
                    "- (нет задач)")))
    ;; Save to work.org under today's heading
    (let ((marker (my/org-ensure-daily-heading)))
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char (marker-position marker))
         (let ((end (save-excursion (org-end-of-subtree t) (point)))
               (existing nil))
           (when (re-search-forward "^\\*\\* Standup " end t)
             (setq existing t)
             (unless (y-or-n-p "Standup уже есть. Перезаписать? ")
               (user-error "Отменено")))
           (if existing
               (progn
                 (beginning-of-line)
                 (let ((sub-end (save-excursion (org-end-of-subtree t) (point))))
                   (delete-region (point) sub-end)))
             (goto-char end)
             (unless (bolp) (insert "\n")))
           (insert (format "** Standup %s\n" (format-time-string "%H:%M")))
           (insert report "\n")))
        (save-buffer)))
    (kill-new report)
    (with-current-buffer (get-buffer-create "*Standup*")
      (erase-buffer)
      (insert report)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Standup сохранён в work.org и скопирован в kill-ring.")))

;;; Workday stats (kept for later use)

(defun my/org-calculate-totals-for-day ()
  "Calculate total effort and clocked time for the current Org heading."
  (interactive)
  (save-excursion
    (let ((total-effort 0)
          (total-clocked (org-clock-sum-current-item)))
      (org-map-entries
       (lambda ()
         (let ((effort (org-entry-get (point) "Effort")))
           (when effort
             (let ((effort-minutes (org-duration-to-minutes effort)))
               (setq total-effort (+ total-effort (truncate effort-minutes)))))))
       nil 'tree)
      (org-back-to-heading t)
      (org-set-property "TotalEffort" (my/minutes-to-hh:mm (truncate total-effort)))
      (org-set-property "TotalClocked" (my/minutes-to-hh:mm (truncate total-clocked)))
      (message "Totals updated: Effort=%s, Clocked=%s"
               (my/minutes-to-hh:mm (truncate total-effort))
               (my/minutes-to-hh:mm (truncate total-clocked))))))

;;; Unplanned work report

(defun my/org-unplanned-report ()
  "Show planned vs unplanned work stats for the past N days."
  (interactive)
  (let* ((days (read-number "Days to analyze: " 7))
         (work-file (expand-file-name "~/src/org/work.org"))
         (planned-minutes 0)
         (unplanned-minutes 0)
         (unplanned-items nil))
    (with-current-buffer (find-file-noselect work-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\*\\* " nil t)
         (let* ((todo-state (org-get-todo-state))
                (heading (org-get-heading t t t t))
                (clocked (org-clock-sum-current-item))
                (tags (org-get-tags))
                (parent-date
                 (save-excursion
                   (org-up-heading-safe)
                   (org-get-heading t t t t)))
                (cutoff (format-time-string "%Y-%m-%d"
                          (time-subtract (current-time) (days-to-time days)))))
           (when (and todo-state
                      parent-date
                      (string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" parent-date)
                      (string<= cutoff parent-date)
                      (> clocked 0))
             (if (member "unplanned" tags)
                 (progn
                   (setq unplanned-minutes (+ unplanned-minutes clocked))
                   (push (format "  %s [%s] %s"
                                 parent-date
                                 (my/minutes-to-hh:mm clocked)
                                 heading)
                         unplanned-items))
               (setq planned-minutes (+ planned-minutes clocked))))))))
    (let* ((total (+ planned-minutes unplanned-minutes))
           (pct (if (> total 0)
                    (round (* 100.0 (/ (float unplanned-minutes) total)))
                  0)))
      (with-current-buffer (get-buffer-create "*Unplanned Work Report*")
        (erase-buffer)
        (insert (format "=== Unplanned Work Report (last %d days) ===\n\n" days))
        (insert (format "Planned:   %s\n" (my/minutes-to-hh:mm planned-minutes)))
        (insert (format "Unplanned: %s (%d%%)\n" (my/minutes-to-hh:mm unplanned-minutes) pct))
        (insert (format "Total:     %s\n" (my/minutes-to-hh:mm total)))
        (when unplanned-items
          (insert "\nUnplanned items:\n")
          (dolist (item (nreverse unplanned-items))
            (insert item "\n")))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; Archive done tasks

(defun my/org-archive-done ()
  "Archive all DONE/CANCELED entries under the current level-1 date heading."
  (interactive)
  (org-back-to-heading t)
  (unless (= (org-current-level) 1)
    (user-error "Курсор должен быть на дневном заголовке (level 1)"))
  (let ((count 0)
        (start (point)))
    (while (let ((end (save-excursion (org-end-of-subtree t) (point))))
             (goto-char start)
             (re-search-forward "^\\*\\* \\(DONE\\|CANCELED\\) " end t))
      (beginning-of-line)
      (org-archive-subtree)
      (setq count (1+ count))
      (goto-char start))
    (message "Archived %d entries." count)))

(provide 'init-org)
