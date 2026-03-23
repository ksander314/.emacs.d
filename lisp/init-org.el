;;; init-org.el --- Org-mode configuration -*- lexical-binding: t -*-
(use-package gnuplot :ensure t :defer t)

;;; Visual: org-modern for pretty headings, tags, tables
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

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
        '(("TODO" . org-warning)
          ("INPROCESS" . "orange")
          ("PAUSE" . "gray")
          ("DONE" . (:foreground "gray" :strike-through t))
          ("CANCELED" . (:foreground "green" :strike-through t))))

  ;; Priorities
  (setq org-priority-faces
        '((?A . (:foreground "red" :weight bold))
          (?B . (:foreground "orange"))
          (?C . (:foreground "gray"))))

  ;; Effort estimates for quick selection via C-c C-x e
  (setq org-global-properties
        '(("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 4:00")))
  (setq org-columns-default-format
        "%40ITEM(Task) %TODO %6Effort(Est){:} %6CLOCKSUM(Actual){:}")

  ;; Habit tracking (visual consistency graph in agenda)
  (require 'org-habit)
  (setq org-habit-graph-column 50)

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
           "* TODO %^{JIRA Key} %?\n:PROPERTIES:\n:JIRA: %\\1\n:END:\n")
          ("p" "Project" entry (file "~/src/org/projects.org")
           "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n")))

  (setq org-agenda-custom-commands
        `(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header
                         ,(lambda () (concat "Сегодня  " (my/org-day-progress))))))
            (todo "INPROCESS"
                  ((org-agenda-overriding-header "В работе")))
            (todo "PAUSE"
                  ((org-agenda-overriding-header "На паузе")))
            (tags-todo "+TODO=\"TODO\""
                       ((org-agenda-files '("~/src/org/work.org"))
                        (org-agenda-overriding-header "Задачи в work.org")))
            (tags-todo "+TODO=\"TODO\""
                       ((org-agenda-files '("~/src/org/inbox.org"))
                        (org-agenda-overriding-header "Inbox")))
            (tags-todo "+TODO=\"TODO\"|+TODO=\"INPROCESS\""
                       ((org-agenda-files '("~/src/org/projects.org"))
                        (org-agenda-overriding-header "Проекты")))))
          ("u" "Unplanned" tags "+unplanned"
           ((org-agenda-overriding-header "Незапланированные задачи")))
          ("i" "In Progress" todo "INPROCESS"
           ((org-agenda-overriding-header "Сейчас в работе")))
          ("r" "Review open items" tags-todo "+TODO=\"TODO\""
           ((org-agenda-files '("~/src/org/reviews.org"
                                "~/src/org/decisions.org"
                                "~/src/org/incidents.org"
                                "~/src/org/people.org"))
            (org-agenda-overriding-header "Открытые пункты (reviews/decisions/incidents/people)")))))

  (setq org-archive-location "~/src/org/work.org_archive::datetree/")

  (add-hook 'org-after-todo-state-change-hook #'my/org-auto-clock-on-state-change)

  ;; Color-code agenda entries by tag
  (add-hook 'org-agenda-finalize-hook #'my/org-agenda-color-by-tag))

(defun my/org-auto-clock-on-state-change ()
  "Auto-start clock on INPROCESS, auto-stop on DONE/PAUSE/CANCELED.
Also auto-set :Resolved: timestamp on incidents when marked DONE."
  (cond
   ((string= org-state "INPROCESS")
    (org-clock-in))
   ((string= org-state "PAUSE")
    (when org-clock-current-task
      (org-clock-out)))
   ((string= org-state "DONE")
    (when org-clock-current-task
      (org-clock-out))
    (when (org-entry-get (point) "Detected")
      (org-set-property "Resolved"
                        (format-time-string "[%Y-%m-%d %a %H:%M]"))))
   ((string= org-state "CANCELED")
    (when org-clock-current-task
      (org-clock-out))))
  (save-buffer))

;;; Agenda color-coding by tag

(defun my/org-agenda-color-by-tag ()
  "Highlight agenda lines by tag: meetings blue, unplanned red."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((tags (get-text-property (point) 'tags)))
        (cond
         ((member "meeting" tags)
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(face (:foreground "SteelBlue"))))
         ((member "unplanned" tags)
          (add-text-properties (line-beginning-position) (line-end-position)
                               '(face (:foreground "tomato"))))))
      (forward-line 1))))

;;; Day progress bar in dashboard

(defun my/org-day-progress ()
  "Return a string showing clocked hours today as a progress bar (target: 8h)."
  (require 'org-clock)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (total-minutes 0)
         (work-file (expand-file-name "~/src/org/work.org")))
    (when (file-exists-p work-file)
      (with-current-buffer (find-file-noselect work-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (re-search-forward (format "^\\* %s$" (regexp-quote today)) nil t)
           (let ((end (save-excursion (org-end-of-subtree t) (point))))
             (while (re-search-forward "^\\*\\* " end t)
               (setq total-minutes (+ total-minutes (org-clock-sum-current-item)))))))))
    (let* ((target 480) ; 8 hours
           (filled (min 20 (round (* 20.0 (/ (float total-minutes) target)))))
           (empty (- 20 filled))
           (bar (concat "▓" (make-string filled ?█) (make-string empty ?░) "▓")))
      (format "%s %s / 8:00" bar (my/minutes-to-hh:mm total-minutes)))))

;;; Pomodoro via org-clock

(defun my/org-pomodoro ()
  "Start a 25-min focused block on current task.  Notifies when done."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (org-todo "INPROCESS")
  (let ((task (org-get-heading t t t t)))
    (run-at-time "25 min" nil
                 (lambda ()
                   (when (fboundp 'notifications-notify)
                     (notifications-notify :title "Pomodoro"
                                           :body "25 минут прошло. Перерыв!"
                                           :urgency 'critical))
                   (when org-clock-current-task
                     (org-clock-out))
                   (message "🍅 Pomodoro done: %s" task)))
    (message "🍅 Pomodoro started: %s (25 min)" task)))

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

;;; Project association — strict completing-read from projects.org headings

(defun my/org-collect-project-names ()
  "Return list of level-1 heading titles from projects.org."
  (let ((file (expand-file-name "~/src/org/projects.org"))
        names)
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward "^\\* " nil t)
           (push (org-get-heading t t t t) names)))))
    (nreverse names)))

(defun my/org-set-project ()
  "Set :Project: property on current heading from projects.org headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let ((projects (my/org-collect-project-names)))
    (unless projects
      (user-error "No projects in projects.org"))
    (org-set-property "Project" (completing-read "Project: " projects nil t))
    (message "Project set.")))

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
       (goto-char (line-beginning-position))
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

(defun my/org-agenda-refile-to-today ()
  "Refile the agenda entry at point to today's heading in work.org."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char pos)
       (my/org-refile-to-today)))
    (org-agenda-redo)))

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
                    (project (org-entry-get (point) "Project"))
                    (unplanned (member "unplanned" (org-get-tags))))
               (when (and todo-state (> clocked 0))
                 (push (format "- [%s] %s%s%s%s"
                               (my/minutes-to-hh:mm clocked)
                               heading
                               (if project (format " {%s}" project) "")
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
  "Archive all DONE/CANCELED entries under the current level-1 date heading.
Prompts for :Project: on entries that lack it before archiving."
  (interactive)
  (org-back-to-heading t)
  (unless (= (org-current-level) 1)
    (user-error "Курсор должен быть на дневном заголовке (level 1)"))
  ;; First pass: ensure all archivable entries have :Project:
  (let ((start (point))
        (projects (my/org-collect-project-names)))
    (unless (or projects
                (y-or-n-p "No projects in projects.org. Archive without :Project:? "))
      (user-error "Add projects to projects.org first"))
    (save-excursion
      (let ((end (copy-marker (save-excursion (org-end-of-subtree t) (point)))))
        (goto-char start)
        (while (re-search-forward "^\\*\\* \\(DONE\\|CANCELED\\) " end t)
          (unless (org-entry-get (point) "Project")
            (when projects
              (let ((proj (completing-read
                           (format "Project for «%s»: "
                                   (org-get-heading t t t t))
                           (cons "[none]" projects) nil t)))
                (unless (string= proj "[none]")
                  (org-set-property "Project" proj))))))
        (set-marker end nil)))
    ;; Second pass: archive
    (let ((count 0))
      (while (let ((end (save-excursion (org-end-of-subtree t) (point))))
               (goto-char start)
               (re-search-forward "^\\*\\* \\(DONE\\|CANCELED\\) " end t))
        (beginning-of-line)
        (org-archive-subtree)
        (setq count (1+ count))
        (goto-char start))
      (message "Archived %d entries." count))))

;;; Weekly review

(defun my/org-weekly-review ()
  "Generate weekly summary: hours per day, top tasks, unplanned %.
Shows in *Weekly Review* buffer and copies to kill-ring."
  (interactive)
  (require 'org-clock)
  (let* ((work-file (expand-file-name "~/src/org/work.org"))
         (today (current-time))
         (dow (string-to-number (format-time-string "%u" today)))
         (monday (time-subtract today (days-to-time (1- dow))))
         (days-data nil)
         (total-planned 0)
         (total-unplanned 0)
         (top-tasks nil))
    (dotimes (i (min dow 5))
      (let* ((date (time-add monday (days-to-time i)))
             (date-str (format-time-string "%Y-%m-%d" date))
             (day-name (format-time-string "%a" date))
             (day-planned 0)
             (day-unplanned 0))
        (with-current-buffer (find-file-noselect work-file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (when (re-search-forward (format "^\\* %s$" (regexp-quote date-str)) nil t)
             (let ((end (save-excursion (org-end-of-subtree t) (point))))
               (while (re-search-forward "^\\*\\* " end t)
                 (let* ((heading (org-get-heading t t t t))
                        (clocked (org-clock-sum-current-item))
                        (tags (org-get-tags)))
                   (when (> clocked 0)
                     (if (member "unplanned" tags)
                         (setq day-unplanned (+ day-unplanned clocked))
                       (setq day-planned (+ day-planned clocked)))
                     (push (cons heading clocked) top-tasks))))))))
        (setq total-planned (+ total-planned day-planned))
        (setq total-unplanned (+ total-unplanned day-unplanned))
        (push (list day-name date-str day-planned day-unplanned) days-data)))
    ;; Sort top tasks by time
    (setq top-tasks (seq-take (sort top-tasks (lambda (a b) (> (cdr a) (cdr b)))) 10))
    (let* ((total (+ total-planned total-unplanned))
           (unplanned-pct (if (> total 0)
                              (round (* 100.0 (/ (float total-unplanned) total)))
                            0))
           (report
            (with-temp-buffer
              (insert "=== Недельный обзор ===\n\n")
              (insert (format "%-5s %-12s %8s %10s %8s\n" "День" "Дата" "Planned" "Unplanned" "Всего"))
              (insert (make-string 50 ?─) "\n")
              (dolist (day (nreverse days-data))
                (let ((dt (+ (nth 2 day) (nth 3 day))))
                  (insert (format "%-5s %-12s %8s %10s %8s\n"
                                  (nth 0 day) (nth 1 day)
                                  (my/minutes-to-hh:mm (nth 2 day))
                                  (my/minutes-to-hh:mm (nth 3 day))
                                  (my/minutes-to-hh:mm dt)))))
              (insert (make-string 50 ?─) "\n")
              (insert (format "%-18s %8s %10s %8s\n" "Итого"
                              (my/minutes-to-hh:mm total-planned)
                              (my/minutes-to-hh:mm total-unplanned)
                              (my/minutes-to-hh:mm total)))
              (insert (format "\nНезапланированная работа: %d%%\n" unplanned-pct))
              (when top-tasks
                (insert "\nТоп задачи по времени:\n")
                (dolist (task top-tasks)
                  (insert (format "  [%s] %s\n"
                                  (my/minutes-to-hh:mm (cdr task))
                                  (car task)))))
              (buffer-string))))
      (kill-new report)
      (with-current-buffer (get-buffer-create "*Weekly Review*")
        (erase-buffer)
        (insert report)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      (message "Weekly review скопирован в kill-ring."))))

;;; EOD reminder

(defun my/org-eod-reminder ()
  "Remind to review the day when quitting Emacs after 17:00."
  (when (and (>= (string-to-number (format-time-string "%H")) 17)
             (my/org-daily-heading))
    (when (y-or-n-p "Конец дня. Показать итоги? ")
      (save-excursion
        (with-current-buffer (find-file-noselect (expand-file-name "~/src/org/work.org"))
          (goto-char (marker-position (my/org-daily-heading)))
          (my/org-calculate-totals-for-day)))))
  t)

(add-hook 'kill-emacs-query-functions #'my/org-eod-reminder)

;;; Code review capture

(defun my/org-capture-code-review ()
  "Capture a code review note with file context and optional code snippet."
  (interactive)
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (line (line-number-at-pos))
         (snippet (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))))
         (comment (read-string "Review comment: "))
         (review-file (expand-file-name "~/src/org/reviews.org")))
    (with-current-buffer (find-file-noselect review-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO %s\n" comment))
      (insert ":PROPERTIES:\n")
      (insert (format ":File: %s:%d\n" (abbreviate-file-name file) line))
      (insert ":END:\n")
      (when snippet
        (let ((mode-name (replace-regexp-in-string
                          "-mode$\\|-ts-mode$" ""
                          (symbol-name major-mode))))
          (insert (format "#+begin_src %s\n%s\n#+end_src\n" mode-name snippet))))
      (save-buffer))
    (message "Review note saved: %s" comment)))

;;; Decision log capture

(defun my/org-capture-decision ()
  "Capture an architecture/technical decision."
  (interactive)
  (let* ((title (read-string "Decision: "))
         (decision-file (expand-file-name "~/src/org/decisions.org")))
    (with-current-buffer (find-file-noselect decision-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO %s %s\n" (format-time-string "%Y-%m-%d") title))
      (insert "** Контекст\n\n")
      (insert "** Варианты\n\n")
      (insert "** Решение\n\n")
      (insert "** Последствия\n\n")
      (save-buffer)
      ;; Position cursor at Контекст for immediate editing
      (goto-char (point-max))
      (re-search-backward "^\\*\\* Контекст$")
      (forward-line 1)
      (switch-to-buffer (current-buffer)))
    (message "Decision log: %s — fill in the sections." title)))

;;; 1-on-1 meeting notes

(defun my/org-one-on-one ()
  "Start 1-on-1 notes for a person.  Creates/finds section in people.org."
  (interactive)
  (let* ((people-file (expand-file-name "~/src/org/people.org"))
         (person (read-string "С кем: "))
         (today (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect people-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       ;; Find or create person heading
       (unless (re-search-forward (format "^\\* %s$" (regexp-quote person)) nil t)
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert (format "* %s\n" person)))
       ;; Add today's date subheading
       (org-end-of-subtree t)
       (unless (bolp) (insert "\n"))
       (insert (format "** %s\n" today))
       (insert "*** Обсудить\n- \n")
       (insert "*** Заметки\n\n")
       (insert "*** Action items\n- [ ] \n")
       (save-buffer)
       ;; Position at "Обсудить" list
       (re-search-backward "^\\*\\*\\* Обсудить$")
       (forward-line 1)
       (end-of-line))
      (switch-to-buffer (current-buffer)))
    (message "1-on-1 с %s — заполни повестку." person)))

;;; Incident log capture

(defun my/org-capture-incident ()
  "Capture a production incident with severity."
  (interactive)
  (let* ((severity (completing-read "Severity: " '("P1" "P2" "P3") nil t))
         (summary (read-string "Summary: "))
         (incident-file (expand-file-name "~/src/org/incidents.org")))
    (with-current-buffer (find-file-noselect incident-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO %s %s %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]") severity summary))
      (insert ":PROPERTIES:\n")
      (insert (format ":Detected: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert ":Resolved: \n")
      (insert ":END:\n")
      (insert "** Timeline\n\n")
      (insert "** Root cause\n\n")
      (insert "** Resolution\n\n")
      (insert "** Action items\n- [ ] \n")
      (save-buffer)
      (re-search-backward "^\\*\\* Timeline$")
      (forward-line 1)
      (switch-to-buffer (current-buffer)))
    (message "Incident %s: %s" severity summary)))

;;; Timesheet export

(defun my/org-timesheet (&optional days)
  "Export clocked time for the last N DAYS as an org table.
Defaults to 5 (work week)."
  (interactive "P")
  (require 'org-clock)
  (let* ((days (or days 5))
         (work-file (expand-file-name "~/src/org/work.org"))
         (rows nil))
    (with-current-buffer (find-file-noselect work-file)
      (org-with-wide-buffer
       (dotimes (i days)
         (let* ((date (time-subtract (current-time) (days-to-time i)))
                (date-str (format-time-string "%Y-%m-%d" date)))
           (goto-char (point-min))
           (when (re-search-forward (format "^\\* %s$" (regexp-quote date-str)) nil t)
             (let ((end (save-excursion (org-end-of-subtree t) (point))))
               (while (re-search-forward "^\\*\\* " end t)
                 (let* ((heading (org-get-heading t t t t))
                        (clocked (org-clock-sum-current-item))
                        (jira (org-entry-get (point) "JIRA")))
                   (when (> clocked 0)
                     (push (list date-str
                                 (my/minutes-to-hh:mm clocked)
                                 (or jira "")
                                 heading)
                           rows))))))))))
    (with-current-buffer (get-buffer-create "*Timesheet*")
      (erase-buffer)
      (insert "| Дата | Время | JIRA | Задача |\n")
      (insert "|------+-------+------+--------|\n")
      (dolist (row (nreverse rows))
        (insert (format "| %s | %s | %s | %s |\n"
                        (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row))))
      (insert "|------+-------+------+--------|\n")
      (org-mode)
      (goto-char (point-min))
      (org-table-align)
      (display-buffer (current-buffer)))
    (message "Timesheet за %d дней." days)))

;;; Energy tracker (uses gnuplot for visualization)

(defun my/org-energy-check ()
  "Log current energy level (1-5) to energy.org table."
  (interactive)
  (let* ((level (read-number "Энергия (1-5): "))
         (energy-file (expand-file-name "~/src/org/energy.org"))
         (timestamp (format-time-string "%Y-%m-%d %a %H:%M")))
    (unless (<= 1 level 5)
      (user-error "Level must be 1-5"))
    (with-current-buffer (find-file-noselect energy-file)
      (goto-char (point-min))
      ;; Create table header if file is empty or has no table
      (unless (re-search-forward "^| Дата" nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "#+TITLE: Energy Tracker\n\n")
        (insert "| Дата | Уровень |\n")
        (insert "|------+---------|\n"))
      ;; Go to end of table
      (goto-char (point-min))
      (re-search-forward "^|" nil t)
      (while (and (not (eobp)) (looking-at-p ".*|"))
        (forward-line 1))
      ;; Back up to last table line
      (forward-line -1)
      (end-of-line)
      (insert (format "\n| %s | %d |" timestamp (truncate level)))
      (org-table-align)
      (save-buffer))
    (message "Energy: %d at %s" (truncate level) timestamp)))

(defun my/org-energy-plot ()
  "Plot energy levels from energy.org using gnuplot."
  (interactive)
  (let ((energy-file (expand-file-name "~/src/org/energy.org")))
    (unless (file-exists-p energy-file)
      (user-error "No energy.org file"))
    (find-file energy-file)
    (goto-char (point-min))
    (re-search-forward "^|" nil t)
    (beginning-of-line)
    (message "Cursor on table. Use C-c \" to plot with orgtbl or add a #+PLOT line.")))

(provide 'init-org)
