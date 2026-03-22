;;; init-focus-shield.el --- Interrupt shield with context save/restore -*- lexical-binding: t -*-

(declare-function org-clocking-p "org-clock")
(declare-function org-clock-in "org-clock")
(declare-function org-clock-out "org-clock")
(declare-function org-clock-goto "org-clock")
(declare-function org-todo "org")
(declare-function org-get-heading "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-end-of-subtree "org")
(declare-function org-up-heading-safe "org")
(declare-function org-entry-get "org")

(defvar my/focus-shield-file "~/src/org/interruptions.org"
  "File to log interruptions.")

(defvar my/focus-shield--frozen-state nil
  "Plist storing frozen context: :window-config :buffer :point :clock-marker :freeze-time :note.")

(defun my/focus-freeze ()
  "Freeze current context — save window layout, position, pause org-clock.
Use when interrupted.  Restore with `my/focus-thaw'."
  (interactive)
  (require 'org-clock)
  (when my/focus-shield--frozen-state
    (if (not (y-or-n-p "Already frozen.  Overwrite? "))
        (user-error "Freeze cancelled")
      (my/focus-shield--write-duration)))
  (let* ((win-config (current-window-configuration))
         (buf (current-buffer))
         (pos (point))
         (clock-marker (when (org-clocking-p) (copy-marker org-clock-marker)))
         (note (read-string "Над чем работал? "))
         (now (current-time)))
    ;; Pause clocked task
    (when clock-marker
      (with-current-buffer (marker-buffer clock-marker)
        (goto-char clock-marker)
        (org-todo "PAUSE")))
    (setq my/focus-shield--frozen-state
          (list :window-config win-config
                :buffer buf
                :point pos
                :clock-marker clock-marker
                :freeze-time now
                :note note))
    (my/focus-shield--log-entry now buf pos clock-marker note)
    (message "Frozen. Работай над прерыванием, потом C-c Z.")))

(defun my/focus-thaw ()
  "Restore frozen context — window layout, cursor position, resume org-clock."
  (interactive)
  (unless my/focus-shield--frozen-state
    (user-error "Nothing frozen"))
  (let* ((state my/focus-shield--frozen-state)
         (win-config (plist-get state :window-config))
         (buf (plist-get state :buffer))
         (pos (plist-get state :point))
         (clock-marker (plist-get state :clock-marker))
         (note (plist-get state :note)))
    ;; Write duration to the log
    (my/focus-shield--write-duration)
    ;; Restore windows
    (set-window-configuration win-config)
    ;; Restore buffer position
    (when (buffer-live-p buf)
      (switch-to-buffer buf)
      (goto-char pos))
    ;; Show note
    (message "Ты работал над: %s" note)
    ;; Offer to resume clocked task
    (when (and clock-marker (marker-buffer clock-marker))
      (when (y-or-n-p "Вернуться к задаче? ")
        (with-current-buffer (marker-buffer clock-marker)
          (goto-char clock-marker)
          (org-todo "INPROCESS"))))
    (setq my/focus-shield--frozen-state nil)))

(defun my/focus-shield--log-entry (time buf pos clock-marker note)
  "Append an interruption entry to `my/focus-shield-file'."
  (let* ((date-heading (format-time-string "%Y-%m-%d" time))
         (time-str (format-time-string "%H:%M" time))
         (buffer-name (if (buffer-file-name buf)
                          (abbreviate-file-name (buffer-file-name buf))
                        (buffer-name buf)))
         (line-num (with-current-buffer buf
                     (save-excursion
                       (goto-char pos)
                       (line-number-at-pos))))
         (task-name (when clock-marker
                      (with-current-buffer (marker-buffer clock-marker)
                        (save-excursion
                          (goto-char clock-marker)
                          (org-get-heading t t t t))))))
    (with-current-buffer (find-file-noselect my/focus-shield-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       ;; Find or create date heading
       (unless (re-search-forward (format "^\\* %s$" (regexp-quote date-heading)) nil t)
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (insert (format "* %s\n" date-heading)))
       ;; Go to end of date subtree
       (org-end-of-subtree t)
       (unless (bolp) (insert "\n"))
       ;; Insert interruption entry
       (insert (format "** %s — Прерывание\n" time-str))
       (insert ":PROPERTIES:\n")
       (insert (format ":FrozenBuffer: %s\n" buffer-name))
       (insert (format ":FrozenLine:   %d\n" line-num))
       (when task-name
         (insert (format ":FrozenTask:   %s\n" task-name)))
       (insert ":Duration:     \n")
       (insert ":END:\n")
       (insert note "\n"))
      (save-buffer))))

(defun my/focus-shield--write-duration ()
  "Write the interruption duration into the last log entry."
  (when my/focus-shield--frozen-state
    (let* ((freeze-time (plist-get my/focus-shield--frozen-state :freeze-time))
           (elapsed (time-subtract (current-time) freeze-time))
           (minutes (truncate (/ (float-time elapsed) 60)))
           (duration-str (format "%d:%02d" (/ minutes 60) (% minutes 60))))
      (with-current-buffer (find-file-noselect my/focus-shield-file)
        (org-with-wide-buffer
         (goto-char (point-max))
         (when (re-search-backward "^:Duration:[ \t]*$" nil t)
           (replace-match (format ":Duration:     %s" duration-str))))
        (save-buffer)))))

(defun my/focus-report ()
  "Show interruption statistics."
  (interactive)
  (unless (file-exists-p my/focus-shield-file)
    (user-error "No interruptions file found"))
  (let ((today (format-time-string "%Y-%m-%d"))
        (week-start (format-time-string "%Y-%m-%d"
                      (time-subtract (current-time)
                                     (days-to-time
                                      (string-to-number (format-time-string "%u"))))))
        (today-count 0) (today-minutes 0)
        (week-count 0) (week-minutes 0)
        (all-count 0) (all-minutes 0))
    (with-current-buffer (find-file-noselect my/focus-shield-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\*\\* \\([0-9:]+\\) — Прерывание$" nil t)
         (let ((date nil) (dur-minutes 0))
           ;; Get date from parent heading
           (save-excursion
             (org-up-heading-safe)
             (setq date (org-get-heading t t t t)))
           ;; Get duration
           (let ((dur (org-entry-get (point) "Duration")))
             (when (and dur (not (string-empty-p (string-trim dur))))
               (setq dur-minutes (truncate (org-duration-to-minutes dur)))))
           (setq all-count (1+ all-count))
           (setq all-minutes (+ all-minutes dur-minutes))
           (when (and date (string= date today))
             (setq today-count (1+ today-count))
             (setq today-minutes (+ today-minutes dur-minutes)))
           (when (and date (string<= week-start date) (string<= date today))
             (setq week-count (1+ week-count))
             (setq week-minutes (+ week-minutes dur-minutes)))))))
    (with-current-buffer (get-buffer-create "*Focus Shield Report*")
      (erase-buffer)
      (insert "=== Focus Shield Report ===\n\n")
      (insert (format "%-20s %5s %10s %10s\n" "Период" "Кол-во" "Время" "Среднее"))
      (insert (make-string 50 ?-) "\n")
      (dolist (row `(("Сегодня" ,today-count ,today-minutes)
                     ("Эта неделя" ,week-count ,week-minutes)
                     ("Всего" ,all-count ,all-minutes)))
        (let* ((label (nth 0 row))
               (count (nth 1 row))
               (mins (nth 2 row))
               (avg (if (> count 0) (/ mins count) 0)))
          (insert (format "%-20s %5d %10s %10s\n"
                          label count
                          (format "%d:%02d" (/ mins 60) (% mins 60))
                          (format "%d:%02d" (/ avg 60) (% avg 60))))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'init-focus-shield)
