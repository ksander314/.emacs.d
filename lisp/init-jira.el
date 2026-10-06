;;; init-jira.el --- Jira: my work and my reviews (jira.el) -*- lexical-binding: t -*-

;; C-c j shows two lists: what is assigned to me, and what waits for my review.
;; C-c J is jira.el's own list of the team sprint.  Its filter is a transient
;; saved value, kept in ~/.emacs.d/transient/values.el (not in git): change it
;; from the menu with l, then save with C-x C-s.
;;
;; The same config runs at home, where ~/.authinfo holds no Jira token.  There
;; this file only defines functions: jira.el is not installed and no key is
;; bound (see the end of the file).

(defun my/jira-url-from-authinfo ()
  "\"https://\" and the *.atlassian.net machine of ~/.authinfo, or nil.
The token's line already names the site, so the address stays out of this
repository.  No trailing slash: jira.el strips \"https://\" and looks the
rest up as that same machine to find the token."
  (require 'seq)
  (when-let* ((host (seq-find (lambda (host)
                                (and (stringp host) (string-suffix-p ".atlassian.net" host)))
                              (mapcar (lambda (entry) (plist-get entry :host))
                                      (auth-source-search :host t :max 100)))))
    (concat "https://" host)))

(defvar my/jira-url (my/jira-url-from-authinfo)
  "The Jira site, or nil on a machine whose ~/.authinfo has no Jira token.")

;; jira.el sets `jira-issues--loading-p' while a search runs and clears it only
;; in the request's callbacks.  A request that dies inside request.el (curl exit
;; 6 on 2026-10-05) never reaches them, and from then on every g and C-c J
;; skips the search silently, leaving the list empty until a restart.
(defun my/jira-issues-clear-loading-flag (&rest _)
  "Clear jira.el's in-flight flag so an explicit refresh always searches."
  (setq jira-issues--loading-p nil))

(with-eval-after-load 'jira-issues
  (advice-add 'jira-issues--refresh :before #'my/jira-issues-clear-loading-flag))

;;; Pieces of a row

(defconst my/jira-board-columns
  '(("Open" "Pending" "Validated Idea")
    ("Develop" "In Progress" "Develop pause" "On Hold")
    ("Ready for review" "Review")
    ("Ready for testing" "Testing" "Testing pause")
    ("Ready for merge")
    ("Deploy" "Ready for deploy")
    ("Done" "Closed" "Cancelled"))
  "Statuses by column of board 451, left to right.
The board's own settings are not readable over MCP, so this is the workflow
order as seen on 2026-10-05; if the board disagrees, fix it here.")

(defun my/jira-column (status)
  "Column of STATUS on the board, counting from 0 at the left; -1 if unknown."
  (or (seq-position my/jira-board-columns status
                    (lambda (column s) (member s column)))
      -1))

(defun my/jira-priority (name)
  "1 for \"P1 – Critical\" … 5 for \"P5 – Background\", 9 for anything else."
  (if (and name (string-match "\\`P\\([1-9]\\)" name))
      (string-to-number (match-string 1 name))
    9))

(defun my/jira-surname-first (name)
  "\"Aleksandr Stepanenko\" -> \"Stepanenko Aleksandr\"."
  (let ((words (split-string (or name "") " " t)))
    (if (cdr words)
        (string-join (cons (car (last words)) (butlast words)) " ")
      (or name ""))))

;; jira.el's own list (C-c J) shows and sorts assignees the same way.
(with-eval-after-load 'jira-utils
  (setf (alist-get :formatter (alist-get :assignee-name jira-issues-fields))
        #'my/jira-surname-first))

(defun my/jira-leaf-p (issue)
  "Non-nil unless ISSUE has subtasks or is a Bucket or an Epic.
Work and review on a task with subtasks happen in the subtasks."
  (let-alist issue
    (and (zerop (length .fields.subtasks))
         (<= (or .fields.issuetype.hierarchyLevel 0) 0))))

(defun my/jira-title (issue)
  "ISSUE's summary; a subtask's is followed by its parent's.
A subtask's own summary is often just \"Back\" or \"QA\"."
  (let-alist issue
    (if (eq .fields.issuetype.subtask t)
        (format "%s · %s" .fields.summary .fields.parent.fields.summary)
      .fields.summary)))

(defun my/jira-hours (seconds)
  "SECONDS as \"20m\" under an hour, \"1.5h\" from an hour on."
  (if (< seconds 3600)
      (format "%dm" (/ seconds 60))
    (format "%gh" (/ (round seconds 360) 10.0))))

(defun my/jira-progress (issue)
  "A bar of the time spent on ISSUE against its original estimate.
The bar turns red once the time spent passes the estimate."
  (let-alist issue
    (let* ((estimate .fields.timetracking.originalEstimateSeconds)
           (spent (or .fields.timetracking.timeSpentSeconds 0))
           (ratio (if (and estimate (> estimate 0)) (/ (float spent) estimate) 0))
           (cells (min 10 (round (* 10 ratio))))
           (bar (concat (make-string cells ?█) (make-string (- 10 cells) ?░))))
      (concat (if (> ratio 1) (propertize bar 'face 'error) bar)
              " " (if (> spent 0) (my/jira-hours spent) "—")
              "/" (if estimate (my/jira-hours estimate) "—")))))

(defun my/jira-returns (issue)
  "How many times ISSUE went from review or later back into work.
Needs the issue's changelog."
  (let ((review (my/jira-column "Review"))
        (count 0))
    (seq-doseq (history (let-alist issue .changelog.histories))
      (seq-doseq (item (alist-get 'items history))
        (let-alist item
          (when (and (equal .field "status")
                     (>= (my/jira-column .fromString) review)
                     (<= 0 (my/jira-column .toString) (1- review)))
            (setq count (1+ count))))))
    count))

(defun my/jira-board-order (a b)
  "Non-nil if issue A goes first: further right on the board, then higher priority."
  (let ((column-a (my/jira-column (let-alist a .fields.status.name)))
        (column-b (my/jira-column (let-alist b .fields.status.name))))
    (if (/= column-a column-b)
        (> column-a column-b)
      (< (my/jira-priority (let-alist a .fields.priority.name))
         (my/jira-priority (let-alist b .fields.priority.name))))))

;;; Whose move

;; An issue keeps its assignee through review and testing, so "assigned to me"
;; is not "my move": the list says who has to act and puts my moves first.
(defconst my/jira-moves
  '(("вернули" . me) ("нет ревьюера" . me) ("в работе" . me) ("пауза" . me)
    ("не начата" . me)
    ("ревью" . others) ("тест" . others) ("мерж" . others) ("деплой" . others))
  "What an issue of mine waits for, in the order the list shows them.")

(defun my/jira-time (string)
  "Seconds since the epoch for Jira's timestamp STRING, or nil."
  (when string
    (require 'iso8601)
    (float-time (encode-time (iso8601-parse string)))))

(defun my/jira-last-status-change (issue)
  "ISSUE's latest status change as (TIME FROM TO), nil if none.
Needs the issue's changelog."
  (let (latest)
    (seq-doseq (history (let-alist issue .changelog.histories))
      (seq-doseq (item (alist-get 'items history))
        (let-alist item
          (when (equal .field "status")
            (let ((time (my/jira-time (alist-get 'created history))))
              (when (or (null latest) (> time (car latest)))
                (setq latest (list time .fromString .toString))))))))
    latest))

(defun my/jira-move (issue)
  "What ISSUE, assigned to me, waits for: a label from `my/jira-moves'."
  (let-alist issue
    (let ((column (my/jira-column .fields.status.name))
          (review (my/jira-column "Review"))
          (change (my/jira-last-status-change issue)))
      (cond ((and change (< column review)
                  (>= (my/jira-column (nth 1 change)) review))
             "вернули")
            ((= column review)
             (if (zerop (length .fields.customfield_10093)) "нет ревьюера" "ревью"))
            ((= column (my/jira-column "Testing")) "тест")
            ((= column (my/jira-column "Ready for merge")) "мерж")
            ((= column (my/jira-column "Deploy")) "деплой")
            ((member .fields.status.name '("Develop pause" "On Hold")) "пауза")
            ((= column (my/jira-column "Develop")) "в работе")
            (t "не начата")))))

(defun my/jira-waiting-p (move)
  "Non-nil if MOVE, a label from `my/jira-moves', is someone else's."
  (eq (cdr (assoc move my/jira-moves)) 'others))

(defun my/jira-move-rank (move)
  "Position of MOVE in `my/jira-moves'."
  (seq-position my/jira-moves move (lambda (entry label) (equal (car entry) label))))

(defun my/jira-standing (issue)
  "Seconds ISSUE has stood in its current status, counted from creation if it
never moved."
  (- (float-time)
     (or (car (my/jira-last-status-change issue))
         (my/jira-time (let-alist issue .fields.created))
         (float-time))))

(defun my/jira-age (seconds)
  "SECONDS as \"3ч\" under a day, \"5д\" from a day on."
  (if (< seconds 86400)
      (format "%dч" (/ seconds 3600))
    (format "%dд" (/ seconds 86400))))

(defun my/jira-move-order (a b)
  "Non-nil if my issue A goes first.
My moves come first, in the order of `my/jira-moves', then by priority.
Issues waiting for others follow, the longest standing first."
  (let ((move-a (my/jira-move a))
        (move-b (my/jira-move b)))
    (cond ((and (my/jira-waiting-p move-a) (my/jira-waiting-p move-b))
           (> (my/jira-standing a) (my/jira-standing b)))
          ((not (equal move-a move-b))
           (< (my/jira-move-rank move-a) (my/jira-move-rank move-b)))
          (t (< (my/jira-priority (let-alist a .fields.priority.name))
                (my/jira-priority (let-alist b .fields.priority.name)))))))

;;; The two lists

;; The whole backlog assigned to me is over a hundred issues, almost all Open,
;; so the not-started ones count only in the current sprint.  Started ones
;; count wherever they are: on 2026-10-05 two of them sat in a future sprint.
(defconst my/jira-mine-jql
  (concat "project = DSHB AND assignee = currentUser()"
          " AND statusCategory != Done AND status != \"Ready for deploy\""
          " AND (sprint in openSprints() OR statusCategory = \"In Progress\")"))

;; cf[10093] is Reviewers.
(defconst my/jira-review-jql
  (concat "project = DSHB AND status in (\"Ready for review\", \"Review\")"
          " AND cf[10093] = currentUser()"
          " AND (assignee != currentUser() OR assignee is EMPTY)"))

(defun my/jira-cell (row name)
  "The cell of tabulated-list ROW under the column called NAME."
  (aref (cadr row)
        (seq-position tabulated-list-format name
                      (lambda (column n) (equal (car column) n)))))

(defun my/jira-sort-status (a b)
  "Tabulated-list predicate: order rows by their status's board column."
  (< (my/jira-column (my/jira-cell a "Статус"))
     (my/jira-column (my/jira-cell b "Статус"))))

(defun my/jira-sort-returns (a b)
  "Tabulated-list predicate: order rows by how often they came back."
  (< (string-to-number (my/jira-cell a "Возвр"))
     (string-to-number (my/jira-cell b "Возвр"))))

(defun my/jira-sort-move (a b)
  "Tabulated-list predicate: order rows as `my/jira-moves' lists the moves."
  (< (my/jira-move-rank (my/jira-cell a "Ход"))
     (my/jira-move-rank (my/jira-cell b "Ход"))))

(defun my/jira-age-hours (age)
  "Hours in an AGE cell such as \"3ч\" or \"5д\"."
  (* (string-to-number age) (if (string-suffix-p "д" age) 24 1)))

(defun my/jira-sort-age (a b)
  "Tabulated-list predicate: order rows by how long they have stood."
  (< (my/jira-age-hours (my/jira-cell a "Стоит"))
     (my/jira-age-hours (my/jira-cell b "Стоит"))))

(defun my/jira-mine-row (issue)
  "ISSUE as a line of the list of my issues."
  (let-alist issue
    (let ((priority (my/jira-priority .fields.priority.name))
          (move (my/jira-move issue)))
      (list .key (vector .key .fields.issuetype.name
                         (propertize move 'face (cond ((my/jira-waiting-p move) 'shadow)
                                                      ((member move '("вернули" "нет ревьюера"))
                                                       'warning)))
                         .fields.status.name
                         (my/jira-age (my/jira-standing issue))
                         (if (= priority 9) "—" (format "P%d" priority))
                         (my/jira-progress issue)
                         (my/jira-title issue))))))

(defun my/jira-review-row (issue)
  "ISSUE as a line of the list waiting for my review."
  (let-alist issue
    (list .key (vector .key .fields.issuetype.name .fields.status.name
                       (my/jira-surname-first .fields.assignee.displayName)
                       (number-to-string (my/jira-returns issue))
                       (my/jira-progress issue)
                       (my/jira-title issue)))))

(defvar-local my/jira-list-jql nil "JQL behind this buffer's list.")
(defvar-local my/jira-list-columns nil "Symbol whose value is this buffer's columns.")
(defvar-local my/jira-list-row nil "Function turning an issue into a row.")
(defvar-local my/jira-list-order nil "Predicate putting this buffer's issues in order.")
(defvar-local my/jira-list-expand nil "The search's expand parameter, if any.")
(defvar-local my/jira-list-sprint nil "Non-nil if this buffer shows the sprint line.")
(defvar-local my/jira-list-issues nil "The issues of Jira's last answer for this buffer.")

(define-derived-mode my/jira-list-mode tabulated-list-mode "Jira"
  "A list of Jira issues: RET details, C status, O browser, a agent-shell, g refresh."
  :interactive nil
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'my/jira-list-fetch nil t)
  (hl-line-mode)
  (tablist-minor-mode))

(defun my/jira-list-show ()
  "Open the issue at point in jira.el's detail view."
  (interactive)
  (jira-detail-show-issue (tabulated-list-get-id)))

(defun my/jira-list-browse ()
  "Open the issue at point in the browser."
  (interactive)
  (jira-actions-open-issue (tabulated-list-get-id)))

(define-key my/jira-list-mode-map (kbd "RET") #'my/jira-list-show)
(define-key my/jira-list-mode-map "C" #'jira-actions-change-issue-menu)
(define-key my/jira-list-mode-map "O" #'my/jira-list-browse)

(defun my/jira-list-redraw ()
  "Rebuild this buffer's columns and rows from Jira's last answer.
Both come from the code as it is now, so a list opened before this file was
reloaded cannot end up with rows of one version under columns of another:
on 2026-10-06 that broke every g with \"Args out of range\"."
  (setq tabulated-list-format (symbol-value my/jira-list-columns))
  (tabulated-list-init-header)
  (setq tabulated-list-entries
        (mapcar my/jira-list-row
                (sort (seq-filter #'my/jira-leaf-p my/jira-list-issues)
                      my/jira-list-order))))

(defun my/jira-list-fetch ()
  "Search this buffer's JQL and redraw the list when the answer comes.
Runs from `tabulated-list-revert-hook', just before g prints the list, so it
redraws from the last answer first."
  (my/jira-list-redraw)
  (let ((buffer (current-buffer)))
    (jira-api-search
     :params `(("jql" . ,my/jira-list-jql)
               ("maxResults" . 100)
               ("fields" . ,(concat "summary,status,issuetype,priority,subtasks,parent,"
                                    "assignee,timetracking,created,customfield_10093"))
               ,@(when my/jira-list-expand `(("expand" . ,my/jira-list-expand))))
     :callback (lambda (data _response)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq my/jira-list-issues (append (alist-get 'issues data) nil))
                     (my/jira-list-redraw)
                     (tabulated-list-print t))))
     :errback (lambda (&rest _) (message "Jira: %s did not load" (buffer-name buffer))))
    (when my/jira-list-sprint
      (my/jira-sprint-header buffer))))

(defun my/jira-list (name jql columns row order &optional expand sprint)
  "Buffer NAME listing JQL under COLUMNS (a symbol), ROW making each line.
ORDER sorts the issues, EXPAND goes to the search, SPRINT non-nil puts the
sprint line on top."
  (with-current-buffer (get-buffer-create name)
    (my/jira-list-mode)
    (setq my/jira-list-jql jql
          my/jira-list-columns columns
          my/jira-list-row row
          my/jira-list-order order
          my/jira-list-expand expand
          my/jira-list-sprint sprint
          ;; tabulated-list-entries, -format and -sort-key are permanent-local:
          ;; the mode call above keeps the previous rows and sorting, and
          ;; printing old rows under new columns failed on every C-c j on
          ;; 2026-10-06, before the request that would have replaced them.
          tabulated-list-sort-key nil)
    (my/jira-list-redraw)
    (tabulated-list-print)
    (my/jira-list-fetch)
    (current-buffer)))

(defconst my/jira-mine-columns
  [("Ключ" 10 t) ("Тип" 10 t) ("Ход" 12 my/jira-sort-move)
   ("Статус" 17 my/jira-sort-status) ("Стоит" 5 my/jira-sort-age)
   ("Приор" 5 t) ("Время" 22 nil) ("Задача" 40 t)]
  "Columns of the list of my issues, in the order `my/jira-mine-row' fills them.")

(defconst my/jira-review-columns
  [("Ключ" 10 t) ("Тип" 10 t) ("Статус" 17 my/jira-sort-status)
   ("Исполнитель" 20 t) ("Возвр" 5 my/jira-sort-returns)
   ("Время" 22 nil) ("Задача" 40 t)]
  "Columns of the review list, in the order `my/jira-review-row' fills them.")

;;; The sprint above the lists

(defconst my/jira-board-id 451
  "Board whose active sprint heads the list of my issues.")

(defun my/jira-noon (time)
  "Local noon of the day TIME falls on, in seconds since the epoch."
  (pcase-let ((`(,_ ,_ ,_ ,day ,month ,year) (decode-time time)))
    (float-time (encode-time 0 0 12 day month year))))

(defun my/jira-workdays (from to)
  "Weekdays from the day of FROM to the day of TO, both counted.
FROM and TO are seconds since the epoch."
  (let ((day (my/jira-noon from))
        (last (my/jira-noon to))
        (count 0))
    ;; Half a day of slack: a daylight-saving change shifts noon by an hour.
    (while (< day (+ last 43200))
      (unless (memq (decoded-time-weekday (decode-time day)) '(0 6))
        (setq count (1+ count)))
      (setq day (+ day 86400)))
    count))

(defun my/jira-sprint-days (sprint)
  "SPRINT's first and last day, as seconds since the epoch.
Read from the name (\"28.09 - 09.10\") first, as the wap-health digest does:
Jira's endDate is the closing Monday morning, its startDate the planning hour."
  (let-alist sprint
    (if (and .name (string-match "\\([0-9]+\\)\\.\\([0-9]+\\) *- *\\([0-9]+\\)\\.\\([0-9]+\\)" .name))
        (let ((year (decoded-time-year (decode-time)))
              (part (lambda (n) (string-to-number (match-string n .name)))))
          (list (float-time (encode-time 0 0 12 (funcall part 1) (funcall part 2) year))
                (float-time (encode-time 0 0 12 (funcall part 3) (funcall part 4) year))))
      (list (my/jira-time .startDate) (my/jira-time .endDate)))))

(defun my/jira-sprint-line (sprint)
  "One line about SPRINT: its name, today's working day of all, its goal."
  (let-alist sprint
    (pcase-let ((`(,first ,last) (my/jira-sprint-days sprint))
                (goal (replace-regexp-in-string
                       "[ \t]*\n[\n \t]*" " · " (string-trim (or .goal "цели нет")))))
      (propertize (format " %s · день %d из %d · %s" .name
                          (my/jira-workdays first (min (float-time) last))
                          (my/jira-workdays first last)
                          goal)
                  'help-echo goal))))

(defun my/jira-sprint-header (buffer)
  "Show the active sprint of `my/jira-board-id' on BUFFER's top line."
  (jira-api-search
   :params '(("jql" . "project = DSHB AND sprint in openSprints()")
             ("maxResults" . 10)
             ("fields" . "customfield_10020"))
   :callback (lambda (data _response)
               (when-let* (((buffer-live-p buffer))
                           (sprint (seq-some
                                    (lambda (issue)
                                      (seq-find (lambda (s)
                                                  (and (equal (alist-get 'state s) "active")
                                                       (eql (alist-get 'boardId s) my/jira-board-id)))
                                                (let-alist issue .fields.customfield_10020)))
                                    (alist-get 'issues data))))
                 (with-current-buffer buffer
                   ;; The tab line sits above tabulated-list's header line and,
                   ;; unlike buffer text, survives every redraw of the list.
                   (setq-local tab-line-format (my/jira-sprint-line sprint))
                   (force-mode-line-update))))
   :errback (lambda (&rest _) (message "Jira: the sprint did not load"))))

(defun my/jira-dashboard ()
  "Show the issues assigned to me and, below them, the ones waiting for my review."
  (interactive)
  (require 'jira)
  (let ((mine (my/jira-list "*Jira: на мне*" my/jira-mine-jql 'my/jira-mine-columns
                            #'my/jira-mine-row #'my/jira-move-order "changelog" t))
        (review (my/jira-list "*Jira: ревью*" my/jira-review-jql 'my/jira-review-columns
                              #'my/jira-review-row #'my/jira-board-order "changelog")))
    (pop-to-buffer-same-window mine)
    (display-buffer review '((display-buffer-reuse-window display-buffer-below-selected)))))

;;; An agent-shell per issue

(defvar my/jira-agent-directory "~/src/backend-dashboard/"
  "Where `my/jira-agent-shell' starts Claude unless given a prefix argument.")

(defvar my/jira-agent-shells (make-hash-table :test #'equal)
  "Issue key -> its agent-shell buffer, so a second press returns to it.")

(defconst my/jira-agent-display
  '((display-buffer-reuse-window display-buffer-pop-up-window)
    (inhibit-same-window . t))
  "Show an issue's shell beside the list, not in place of it.")

(defun my/jira-summary-at-point (key)
  "KEY's summary as this buffer shows it, or nil if it does not."
  (cond ((derived-mode-p 'my/jira-list-mode)
         (let ((entry (tabulated-list-get-entry)))
           (aref entry (1- (length entry)))))
        ((hash-table-p (bound-and-true-p jira-issues-key-summary-map))
         (gethash key jira-issues-key-summary-map))))

(defun my/jira-agent-shell (key &optional directory)
  "Open a Claude agent-shell about Jira issue KEY, working in DIRECTORY.
The issue's key, summary and link wait in the prompt for a question;
nothing is sent.  Pressed again on the same issue, returns to its shell.
With a prefix argument, asks for DIRECTORY instead of using
`my/jira-agent-directory'."
  (interactive
   (list (jira-utils-marked-item)
         (if current-prefix-arg
             (read-directory-name "Агент в каталоге: " "~/src/")
           my/jira-agent-directory)))
  (unless key
    (user-error "Курсор не на задаче"))
  (require 'agent-shell)
  (let ((shell (gethash key my/jira-agent-shells)))
    (if (buffer-live-p shell)
        (pop-to-buffer shell my/jira-agent-display)
      (let* ((summary (my/jira-summary-at-point key))
             (text (format "Задача %s%s\n%s/browse/%s\n\n" key
                           (if summary (concat " — " summary) "")
                           jira-base-url key))
             (default-directory (file-name-as-directory (expand-file-name directory)))
             ;; Always a fresh session: the default strategy would first ask
             ;; which earlier session of this project to resume.
             (agent-shell-session-strategy 'new)
             (agent-shell-display-action my/jira-agent-display))
        (setq shell (agent-shell-start
                     :config (agent-shell-anthropic-make-claude-code-config)))
        (puthash key shell my/jira-agent-shells)
        ;; If the shell has no prompt yet, agent-shell waits for one itself
        ;; and returns nil.
        (when-let* ((end (alist-get :end (agent-shell-insert
                                          :text text :no-focus t :shell-buffer shell)))
                    (window (get-buffer-window shell)))
          (set-window-point window end))))))

(define-key my/jira-list-mode-map "a" #'my/jira-agent-shell)

;;; Time on an issue: the org clock in work.org, then a Jira worklog

;; The clock runs on today's entry for the issue under today's heading of
;; work.org, the place my/org-standup and my/org-timesheet read, with the key
;; in :JIRA:.  INPROCESS and PAUSE start and stop it through
;; `my/org-auto-clock-on-state-change', so the Focus Shield pauses it too.
;; :JIRA_SENT: on an entry holds the minutes of it already sent to Jira.

(defvar my/jira-org-file "~/src/org/work.org"
  "The org file whose daily headings hold the clocked issues.")

(defun my/jira-org-entry (key title)
  "Marker of today's entry for issue KEY, made with TITLE if missing."
  (let ((day (my/org-ensure-daily-heading)))
    (with-current-buffer (marker-buffer day)
      (org-with-wide-buffer
       (goto-char day)
       (let ((end (save-excursion (org-end-of-subtree t t) (point)))
             (found nil))
         (while (and (not found) (re-search-forward "^\\*\\* " end t))
           (when (equal (org-entry-get (point) "JIRA") key)
             (setq found (point-marker))))
         (or found
             (progn
               (goto-char end)
               (unless (bolp) (insert "\n"))
               (insert (format "** TODO %s %s\n" key (or title "")))
               (forward-line -1)
               (org-entry-put (point) "JIRA" key)
               (point-marker))))))))

(defun my/jira-clocked-key ()
  "The :JIRA: key of the entry the org clock runs on, or nil."
  (and (org-clocking-p) (org-entry-get org-clock-marker "JIRA")))

(defun my/jira-clock-toggle (key)
  "Start the clock on issue KEY, or stop it if it already runs there.
Whatever else the clock ran on is paused first."
  (interactive (list (jira-utils-marked-item)))
  (unless key
    (user-error "Курсор не на задаче"))
  (require 'org-clock)
  (let ((running (my/jira-clocked-key)))
    (when (org-clocking-p)
      (org-with-point-at org-clock-marker (org-todo "PAUSE")))
    (if (equal running key)
        (message "Jira: часы на %s остановлены" key)
      (org-with-point-at (my/jira-org-entry key (my/jira-summary-at-point key))
        ;; An entry left INPROCESS (a restart, a Focus Shield pause) would
        ;; not change state, and the hook would not clock it in.
        (if (equal (org-get-todo-state) "INPROCESS")
            (org-clock-in)
          (org-todo "INPROCESS")))
      (message "Jira: часы идут на %s" key))))

(defun my/jira-unsent-minutes (key)
  "Minutes clocked on issue KEY and not sent to Jira yet."
  (with-current-buffer (find-file-noselect my/jira-org-file)
    (let ((total 0))
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (setq total (+ total (- (org-clock-sum-current-item)
                                  (string-to-number
                                   (or (org-entry-get (point) "JIRA_SENT") "0"))))))
        (format "JIRA=\"%s\"" key) 'file))
      total)))

(defun my/jira-mark-sent (key)
  "Record that everything clocked on issue KEY is now in Jira."
  (with-current-buffer (find-file-noselect my/jira-org-file)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (org-entry-put (point) "JIRA_SENT"
                       (number-to-string (org-clock-sum-current-item))))
      (format "JIRA=\"%s\"" key) 'file))
    (save-buffer)))

(defun my/jira-stop-clock-on (key)
  "Pause the clock if it runs on issue KEY, so its time is counted."
  (when (equal (my/jira-clocked-key) key)
    (org-with-point-at org-clock-marker (org-todo "PAUSE"))))

(defun my/jira-send-time (key)
  "Send the time clocked on issue KEY since the last sending as a worklog."
  (interactive (list (jira-utils-marked-item)))
  (require 'org-clock)
  (my/jira-stop-clock-on key)
  (let ((minutes (my/jira-unsent-minutes key)))
    (if (< minutes 1)
        (message "Jira: на %s нового времени нет" key)
      (jira-api-call
       "POST" (format "issue/%s/worklog" key)
       :data `(("timeSpentSeconds" . ,(* 60 minutes)))
       :callback (lambda (&rest _)
                   (my/jira-mark-sent key)
                   (message "Jira: на %s записано %s" key (my/minutes-to-hh:mm minutes)))))))

(defun my/jira-to-review (key)
  "Move issue KEY to review, logging its unsent clocked time in the same request.
The review transition will not go without time spent, and jira.el's C has
nowhere to put it.  With nothing clocked, asks for the time."
  (interactive (list (jira-utils-marked-item)))
  (require 'org-clock)
  (my/jira-stop-clock-on key)
  (let* ((minutes (my/jira-unsent-minutes key))
         (spent (if (>= minutes 1)
                    (format "%dm" minutes)
                  (read-string (format "На %s ничего не начислено. Потрачено (30m, 1h): " key)))))
    (jira-api-call
     "GET" (format "issue/%s/transitions" key)
     :callback
     (lambda (data _response)
       (if-let* ((transition (seq-find (lambda (transition)
                                         (member (let-alist transition .to.name)
                                                 '("Ready for review" "Review")))
                                       (alist-get 'transitions data))))
           (jira-api-call
            "POST" (format "issue/%s/transitions" key)
            :data `(("transition" . (("id" . ,(alist-get 'id transition))))
                    ("update" . (("worklog" . [(("add" . (("timeSpent" . ,spent))))]))))
            ;; The answer is an empty 204, which json-read would call an error.
            :parser #'ignore
            :callback (lambda (&rest _)
                        (my/jira-mark-sent key)
                        (message "Jira: %s ушла на ревью, записано %s" key spent)))
         (message "Jira: из текущего статуса %s на ревью не перевести" key))))))

(define-key my/jira-list-mode-map "i" #'my/jira-clock-toggle)
(define-key my/jira-list-mode-map "w" #'my/jira-send-time)
(define-key my/jira-list-mode-map "r" #'my/jira-to-review)
(with-eval-after-load 'jira-issues
  (define-key jira-issues-mode-map "a" #'my/jira-agent-shell))
(with-eval-after-load 'jira-detail
  (define-key jira-detail-mode-map "a" #'my/jira-agent-shell))

;;; Wiring, only where there is a Jira token

;; :custom, not :config: C-c J autoloads `jira-issues' from jira-issues.el,
;; which never loads jira.el, so a :config block would never run and every
;; request would go to an empty URL with empty credentials.
(when my/jira-url
  (use-package jira
    :bind (("C-c j" . my/jira-dashboard)
           ("C-c J" . jira-issues))
    :hook (jira-issues-mode . hl-line-mode)
    :custom
    (jira-base-url my/jira-url)
    ;; A sprint without other people's subtasks is ~50 issues; the default
    ;; page of 30 would split it in two.
    (jira-issues-max-results 100)
    ;; Parent Key, because a subtask's own summary is often just "Back" or "QA".
    (jira-issues-table-fields
     '(:key :issue-type-name :status-name :assignee-name :parent-key :summary))))

(provide 'init-jira)
