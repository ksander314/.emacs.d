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

(defun my/jira-mine-row (issue)
  "ISSUE as a line of the list of my issues."
  (let-alist issue
    (let ((priority (my/jira-priority .fields.priority.name)))
      (list .key (vector .key .fields.issuetype.name .fields.status.name
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
(defvar-local my/jira-list-row nil "Function turning an issue into a row.")
(defvar-local my/jira-list-expand nil "The search's expand parameter, if any.")

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

(defun my/jira-list-fetch ()
  "Search this buffer's JQL and redraw the list when the answer comes."
  (let ((buffer (current-buffer)))
    (jira-api-search
     :params `(("jql" . ,my/jira-list-jql)
               ("maxResults" . 100)
               ("fields" . "summary,status,issuetype,priority,subtasks,parent,assignee,timetracking")
               ,@(when my/jira-list-expand `(("expand" . ,my/jira-list-expand))))
     :callback (lambda (data _response)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq tabulated-list-entries
                           (mapcar my/jira-list-row
                                   (sort (seq-filter #'my/jira-leaf-p (alist-get 'issues data))
                                         #'my/jira-board-order)))
                     (tabulated-list-print t))))
     :errback (lambda (&rest _) (message "Jira: %s did not load" (buffer-name buffer))))))

(defun my/jira-list (name jql format row &optional expand)
  "Buffer NAME listing JQL in columns FORMAT, ROW making each line."
  (with-current-buffer (get-buffer-create name)
    (my/jira-list-mode)
    (setq my/jira-list-jql jql
          my/jira-list-row row
          my/jira-list-expand expand
          tabulated-list-format format)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (my/jira-list-fetch)
    (current-buffer)))

(defconst my/jira-mine-columns
  [("Ключ" 10 t) ("Тип" 10 t) ("Статус" 17 my/jira-sort-status)
   ("Приор" 5 t) ("Время" 22 nil) ("Задача" 40 t)]
  "Columns of the list of my issues, in the order `my/jira-mine-row' fills them.")

(defconst my/jira-review-columns
  [("Ключ" 10 t) ("Тип" 10 t) ("Статус" 17 my/jira-sort-status)
   ("Исполнитель" 20 t) ("Возвр" 5 my/jira-sort-returns)
   ("Время" 22 nil) ("Задача" 40 t)]
  "Columns of the review list, in the order `my/jira-review-row' fills them.")

(defun my/jira-dashboard ()
  "Show the issues assigned to me and, below them, the ones waiting for my review."
  (interactive)
  (require 'jira)
  (let ((mine (my/jira-list "*Jira: на мне*" my/jira-mine-jql
                            my/jira-mine-columns #'my/jira-mine-row))
        (review (my/jira-list "*Jira: ревью*" my/jira-review-jql
                              my/jira-review-columns #'my/jira-review-row "changelog")))
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
