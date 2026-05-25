;;; init-keystroke-log.el --- Keystroke analytics -*- lexical-binding: t -*-
;; Frequencies, bigram timing, typo detection, chord/mode/layout context

(defvar my/klog-file (expand-file-name "keystroke-log.csv" user-emacs-directory))
(defvar my/klog-last-key nil)
(defvar my/klog-last-time nil)
(defvar my/klog-buffer nil)
(defvar my/klog-flush-interval 300)
(defvar my/klog-timer nil)
(defvar my/klog--layout nil)
(defvar my/klog--layout-timer nil)
(defvar my/klog--layout-refresh-interval 5)

(defconst my/klog-header
  "timestamp_ms,event,key,prev_key,interval_ms,command,major_mode,layout,input_method\n")

(defconst my/klog--noise-event-syms
  '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5
    down-mouse-1 down-mouse-2 down-mouse-3
    drag-mouse-1 drag-mouse-2 drag-mouse-3
    double-mouse-1 double-mouse-2 double-mouse-3
    triple-mouse-1 triple-mouse-2 triple-mouse-3
    wheel-up wheel-down
    double-wheel-up double-wheel-down
    triple-wheel-up triple-wheel-down
    mouse-movement mouse-set-point
    switch-frame select-window
    focus-in focus-out
    help-echo))

(defun my/klog--refresh-layout ()
  "Refresh cached macOS keyboard layout name."
  (condition-case nil
      (let ((s (shell-command-to-string
                "defaults read com.apple.HIToolbox AppleSelectedInputSources 2>/dev/null | awk -F '= ' '/KeyboardLayout Name/ {gsub(/[\";]/, \"\", $2); gsub(/^[ \\t]+|[ \\t]+$/, \"\", $2); print $2; exit}'")))
        (setq my/klog--layout (string-trim s)))
    (error nil)))

(defun my/klog--migrate-old-file ()
  "If existing CSV uses old schema, rotate it aside so we start fresh."
  (when (file-exists-p my/klog-file)
    (let ((first-line
           (with-temp-buffer
             (insert-file-contents my/klog-file nil 0 256)
             (buffer-substring (point-min)
                               (min (point-max) (line-end-position))))))
      (unless (string-prefix-p (substring my/klog-header 0 -1) first-line)
        (let ((archived (format "%s.%s.old"
                                my/klog-file
                                (format-time-string "%Y%m%d-%H%M%S"))))
          (rename-file my/klog-file archived)
          (message "Keystroke log: rotated old-schema file to %s" archived))))))

(defun my/klog-init-buffer ()
  (unless my/klog-buffer
    (my/klog--migrate-old-file)
    (setq my/klog-buffer (generate-new-buffer " *keystroke-log*"))
    (with-current-buffer my/klog-buffer
      (unless (file-exists-p my/klog-file)
        (insert my/klog-header)))))

(defun my/klog-flush ()
  (when (and my/klog-buffer (buffer-live-p my/klog-buffer))
    (with-current-buffer my/klog-buffer
      (when (> (buffer-size) 0)
        (append-to-file (point-min) (point-max) my/klog-file)
        (erase-buffer)))))

(defun my/klog--csv-sanitize (s)
  "Make string safe for one CSV cell."
  (cond ((or (null s) (string= s "")) "")
        ((string= s ",") "COMMA")
        ((string= s "\n") "RET")
        ((string= s " ") "SPC")
        ((string= s "\t") "TAB")
        ((string-match-p "[,\n\"]" s)
         (concat "\"" (replace-regexp-in-string "\"" "\"\"" s) "\""))
        (t s)))

(defun my/klog--is-noise-event-p (keys)
  "Return non-nil if KEYS represents a mouse/scroll/focus event we skip."
  (or (null keys)
      (= (length keys) 0)
      (let ((ev (aref keys 0)))
        (cond
         ((symbolp ev) (memq ev my/klog--noise-event-syms))
         ((consp ev) (memq (car-safe ev) my/klog--noise-event-syms))
         (t nil)))))

(defun my/klog-record ()
  (when (and this-command
             (not (derived-mode-p 'eshell-mode 'term-mode)))
    (let ((keys (this-command-keys-vector)))
      (unless (my/klog--is-noise-event-p keys)
        (my/klog-init-buffer)
        (let* ((now (float-time))
               (now-ms (truncate (* now 1000)))
               (interval (if my/klog-last-time
                             (truncate (* (- now my/klog-last-time) 1000))
                           -1))
               (is-backspace (memq this-command
                                   '(delete-backward-char
                                     backward-delete-char-untabify)))
               (is-self-insert (eq this-command 'self-insert-command))
               (event (cond (is-backspace "backspace")
                            (is-self-insert "char")
                            (t "chord")))
               (raw-key (cond (is-backspace "BS")
                              (is-self-insert
                               (let ((ev (aref keys (1- (length keys)))))
                                 (if (characterp ev) (string ev) (format "%S" ev))))
                              (t (key-description keys))))
               (key (my/klog--csv-sanitize raw-key))
               (prev (my/klog--csv-sanitize (or my/klog-last-key "")))
               (command (symbol-name this-command))
               (mode (symbol-name major-mode))
               (layout (my/klog--csv-sanitize (or my/klog--layout "")))
               (im (my/klog--csv-sanitize (or current-input-method ""))))
          (with-current-buffer my/klog-buffer
            (goto-char (point-max))
            (insert (format "%d,%s,%s,%s,%d,%s,%s,%s,%s\n"
                            now-ms event key prev interval
                            command mode layout im)))
          (setq my/klog-last-key (unless is-backspace key))
          (setq my/klog-last-time now))))))

(defun my/klog-start ()
  "Start keystroke logging."
  (interactive)
  (add-hook 'post-command-hook #'my/klog-record)
  (setq my/klog-timer
        (run-with-timer my/klog-flush-interval my/klog-flush-interval
                        #'my/klog-flush))
  (setq my/klog--layout-timer
        (run-with-idle-timer my/klog--layout-refresh-interval t
                             #'my/klog--refresh-layout))
  (run-with-idle-timer 1 nil #'my/klog--refresh-layout)
  (message "Keystroke logging started"))

(defun my/klog-stop ()
  "Stop keystroke logging and flush."
  (interactive)
  (remove-hook 'post-command-hook #'my/klog-record)
  (when my/klog-timer (cancel-timer my/klog-timer) (setq my/klog-timer nil))
  (when my/klog--layout-timer
    (cancel-timer my/klog--layout-timer)
    (setq my/klog--layout-timer nil))
  (my/klog-flush)
  (message "Keystroke logging stopped, data saved to %s" my/klog-file))

(my/klog-start)
(add-hook 'kill-emacs-hook #'my/klog-flush)

;; --- Analysis ---

(defun my/klog--parse-csv-line (line)
  "Split LINE into fields, respecting quoted values containing commas."
  (let ((fields nil) (start 0) (i 0) (in-quote nil) (len (length line)))
    (while (< i len)
      (let ((c (aref line i)))
        (cond ((and (not in-quote) (eq c ?,))
               (push (substring line start i) fields)
               (setq start (1+ i)))
              ((eq c ?\")
               (setq in-quote (not in-quote)))))
      (setq i (1+ i)))
    (push (substring line start) fields)
    (nreverse fields)))

(defun my/klog--read-csv ()
  "Read log CSV into list of records.  Tolerates 5+ field rows for back-compat."
  (let (rows)
    (with-temp-buffer
      (insert-file-contents my/klog-file)
      (forward-line 1) ; skip header
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (fields (my/klog--parse-csv-line line)))
          (when (>= (length fields) 5)
            (push fields rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun my/klog-typo-report ()
  "Show top bigrams that lead to backspace (likely typos)."
  (interactive)
  (let ((typos (make-hash-table :test 'equal))
        (rows (my/klog--read-csv))
        (prev-event nil) (prev-char nil) (prev-prev-char nil))
    (dolist (row rows)
      (let ((event (nth 1 row))
            (char (nth 2 row))
            (pchar (nth 3 row)))
        (when (and (string= event "backspace")
                   prev-char prev-prev-char
                   (string= prev-event "char"))
          (let ((bigram (concat prev-prev-char " → " prev-char)))
            (puthash bigram (1+ (gethash bigram typos 0)) typos)))
        (setq prev-prev-char (if (string= event "char") prev-char nil))
        (setq prev-char (if (string= event "char") char nil))
        (setq prev-event event)))
    (my/klog--show-hash-report typos "Typo Report: bigrams followed by backspace")))

(defun my/klog-char-freq-report ()
  "Show character frequency report."
  (interactive)
  (let ((freq (make-hash-table :test 'equal))
        (rows (my/klog--read-csv)))
    (dolist (row rows)
      (when (string= (nth 1 row) "char")
        (let ((c (nth 2 row)))
          (puthash c (1+ (gethash c freq 0)) freq))))
    (my/klog--show-hash-report freq "Character Frequency Report")))

(defun my/klog-bigram-speed-report ()
  "Show average typing speed between character pairs (ms)."
  (interactive)
  (let ((bigram-times (make-hash-table :test 'equal))
        (rows (my/klog--read-csv)))
    (dolist (row rows)
      (when (and (string= (nth 1 row) "char")
                 (not (string= (nth 3 row) ""))
                 (> (string-to-number (nth 4 row)) 0)
                 (< (string-to-number (nth 4 row)) 2000)) ; ignore pauses > 2s
        (let ((bigram (concat (nth 3 row) " → " (nth 2 row)))
              (ms (string-to-number (nth 4 row))))
          (puthash bigram
                   (cons ms (gethash bigram bigram-times nil))
                   bigram-times))))
    ;; Convert to averages
    (let ((avg-table (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                 (when (>= (length v) 3) ; need at least 3 samples
                   (puthash k (/ (apply #'+ v) (length v)) avg-table)))
               bigram-times)
      (my/klog--show-hash-report avg-table "Bigram Speed Report (avg ms, lower=faster)"
                                 'ascending))))

(defun my/klog-chord-freq-report ()
  "Show frequency of chords (keys with modifiers) by command."
  (interactive)
  (let ((freq (make-hash-table :test 'equal))
        (rows (my/klog--read-csv)))
    (dolist (row rows)
      (when (and (>= (length row) 6) (string= (nth 1 row) "chord"))
        (let ((entry (format "%s [%s]" (nth 2 row) (nth 5 row))))
          (puthash entry (1+ (gethash entry freq 0)) freq))))
    (my/klog--show-hash-report freq "Chord Frequency Report: key [command]")))

(defun my/klog-mode-distribution-report ()
  "Show how typing distributes across major modes."
  (interactive)
  (let ((dist (make-hash-table :test 'equal))
        (rows (my/klog--read-csv)))
    (dolist (row rows)
      (when (>= (length row) 7)
        (let ((mode (nth 6 row)))
          (puthash mode (1+ (gethash mode dist 0)) dist))))
    (my/klog--show-hash-report dist "Major-mode Distribution")))

(defun my/klog-layout-distribution-report ()
  "Show how typing distributes across macOS keyboard layouts."
  (interactive)
  (let ((dist (make-hash-table :test 'equal))
        (rows (my/klog--read-csv)))
    (dolist (row rows)
      (when (>= (length row) 8)
        (let ((layout (or (nth 7 row) "")))
          (when (string= layout "") (setq layout "(unknown)"))
          (puthash layout (1+ (gethash layout dist 0)) dist))))
    (my/klog--show-hash-report dist "macOS Layout Distribution")))

(defun my/klog--show-hash-report (hash title &optional sort-order)
  "Display hash table as sorted report in a buffer."
  (let ((items nil))
    (maphash (lambda (k v) (push (cons k v) items)) hash)
    (setq items (sort items (lambda (a b)
                              (if (eq sort-order 'ascending)
                                  (< (cdr a) (cdr b))
                                (> (cdr a) (cdr b))))))
    (with-current-buffer (get-buffer-create "*Keystroke Report*")
      (erase-buffer)
      (insert (format "=== %s ===\n\n" title))
      (insert (format "%-40s %s\n" "Key/Bigram" "Count/ms"))
      (insert (make-string 55 ?-) "\n")
      (dolist (item (seq-take items 50))
        (insert (format "%-40s %d\n" (car item) (cdr item))))
      (insert (format "\nTotal entries: %d\n" (length items)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'init-keystroke-log)
