;; Keystroke analytics: frequencies, bigram timing, typo detection

(defvar my/klog-file (expand-file-name "keystroke-log.csv" user-emacs-directory))
(defvar my/klog-last-char nil)
(defvar my/klog-last-time nil)
(defvar my/klog-second-last-char nil)
(defvar my/klog-buffer nil)
(defvar my/klog-flush-interval 300)
(defvar my/klog-timer nil)

(defun my/klog-init-buffer ()
  (unless my/klog-buffer
    (setq my/klog-buffer (generate-new-buffer " *keystroke-log*"))
    (with-current-buffer my/klog-buffer
      (unless (file-exists-p my/klog-file)
        (insert "timestamp_ms,event,char,prev_char,interval_ms\n")))))

(defun my/klog-flush ()
  (when (and my/klog-buffer (buffer-live-p my/klog-buffer))
    (with-current-buffer my/klog-buffer
      (when (> (buffer-size) 0)
        (append-to-file (point-min) (point-max) my/klog-file)
        (erase-buffer)))))

(defun my/klog-record ()
  (when (and (not (derived-mode-p 'vterm-mode 'eshell-mode 'term-mode))
             (or (eq this-command 'self-insert-command)
                 (eq this-command 'delete-backward-char)
                 (eq this-command 'backward-delete-char-untabify)))
    (my/klog-init-buffer)
    (let* ((now (float-time))
           (now-ms (truncate (* now 1000)))
           (interval (if my/klog-last-time
                         (truncate (* (- now my/klog-last-time) 1000))
                       -1))
           (is-backspace (or (eq this-command 'delete-backward-char)
                             (eq this-command 'backward-delete-char-untabify)))
           (char (if is-backspace
                     "BS"
                   (let ((keys (this-command-keys)))
                     (if (stringp keys) keys
                       (string (aref keys 0))))))
           (prev (or my/klog-last-char "")))
      ;; Sanitize for CSV
      (when (string= char ",") (setq char "COMMA"))
      (when (string= char "\n") (setq char "RET"))
      (when (string= char " ") (setq char "SPC"))
      (when (string= char "\t") (setq char "TAB"))
      (when (string= prev ",") (setq prev "COMMA"))
      (when (string= prev "\n") (setq prev "RET"))
      (when (string= prev " ") (setq prev "SPC"))
      (when (string= prev "\t") (setq prev "TAB"))
      (with-current-buffer my/klog-buffer
        (goto-char (point-max))
        (insert (format "%d,%s,%s,%s,%d\n"
                        now-ms
                        (if is-backspace "backspace" "char")
                        char prev interval)))
      ;; Track state for bigrams and typos
      (setq my/klog-second-last-char my/klog-last-char)
      (setq my/klog-last-char (unless is-backspace char))
      (setq my/klog-last-time now))))

(defun my/klog-start ()
  "Start keystroke logging."
  (interactive)
  (add-hook 'post-command-hook #'my/klog-record)
  (setq my/klog-timer
        (run-with-timer my/klog-flush-interval my/klog-flush-interval #'my/klog-flush))
  (message "Keystroke logging started"))

(defun my/klog-stop ()
  "Stop keystroke logging and flush."
  (interactive)
  (remove-hook 'post-command-hook #'my/klog-record)
  (when my/klog-timer (cancel-timer my/klog-timer) (setq my/klog-timer nil))
  (my/klog-flush)
  (message "Keystroke logging stopped, data saved to %s" my/klog-file))

(my/klog-start)
(add-hook 'kill-emacs-hook #'my/klog-flush)

;; --- Analysis ---

(defun my/klog--read-csv ()
  "Read log CSV into list of records."
  (let (rows)
    (with-temp-buffer
      (insert-file-contents my/klog-file)
      (forward-line 1) ; skip header
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (fields (split-string line ",")))
          (when (= (length fields) 5)
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
      (insert (format "%-20s %s\n" "Key/Bigram" "Count/ms"))
      (insert (make-string 35 ?-) "\n")
      (dolist (item (seq-take items 50))
        (insert (format "%-20s %d\n" (car item) (cdr item))))
      (insert (format "\nTotal entries: %d\n" (length items)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'init-keystroke-log)
