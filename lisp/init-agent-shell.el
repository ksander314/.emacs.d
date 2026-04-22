;;; init-agent-shell.el --- Agent-shell configuration -*- lexical-binding: t -*-
(use-package agent-shell
  :ensure t
  :bind ("C-c A" . agent-shell)
  :config
  (setq agent-shell-anthropic-claude-acp-command
        '("claude-agent-acp" "--effort" "max"))
  (unless (executable-find "claude-agent-acp")
    (message "claude-agent-acp not found; run: npm install -g @zed-industries/claude-agent-acp")))


;;; Persistent alert stack for agent-shell

(defvar my/agent-alerts nil
  "Stack of active agent-shell alerts.
Each element is a plist (:type :message :buffer :time).")

(defvar my/agent-alert--subscriptions nil
  "Alist mapping shell buffers to their subscription tokens.")

(defun my/agent-alert--on-permission (event)
  "Handle permission-request EVENT by adding a persistent alert."
  (let* ((tool-call (map-nested-elt event '(:data :tool-call)))
         (kind (capitalize (or (map-elt tool-call :kind) "Permission")))
         (msg (or (map-elt tool-call :title) "")))
    (when (fboundp 'agent-shell--shorten-paths)
      (setq msg (agent-shell--shorten-paths msg)))
    (when (> (length msg) 60)
      (setq msg (concat (substring msg 0 57) "...")))
    (my/agent-alert-push kind msg (current-buffer))))

(defun my/agent-alert--on-turn-complete (event)
  "Handle turn-complete EVENT by adding a persistent alert."
  (let ((status (if (equal (map-nested-elt event '(:data :stop-reason))
                           "end_turn")
                    "Success" "Failed")))
    (my/agent-alert-push "Done" status (current-buffer))))

(defun my/agent-alert--buffer-visible-p (buf)
  "Return non-nil if BUF is shown in any window."
  (and (buffer-live-p buf)
       (get-buffer-window buf t)))

(defun my/agent-alert-push (type message shell-buffer)
  "Add an alert to the stack and refresh display.
Skip if SHELL-BUFFER is already visible in a window."
  (unless (my/agent-alert--buffer-visible-p shell-buffer)
    (push (list :type type :message message :buffer shell-buffer
                :time (format-time-string "%H:%M"))
          my/agent-alerts)
    (my/agent-alert-refresh)))

(defun my/agent-alert-goto ()
  "Dismiss alert at point and switch to its agent-shell buffer."
  (interactive)
  (when-let ((alert (get-text-property (point) 'my/alert)))
    (let ((buf (plist-get alert :buffer)))
      (setq my/agent-alerts (delq alert my/agent-alerts))
      (my/agent-alert-refresh)
      (when (buffer-live-p buf)
        (pop-to-buffer
         (or (and (fboundp 'agent-shell-viewport--buffer)
                  (agent-shell-viewport--buffer
                   :shell-buffer buf :existing-only t))
             buf))))))

(defun my/agent-alert-dismiss-all ()
  "Clear all alerts and hide the window."
  (interactive)
  (setq my/agent-alerts nil)
  (my/agent-alert-refresh))

(defvar my/agent-alert-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/agent-alert-goto)
    (define-key map [mouse-1] #'my/agent-alert-goto)
    (define-key map (kbd "d")   #'my/agent-alert-goto)
    (define-key map (kbd "q")   #'my/agent-alert-dismiss-all)
    map))

(define-derived-mode my/agent-alert-list-mode special-mode "Alerts"
  "Major mode for the agent-shell persistent alert stack.
\\<my/agent-alert-list-mode-map>
RET / click  dismiss alert and switch to its shell buffer
d            same as RET
q            dismiss all alerts"
  (setq-local truncate-lines t))

(defun my/agent-alert-refresh ()
  "Redraw *Agent Alerts* buffer, or hide it when empty."
  (if (null my/agent-alerts)
      (when-let ((buf (get-buffer "*Agent Alerts*")))
        (when-let ((win (get-buffer-window buf t)))
          (delete-window win))
        (kill-buffer buf))
    (let ((buf (get-buffer-create "*Agent Alerts*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'my/agent-alert-list-mode)
            (my/agent-alert-list-mode))
          (dolist (alert my/agent-alerts)
            (let ((start (point)))
              (insert
               (propertize (format " %s " (plist-get alert :time))
                           'face 'shadow)
               (propertize (format "%-10s" (plist-get alert :type))
                           'face 'warning)
               (plist-get alert :message)
               "\n")
              (put-text-property start (point) 'my/alert alert)
              (put-text-property start (1- (point)) 'mouse-face 'highlight)))))
      (display-buffer-in-side-window
       buf '((side . bottom)
             (slot . 1)
             (window-height . fit-window-to-buffer)
             (window-parameters . ((no-delete-other-windows . t))))))))

(defun my/agent-alert-subscribe ()
  "Subscribe to agent-shell events in the current buffer."
  (let ((buf (current-buffer)))
    (my/agent-alert-unsubscribe buf)
    (setf (alist-get buf my/agent-alert--subscriptions)
          (list
           (agent-shell-subscribe-to
            :shell-buffer buf
            :event 'permission-request
            :on-event #'my/agent-alert--on-permission)
           (agent-shell-subscribe-to
            :shell-buffer buf
            :event 'turn-complete
            :on-event #'my/agent-alert--on-turn-complete)))))

(defun my/agent-alert-unsubscribe (&optional shell-buffer)
  "Unsubscribe from events in SHELL-BUFFER."
  (let ((buf (or shell-buffer (current-buffer))))
    (dolist (token (alist-get buf my/agent-alert--subscriptions))
      (agent-shell-unsubscribe :subscription token))
    (setq my/agent-alert--subscriptions
          (assq-delete-all buf my/agent-alert--subscriptions))))

(add-hook 'agent-shell-mode-hook #'my/agent-alert-subscribe)

(defun my/agent-alert-auto-dismiss (_frame)
  "Dismiss alerts whose shell buffer is now visible in a window."
  (when my/agent-alerts
    (let (dominated)
      (dolist (alert my/agent-alerts)
        (when (my/agent-alert--buffer-visible-p (plist-get alert :buffer))
          (push alert dominated)))
      (when dominated
        (dolist (a dominated)
          (setq my/agent-alerts (delq a my/agent-alerts)))
        (my/agent-alert-refresh)))))

(add-hook 'window-buffer-change-functions #'my/agent-alert-auto-dismiss)
(add-hook 'window-selection-change-functions #'my/agent-alert-auto-dismiss)

(provide 'init-agent-shell)
