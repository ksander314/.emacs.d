;;; init-agent-shell.el --- Agent-shell configuration -*- lexical-binding: t -*-
(use-package agent-shell
  :ensure t
  :bind (("C-c A" . agent-shell)
         ("C-c O" . agent-shell-opencode-start-agent))
  :config
  ;; Neither effort nor model is a CLI flag: as of claude-agent-acp 0.63.0 the
  ;; bridge only parses --claudeai, --cli, --console and --hide-claude-auth.
  ;; Effort comes from ~/.claude/settings.json ("effortLevel": "xhigh"); the
  ;; model comes from ANTHROPIC_MODEL below.
  (setq agent-shell-anthropic-claude-acp-command
        '("claude-agent-acp"))
  ;; Pin the model.  claude-agent-acp resolves it as ANTHROPIC_MODEL >
  ;; settings.json "model" > resumed session > models[0]; unpinned it lands on
  ;; models[0] ("Default"), which follows whatever the bundled
  ;; @anthropic-ai/claude-agent-sdk treats as default.  Setting it here rather
  ;; than in settings.json leaves the terminal CLI on its own default.
  ;; acp.el prepends these onto `process-environment', so PATH still resolves.
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_MODEL" "claude-opus-5"))
  (unless (executable-find "claude-agent-acp")
    (message "claude-agent-acp not found; run: npm install -g @agentclientprotocol/claude-agent-acp"))
  ;; OpenCode ACP backend — drives a local Ollama model (Gemma 4) in
  ;; Claude-Code style: tool calls, file edits, shell.  The concrete model is
  ;; chosen in ~/.config/opencode/opencode.json (default:
  ;; ollama/gemma4:31b-it-q8_0, served by the local ollama daemon on :11434).
  ;; Local models need no API key — the default authentication is already
  ;; (agent-shell-opencode-make-authentication :none t), so nothing to set.
  ;; Launch with M-x agent-shell-opencode-start-agent (bound to C-c O below).
  (setq agent-shell-opencode-acp-command '("opencode" "acp"))
  (unless (executable-find "opencode")
    (message "opencode not found; run: npm install -g opencode-ai"))
  ;; RET inserts newline; M-RET submits — prevents accidental sends
  (define-key agent-shell-mode-map (kbd "RET") #'newline)
  (define-key agent-shell-mode-map (kbd "<return>") #'newline)
  (define-key agent-shell-mode-map (kbd "M-RET") #'shell-maker-submit)
  (define-key agent-shell-mode-map (kbd "<M-return>") #'shell-maker-submit)
  ;; Reveal the concrete model in the header.  No option's :name carries a
  ;; version -- they read "Default (recommended)", "Opus (1M context)",
  ;; "Sonnet", "Haiku" -- while every :description leads with the real one
  ;; ("Opus 5 with 1M context", "Sonnet 5", "Haiku 4.5") before a " · "
  ;; marketing tail.  So prefer the description's head for every option, not
  ;; just the generic "default": gating on "default" broke the moment
  ;; ANTHROPIC_MODEL pinned a concrete id (model-id became "opus[1m]", whose
  ;; :name is likewise version-less).  Falls back to :name when an option
  ;; carries no description.
  (defun my/agent-shell-concrete-model-name (orig-fn state)
    "Around-advice for `agent-shell-get-model-name'.
Return the concrete model taken from the current option's :description
\(the segment before \" · \"), falling back to ORIG-FN's name for STATE
when no description is advertised."
    (let ((name (funcall orig-fn state))
          (model-id (agent-shell--current-model-id state)))
      (if-let* ((model (seq-find (lambda (m)
                                   (equal (map-elt m :model-id) model-id))
                                 (agent-shell--get-available-models state)))
                (desc (map-elt model :description))
                (concrete (string-trim (car (split-string desc "·"))))
                ((not (string-empty-p concrete))))
          concrete
        name)))
  (advice-add 'agent-shell-get-model-name :around
              #'my/agent-shell-concrete-model-name))


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

;;; Context-usage meter for OpenCode
;;
;; OpenCode's ACP reports per-turn token counts but not the context window
;; size, so agent-shell's context indicator (which needs :context-size) stays
;; hidden for OpenCode even though it renders for Claude.  Synthesize the two
;; missing fields from the reported input-token count and a known window size
;; so the existing `agent-shell--context-usage-indicator' lights up.

(require 'map)

(defvar my/agent-shell-opencode-context-size 131072
  "Assumed context window (tokens) for OpenCode/ollama models.
Keep in sync with ollama's OLLAMA_CONTEXT_LENGTH.")

(defun my/agent-shell-opencode-fill-context (&rest args)
  "Populate :context-used/:context-size for OpenCode usage STATE.
Intended as `:after' advice on `agent-shell--save-usage'.  OpenCode reports
input/total tokens but not context size, so derive the meter's inputs and
refresh the header to render it like Claude's.  ARGS is the advised call's
keyword argument list."
  (when-let* ((state (plist-get args :state))
              ((eq (map-elt (map-elt state :agent-config) :identifier) 'opencode))
              (usage (map-elt state :usage))
              (used (map-elt usage :input-tokens))
              ((> used 0)))
    (map-put! usage :context-used used)
    (map-put! usage :context-size my/agent-shell-opencode-context-size)
    (map-put! state :usage usage)
    (when-let* ((buf (map-elt state :buffer))
                ((buffer-live-p buf))
                ((fboundp 'agent-shell--update-header-and-mode-line)))
      (with-current-buffer buf
        (agent-shell--update-header-and-mode-line)))))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--save-usage :after
              #'my/agent-shell-opencode-fill-context))

(provide 'init-agent-shell)
