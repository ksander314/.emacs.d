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


;;; Stop the graphical header from cutting its own text off
;;
;; The header is an SVG whose canvas is cropped to the width agent-shell
;; predicts the text needs: `agent-shell--render-header-model-uncached' shrinks
;; it to `agent-shell--svg-content-width' + 16.  The prediction comes from
;; `string-pixel-width' (Emacs' font backend); the drawing is done by librsvg,
;; which resolves fonts through fontconfig and FreeType.  On this machine the
;; two disagree badly and the canvas lands at roughly half the width the text
;; occupies, so the top line stopped mid-word:
;;
;;   Claude ➤ Opus 5 with 1M context ➤ Max ➤ Byp
;;
;; Cause: the only Inconsolata installed is the Google Fonts variable file
;; ~/Library/Fonts/Inconsolata[wdth,wght].ttf.  Emacs instantiates it at the
;; default width axis (wdth=100, advance 0.5em -> 9px at 18px), while pango
;; collapses the named instances to two buckets and picks wdth=200 (advance
;; ~1em -> 18px).  Ten "M" at font-size 18 measure 90px in Emacs and 179px in
;; librsvg -- and the wide instance is also why the header text looked
;; loosely spaced next to the buffer's own Inconsolata.
;;
;; Two changes, both needed.  Declaring a non-variable family alone would
;; still under-predict (Emacs would keep measuring in Inconsolata's 9px cell
;; while librsvg drew Menlo's 10.8px one); measuring in the declared family
;; alone would faithfully predict the wrong 9px, since Emacs has no way to
;; render the wdth=200 instance pango picks.

(defvar my/agent-shell-header-font-family "Menlo"
  "Font family declared in the agent-shell header SVG, or nil to keep the default.
Must be a family librsvg and Emacs measure alike: ten \"M\" at font-size
18 come to 108px under librsvg and 110px under Emacs for Menlo, against
179 vs 90 for Inconsolata.  Erring high is the safe direction -- the
canvas ends up a little wider than the text rather than cropping it.")

(defvar my/agent-shell--measure-buffers nil
  "Alist mapping (FAMILY . SIZE) to a buffer used for pixel measurement.
Buffers are reused: `string-pixel-width' consults the buffer's
`face-remapping-alist', so one throwaway buffer per font is enough.")

(defun my/agent-shell--measure-buffer (family size)
  "Return a buffer whose default face is FAMILY at pixel SIZE.
Returns nil when FAMILY is not installed, so callers fall back to
measuring in the current buffer."
  (when (and (stringp family) (find-font (font-spec :family family)))
    (let* ((size (if (numberp size) (truncate size) (frame-char-height)))
           (key (cons family size))
           (buf (alist-get key my/agent-shell--measure-buffers nil nil #'equal)))
      (unless (buffer-live-p buf)
        (setq buf (generate-new-buffer
                   (format " *agent-shell-measure %s %d*" family size) t))
        (with-current-buffer buf
          (setq-local face-remapping-alist
                      `((default (:font ,(font-spec :family family :size size))))))
        (setf (alist-get key my/agent-shell--measure-buffers nil nil #'equal) buf))
      buf)))

(defun my/agent-shell--svg-text-width (node)
  "Return the pixel width of SVG text NODE, measured in NODE's own font.
Override for `agent-shell--svg-text-width', which measures with the
current buffer's default face.  The header SVG names its font family and
pixel size on the `text' node, so honour those instead: the point of the
measurement is to predict what librsvg will draw, and librsvg draws in
the declared font, not in whatever face the calling buffer happens to
carry.  Falls back to the buffer's face when the family is unavailable."
  (let ((buf (my/agent-shell--measure-buffer (dom-attr node 'font-family)
                                             (dom-attr node 'font-size))))
    (seq-reduce (lambda (total child)
                  (if (and (consp child) (eq (dom-tag child) 'tspan))
                      (+ total
                         (string-to-number (format "%s" (or (dom-attr child 'dx) 0)))
                         (string-pixel-width (or (car (dom-children child)) "") buf))
                    total))
                (dom-children node)
                0)))

(defun my/agent-shell--header-font-family (model)
  "Declare `my/agent-shell-header-font-family' in header MODEL.
Filter-return advice for `agent-shell--make-header-model'.  MODEL's
`:font-family' otherwise follows the default face, which here is a
variable font librsvg renders at twice Emacs' advance width."
  (when (and (consp model)
             my/agent-shell-header-font-family
             (find-font (font-spec :family my/agent-shell-header-font-family)))
    (setf (alist-get :font-family model) my/agent-shell-header-font-family))
  model)

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--svg-text-width :override
              #'my/agent-shell--svg-text-width)
  (advice-add 'agent-shell--make-header-model :filter-return
              #'my/agent-shell--header-font-family))


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

;;; Keep macOS awake while the agent is working
;;
;; In the terminal Claude runs as `caffeinate claude', so the CLI holds a
;; no-sleep assertion for its whole life and keeps working behind a locked
;; screen.  agent-shell offers the same wrapping point --
;; `agent-shell-command-prefix' is prepended to the ACP command -- but Emacs
;; outlives any single turn, so a static prefix would forbid sleep for as
;; long as a shell buffer exists, and would also wrap every tool-call shell
;; command (see `agent-shell--build-command-for-execution').  Hold the
;; assertion only while a turn is in flight instead: acquire on
;; `input-submitted', release once no shell is busy.  A pending permission
;; counts as idle -- nothing progresses until the human answers -- and
;; `permission-response' re-acquires for the rest of the turn.

(defvar my/agent-caffeinate--process nil
  "Live `caffeinate' process holding the no-sleep assertion, or nil.")

(defvar my/agent-caffeinate-timeout 14400
  "Seconds after which the `caffeinate' assertion expires on its own.
Backstop for a turn that never reports completion, or for an Emacs that
dies without reaping its children: a leaked assertion would otherwise
keep the machine awake indefinitely.  A turn longer than this loses the
assertion mid-flight.")

(defun my/agent-caffeinate--acquire (&optional _event)
  "Hold a no-sleep assertion for the duration of an agent turn.
No-op when one is already held.  Ignores its EVENT argument so it can
serve directly as an `agent-shell-subscribe-to' handler."
  (when (and (eq system-type 'darwin)
             (not (process-live-p my/agent-caffeinate--process))
             (executable-find "caffeinate"))
    ;; -i: no idle sleep -- what bare `caffeinate' asserts, and the one that
    ;; matters behind a locked screen.  -s: no system sleep, honoured on AC
    ;; power only, which is what survives a closed lid.  -m: no disk idle
    ;; sleep.  Deliberately no -d: the display may still sleep.
    (setq my/agent-caffeinate--process
          (make-process
           :name "agent-caffeinate"
           :command (list "caffeinate" "-i" "-s" "-m"
                          "-t" (number-to-string my/agent-caffeinate-timeout))
           :noquery t))))

(defun my/agent-caffeinate--release ()
  "Drop the no-sleep assertion, if held.
macOS reclaims the assertion as soon as the owning process dies, so
killing `caffeinate' is enough."
  (when (process-live-p my/agent-caffeinate--process)
    (delete-process my/agent-caffeinate--process))
  (setq my/agent-caffeinate--process nil))

(defun my/agent-caffeinate--release-when-idle (&optional _event)
  "Drop the assertion unless another agent-shell is still mid-turn.
Skips the buffer this event came from: its turn is over (or its buffer is
being killed), yet `shell-maker-busy' may not say so yet -- and a stuck
assertion is worse than an early release."
  (let ((origin (current-buffer)))
    (unless (seq-some
             (lambda (buf)
               (and (not (eq buf origin))
                    (with-current-buffer buf
                      (and (derived-mode-p 'agent-shell-mode)
                           (eq (ignore-errors (agent-shell-status)) 'busy)))))
             (buffer-list))
      (my/agent-caffeinate--release))))

(defun my/agent-caffeinate-subscribe ()
  "Tie the no-sleep assertion to turn boundaries in the current buffer.
Both handlers are idempotent, so a re-run of `agent-shell-mode' needs no
unsubscribe bookkeeping."
  (dolist (event '(input-submitted permission-response))
    (agent-shell-subscribe-to :shell-buffer (current-buffer)
                              :event event
                              :on-event #'my/agent-caffeinate--acquire))
  (dolist (event '(turn-complete permission-request error clean-up))
    (agent-shell-subscribe-to :shell-buffer (current-buffer)
                              :event event
                              :on-event #'my/agent-caffeinate--release-when-idle)))

(add-hook 'agent-shell-mode-hook #'my/agent-caffeinate-subscribe)
(add-hook 'kill-emacs-hook #'my/agent-caffeinate--release)

(provide 'init-agent-shell)
