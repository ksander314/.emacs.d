;;; init-agent-shell.el --- Agent-shell configuration -*- lexical-binding: t -*-
(use-package agent-shell
  :ensure t
  :bind ("C-c A" . agent-shell)
  :config
  (unless (executable-find "claude-agent-acp")
    (message "claude-agent-acp not found; run: npm install -g @zed-industries/claude-agent-acp"))

  (defun my/agent-shell-clear ()
    "Clear the agent-shell session by restarting the ACP connection.
Claude Code local commands like /clear don't send a proper ACP
response, which leaves shell-maker stuck.  This works around the
issue by shutting down the session and clearing the buffer so the
next command starts a fresh conversation."
    (interactive)
    (unless (derived-mode-p 'agent-shell-mode)
      (user-error "Not in an agent-shell buffer"))
    (agent-shell--shutdown)
    (call-interactively #'shell-maker-clear-buffer)
    (shell-maker--output-filter (shell-maker--process)
                                (shell-maker-prompt shell-maker--config))
    (setq shell-maker--busy nil)
    (set-buffer-modified-p nil)
    (message "Session cleared"))

  (define-key agent-shell-mode-map (kbd "C-c C-l") #'my/agent-shell-clear)

  (defun my/agent-shell-intercept-clear (orig-fn &rest args)
    "Intercept /clear before it reaches ACP and use `my/agent-shell-clear'.
Claude Code doesn't send a session/prompt response for /clear,
which leaves shell-maker--busy stuck at t."
    (let ((input (plist-get args :input)))
      (if (and (derived-mode-p 'agent-shell-mode)
               input
               (string-equal "/clear" (string-trim input)))
          (progn
            (my/agent-shell-clear)
            nil)
        (apply orig-fn args))))
  (advice-add #'shell-maker--clear-input-for-execution
              :around #'my/agent-shell-intercept-clear))

(use-package nerd-icons :ensure t)

(use-package knockknock
  :vc (:url "https://github.com/xenodium/knockknock"
       :rev :newest))

(use-package agent-shell-knockknock
  :vc (:url "https://github.com/xenodium/agent-shell-knockknock"
       :rev :newest)
  :after (agent-shell knockknock)
  :hook (agent-shell-mode . agent-shell-knockknock-mode))

(provide 'init-agent-shell)
