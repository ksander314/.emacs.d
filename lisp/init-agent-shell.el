(use-package system-packages :demand t)

(use-package agent-shell
  :ensure t
  :ensure-system-package
  (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")
  :bind ("C-c a" . agent-shell))

(provide 'init-agent-shell)
