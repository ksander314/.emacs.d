(use-package system-packages :demand t)

(use-package agent-shell
  :ensure t
  :ensure-system-package
  (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")
  :bind ("C-c a" . agent-shell))

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
