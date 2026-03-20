(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package magit
  :bind ("C-c g g" . magit-status))

(use-package git-messenger
  :bind (("C-c g p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message)))

(use-package gptel-magit
  :after magit
  :config
  (gptel-magit-install)
  (add-hook 'magit-mode-hook
            (lambda ()
              (setq-local gptel-magit-commit-prompt
                          "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

The commit message should be structured as follows:

    <TASKID>: <description>

    [optional body]

- Always derive the JIRA key from the current Git branch name.
  Match only the first occurrence of ^([A-Za-z]+-[0-9]+) at the start of the branch, uppercase it, and use that as <TASK-ID>.
  (e.g. wtsn-3551-tv-ingress-namespaced → WTSN-3551)
- If no branch-derived key is found, omit the <TASK-ID> and print only the description.
- Try to limit the whole subject line to 60 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- A longer commit body MAY be provided after the short description, providing additional contextual information about the code changes. The body MUST begin one blank line after the description.
- Use the imperative mood in the subject line
- Keep the body short and concise (omit it entirely if not useful)"))))

(provide 'init-git)
