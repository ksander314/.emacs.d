;;; init-git.el --- Git configuration -*- lexical-binding: t -*-
(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

(use-package magit
  :ensure t
  :bind ("C-c g g" . magit-status))

(defun my/markdown-toggle-pretty ()
  "Toggle between pretty and raw markdown view."
  (interactive)
  (if markdown-hide-markup
      (progn
        (markdown-toggle-markup-hiding)
        (setq-local markdown-header-scaling nil)
        (markdown-update-header-faces markdown-header-scaling)
        (variable-pitch-mode -1))
    (markdown-toggle-markup-hiding)
    (setq-local markdown-header-scaling t)
    (markdown-update-header-faces markdown-header-scaling)
    (variable-pitch-mode 1)))

(use-package markdown-mode
  :ensure t
  :defer t
  :bind (:map markdown-mode-map
              ("C-c C-v" . my/markdown-toggle-pretty))
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . variable-pitch-mode))
  :custom
  (markdown-header-scaling t)
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc -f markdown -t html5"))

(provide 'init-git)
