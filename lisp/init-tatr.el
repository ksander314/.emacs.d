;;; init-tatr.el --- tatr task tracker: local defaults and keys -*- lexical-binding: t -*-
(require 'tatr)

;; Upstream starts every task at `scope', which is rexim's own tag and is
;; declared in his tasks/tags rather than in any of mine.  Left alone it would
;; put an undeclared tag on everything created from Emacs, so tasks start bare
;; and `tatr ls not tagged' finds the ones that stayed that way.
(setq tatr-default-tags nil)

;; C-c t is already my/org-quick-task (init.el), so the tracker takes C-c n.
(defvar my/tatr-map (make-sparse-keymap)
  "Prefix map for `tatr' commands.")
(global-set-key (kbd "C-c n") my/tatr-map)
(define-key my/tatr-map (kbd "n") #'tatr-create-from-title)
(define-key my/tatr-map (kbd "t") #'tatr-create-from-todo-at-point)
(define-key my/tatr-map (kbd "f") #'tatr-find-by-huid)
(define-key my/tatr-map (kbd "r") #'tatr-grep-referers)
(define-key my/tatr-map (kbd "y") #'tatr-copy-huid-to-clipboard)

(provide 'init-tatr)
