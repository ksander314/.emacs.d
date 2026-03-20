;;; early-init.el --- Early init -*- lexical-binding: t -*-
;; Faster startup: reduce GC and disable file-name-handler during init
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist)))

(setq package-enable-at-startup nil)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
