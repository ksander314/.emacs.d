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

;; Native compilation: the libgccjit 14.3 bundled in Emacs.app guesses the
;; macOS version from the kernel (Darwin 27 gives 18.0), clang rejects 18.0
;; and every .eln build fails.  Pass the real version; "-Wl,-w" is the stock
;; value.  Here and not in init.el so the first compile (straight.el) gets it.
(when (and (eq system-type 'darwin) (native-comp-available-p))
  (when-let* ((ver (car (ignore-errors
                          (process-lines "/usr/bin/sw_vers" "-productVersion")))))
    (setq native-comp-driver-options
          (list "-Wl,-w" (concat "-mmacosx-version-min=" ver)))))
