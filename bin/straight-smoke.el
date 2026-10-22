;;; straight-smoke.el --- Load init.el in batch Emacs, print what fails -*- lexical-binding: t -*-

;;; Commentary:

;; Run by bin/straight-update.  This Emacs shares ~/.emacs.d with the one in
;; use, so none of these may be left to write the desktop, the history files
;; or the keystroke log.

;;; Code:

(require 'desktop)
(require 'server)
(dolist (f '(desktop-kill desktop-save server-start))
  (defalias f #'ignore))
(defalias 'server-running-p (lambda (&rest _) t))
(with-eval-after-load 'savehist (defalias 'savehist-autosave #'ignore))
(with-eval-after-load 'org-clock (defalias 'org-clock-save #'ignore))
(with-eval-after-load 'init-keystroke-log
  (defalias 'my/klog-flush #'ignore)
  (defalias 'my/klog--refresh-layout #'ignore))

(defun straight-smoke--report (what err)
  "Print a smoke line saying WHAT failed with ERR."
  (princ (format "smoke: %s: %s\n" what (error-message-string err))))

(defun straight-smoke--print-chain (_err)
  "Print the require and load frames that led to the error being signaled."
  (dolist (frame (backtrace-frames))
    (when (memq (nth 1 frame) '(require load))
      (princ (format "smoke:   in (%s %S)\n" (nth 1 frame) (car (nth 2 frame)))))))

(defun straight-smoke--cancel-timers ()
  "Cancel every timer, so none fires while this Emacs waits on a process."
  (dolist (timer (append timer-list timer-idle-list nil))
    (cancel-timer timer)))

(condition-case err
    (handler-bind ((error #'straight-smoke--print-chain))
      (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
      (load (expand-file-name "init.el" user-emacs-directory) nil t))
  (error (straight-smoke--report "init.el" err)))
(straight-smoke--cancel-timers)

(dolist (pkg (directory-files (expand-file-name "straight/build" user-emacs-directory)
                              nil "\\`[^.]"))
  (when (and (not (featurep (intern pkg))) (locate-library pkg))
    (condition-case err
        (require (intern pkg))
      (error (straight-smoke--report (format "(require '%s)" pkg) err)))))
(straight-smoke--cancel-timers)

(let* ((out (make-temp-file "straight-smoke" t))
       (byte-compile-dest-file-function
        (lambda (file) (expand-file-name (concat (file-name-nondirectory file) "c") out))))
  (dolist (file (cons (expand-file-name "init.el" user-emacs-directory)
                      (directory-files (expand-file-name "lisp" user-emacs-directory)
                                       t "\\.el\\'")))
    (condition-case err
        (byte-compile-file file)
      (error (straight-smoke--report (format "compiling %s" file) err))))
  (delete-directory out t))

(setq kill-emacs-hook nil)
(princ "smoke: done\n")
;;; straight-smoke.el ends here
