;;; check-elisp.el --- Byte-compile and checkdoc one file, as Flymake does -*- lexical-binding: t -*-

;;; Commentary:

;; bin/check-elisp loads this in a batch Emacs with the arguments
;; ROOT FILE [compile].  ROOT is the checkout whose straight/build
;; supplies the packages.
;; Prints FILE:LINE: MESSAGE for each finding and exits 1 if there is any.
;; Without the word "compile" only checkdoc runs.

;;; Code:

(require 'bytecomp)
(require 'flymake)

(let* ((root (nth 0 command-line-args-left))
       (file (expand-file-name (nth 1 command-line-args-left)))
       (compile (equal (nth 2 command-line-args-left) "compile"))
       (findings nil))
  (setq command-line-args-left nil)
  (setq load-path (append (list (file-name-directory file))
                          (directory-files (expand-file-name "straight/build" root)
                                           t "\\`[^.]")
                          load-path))
  (when compile
    (let ((byte-compile-dest-file-function #'ignore)
          (byte-compile-log-warning-function
           (lambda (string &optional position &rest _)
             (push (cons position string) findings)
             t)))
      (byte-compile-file file)))
  (with-current-buffer (find-file-noselect file)
    (emacs-lisp-mode)
    (elisp-flymake-checkdoc
     (lambda (diagnostics &rest _)
       (dolist (d diagnostics)
         (push (cons (flymake-diagnostic-beg d) (flymake-diagnostic-text d)) findings))))
    (dolist (finding (nreverse findings))
      (princ (format "%s:%d: %s\n" file
                     (line-number-at-pos (or (car finding) 1))
                     (cdr finding)))))
  (setq kill-emacs-hook
        (seq-filter (lambda (fn)
                      (memq fn '(org-persist-clear-storage-maybe
                                 org-babel-remove-temporary-directory
                                 org-babel-remove-temporary-stable-directory)))
                    kill-emacs-hook))
  (kill-emacs (if findings 1 0)))

;;; check-elisp.el ends here
