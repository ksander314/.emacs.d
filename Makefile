.PHONY: compile clean

compile:
	emacsclient -e '(byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)'

clean:
	rm -f lisp/*.elc
