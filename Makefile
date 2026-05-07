EMACS = emacs

.PHONY: check-byte-compile-dot-emacs
check-byte-compile-dot-emacs:
	$(EMACS) --batch --load "$(curdir)/.emacs-defs" -f 'my-lisp--load-all' --eval='(byte-compile-file ".emacs-my")'
	rm -f .emacs-my.elc

# @see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Compilation-Functions.html


.PHONY: check check-byte-compile-init
# --batch ?
check-byte-compile-init:
	$(EMACS) --debug-init  --eval='(progn (setq byte-compile-debug t)(setq byte-compile-error-on-warn t))'


check: check-byte-compile-init
