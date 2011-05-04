;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux wysihtml package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The wysihtml package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(if (featurep 'mule)
    (setq load-path (cons (concat "/usr/share/"
				  (symbol-name flavor)
				  "/site-lisp/wysihtml") load-path)))
(autoload 'wysihtml-mode "wysihtml" nil t)
(autoload 'wysidocbookxml-mode "wysidocbookxml" nil t)

