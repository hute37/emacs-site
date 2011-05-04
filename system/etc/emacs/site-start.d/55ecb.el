;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux ecb package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The ecb package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(cond
 ((file-exists-p "/usr/share/emacs/site-lisp/ecb/ecb.el")
  (setq load-path (cons (concat "/usr/share/"
				(symbol-name flavor)
				"/site-lisp/ecb") load-path))
  (setq ecb-help-html-path "/usr/share/doc/ecb/html/ecb.html")
  (setq ecb-help-info-path "/usr/share/info/ecb.info.gz")
  (require 'ecb-autoloads)
;; The following was added to fix bug #269368 - you need cedet.el
;; loaded to not let ecb check for old semantic/speedbar/eieio versions
;; and fail if not found. With it loaded it doesnt check and works...
  (require 'cedet)))

