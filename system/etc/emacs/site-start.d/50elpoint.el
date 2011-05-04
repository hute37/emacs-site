;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian elpoint package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The elpoint package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:

(when (and (or (eq flavor 'emacs21) (eq flavor 'emacs-snapshot))
	   (eq window-system 'x))
  (if (not (file-exists-p "/usr/share/emacs/site-lisp/elpoint"))
      (message "Package elpoint removed but not purged.  Skipping setup.")
    (debian-pkg-add-load-path-item
     (concat "/usr/share/" (symbol-name debian-emacs-flavor)
	     "/site-lisp/elpoint"))

    (setq scalable-fonts-allowed t)
    (setq font-list-limit 3000)
    (require 'ept-setup)))
