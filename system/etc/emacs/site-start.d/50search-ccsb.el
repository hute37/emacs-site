;; 
;; /etc/emacs/site-start.d/50search-ccsb.el
;;
;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux search-ccsb package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The search-ccsb package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(setq load-path (cons (concat "/usr/share/"
                              (symbol-name flavor)
			      "/site-lisp/search-ccsb") load-path))
(autoload 'search-ccsb "search-ccsb" 
  "Search tool for the The Collection of Computer Science Bibliographies" t)


