;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian nxml-mode package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

(if (not (file-exists-p "/usr/share/emacs/site-lisp/nxml-mode"))
    (message "Package nxml-mode removed but not purged.  Skipping setup.")

  ;; The nxml-mode package follows the Debian/GNU Linux 'emacsen' policy and
  ;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
  ;; xemacs19, emacs20, xemacs20...).  The compiled code is then
  ;; installed in a subdirectory of the respective site-lisp directory.
  ;; We have to add this to the load-path:
  (debian-pkg-add-load-path-item (concat "/usr/share/"
                                         (symbol-name flavor)
                                         "/site-lisp/nxml-mode"))

  ;; Load the package.  Note that we have to load the *source* of rng-auto
  ;; for it to properly find the schemas.
  (load "rng-auto.el")

  ;; Comment this out if you want to use PSGML or another mode to edit
  ;; these files.
  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
              auto-mode-alist)))
