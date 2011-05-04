;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux html-helper-mode package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The html-helper-mode package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(debian-pkg-add-load-path-item
 (concat "/usr/share/"
         (symbol-name debian-emacs-flavor)
         "/site-lisp/html-helper-mode"))

;; Put the uncompiled code in the path too
(setq load-path
      (append load-path
              (list "/usr/share/emacs/site-lisp/html-helper-mode")))

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

;; Add to head of magic-mode-alist, if available
(when (boundp 'magic-mode-alist)
  (setq magic-mode-alist (cons '("<html" . html-helper-mode)
                               magic-mode-alist)))

;; You may want to comment out some/all of the following lines if you
;; don't want html-helper-mode enabled by default for these filename
;; extensions.
(setq auto-mode-alist (cons '("\\.htm$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.shtml$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jsp$" . jsp-html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.php$" . php-html-helper-mode) auto-mode-alist))
