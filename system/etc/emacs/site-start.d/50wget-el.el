;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian emacs-wget package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>
;; Adapted for dh-make by Jim Van Zandt <jrv@vanzandt.mv.com>

;; The emacs-wget package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavor' (emacs19,
;; xemacs19, emacs20, xemacs20...).  The compiled code is then
;; installed in a subdirectory of the respective site-lisp directory.
;; We have to add this to the load-path:
(let ((package-dir (concat "/usr/share/"
                           (symbol-name flavor)
                           "/site-lisp/wget-el")))
  (when (file-directory-p package-dir)
        (setq load-path (cons package-dir load-path))))

(autoload 'wget "wget" "wget interface for Emacsen." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(eval-after-load "w3m"
  '(load "w3m-wget"))
(autoload 'w3-wget "w3-wget" "wget interface for Emacs/W3." t)

(eval-after-load "wget"
  '(progn
    (setq wget-basic-options (cons "-equiet=off" wget-basic-options))
    (setq wget-basic-options (cons "-P." wget-basic-options))))
