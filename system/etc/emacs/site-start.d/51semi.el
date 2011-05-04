;;; 51semi.el --- Debian semi startup file  -*-mode: emacs-lisp;-*-

;;; Code:

(let ((elc-dir (concat "/usr/share/" (symbol-name flavor) "/site-lisp/semi")))
  (if (not (file-directory-p elc-dir))
      ()
    (if (featurep 'xemacs)
	(if (featurep 'mule)
	    (and (file-directory-p (concat elc-dir "/mule"))
		 (setq elc-dir (concat elc-dir "/mule")))
	  (and (file-directory-p (concat elc-dir "/nomule"))
	       (setq elc-dir (concat elc-dir "/nomule")))))
    (if (fboundp 'debian-pkg-add-load-path-item)
	(debian-pkg-add-load-path-item elc-dir)
      (setq load-path (cons elc-dir load-path)))
    (setq mime-edit-split-message nil)
    (setq pgg-encrypt-for-me t)
    ;;(setq pgg-messages-locale "C") ;; default value is nil
    ;;(setq pgg-gpg-messages-locale pgg-messages-locale)
    (setq mime-setup-enable-inline-html nil)
    ;;(require 'mime-w3m) ;; text/html rendering with w3m-el
    ;;
    ))

;;; 51semi.el ends here
