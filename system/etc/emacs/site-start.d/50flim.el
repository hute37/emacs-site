;;; 50flim.el --- Debian flim startup file  -*-mode: emacs-lisp;-*-

;;; Code:

(let ((elc-dir (concat "/usr/share/" (symbol-name flavor) "/site-lisp/flim")))
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
    (if (featurep 'xemacs)
	;; Use XEmacs's mmencode (default values)
	()
      ;; Use metamail's mimencode
      (setq base64-external-encoder '("mimencode"))
      (setq base64-external-decoder '("mimencode" "-u"))
      (setq base64-external-decoder-option-to-specify-file '("-o"))
      (setq quoted-printable-external-encoder '("mimencode" "-q"))
      (setq quoted-printable-internal-encoding-limit
	    (if (and (featurep 'xemacs) (featurep 'mule))
		0
	      (require 'path-util)
	      (if (exec-installed-p "mimencode")
		  1000
		nil)))
      (setq quoted-printable-external-decoder '("mimencode" "-q" "-u"))
      (setq quoted-printable-external-decoder-option-to-specify-file '("-o"))
      (setq gzip64-external-encoder '("sh" "-c" "gzip -c | mimencode"))
      (setq gzip64-external-decoder '("sh" "-c" "mimencode -u | gzip -dc"))
      ;;
      )
    ;;
    ))

;;; 50flim.el ends here
