;;; 65wl.el --- Debian wl startup file  -*-mode: emacs-lisp;-*-

;;; Code:

(let ((elc-dir (concat "/usr/share/" (symbol-name flavor) "/site-lisp/wl")))
  (if (or (not (file-exists-p "/usr/lib/emacsen-common/packages/install/wl"))
	  (not (file-directory-p elc-dir)))
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
    (autoload 'wl-user-agent-compose "wl-draft" nil t)
    (if (fboundp 'define-mail-user-agent)
	(define-mail-user-agent
	  'wl-user-agent
	  'wl-user-agent-compose
	  'wl-draft-send
	  'wl-draft-kill
	  'mail-send-hook))
    (autoload 'wl "wl" "Wanderlust" t)
    (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
    (autoload 'wl-draft "wl" "Write draft with Wanderlust." t)
    (setq wl-icon-directory "/usr/share/pixmaps/wl/")
    (setq ssl-certificate-directory "/etc/ssl/certs")
    (setq ssl-certificate-verification-policy 3)
    (setq elmo-archive-tar-method-alist
	  '((ls    . ("tar" "-tf"))
	    (cat   . ("tar" "-Oxf"))
	    (ext   . ("tar" "-xf"))
	    ;;(rm    . ("tar" "--delete" "-f")) ;; well not work
	    ))
    (setq elmo-archive-tgz-method-alist
	  '((ls         . ("tar" "-ztf"))
	    (cat        . ("tar" "-Ozxf"))
	    (create     . ("tar" "-zcf"))
	    ;;(rm         . elmo-archive-tgz-rm-func)
	    (cp         . elmo-archive-tgz-cp-func)
	    (mv         . elmo-archive-tgz-mv-func)
	    (ext        . ("tar" "-zxf"))
	    ;; tgz special method
	    (decompress . ("gzip" "-d"))
	    (compress   . ("gzip"))
	    (append     . ("tar" "-uf"))
	    ;;(delete     . ("tar" "--delete" "-f")) ;; well not work
	    ))
    (if (and (featurep 'xemacs)
	     (fboundp 'user-mail-address)
	     (boundp 'user-mail-address)
	     (not user-mail-address)
	     (boundp 'query-user-mail-address)
	     (not query-user-mail-address))
        (user-mail-address))
    (if (not (and (boundp 'user-mail-address)
		  (> (length user-mail-address) 0)))
	(setq wl-from
	      (concat
	       (if (and (boundp 'user-full-name)
			(> (length user-full-name) 0))
		   (concat user-full-name " <"))
	       (if (and (boundp 'user-login-name)
			(> (length user-login-name) 0))
		   user-login-name
		 (if (fboundp 'user-login-name)
		     (user-login-name)
		   ""))
	       "@"
	       (if (and (boundp 'mail-host-address)
			(> (length mail-host-address) 0))
		   mail-host-address
		 (if (fboundp 'system-name)
		     (system-name)
		   "localhost"))
	       (if (and (boundp 'user-full-name)
			(> (length user-full-name) 0))
		   ">"))))
    ;;
    ))

;;; 65wl.el ends here
