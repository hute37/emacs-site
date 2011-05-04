;; 00debian-vars.el
;;
;; Initialize some emacs variables from debian policy files.
;; 
;; Copyright (C) 1997, Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; original Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; enhanced and documented by: Mark Eichin <eichin@kitten.gen.ma.us>

;;=============================================================================
;; Autoloaded section.
;;=============================================================================


;;;###

;;;***


;;=============================================================================
;; Configuration section.
;;=============================================================================
(defun debian-file->string (name &optional func)
  "Convert a file into a string"
  (interactive "fFile name : ")
  (let ((filename (expand-file-name name)))
    (if (not (file-readable-p filename))
	nil
      (let ((buf (create-file-buffer filename))
	    ret)
	(save-excursion
	  (set-buffer buf)
	  (insert-file-contents filename)
	  (if func (funcall func))
	  (setq ret (buffer-string)))
	(kill-buffer buf)
	ret))))

(defun debian-clean-mailname ()
  (while (search-forward "\n" nil t)
    (replace-match "" nil t)))

;; Particular variables, and their justification:
;; policy/ch4.html, 4.3 Mail processing on Debian systems, /etc/mailname
;; policy/ch-binarypkg.html, 3.5 Maintainer scripts, /etc/news/server

(let ((mailname
       (debian-file->string "/etc/mailname" (function debian-clean-mailname))))
  (if (not mailname)
      (message "No /etc/mailname. Reverting to default...")
    (setq mail-host-address mailname)))

;; Don't need to check NNTPSERVER for override, gnus does that for us.
(if (file-readable-p "/etc/news/server")
    (defvar gnus-nntpserver-file "/etc/news/server"))

;;; 00debian-vars.el ends here
