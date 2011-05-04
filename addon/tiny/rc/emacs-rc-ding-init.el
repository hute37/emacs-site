;; @(#) emacs-rc-ding-init.el  -- Initial Emacs Gnus news reader settings
;; @(#) $Id: emacs-rc-ding-init.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{ Documentation
;;; Documentation:
;;
;;  File id
;;
;;      .Copyright (C)  1997-2000 Jari Aalto
;;      .Author:        Jari Aalto <jari.aalto@poboxes.com>
;;	.Maintainer:	Jari Aalto <jari.aalto@poboxes.com>
;;	.Created:	1997-10
;;	.Keywords:	tools
;;
;;      This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that.. Just rename this file to whatever is shown
;;      in the first line. You load this file from .emacs in the following
;;      manner
;;
;;  Warning
;;
;;	*********************************************************************
;;	This file won't work for you
;;
;;      Use the code presented in this file AT YOUR OWN RISK. I WILL NOT
;;      ANSWER TO YOUR HELP CALLS IF LOST MESSAGES or if your Gnus is
;;      totally messed up. This file is offered as _an_ _example_, and it
;;      is not a product that I support for others than myself.
;;	*********************************************************************
;;
;;  Installation
;;
;;      This file is loaded from the main Gnus loader ~/elisp/rc/emacs-rc-ding.el
;;
;;  Description
;;
;;      This file configures Gnus5.3(19.34) .. Qgnus 0.37+ whatever
;;      I have created necessary nnml server to divide my mail into
;;      separate directories:
;;
;;	    ~/Mail/list/	All mailing lists under this dir
;;	    ~/Mail/mail/	My regular mail user this
;;	    ~/Mail/junk/	Unimportant mail
;;	    ~/Mail/tpu/		www.tpu.fi  mail
;;	    ~/Mail/uta/		www.uta.fi  mail
;;
;;	Gnus line in the *Group* looks like
;;
;;	    nnml+list:procmail
;;	    nnml+list:ding
;;	    ...
;;	    nnml+mail:music
;;	    nnml+mail:emacs
;;	    ...
;;	    nnml+junk:ube	Advertisements, spam
;;
;;	You create virtual server with from Server buffer (press ^ in
;;      group mode) and add definitions like this ( "c" to copy nnml,
;;      the "e" to edit):
;;
;;          (nnml "junk"
;;          	  (nnml-directory "~/Mail/junk")
;;          	  (nnml-active-file "~/Mail/junk/active")
;;          	  (nnml-newsgroups-file "~/Mail/junk/newsgroups"))
;;          ;;
;;


;;}}}

;;; Change Log:
;;; Code:


;;{{{ The very first settings

;;; ............................................................ &load ...

;;  When I'm loading or compiling this file I want to know exactly what's
;;  happening, Print messages along the way otherwise it is hard time
;;  to debug this large file.



(eval-when-compile
  (require 'cl)
  (require 'advice))


(require 'emacs-rc-lib)
(require 'emacs-rc-path)

(defconst debug-on-error t)             ;Can't live without this.

(eval-and-compile
  (autoload 'nnheader-concat "nnheader"))


;; (symbol-function 'temp-directory)
;; --> (autoload "" nil nil nil) ???

(when (fboundp 'temp-directory)
  (let ((str (prin1-to-string (symbol-function 'temp-directory))))
    (when (string-match "autoload +\"\"" str)
      (fmakunbound 'temp-directory))))

(defun my-gnus-directory-set-1 (news mail)
  "Set location of Gnus directories relative to NEWS and MAIL dirs.

In UNIX the directories are easily kept in $HOME, but in Win32
with partitioned disks, it is more sensible to put News to separate
partition among with other data that does not need to be
backuped. THe News directory may easily grow to 2G while you
download interesting groups with gnus agent."

  ;; Directory variable from which all other Gnus file variables are derived.

  ;; gnus-home-directory
  ;; gnus-home-directory
  ;; (getenv "SAVEDIR")
  ;; --> (defconst gnus-directory (my-directory-location 'mail)


  ;;   Some of these vars were misconfigures and I spent whole lot of
  ;;   time wondering why gnus won't read my mail. Confirm
  ;;   that the setup is ok.

  ;; nnfolder-directory ~/Mail
  ;; nnml-directory ~/Mail
  ;; nnml-active-file
  ;; nnml-newsgroups-file
  ;; nnmh-directory ~/News/drafts
  ;;

  ;; nndraft-directory

  ;;   some of my old settings may have set this accidentally.
  ;;   Reset it now, so that I don't get message "no newsgroups..."

  (defconst gnus-startup-file-coding-system 'iso-8859-1)
  (defconst gnus-startup-file     (my-config-path "newsrc"))

  (defconst nnfolder-directory    (nnheader-concat mail "nnfolder"))
  (defconst nnfolder-active-file  (nnheader-concat
				   nnfolder-directory "active"))

  (defconst nntp-authinfo-file  (my-config-path "gnus-nntp-authinfo"))


  (defconst nnml-directory  mail)

  (unless (file-directory-p nnml-directory)
    (error "nnml-directory does not exist. %s"  nnml-directory))

  (unless (file-directory-p nnfolder-directory)
    (error "nnfolder-directory does not exist. %s"  nnfolder-directory))

  ;; gnus-message-archive-method
  (unless (file-directory-p (nnheader-concat nnfolder-directory "archive"))
    (error "archive does not exist under %s" nnfolder-directory))

  ;; nnmh-directory message-directory
  ;; (setq nnmh-directory nndraft-directory)

  ;; nnimap-directory

  ;; message-auto-save-directory  "~/Mail/drafts/"

  (defconst message-directory              mail)
  (defconst message-auto-save-directory    (nnheader-concat mail "drafts"))

  (if (not (file-exists-p message-auto-save-directory ))
      (make-directory message-auto-save-directory 'parens))

  (defconst gnus-directory news)

  ;; This is also score files directory
  (defconst gnus-kill-files-directory news)

  ;; --> function gnus-home-score-file
  ;; gnus-home-adapt-file

  (defconst nnkiboze-directory (nnheader-concat news "kiboze"))

  (if (not (file-exists-p nnkiboze-directory))
      (make-directory nnkiboze-directory 'parens))

  (defconst nnslashdot-directory   (nnheader-concat news "slashdot"))
  (defconst nnslashdot-active-file (nnheader-concat news "slashdot/active"))
  (defconst nnultimate-directory   (nnheader-concat news "ultimate"))
  (defconst nnwarchive-directory   (nnheader-concat news "warchive"))

  ;; gnus-cache-active-file

  (defconst gnus-cache-directory   (nnheader-concat news "cache"))
  (defconst gnus-cache-active-file (nnheader-concat news "cache/active"))
  (defconst gnus-agent-directory   (nnheader-concat news "agent"))

  (defconst gnus-article-save-directory news)

  ;; gnus-dribble-directory
  ;; gnus-default-directory

  ;; FTP
  ;; gnus-group-archive-directory
  ;; gnus-group-recent-archive-directory


  )


(defun my-gnus-directory-set ()
  "Set Gnus directory location.
Function `my-directory-location' knows what environment
variables to read and where are the default directories."
  (my-gnus-directory-set-1
   (my-directory-location 'news)
   (my-directory-location 'mail)))


(my-gnus-directory-set)


(eval-and-compile
  (require 'custom))   ;Before we load gnus

(require 'gnus)


(ignore-errors
  (require 'gnus-sum)
  (require 'gnus-art))

(require 'nnfolder)

(eval-and-compile

  ;;  This prevent some error I had...don't remember any more what

  (autoload 'mail-file-babyl-p          "mail-utils")
  (defalias 'rmail-file-p               'mail-file-babyl-p)


  (autoload 'browse-url-lynx-emacs      "browse-url")
  (autoload 'browse-url-netscape        "browse-url")

  (defvar my-:window-system             ;Running quality term this time?
    (symbol-value 'window-system))	;Hmm, obsolete in XEmacs

  )

;;  <morioka@jaist.ac.jp>
;;  XEmacs 20.2 does not have raw-text coding-system.  I think it is
;;  better to update to XEmacs 20.3, but following code may be available:

(when (fboundp 'find-coding-system)
  (or (find-coding-system 'raw-text)
      (copy-coding-system 'no-conversion 'raw-text)))


;;; ........................................................... &other ...

(defconst mail-generate-iso-dates t)
(defconst gnus-startup-file-coding-system 'iso-8859-1)
(defconst mail-source-delete-incoming t)   ;; don't generate ~/Mail/Incoming*

(defconst gnus-group-jump-to-group-prompt "nnml:")

;;; ......................................................... pgp-mime ...

;; Insert an attribute, postprocess=pgp-sign (or pgp-encrypt),
;; into the mml tag to be signed (or encrypted).

(require 'gnus-art)

(setq mml2015-use 'mailcrypt)
(setq mm-verify-option 'never)
(setq mm-decrypt-option 'never)
(setq gpg-passphrase-timeout 3600)

;; (setq mml2015-use 'gpg)


;;; .......................................................... mailcap ...
;;; The format is defined/suggested in RFC 1524.
;;;
;;; http://www.ietf.org/rfc/rfc1524.txt
;;; http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/mailcap.html


(when (ignore-errors (require 'mailcap))

  ;; Remove application/octet-stream entry in win32 and use shelex to
  ;; to launch the file. You need to add this to the ~/.mailcap too
  ;;
  ;;  ("octet-stream"
  ;;   (viewer . "shelex %s")
  ;;   (type . "application/octet-stream"))

  (when (and nil (executable-find "shelex"))
    (let* ((application (assoc "application" mailcap-mime-data))
	   (octet-stream (assoc "octet-stream" application)))
      (when (symbolp (cdr (assoc 'viewer octet-stream)))
	(delete octet-stream application))))


  ;; (mailcap-add "text/x-patch" 'my-mode-inline-text)

  (aput 'mailcap-mime-extensions ".diff"  "text/x-patch")
  (aput 'mailcap-mime-extensions ".patch" "text/x-patch")
  (aput 'mailcap-mime-extensions ".el" "text/x-emacs-lisp")


  (setq mm-body-charset-encoding-alist '((iso-8859-1 . 8bit)))

  (defconst my-mode-content-transfer-encoding-defaults
    '(("text/x-patch" 8bit)
      ("text/.*" qp-or-base64)
      ("message/rfc822" 8bit)
      ("application/emacs-lisp" 8bit)
      ("text/x-patch" 8bit)
      (".*" qp-or-base64)
      ))

  (setq mm-content-transfer-encoding-defaults
	my-mode-content-transfer-encoding-defaults)

  ;; For Some reason Win32 XEmacs needs these load commands

  (when (boundp 'xemacs-logo)
    (require 'mm-util)
    (require 'mm-decode)
    )

  ;; prefer to show plain text over markup for multipart/alternative

  (defconst mm-discouraged-alternatives
    '("text/html" "text/richtext"))

  (require 'mm-decode)

  (when (boundp 'mm-automatic-display)
    (defconst mm-automatic-display
      (delete "text/html" mm-automatic-display)))

  )

(defun my-mailcap-mime-types ()
  "Return a list of MIME media types."
  (delete nil
          (mm-delete-duplicates
           (apply 'append
                  (mapcar 'cdr mailcap-mime-extensions)
                  (mapcar
		   '(lambda (x)
		      (and (not (string-match "\\.?\\*" (car x)))
			   (mapcar
			    '(lambda (x)
			       (and (not (string-match
					  "\\.?\\*" (car x)))
				    (cdr (assoc 'type x))))
			    (cdr x))))
		   mailcap-mime-data)))))




;;}}}

;;{{{ Override functions.


(defun my-gnus-override () ;; Must be called manually to install these.
  (interactive)

  ;;   I don't want to see those yes/no confirmations, simple
  ;;   y/n will do Thank you.

  (defalias 'gnus-yes-or-no-p 'gnus-y-or-n-p)

  ;;  added "(when funcs"

  (defun gnus-run-hooks (&rest funcs)
    "Does the same as `run-hooks', but saves excursion."
    (when funcs				;<< Added this test
      (let ((buf (current-buffer)))
	(unwind-protect
	    (apply 'run-hooks funcs)
	  (set-buffer buf)))))

  ;;  gnus-art.el,
  ;;  We don't need this function.
  ;;  I do this manually in my-gnus-article-display

  ;; (defun gnus-article-hide-headers-if-wanted (&rest args) nil)

  )


;;; ........................................................... select ...

(defconst gnus-secondary-select-methods '((nnml "")))


(setq
 ;;     Err, I use common convention for all my emacs-rc-xxx.el files, so tell
 ;;     Gnus about it so that command "r" in *Grup* buffer reloads right file

 gnus-novice-user               nil
 gnus-save-newsrc-file          nil ;; save no .newsrc file, I use only Gnus
 gnus-inhibit-startup-message   t   ;; It's nice, but really...

 gnus-verbose                   10
 nntp-record-commands           t
 gnus-verbose-backends          10
 gnus-always-read-dribble-file  t
 gnus-after-getting-new-news-hook nil

 ;;     This is a bit more relaxing than "!". maybe better contrast
 ;;     to dormant, delete, expire marks.
 ;;
 ;; gnus-ticked-mark            ?-

 )

;; Original: "^[]>|}+ ]*[]>|}+]\\(.*>\\)?\\|^.*>"
;;
;; People also use ":" at the start of their replies

(defconst gnus-cite-prefix-regexp
  "^[]>|}+: ]*[]>|}+:]\\(.*>\\)?\\|^.*>" )



;;}}}
;;{{{ Agent

;;; ........................................................... &agent ...
;;;  emacs -batch -l ~/.emacs.ding.el -f gnus-agent-batch

(require 'gnus-agent)

(defconst gnus-agent-high-score 500)
(defconst gnus-agent-low-score  -500)


;;(if (and (boundp 'gnus-agent-send-mail-function)
;;	   gnus-agent-send-mail-function)
;;    (setq message-send-mail-function gnus-agent-sendmail-function)
;;  (setq gnus-agent-send-mail-function message-send-mail-function))


;;; ----------------------------------------------------------------------
;;;
(defun my-gnus-agent-hook  ()
  "Agent customization."
  (interactive)
  (let* (
         )
    ;;  This is my Finnish keyboard A: key, non-shifted and shifted

    (define-key gnus-agent-summary-mode-map "{"
      'gnus-agent-mark-article)

    (define-key gnus-agent-summary-mode-map "["
      'gnus-agent-mark-article)

    (define-key gnus-agent-summary-mode-map [(f12)]
      'gnus-agent-toggle-plugged)

        ))


(defun my-gnus-agent-mode-hook  ()
  "Agent mode settings."
  (let* (
         )
    ;; 1998-03-16 DING-L Mike McEwan <mike@lotusland.demon.co.uk>

    (if (and (boundp 'gnus-agent-group-mode)
	     (symbol-value 'gnus-agent-group-mode))
	(gnus-run-hooks 'gnus-agent-group-mode-hook))

    (if (and (boundp 'gnus-agent-summary-mode)
	     (symbol-value 'gnus-agent-summary-mode))
	(gnus-run-hooks 'gnus-agent-summary-mode-hook))

    (if (and (boundp 'gnus-agent-server-mode)
	     (symbol-value 'gnus-agent-server-mode))
	(gnus-run-hooks 'gnus-agent-server-mode-hook))
    ))



(add-hook 'gnus-agent-mode-hook 'my-gnus-agent-mode-hook)

(eval-after-load "gnus-agent"
  '(progn (my-gnus-agent-hook)))

(unless (featurep 'gnus-agent)
  (load "gnus-agent" 'noerr))

(when (file-directory-p "C:/")
  (when (fboundp 'gnus-agentize)  ;; #offline news reading
    (defconst gnus-plugged nil)
    (gnus-agentize)
    (define-key gnus-agent-group-mode-map   "Je" 'gnus-agent-expire)
    (define-key gnus-agent-summary-mode-map "Je" 'gnus-agent-expire)
    (define-key gnus-agent-server-mode-map  "Je" 'gnus-agent-expire)
    ))



;; 1998-11-27 Tom Breton <tob@world.std.com>  gnu.emacs.gnus
;; Article <m3pva9eo4v.fsf@world.std.com>
(defun my-gnus-summary-act-on-articles (func &optional arg)
  "Perform the given operation on all articles that are process/prefixed."

  (let ((articles (gnus-summary-work-articles arg))
	 article)
    (if
      (or
	(not (commandp func ))
	(eq func 'undefined))

      (gnus-error 1 "Undefined function")
      (save-excursion
	(while articles
	  (gnus-summary-goto-subject (setq article (pop articles)))
	  (let (gnus-newsgroup-processable)
	    (command-execute func))
	  (gnus-summary-remove-process-mark article)))))
  (gnus-summary-position-point))

(defun my-gnus-agent-toggle-thread (arg)
  "Toggle an entire thread for agent downloading."
  (interactive "P")
  (gnus-uu-mark-thread)
  (my-gnus-summary-act-on-articles 'gnus-agent-toggle-mark arg))



;;}}}

(my-gnus-override)

(provide 'emacs-rc-ding-init)

;;; End of file
