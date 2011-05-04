;;; @(#) tinyrmail.el --- RMAIL add-ons, pgp, mime labels, Spam complaint.
;;; @(#) $Id: tinyrmail.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1996-2002 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;; Created:      1996-06
;; Keywords:     mail
;;
;; To get information on this program use ident(1) or do M-x tinyrmail-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinyrmail|Jari Aalto|jari.aalto@poboxes.com|
;; RMAIL add-ins, e.g. label summary AND, detect incoming PGP, MIME mail|
;; 2002-08-04|$Revision: 1.1 $|~/misc/tinyrmail.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install


;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp load path, add following into your
;;  ~/.emacs startup file. Rip code with with tinylib.el/ti::package-rip-magic
;;
;;* _
;;*     (require 'tinyrmail)
;;
;;  or prefer this; your .emacs loads up much quicker
;;
;;* _
;;* (autoload 'tinyrmail-rmail-summary-by-labels-and "tinyrmail" t t)
;;* (autoload 'tinyrmail-install			"tinyrmail" t t)
;;* (add-hook 'rmail-mode-hook			'tinyrmail-install)
;;
;;  You can also install the attached PGP signed file.
;;  Before you do this, you need commands 'pgp' and 'tar' along
;;  Emacs exec-path
;;
;;      M-x tinyrmail-install-files
;;
;;  After you have unpacked the anti-spam text file, set these variables
;;
;;	(setq tinyrmail-:ube-ignore-site-regexp "YOUR-DOMAIN-REGEXP")
;;	(setq tinyrmail-:ube-message-file           "~/txt/spam.txt")
;;
;;  If you have any questions, use this function
;;
;;      M-x tinyrmail-submit-bug-report       ,send bug report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:


;;  Preface, overview of features
;;
;;	1998-01: I no longer use RMAIL, but Gnus, and support for this
;;	module is questionable. If you're using RMAIL or considering using
;;	it, please strongly think if you could use `Gnus' instead. I've
;;	written another module *tinygnus.el* whehre I can provide better
;;	support than for this on.
;;
;;	o   Detect PGP, MIME mail and label incoming messages accordingly.
;;	    User can add more checking functions and labels to incoming email
;;	    messages
;;	o   New label summary cmd with AND e.g. finding {pgp,v} verified pgp
;;	o   Flag incoming mail as deleted by regexp.
;;	o   "S" command for Spam message reply.
;;	o   Commands to fix your RMAIL messages.
;;	o   advice: "n" and "p" do not to auto display msg in Summary buffer
;;	o   advice: mouse click in Summary does not automatically update msg
;;	o   advice: `rmail-ignored-headers' now reformats old messages too.
;;
;;  Description
;;
;;	This little package offers some autmatic detection of PGP
;;	MIME  mails: It attaches labels to your incoming mails.
;;	There is also new summary function, which enables you to
;;	make a query by ANDing the labels in your RMAIL.
;;
;;	This means, that you can now classify your message, like this:
;;
;;	    BASE
;;	    SUBSET-IDENTIFIER
;;                MINOR-IDENTIFIER
;;                   NOTE-IDENTIFIER
;;
;;       Eg. For PGP mails I have
;;
;;          {pgp}
;;          {pgp,v}         -- verified signature
;;          {pgp,u}         -- not verified
;;          {pgp,v,e}       -- verified and encrypted
;;
;;	The normail rmail's summary function gives you the OR summary, which
;;	would mean, that if you wanted symmary by {pgp,v}, it would give
;;	you all mail that has either {v} or {pgp} somewhere. Well, this
;;	summary is not suitable if you use one CHAR to denote attributes
;;	of your base-identifiers (multichar)
;;
;;  Automatic deletion of incoming mail
;;
;;	There is default function to mark messages as deleted according
;;	to regexp. Please configure this variable to suit your needs:
;;
;;	    tinyrmail-:delete-regexp
;;
;;	If you want more personal control whether the mail
;;	should be deleted or not, please remove the default delete function
;;	and add your own:
;;
;;	    (add-hook 'tinyrmail-:load-hook 'my-tinyrmail-:load-hook)
;;
;;	    (defun my-tinyrmail-:load-hook ()
;;	      "Cancel some default settings and modify parameters."
;;	      (remove-hook 'tinyrmail-:get-new-mail-hook
;;                         'tinyrmail-delete-function)
;;	      (add-hook    'tinyrmail-:get-new-mail-hook
;;                         'my-rmail-delete-function))
;;
;;
;;	    (defun my-rmail-delete-function ()
;;	     ...)
;;
;;  Replying to UBE aka spam messages
;;
;;	The spamming has become a very serious promlem in private
;;	email communication. Not only the spammers send mail to newsgroups,
;;	but they are harrashing private mailboxes as well.
;;
;;	With this package you can send reply to a spammer and complaint
;;	to all postmasters that have allowed the spam mail to travel through
;;	their networks. The addresses are parsed from the Received headers
;;	of the message and verified by `nslookup'. ou can turn off the
;;      nslookup if you wasn't faster response, but this may result
;;      bounched messages back to you.
;;
;;	    (setq tinyrmail-:ube-use-nslooup t)
;;
;;	So that you don't sned complaint to your local admin and not to
;;	some other known middleman domain (like forwarding service), you
;;	shoul also set following variable to regular expression to reject
;;	some addresses:
;;
;;	    (setq tinyrmail-:ube-ignore-site-regexp
;;                "YOUR-SITE\\|155\\.0\\.233")
;;
;;	The text to the beginning of message is read from file pointed
;;	by `tinyrmail-:ube-message-file'.
;;
;;  New commands in RMAIL
;;
;;	Refer to function tinyrmail-define-default-keys for exact setup.
;;	Currently the only new command added is
;;
;;	    "L"	tinyrmail-rmail-summary-by-labels-and
;;	    "U" tinyrmail-ube-send-to-postmasters
;;	        UBE = Unsolicited Bulk Email
;;
;;  Fixing RMAIL format
;;
;;	Sometimes you may get following error after you have hit "g"
;;	to get new mail: "Cannot convert to babyl". The reason for
;;	this behavior is still not quite clear to me, but the cause
;;	is in the incoming message that does not have
;;
;;	    From
;;
;;	Field at the beginning of message. I have seen even some garbage
;;	Prepended to field so that it looked like
;;
;;	    m?From
;;
;;	What have to start editing the RMAIL file directly to fix its
;;	format. Change the mode to text-mode, run M-x widen and search the
;;	last message that rmail was not able to read. You will easily find the
;;	point where "**** EOOH" markers do not appear any more.
;;
;;	Now starts the fixing part to make rmail happy again:
;;
;;	o   Make sure From line is left flushed. Edit if needed and put
;;	    lines in their right places.
;;	o   Select all individual message's headers at a time.
;;	o   Call function tinyrmail-fix-make-rmail-message-header
;;	    which you should propably bound to some convenient key.
;;	    The ESC-z combination is propably free for temporary use.
;;	    (local-set-key "\ez" 'tinyrmail-fix-make-rmail-message-header)
;;
;;	After you have converted all headers to rmail format, you can
;;	start rmail again with command
;;
;;	    M-x rmail-mode
;;
;;	If you made any mistakes, rmail will let you know and you have to
;;	repeat the header fixing again. (possibly removing the prevous
;;	EOOOH markers and reconverting them). We aren't quite finished
;;	yet. You see, on error, rmail leaves the read mail into your home
;;	directory. Please check that
;;
;;	    ~/.newmail-USERNAME
;;
;;	file doesn't contain any new message that aren't already in your RMAIL
;;	buffer. If there is only old message, delete that file. Now we
;;	have finished and you can again use "g" to get new mail.
;;
;;  Standard Rmail distribution changes
;;
;;	This package changes the standard Rmail distribution sligtly and here
;;	summary. If you want to disable these features or only use some of
;;	them, you have to put separate configuration to your .emacs.
;;	To disable forms:
;;
;;	    (setq tinyrmail-:load-hook '(tinyrmail-install))
;;
;;	To disable advices, you do
;;
;;	    (setq tinyrmail-:load-hook '(tinyrmail-install my-tinyrmail-install))
;;
;;	    (defun my-tinyrmail-install ()
;;	      (ti::advice-control
;;		'(rmail-show-message
;;		  rmail-summary-enable
;;		  rmail-summary-next-msg
;;		  )
;;		 "^tinyrmail"
;;		 'disable
;;		 ))
;;
;;
;;	`tinyrmail-:forms-rmail'
;;
;;	o   Every time RMAIL package is loaded these forms are executed.
;;	o   These define some keybindings to summary buffer
;;	    that I have found appropriate. Mouse-2 selects message
;;	    (and does not yank as the original). RET key also selects message.
;;	o   The post command hook is cleared so that you can search regexp
;;	    in summary buffer. Normally moving a cursor would move the
;;	    current message too.
;;	o   The "q" quit key is too easily pressed and I have removed it
;;	    alltogether. If I really want to quit RMAIL, I usually
;;	    quit Emacs too.
;;
;;	Advices:
;;
;;	*rmail-show-message* active
;;
;;	The message's headers are now always reformatted. If you change
;;	variable `rmail-ignored-headers', the old messages are not affected
;;	until you "t"oggle headers. This advice does it for you
;;	automatically every time you select message. This advice slows
;;	message displaying a bit, but for me, it isn't very noticeable.
;;	You can very well turn this off if you dont' change content of
;;	`rmail-ignored-headers'.
;;
;;	*rmail-summary-enable* active
;;
;;	This replaces whole function. The original function did automatic
;;	message update whenever you moved around summary buffer. Now you
;;	can keep summary buffer search separated from the current
;;	message displayed.
;;
;;	*rmail-summary-next-msg* active
;;
;;	Same as above.
;;

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: variables

;;; ......................................................... &require ...

(require  'rmail)			;Uses macros from there
(require  'tinylibm)

(eval-and-compile
  (autoload 'rmail-new-summary "rmailsum"))

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyRmail tinyrmail-: mail
  "Additional features to RMAIL.
Overview of features

	o   Detect PGP, MIME mail and label incoming messages accordingly.
	    User can add more checking functions and labels to incoming email
	    messages
	o   New label summary cmd with AND, e.g. {pgp,v} for verified pgp mails.")


;;; ......................................................... &v-hooks ...

(defcustom tinyrmail-:load-hook '(tinyrmail-install tinyrmail-install-forms)
  "*Hook that is run when package is loaded."
  :type 'hook
  :group 'TinyRmail)

(defcustom tinyrmail-:rmail-get-new-mail-before-hook nil
  "*Additional hook added by advice in package tinyrmail.el.
Hook run just before new mail is fetched.
Contain default function `tinyrmail-rmail-get-new-mail-before-function',
which saves the Rmail message pointers before getting new mail."
  :type 'hook
  :group 'TinyRmail)


(defcustom tinyrmail-:get-new-mail-hook  nil
  "*Hook run inside each _new_ mail message.
The default function `tinyrmail-delete-function' reads variable
`tinyrmail-:delete-regexp' and marks buffer as deleted if the regexp
matches message contents."
  :type 'hook
  :group 'TinyRmail)


(defcustom tinyrmail-:ube-send-to-postmasters-hook  nil
  "Hook run after `ube-send-to-postmasters-hook' has made the message."
  :type 'hook
  :group  'TinyRmail)


;;; ........................................................ &v-public ...
;;; User configurable


(defcustom tinyrmail-:delete-regexp
  (concat
   "make.*money"
   "\\|this is your chance.*money")
  "*Mark messge deleted if this regexp match.
If this regexp is nil, no mail is marked as deleted.
This variable is efective only if `tinyrmail-delete-function' is
installed into `tinyrmail-:get-new-mail-hook'."
  :type '(string :tag "Regexp")
  :group 'TinyRmail)


(defcustom tinyrmail-:ube-ignore-site-regexp  nil
  "*Regexp to drop out site names in the Received header list.

You set this variable to match your local domain names, so that the
spam complaints to CC postmasters doesn't go there."
  :type '(string :tag "Regexp")
  :group 'TinyRmail)


(defcustom tinyrmail-:label-table
  '((ti::mail-pgp-p   "pgp")
    (ti::mail-mime-p  "mime"))
  "*Labels to attach to new RMAIL messages.
Format is

  '((CHECK-FUNCTION STRING-OR-SYMBOL) (F S) ..).

The STRING-OR-SYMBOL may be either \"string\" or variable name
'lisp-var, where its `symbol-value' is used.

The CHECK-FUNCTION is run without arguments inside every new
message and it should return. This can also be a lisp form if
the elt is not function symbol.

  nil         ,if no action should be taken
  t           ,if the STRING-OR-SYMBOL should be used for labelling
  string      ,that string is used for labelling."
  :type '(repeat
	  (list
	   (function :tag "Check function")
	   (choice
	    :inline t
	    (string :tag "String Label")
	    (symbol :tag "Var Symbol"))))
  :group 'TinyRmail)


(defcustom tinyrmail-:ube-use-nslooup  t
  "If non-nil, veryfy Received header address with nslookup.
Using nslookup also filter duplicate addresses."
  :type 'boolean
  :group 'TinyRmail)


(defcustom tinyrmail-:ube-message-file  nil
  "File to prepend to UBE complaint message.
See `tinyrmail-ube-send-to-postmasters',
prepend this text before inserting original message.

It should contain educational text to teach about proper email
etiquette and explain why spamming is very intrucive way to
reach possible customers.

The default text for this file can be found from tinyrmail.el

Note: compressed file support:

  If the filename in this variable is FILE.txt, then we automatically
  serch for extension .txt.gz and .txt.Z also if no plain .txt
  file is found."
  :type 'file
  :group 'TinyRmail)



;;}}}
;;{{{ setup: private

;;; ....................................................... &v-private ...

(defvar tinyrmail-:rmail-info-list  nil
  "Values of saved message counters before we get new mail.")


(defconst tinyrmail-:forms-rmail
  '(progn
     (when (boundp 'rmail-summary-mode-map)

       (cond
	((emacs-p)
	 ;;  mouse-2 is paste, move it to select a buffer.
	 ;;  See the rmail advices.
	 (define-key rmail-summary-mode-map [down-mouse-2]
	   'rmail-summary-goto-msg)

	 (define-key rmail-summary-mode-map [mouse-2]
	   'rmail-summary-goto-msg))
	(t
	 (define-key rmail-summary-mode-map [(button2up)]
	   'rmail-summary-goto-msg)

	 (define-key rmail-summary-mode-map [(button2)]
	   'rmail-summary-goto-msg)))

       ;;  Enter selects a message too
       (define-key rmail-summary-mode-map "\C-m" 'rmail-summary-goto-msg)

       ;; rmailsum.el makes this buffer local, loop all rmail summary
       ;; buffers and remove function from post-command-hook.

       (ti::dolist-buffer-list
	(eq major-mode 'rmail-summary-mode)
	(not 'temp-buffers)
	(not 'exclude)
	(progn
	  (remove-hook 'post-command-hook 'rmail-summary-rmail-update)))

       ;; disable "quit", it's too risky. I want to be in RMAIL,
       ;; and only there hit the "q" key.
       ;;

       (define-key rmail-summary-mode-map "q" 'ignore))

     (when (boundp 'rmail-mode-map)
       (define-key rmail-mode-map "q"
	 '(lambda ()
	    "Confirm quit."
	    (interactive)
	    (if (y-or-n-p "Really quit RMAIL ")
		(rmail-quit))))))
  "Additional forms to `after-load-alist'.
Set this variable to '(progn) if you want to disable these features.")

;;}}}
;;{{{ version

;;; ......................................................... &version ...

;;;###autoload (autoload 'tinyrmail-version "tinyrmail" "Display commentary." t)

(eval-and-compile
(ti::macrof-version-bug-report
 "tinyrmail.el"
 "tinyrmail"
 tinyrmail-:version-id
 "$Id: tinyrmail.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinyrmail-:version-id
   tinyrmail-:rmail-info-list
   tinyrmail-:load-hook
   tinyrmail-:rmail-get-new-mail-before-hook
   tinyrmail-:get-new-mail-hook
   tinyrmail-:ube-send-to-postmasters-hook
   tinyrmail-:delete-regexp
   tinyrmail-:ube-ignore-site-regexp
   tinyrmail-:label-table
   tinyrmail-:ube-use-nslooup
   tinyrmail-:ube-message-file)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ Installation

;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload 'tinyrmail-install-files "tinyrmail" t t)
(ti::macrof-install-pgp-tar tinyrmail-install-files "tinyrmail.el")

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-install-forms  ()
  "Some other things to do to get all installed.
See source code for
better explanation."
  (interactive)

  (when (boundp 'rmail-summary-mode-map)
    (eval tinyrmail-:forms-rmail))		;run it immediately

   (cond
    ((not (fboundp 'eval-after-load))
     (load "rmailsum")
     (load "rmail")
     (eval tinyrmail-:forms-rmail))
    ((fboundp 'eval-after-load)
     ;;  Quiet XEmacs 19.14 compiler who says this function doesn't exist
     (ti::funcall 'eval-after-load "rmailsum"  tinyrmail-:forms-rmail)
     (ti::funcall 'eval-after-load "rmail"     tinyrmail-:forms-rmail))))


;;; ----------------------------------------------------------------------
;;; - If more commnds are added, I make this a separate minor mode...
;;;
(defun tinyrmail-define-default-keys  ()
  "Define keys to various maps."
  (interactive)

  ;; Making summaries by ANDING labels.

  (when (boundp 'rmail-mode-map)
    (define-key rmail-mode-map "L" 'tinyrmail-rmail-summary-by-labels-and)
    (define-key rmail-mode-map "U" 'tinyrmail-ube-send-to-postmasters))

  ;;  This is not loaded, that's why symbol-value to shut up byte
  ;;  compiler.

  (when (boundp 'rmail-summary-mode-map)
    (define-key
      (symbol-value 'rmail-summary-mode-map)
      "L"
      'tinyrmail-rmail-summary-by-labels-and)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-install-advices  (&optional remove verb)
  "Install advices. Optionally REMOVE advices. VERB."
  (interactive "P")
  (ti::advice-control
   '(rmail-get-new-mail)
   "^tinyrmail-"
   remove
   (or verb
       (interactive-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-install (&optional remove)
  "Install package hooks. Optionally REMOVE installation.
Can't restore changes to keymaps."
  (interactive "P")
  (let* ((f (if remove 'remove-hook 'add-hook)))
    ;; Set up RMAIL for PGP

    (funcall f 'rmail-get-new-mail-hook 'tinyrmail-rmail-get-new-mail-function)
    (funcall f 'tinyrmail-:rmail-get-new-mail-before-hook
	       'tinyrmail-rmail-get-new-mail-before-function)

    ;; New commands

    (funcall f 'rmail-mode-hook		'tinyrmail-define-default-keys)
    (funcall f 'rmail-summary-mode-hook 'tinyrmail-define-default-keys)
    (funcall f 'gnus-article-mode-hook  'tinyrmail-define-default-keys)
    (tinyrmail-define-default-keys)		;Install immediately too

    (tinyrmail-install-advices remove)))

;;}}}
;;{{{ rmail, labels

;;; .................................................... &rmail-labels ...

;;; ----------------------------------------------------------------------
;;; see rmailsum.el
;;;
;;;###autoload
(defun tinyrmail-rmail-summary-by-labels-and (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas.
This summary is prduced by _ANDING_ the labels."
  (interactive "s(AND) Labels to summarize by: ")
  (if (string= labels "")
      (setq labels (or rmail-last-multi-labels
                       (error "No label specified"))))
  (setq rmail-last-multi-labels labels)
  (rmail-new-summary (concat "labels " labels)
                     (list 'rmail-summary-by-labels labels)
                     'tinyrmail-rmail-message-labels-and-p
		     ;; convert to list of label string
		     ;;
		     (split-string labels "[ ,]+")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-rmail-message-labels-and-p (msg labels)
  "Check and condition in MSG nbr with LABELS LIST."
  (let* ((copy	labels)	;since labels list vanishes in loop
	 (i	0))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (rmail-msgbeg msg))
	(forward-char 3)

	(dolist (elt labels)
	  ;; May look like this:
	  ;;
	  ;;    1,, pgp, v,
	  (if (looking-at (concat ".* " elt ","))
	      (incf  i)))))
    ;;  Must have as many hits as labels passed to function
    (eq (length copy) i)))


;;}}}
;;{{{ rmail, new message


;;; .................................................... &new-messages ...


;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-delete-function  ()
  "Mark messages as deleted if it find regexp `tinyrmail-:delete-regexp'.
This function is in `tinyrmail-:get-new-mail-hook'."
  (ti::pmin)
  (if (and (stringp tinyrmail-:delete-regexp)
	   (re-search-forward tinyrmail-:delete-regexp nil t))
      (rmail-delete-message)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyrmail-rmail-new-message-ptr ()
  "Return first new message NBR.
Function must be called only after the \"g\" key, in `rmail-get-new-mail'."
  (if (and tinyrmail-:rmail-info-list
	   (not (eq (car tinyrmail-:rmail-info-list)
		    rmail-total-messages))
	   (integerp (car tinyrmail-:rmail-info-list)))
      (1+ (car tinyrmail-:rmail-info-list))
    ;;  Whan you first hit M-x RMAIL, this tells you the first message
    (rmail-first-unseen-message)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-rmail-get-new-mail-function  ()
  "Loop over every incoming mail message and do labelling."
  (let* ((table                 tinyrmail-:label-table)
	 (rmail-current-message rmail-current-message)
	 nbr
	 list
	 func
	 label
	 stat)

    ;; Is there new mail, maybe some of them are not read yet?

    (setq nbr (tinyrmail-rmail-new-message-ptr))

    (when nbr
      (while (< nbr (1+ rmail-total-messages))

	(ti::mail-rmail-do-message-macro nbr nil
	  (setq rmail-current-message nbr)

	  (setq list table)
	  (dolist (elt list)

            ;; .......................................... handle table ...

	    (setq func  (nth 0 elt)
		  label (nth 1 elt)
		  stat  (if (symbolp func)
			    (funcall func)
			  (eval func)))

            ;; .............................................. evaluate ...

	    (if (symbolp label)
		(setq label (symbol-value label)))

            ;; ............................................. add label ...

	    (cond
	     ((stringp stat)
	      (rmail-add-label stat))
	     ((and stat (stringp label))
	      (rmail-add-label label))
	     ((and stat (not (stringp label)))
	      (error "Label is not a string %s %s" label table))))
	  (run-hooks 'tinyrmail-:get-new-mail-hook))
	(incf  nbr)))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-rmail-get-new-mail-before-function  ()
  "Reset some values before getting mail."
  (setq
   tinyrmail-:rmail-info-list
   (list
    rmail-total-messages
    rmail-current-message
    rmail-message-vector
    rmail-deleted-vector
    rmail-summary-vector)))


;;; ----------------------------------------------------------------------
;;;
(defadvice rmail-get-new-mail  (before tinyrmail-hook act)
  "Run hook 'tinyrmail-:rmail-get-new-mail-before-hook'."
  (run-hooks 'tinyrmail-:rmail-get-new-mail-before-hook))

;;}}}
;;{{{ UBE: Spam reply

;;; ............................................................ &spam ...

(defun tinyrmail-ube-cc-spam-archive  ()
  "Add CC for http://www.spam-archive.org/ that fight against spam."
  (interactive)
  (ti::mail-add-to-field-string "CC" "spam-list@toby.han.de" "cTo"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyrmail-ube-send-to-postmasters ()
  "Reply to spam message by sending 'remove' request and complaint.

o  Send 'remove' request to the From address.
o  Copy the current message with original headers
o  Parse all received sites from the message and CC
   appropriate postmasters
o  Insert custom anti-spam text from file.

References:

  `tinyrmail-:ube-send-to-postmasters-hook'
  'tinyrmail-:ube-use-nslooup'
  `tinyrmail-:ube-message-file'
  `tinyrmail-:ube-ignore-site-regexp'"
  (interactive)
  (let* ((orig-msg  (ti::mail-rmail-copy-message))
	 (file      tinyrmail-:ube-message-file)
	 (ignore-re tinyrmail-:ube-ignore-site-regexp)
	 (ns-use    tinyrmail-:ube-use-nslooup)
	 mail-archive-file-name
	 ns-list addr-list
	 from
	 subj
	 list
	 elt)

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. checkings . .

    (if (null tinyrmail-:ube-ignore-site-regexp)
	(error "Please set tinyrmail-:ube-ignore-site-regexp."))

    (cond
     ((not (stringp file))
      (error "Please set tinyrmail-:ube-message-file."))

     ((file-exists-p (setq elt (concat file ".gz")))
      (ti::use-file-compression)
      (setq file elt))

     ((file-exists-p (setq elt (concat file ".Z")))
      (ti::use-file-compression)
      (setq file elt))

     ((not (file-exists-p file))
      (error "tinyrmail-:ube-message-file does not exist %s"
	     tinyrmail-:ube-message-file)))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . get travel sites . .

    (ti::mail-rmail-do-message-macro rmail-current-message 'orig
      (pop-to-buffer (current-buffer))
      (setq list (ti::mail-parse-received ignore-re)))


    (if (null list)			;Didn't find any!!
	(error "\
No valid Received headers according to tinyrmail-:ube-ignore-site-regexp."))
    (setq addr-list (reverse addr-list)) ;First sender first.

    (if (null ns-use)
	(setq ns-list list)
      (unless (setq ns-list (ti::mail-nslookup list nil 'verbr))
	(error "Couldn't find valid IP addresses via nslookup.")))

    (dolist (elt ns-list)
      (setq elt  (car-safe (nth 1 elt))) ;'(IP (name . addr))
      (when (stringp elt)
	(add-to-list 'addr-list (concat "postmaster@" elt))))

    (setq from (ti::mail-get-field-1 "from")
	  subj (ti::mail-get-field-1 "subject"))

    (mail)

    ;;  We add two spaces, so that possible TinyMail turns off.
    ;;
    (ti::mail-kill-field "To" (concat "  " from))
    (ti::mail-kill-field "Subject" (format " ABUSE (was: %s)" subj))

    ;;  Add "postmaster" fields
    ;;
    (when (setq addr-list (cdr addr-list))
      (ti::mail-add-field "CC" (ti::list-join addr-list ", ")
			"To" nil 'replace))


    (ti::mail-kill-field "fcc")
    (ti::mail-text-start 'move)

    (insert-file-contents file)

    (ti::pmax)
    (insert "\n" orig-msg)
    (ti::mail-text-start 'move)

    (run-hooks 'tinyrmail-:ube-send-to-postmasters-hook)

    (message "TIRM ube: When you're ready, send message.")))

;;}}}
;;{{{ Advice

;;; .......................................................... &advice ...


;;; ----------------------------------------------------------------------
;;; (ad-unadvise 'rmail-show-message)
;;;
(defadvice rmail-show-message  (before tirm act)
  "Reformat message.
If you change the `rmail-ignored-headers' it won't affect the current
messages unless you hit 't' to toggle headers. This advice reformats
message every time the message is shown."
  ;;   We do}t want expunge to call use, only direct
  ;;   show message command.
  (when (interactive-p)
    (ti::widen-safe
      (rmail-maybe-set-message-counters)
      (narrow-to-region (rmail-msgbeg (ad-get-arg 0)) (point-max))
      (let ((buffer-read-only nil))
	(goto-char (point-min))
	(forward-line 1)

	;;  Convert 1 --> 0, otherwise format command barfs.

	(delete-char 1)
	(insert "0")

	(forward-line 1)

	(let ((case-fold-search t))
	  (while (looking-at "Summary-Line:\\|Mail-From:")
	    (forward-line 1)))

	(insert "*** EOOH ***\n")
	(forward-char -1)
	(search-forward "\n*** EOOH ***\n")
	(forward-line -1)

	(let ((temp (point)))
	  (and (search-forward "\n\n" nil t)
	       (delete-region temp (point))))

	(goto-char (point-min))
	(search-forward "\n*** EOOH ***\n")

	(rmail-reformat-message (point-min) (point-max))))))

;;; ----------------------------------------------------------------------
;;; - Copy from rmailsum.el
;;; - This would normally cause automatic update by mouse click, disable it
;;; - I want to select message with RETURN or mouse-2. This way I can
;;;   move around the buffer and leave the message in RMAIL untouched.
;;;
(defadvice rmail-summary-enable (around tirm act)
  "Replace function.
Disable automatic update when mouse - 1 is pressed or cursor is moved.
You can browse the summary buffer more freely and keep the
selected message in RMAIL."
  (use-local-map rmail-summary-mode-map)
  ;; (add-hook 'post-command-hook 'rmail-summary-rmail-update)
  (setq revert-buffer-function 'rmail-update-summary))

;;; ----------------------------------------------------------------------
;;; - Copy from rmailsum.el.
;;; - I hate when I can't browse forward without getting
;;;   the Summary. Grr...
;;;
(defadvice rmail-summary-next-msg (around tirm act)
  "Replace function. Disbale automatic showing of summary buffer."
  (forward-line 0)
  (and (> number 0) (end-of-line))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	(non-del-msg-found nil))
    (while (and (> count 0) (setq non-del-msg-found
				  (or (funcall search "^....[^D]" nil t)
                                      non-del-msg-found)))
      (setq count (1- count))))
  (beginning-of-line)

;;; this does automatic update, "p", "n" and mouse click
;;;  (display-buffer rmail-buffer)
  nil)

;;}}}
;;{{{ Fixing RMAIL messages

;;; ............................................................. &fix ...


;;; ----------------------------------------------------------------------
;;; - When you run RMAIL over FCC'd file, and afterwards add more to that
;;;   FCC mail, the file may become corrupt so that RMAIL can't read all
;;;   messages in it.
;;; - This little function, when header region is selected, converts
;;;   the headers to Rmail, so that summary can be used.
;;; - I don't understand why my 'From ' field goes totally wrong...
;;;
;;;
(defun tinyrmail-fix-make-rmail-message-header (beg end)
  "Fix RMAIL header in BEG END.
To use this function you must do this.

1. Be in RMAIL buffer
2. Change mode to text with \\[text-mode]
3. run \\[widen]
4. Select message's full headers
5. Call this function

After the call, the appropriate RMAIL message format for headers has been
created."
  (interactive "r")
  (let* (;; START and END headers strings
	 (s-h (concat  (char-to-string ?\037) "\f\n1,,\n")) ;start header
	 (e-h "*** EOOH ***\n")
	 blk line
	 from date
	 rmail-lines)
    (ti::keep-lower-order beg end)

    ;;  These lines are show in the real rmail message, rest are hidden.

    (setq rmail-lines
	  (ti::buffer-grep-lines
	   "^To:\\|^From:\\|^date:\\|^Subject:" beg end))


    (setq blk (buffer-substring beg end))
    (kill-region beg end)

    (goto-char beg)
    (insert s-h blk "\n" e-h)
    (goto-char beg) (forward-line 2)

    (if (null ;; Is this corrupted From line ?
	 (looking-at "From\\( [a-zA-Z]+ \\)\\([FSMTWS].*\\)"))
	nil
      (setq from (buffer-substring (match-beginning 1) (match-end 1)))
      (setq date (buffer-substring (match-beginning 2) (match-end 2)))
      (kill-line)

      (setq line (concat "Date: " date "\n" "From:" from))
      (insert line)

      (if (null (re-search-forward (regexp-quote "***")))
	  (message "Not found [***]")
	(forward-line 1)
	(setq beg (point))
	(insert blk)
	(goto-char beg)
	(kill-line)
	(insert line)
	(re-search-forward "^Subject")
	(forward-line)))

    ;; If there is no babyl at all we may want to insert the RMAIL headers
    ;;
    (when (y-or-n-p "insert Rmail headers too?")
      (if (null (re-search-forward (regexp-quote "***")))
	  (message "Can't find ***")
	(forward-line 1)
	(insert (mapconcat 'concat rmail-lines "\n") "\n")))))


;;}}}

;;{{{ example

;;; ......................................................... &example ...
;;
;; Here is sample text file that you can use for anti-spam replies
;; To unpack this call M-x tinyrmail-install-files
;;
;; file pkg.tar:
;; rw-r--r-- 240/200   2588 Nov 24 22:10 1996 spam.txt
;;
;; -----BEGIN PGP MESSAGE-----
;; Version: 2.6.3ia
;; Comment: Base64 signed. File: pkg.tar uncompresses to approx. 10K
;;
;; owHtV8+LHEUY3SA5dIuecvEg1kHYXZjp/ZHdDcRD2CQmbtiEJZslBBGp7q6ZqWx1
;; VaeqeieTP0BUBK968OJFPejVHwf15EUQFUUEb4J4CXjx4CX6vqrp2Y2gJ4Mo/R12
;; 6Or68X3vve9V70tzew8dnzu5cevEpx/d/fbiicfXjx1beejY73uPvLD21dd3zz55
;; 7/XjH/904/o9/d7nT+zku2/8NrmkH/v1zV8++GZx9f0f3vni5f1PXn3xlZ/VWz++
;; dvvLz75/+/m73/U/fDR/d+Hh/LjzpdRzCFfzKvO3/dwDjGXExtpa+D25sRx/V+Iv
;; Yn35JN5trK5trKyun1rewPjK6qnl9bnlB5lUG43z3OLIm5wrb/56nq7c3+4zLWb2
;; +x+JNB15X59eWipMldVWVkILn+FhiZSxL/VwKW8mwuZizK3IRr5S6bMXMclyJe+I
;; kh0I66TRWZY9l6abSgrNPVaxC41vrGDngK+pMCdNk8sGA7KqjfVcezXpMV5iuZeO
;; Fkh9wJ08EGrCFpzQJY35Ef66NPGGOexitOgxy/1IWLzimpnBQFiayJmTXtCgxx8x
;; YQXeDoVPE6kHxlbIyWg2sKZaZNIxrirjPGu0pPS5wplW4FCPgjAPO6TJFp4sgcGu
;; mNyUE6bkvnBMeqStSyYHbGKaWQWC5ZM0IcyqmLhgV4SnKfNKhTyxhtbxCJFgA25Z
;; BUTSpBamViJWFPb03vICB19DoWKe0rWCIwNOe6bJZnumxbuzihf7SjpMP2/Ccsyl
;; gsbAmJ4tA5s110jPWGbskGt5J+IBWHNBBUt/hm2xGlgYHRaP+IFgPE1so0TfDPp+
;; 1FT5U5gzlihHCyTAoAtUNAkcBWjp6fCwwMXuzuZl16KRsa35ihXInUuNQ7TxKA2k
;; Zmm6a9iYFkBkrJowU1OC7gxUE0AI48p4ByhR2cSFSonkQwIicazlbZoBLaSDDJ2o
;; hHORDFIa1U8ralM3KiIidZoMo7p7KNAHhlCT832BZYXHMmR73VhVXpelYNdFHnNk
;; QVFxK8t4XVvDixGzcjjySGBMsgvL2GxdUvOhcBnbntYVoZMQGUmNXTrHdoTWAn2y
;; 6RXYlEWaXBWFsaXrsV2NM5SIYqwIeUN94ULBrWBaLY9FnrELoB99UoFgxivTQB44
;; tAL+kxmalIKAOMfSj+LSXLgwDw8VCBwAFbWPOl3o8lZrxu6LMk1Ko+eD6ELn7sd+
;; xIz5kpHSjrQ7sQNEQGWbLHG9pQmYsEFQ4NDyeiQLF4p0SLlEljXPpZKecPLG0Cso
;; m9iVxDzSRRNPWyqoNScCtUdHUcahwbAPdQYaVliSMiyCHTGKHs6KE9tm1QZYiniW
;; UBI9zoPd0NDQTlgFYRGZoeFLwFJIEZSS3KAOBP7RoMYBoCGypsol0WOFYCUpejwS
;; UcFajF2bmmOlUMSI9CTXikMY1qDXKGVMnJ3cQ2vH3dIE6TWevGrQ2ibcRlpAtA8l
;; KjHkOcDBDpQxJ+ILobB9mgS/mG6ZsYUtD4fxVh5IrmBHFmaOt9kiORNrqOcGaRKX
;; 5yTiYK3ADs4bwL7VhKIheFcTyawfPIJcAaIkcqZOfGgKYCtWXPZwQhPlJUlOyHQg
;; xrPqAO41WbsABGTdFAXSHjSKNIwLp0DKhDvBfjpN2TQuwpSnl4UJO3IHQ7fgNGAy
;; 1cMzwf6wXKk+cpdDTeom2XjMF3QKUUr9yy5T/vfdalYM4cfYZjLlPnZ6bCqj+0qC
;; EKlLcbtt9QkfRSWjsaBiuosZYexAJuXqWkWBYdM4THREdwUEySapP6UG2FYGQiGn
;; XkyXzGAI5H2AqW5yJYt43YbcaO+M7TTh0mR7V7fTZIGXpY02Ocufpi2SWsNARnhw
;; ut17bSfhjiOnuO8+JzmHe4IaifwiZMylIglE0z8vXWEFdRxHD50LvMkCvO05+gxh
;; O9Fl6NOBZDMdpfYYWtPUhxcjvh2I0SPMTw0KfdQPCdwk6WoBa0AXURcgP1kQHeTA
;; m9g9bBkda8SxruLwNF/jeiWywr0PkY0Yp2N9xq3PjkwB3rm8c3SEPqJqCNU6jBak
;; iiIjE7/VCPenlQvISfNKRIUvwiUaVZJRQOol27zGNre32w8OKozESGZP3KkGrd/a
;; Nyhw9NUVfTfAjDrNfLiC6eMLdR1iNnPLllncWWWDDw+0y9Nwuo7//z////5/IF10
;; 0UUXXXTRRRdddNFFF1100UUXXXTRRRdddNFFF138k/EH
;; =iNSX
;; -----END PGP MESSAGE-----

;;}}}

(add-hook 'tinyrmail-:get-new-mail-hook
	  'tinyrmail-delete-function)

(add-hook 'tinyrmail-:ube-send-to-postmasters-hook
	  'tinyrmail-ube-cc-spam-archive)

(provide   'tinyrmail)
(run-hooks 'tinyrmail-:load-hook)

;;; tinyrmail.el ends here
