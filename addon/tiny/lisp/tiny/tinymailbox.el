;;; @(#) tinymailbox.el --- Berkeley style aka std. mailbox browsing minor mode
;;; @(#) $Id: tinymailbox.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1997-09
;; Keywords:        tools
;;
;; To get information on this program use ident(1) or do M-x
;; tinymailbox-version. Look at the code with folding.el

;; LCD Archive Entry:
;; tinymailbox|Jari Aalto|jari.aalto@poboxes.com|
;; (m)ail(b)o(x) minor mode. Browse and manage std Unix .mbx .mbox files.|
;; 2002-08-01|$Revision: 1.1 $|~/misc/tinymailbox.el.Z|

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
;;  ~/.emacs startup file. Code can be extracted with function
;;  tinylib.el/ti::package-rip-magic
;;
;;* _
;;* (require 'tinymailbox)
;;* _
;;
;;  You can also use the preferred way: autoload
;;
;;* (autoload 'tinymailbox-mode          "tinymailbox "" t)
;;* (autoload 'turn-on-tinymailbox-mode  "tinymailbox "" t)
;;* (autoload 'turn-off-tinymailbox-mode "tinymailbox "" t)
;;* ;;  Put all minor mode activations below C-c m map
;;* ;;  m)ailbox minor mode
;;* ;;
;;* (global-set-key "\C-cmm"  'tinymailbox-mode)
;;* _
;;* ;; also do this so that the mode is invoked automatically
;;* ;; When you load your mailbox. This line was automatically installed
;;* ;; if you used the the `require' install method.
;;* ;;
;;* (require 'tinylibm)
;;* (ti::assoc-replace-maybe-add
;;*   'auto-mode-alist
;;*   '(("\\.mbo?x\\'"    . turn-on-tinymailbox-mode)))
;;
;;  If you have any questions, use this function to contact author
;;
;;       M-x tinymailbox-submit-bug-report


;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:
;;
;;  Preface, sep 1997
;;
;;	It is possible to use Procmail http://www.procmail.org/ to manage
;;	growing incoming mail. But sometimes your recipes go wrong and
;;      mail ends up folders that you dind't intend to.
;;	People usually direct UBE, UCE and Spam mail to different folders, but
;;      sometimes procmail filter just guesses wrong and it sends perfetly
;;      valid mail into one of these reject folders. It is good to check
;;	the Spam mailboxes manually for valid mail and then extract it out
;;	of them. Not very nice job to do. At the the time Gnus was not
;;	available for managing multiple forlders so I decided to pull out
;;	some old code and make it a package.
;;
;;  Overview of features
;;
;;	o   Browse standard unix mailbox .mbox .mbx .spool
;;	o   Kill, copy messages from mailbox. Copy message bodies.
;;	o   Highlighting and defcustom supported.
;;	o   Hide or show headers during mailbox browsing.
;;	o   Simple summaries can be done with `occur' command. Eg. to browse
;;	    messages based on `From' or `Subject' Headers.
;;
;;  Showing and hiding headers
;;
;;	When you browse a mail folder, it has lot of attached headers,
;;	which don't interest you at all when you want to look at the
;;	messages itself. for example, here is one typical header from
;;	a test message
;;
;;          From nobody Sun Sep 28 20:57:48 1997
;;          To: nobody
;;          Subject: Re: bandwidth (was: [RePol] check this issue)
;;          References: <tbd8lwmfid.fsf@totally-fudged-out-message-id>
;;          From: Foo bar <judgeDredd@marylyn.com>
;;          Date: 28 Sep 1997 20:57:47 +0300
;;          In-Reply-To: Jeff's message of "Tue, 23 Sep 1997 01:35:26 -0400"
;;          Message-ID: <tbiuvlmick.fsf@marylyn.com>
;;          X-Mailer: Quassia Gnus v0.11/Emacs 19.34
;;          Lines: 3
;;          Xref: marylyn.com junk-test:4
;;          X-Gnus-Article-Number: 4   Sun Sep 28 20:57:48 1997
;;
;;	When you go from this message with `tinymailbox-forward', the headers
;;      that you're interested in are only shown according to
;;      `tinymailbox-:header-show-regexp'. The messages headers are collapsed
;;      as you move around the messages. This approach was chosen, so that
;;      parsing a big message file (Gnus nnfolder backend) wouldn't put you
;;      on hold while the headers were collapsed. Now the headers are
;;      handled while you browse forward and backward. The above headers
;;      lookes like this after
;;	processing it:
;;
;;          To: nobody
;;          Subject: Re: bandwidth (was: [RePol] check this issue)
;;          From: Foo bar <judgeDredd@marylyn.com>
;;          Date: 28 Sep 1997 20:57:47 +0300
;;          X-Mailer: Quassia Gnus v0.11/Emacs 19.34
;;          X-Gnus-Article-Number: 4   Sun Sep 28 20:57:48 1997
;;
;;	By default all the `X-' headers are shown, so you may want to make
;;	the `tinymailbox-:header-show-regexp' a bit more restrictive if your
;;	messages have too much X-headers. You can toggle this message
;;	hiding feature with
;;
;;	    C-q	    or tinymailbox-header-hide-mode
;;
;;  Copying or deleting messages
;;
;;	When you browse the mailbox, you can perform copy or delete on
;;	the current message with following commands.
;;
;;	    ' RET   tinymailbox-copy
;;	    ' SPC   tinymailbox-copy-body
;;	    ' d     tinymailbox-delete
;;
;;  Moving between the messages
;;
;;	There are couple of movement commands that let you jump from
;;	one message to another. See also variable `tinymailbox-:move-header-regexp'
;;
;;	    C-p	    tinymailbox-forward-body  or Ctrl-home
;;	    C-n	    tinymailbox-backward-body or Ctrl-end
;;	    home    tinymailbox-forward	(see tinymailbox-:move-header-regexp)
;;	    end	    tinymailbox-backward
;;
;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)
(require 'sendmail)

(eval-when-compile (ti::package-use-dynamic-compilation))

(eval-and-compile
  (autoload 'mail-fetch-field		"mail-utils")
  (autoload 'string-rectangle		"rect" "" t))


(ti::package-defgroup-tiny TinyMailbox tinymailbox-: tools
  "Mailbox management minor mode.
  Overview of features

	o   Browse standard unix mailbox .mbox .mbx .spool
	o   Kill, copy messages from mailbox. Copy message bodies.
	o   Highlighting and defcustom supported.
	o   Hide or show headers during mailbox browsing.
	o   Simple summaries can be done with `occur' command. Eg. to browse
	    messages based on `From' or `Subject' Headers.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinymailbox-:load-hook nil
  "*Hook run when file has been loaded."
  :type 'hook
  :group 'TinyMailbox)

;;; ......................................................... &private ...

(defvar tinymailbox-:last-file nil
  "Last file used by `tinymailbox-message-to-folder'.")

;;; ........................................................ &v-public ...

(defcustom tinymailbox-:font-lock-keywords
  '(("From:[ \t]*\\(.*\\)"
     (1 font-lock-function-name-face))

    ("Reply-To:[ \t]*\\(.*\\)"
     (1 font-lock-function-name-face))

    ("Subject:[ \t]*\\(.*\\)"
     (1  font-lock-keyword-face))

    ("^\\(X-[A-Za-z0-9-]+\\|Date\\):[ \t]*\\(.*\\)"
     (1  font-lock-reference-face)))
  "*Font lock keywords."
  :type   'sexp
  :group  'TinyMailbox)


(defcustom tinymailbox-:auto-mode-alist
  '(("\\.mbo?x\\'"    . tinymailbox-mode)

    ;;  Gnus 5 `nnml' backend where procmail should deliver to
    ;;  list.xxxx.spool
    ;;

    ("\\.spool\\'"    . tinymailbox-mode))
  "Items to add to `auto-mode-alist' to turn `tinymailbox-mode' on when file load."
  :type '(repeat
	  (list
	   (string :tag "File Regexp")
	   (const 'tinymailbox-mode)))
  :group  'TinyMailbox)


(defcustom tinymailbox-:move-header-regexp "^Subject:"
  "Regexp that is use in movement commands. See `tinymailbox-forward'."
  :type  'string
  :group  'TinyMailbox)


(defcustom tinymailbox-:header-show-regexp
  "^Subject:\\|^To:\\|^From:\\|^Newsgroups:\\|^X-\\|^Date:"
  "Regexp to show the interesting headers. Others will be hidden."
  :type  'string
  :group 'TinyMailbox)

(defcustom tinymailbox-:header-hide-mode t
  "If non-nil then uninteresting headers are hidden while you move."
  :type  'boolean
  :group 'TinyMailbox)




;;; .......................................................... &v-menu ...

(defcustom tinymailbox-:menu-use-flag t
  "*Non-nil means to use echo-area menu."
  :type  'boolean
  :group 'TinyMailbox)

(defvar tinymailbox-:menu-main
  (list
   '(format
     "%sTinyMbx: hdr)+-C-q  copy)RETSPC m)ail oO)ccur f)ld F)ile ?H) d)el x)mode off"
     (if current-prefix-arg
	 (format "%s "  (prin1-to-string current-prefix-arg)) "" ))
   '(
     (?+     . ( (call-interactively 'tinymailbox-header-show)))
     (?-     . ( (call-interactively 'tinymailbox-header-hide)))
     (?\C-q  . ( (call-interactively 'tinymailbox-header-hide-mode)))
     (?d     . ( (call-interactively 'tinymailbox-delete)))
     (?\C-m  . ( (call-interactively 'tinymailbox-copy)))
     (?\     . ( (call-interactively 'tinymailbox-copy-body)))
     (?m     . ( (call-interactively 'tinymailbox-mail)))
     (?o     . ( (call-interactively 'call-interactively 'tinymailbox-occur)))
     (?O     . ( (call-interactively 'tinymailbox-occur-subject)))
     (?f     . ( (call-interactively 'tinymailbox-message-to-folder)))
     (?F     . ( (call-interactively 'tinymailbox-message-to-file)))
     (??     . 'tinymailbox-:menu-help)
     (?H     . 'tinymailbox-:menu-help)
     (?x     . ( (call-interactively 'turn-off-tinymailbox-help)))))
  "*TinyMailbox echo menu.

Header controls:

    +    Show headers
    -    Hide headers
    C-q  Toggle header mode
    d    Header delete

Copy options

    RET  Copy message
    SPC  Copy body

Transfer options

    f    Append message to a folder
    F    Append message to a file

Miscellaneous

    ?    Help menu
    H    Help menu
    x    Exit mode")




;;;###autoload (autoload 'tinymailbox-version "tinymailbox" "Display commentary" t)
(eval-and-compile
(ti::macrof-version-bug-report
 "tinymailbox.el"
 "tinymailbox"
 tinymailbox-:version-id
 "$Id: tinymailbox.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinymailbox-:version-id
   tinymailbox-:load-hook
   tinymailbox-:last-file
   tinymailbox-:font-lock-keywords
   tinymailbox-:auto-mode-alist
   tinymailbox-:move-header-regexp
   tinymailbox-:header-show-regexp
   tinymailbox-:header-hide-mode
   tinymailbox-:menu-use-flag
   tinymailbox-:menu-main)
 '(tinymailbox-:debug-buffer)))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ minor mode

;;;###autoload (autoload 'tinymailbox-install-mode  "tinymailbox" "" t)
;;;###autoload (autoload 'tinymailbox-mode	    "tinymailbox" "" t)
;;;###autoload (autoload 'turn-on-tinymailbox-mode  "tinymailbox" "" t)
;;;###autoload (autoload 'turn-off-tinymailbox-mode "tinymailbox" "" t)
;;;###autoload (autoload 'tinymailbox-commentary    "tinymailbox" "" t)


(eval-and-compile
(ti::macrof-minor-mode-wizard
 "tinymailbox-" " Mbx" "'"  "Mbx" 'TinyMailbox "tinymailbox-:"

  "Unix mailbox minor mode.

You use this minor mode to browse your .mbx and .mbox files or any file
hich is stored in standard unix mailbox format (like news articles).  The
file format is as follows. notice that there is no mistake, the first
'From ' field marks the message biginning and there is no colon.

    From Foo Wee Gee <Gee@this.is>
    Subject: Swiss Yodddla-laddli-duu
    Newsgroups: nothing.interesting

    BODY 1 OF MESSAGE

    From Foo Wee Gee <Gee@this.is>
    Subject: Swiss Yodddla-laddli-duu
    Newsgroups: nothing.interesting

    BODY 2 OF MESSAGE

Mode description:

Prefix key to access the minor mode is defined in `tinymailbox-:mode-prefix-key'

\\{tinymailbox-:mode-map}"

  "TinyMailbox"

  (progn				;Some mode specific things? No?
    (tinymailbox-font-lock)

    ;;  When mode is turned off, we must kill the text properties we used

    (unless tinymailbox-mode
      (save-excursion
	(ti::text-property-search-and-modify '(owner timbx) nil))))

  "Mailbox mode"
  (list                                  ;arg 10
   tinymailbox-:mode-easymenu-name
   "----"
   ["Message forward"	    tinymailbox-forward		t]
   ["Message backward"	    tinymailbox-backward	t]
   ["Body forward"	    tinymailbox-forward-boby	t]
   ["Body backward"	    tinymailbox-backward-body	t]

   "----"

   ["Header Hide"	    tinymailbox-header-hide	t]
   ["Header Show"	    tinymailbox-header-show	t]
   ["Header show/hide mode" tinymailbox-header-hide-mode t]

   "----"

   ["Copy message"	    tinymailbox-copy		t]
   ["Copy message body"	    tinymailbox-copy-body	t]
   ["Delete message"	    tinymailbox-delete		t]

   "----"

   ["Append to file"	    tinymailbox-message-to-folder t]
   ["Write to file"	    tinymailbox-message-to-file	t]
   ["Send email to sender"  tinymailbox-mail		t]

   "----"

   ["Make Summary (occur)"  tinymailbox-occur		      t]
   ["Make Summary (occur subject)"  tinymailbox-occur-subject t]

   "----"
   ["Keyboard menu"         tinymailbox-menu-main       t]
   ["Package version"	    tinymailbox-version         t]
   ["Package commentary"    tinymailbox-commentary	t]
   ["Mode help"		    tinymailbox-mode-help	t]
   ["Mode off"		    turn-off-tinymailbox-mode	t])

  (progn

    (cond
     (tinymailbox-:menu-use-flag
      ;;  Using menu to remeber commands is easier if you don't use
      ;;  menu bar at all.

      (define-key   root-map [(home)]          'tinymailbox-backward)
      (define-key   root-map [(end)]           'tinymailbox-forward)

      (define-key   root-map "\C-p"            'tinymailbox-backward-body)
      (define-key   root-map "\C-n"            'tinymailbox-forward-body)
      (define-key   root-map [(control home)]  'tinymailbox-backward-body)
      (define-key   root-map [(control end)]   'tinymailbox-forward-body)

      (define-key root-map p 'tinymailbox-menu-main))
     (t
      (define-key   root-map [(home)]          'tinymailbox-backward)
      (define-key   root-map [(end)]           'tinymailbox-forward)


      (define-key   root-map "\C-p"            'tinymailbox-backward-body)
      (define-key   root-map "\C-n"            'tinymailbox-forward-body)
      (define-key   root-map [(control home)]  'tinymailbox-backward-body)
      (define-key   root-map [(control end)]   'tinymailbox-forward-body)

      (define-key   map  "+"     'tinymailbox-header-show)
      (define-key   map  "-"     'tinymailbox-header-hide)
      (define-key   map "\C-q"   'tinymailbox-header-hide-mode)
      (define-key   map  "d"     'tinymailbox-delete)
      (define-key   map  "\C-m"  'tinymailbox-copy)
      (define-key   map  " "     'tinymailbox-copy-body)

      (define-key   map  "m"     'tinymailbox-mail)
      (define-key   map  "o"     'tinymailbox-occur)
      (define-key   map  "O"     'tinymailbox-occur-subject)

      (define-key   map  "f"     'tinymailbox-message-to-folder)
      (define-key   map  "F"     'tinymailbox-message-to-file)

      (define-key   map "?"      'tinymailbox-help)

      (define-key   map  "Hm"	 'tinymailbox-mode-help)
      (define-key   map  "Hc"	 'tinymailbox-commentary)
      (define-key   map  "Hv"	 'tinymailbox-version)

      (define-key   map  "x"     'turn-off-tinymailbox-mode)

      (message "TinyMailbox: Use home/end to move between messages."))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-menu-main (&optional arg)
  "Show echo area menu and pass ARG to `ti::menu-menu'."
  (interactive "P")
  (ti::menu-menu 'tinymailbox-:menu-main arg))

;;}}}
;;{{{ Install

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-mode-candidate-p ()
  "Return non-nil is buffer is candidate for `tinymailbox-mode'."
  (and (not (or (memq major-mode
		 '(vm-mode
		   rmail-mode
		   article-mode
		   message-mode
		   mail-mode
		   gnus-summary-mode))
		(string-match
		 ;; Do not activate on
		 ;;
		 ;;  *.log
		 ;;  *.tmp
		 ;;  .procmailrc (dot files in general)
		 ;;
		 "^\\.\\|\\.\\(log\\|tmp\\)$\\|VM\\|RMAIL"
		 (or (buffer-name) ""))))
       (ti::mail-mailbox-p)))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinymailbox-mode-maybe ()
  "Turn on `tinymailbox-mode' if buffers looks like Berkeley mailbox."
  (when (tinymailbox-mode-candidate-p)
    (turn-on-tinymailbox-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-install (&optional uninstall)
  "Turn install mode, or optionally UNINSTALL."
  (interactive "P")
  (cond
   (uninstall
    (ti::assoc-replace-maybe-add
     'auto-mode-alist tinymailbox-:auto-mode-alist 'remove))
   (t
    (ti::assoc-replace-maybe-add
     'auto-mode-alist tinymailbox-:auto-mode-alist)))
  (ti::add-hooks 'find-file-hooks
		 'turn-on-tinymailbox-mode-maybe
		 uninstall)
  (when (interactive-p)
    (message "TinyMailbox %s"
	     (if uninstall
		 "uninstalled"
	       "installed"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-font-lock ()
  "Add/remove font lock support if `font-lock-mode' exist."
  (interactive)
  (let* ((sym 'font-lock-keywords)
	 orig)
    (when (and (boundp sym)
	       (ti::colors-supported-p))
      (cond
       (tinymailbox-mode
	(put 'tinymailbox-:font-lock-keywords 'original (symbol-value sym))
	(set sym tinymailbox-:font-lock-keywords)
	(turn-on-font-lock))
       (t
	(when (ti::listp
	       (setq orig
		     (get 'tinymailbox-:font-lock-keywords 'original)))
	  (set sym orig))))
      (when (and (boundp 'font-lock-mode)
		 (symbol-value 'font-lock-mode))
	;;  fontify approx. 50 lines or until point-max
	(font-lock-fontify-region
	 (point)
	 (min (+ (point) (* 80 50)) (point-max)))))))

;;}}}
;;{{{ Macros

;;; ----------------------------------------------------------- &macro ---
;;;
(put 'tinymailbox-message-macro 'lisp-indent-function 0)
(defmacro tinymailbox-message-macro (&rest body)
  "Do BODY on message. You can refer to `beg' and `end' for message region."
  (`
   (let* ((opoint  (point))
	  beg
	  end)
     ;; Just to make byteCompiler happy
     (if (null opoint)	(setq opoint nil))
     (if beg		(setq beg t))
     (if end		(setq end t))

     (tinymailbox-begin 'backward) (setq beg (point))
     (tinymailbox-begin)

     ;;   txt txt
     ;;   Last line of previous message is here....
     ;;
     ;;   From asdasdasdadas
     ;;   X-Header: blah
     ;;   ...

     (if (looking-at "From ")
	 (backward-line 1))		;Fix position a bit
     (setq end (point))

     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinymailbox-header-macro 'lisp-indent-function 0)
(defmacro tinymailbox-header-macro (&rest body)
  "Do BODY on message. You can refer to `beg' and `end' for message region."
  (`
   (let* (beg
	  end)
     ;; Just to make byteCompiler happy

     (if beg (setq beg t))
     (if end (setq end t))

     (tinymailbox-begin 'backward)	    (setq beg (point))
     (re-search-forward "^[ \t]*$")
     (beginning-of-line) (setq end (point))

     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinymailbox-paragraph-macro 'lisp-indent-function 0)
(defmacro tinymailbox-paragraph-macro (&rest body)
  "Set paragraph values locally while executing BODY."
  (`
   (let* ((sentence-end         "[.?!]*[ \n]+")
          (paragraph-start      "^[ \t]*$")
          (paragraph-separate   paragraph-start))
     (,@ body))))

;;}}}
;;{{{ misc

;;; ............................................................ &misc ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-p ()
  "Check if point is inside header."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[A-Z][^:]+: ")))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-message-move-beginning ()
  "Move to message beginning."
  (re-search-backward "^From " nil t))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-overlay (act &optional beg end)
  "If ACT is 'hide, hide overlay, otherwise highlight BEG END."
  (let* ((ov
          (if (boundp 'mouse-drag-overlay) ;Emacs, use this by default
              'mouse-drag-overlay
            'primary-selection-extent)))
    (cond
     ((eq act 'hide)
      (ti::xe-overlay-move ov 1 1)
      (pop-mark))
     (t
      (ti::xe-overlay-move ov beg end)
      (setq ov (symbol-value ov))
      (when (emacs-p)
        (push-mark
         (if (emacs-p)
             (ti::funcall 'overlay-start ov)
           (ti::funcall 'extent-start-position ov))
         t t)
        (push-mark
         (if (emacs-p)
             (ti::funcall 'overlay-end ov)
           (ti::funcall 'extent-end-position ov))
         t t)) ;; when
      (setq this-command 'set-mark)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-next ()
  "Find next header forward."
  (if (looking-at "^[^ \t\n]")
      (forward-line 1))
  (while (and (not (eobp)) (looking-at "^[ \t]"))
    (forward-line 1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-show-or-hide ()
  "Check `tinymailbox-:header-hide-mode' and act according to it."
  (if tinymailbox-:header-hide-mode
      (tinymailbox-header-hide)
    (tinymailbox-header-show)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-show ()
  "Call `tinymailbox-header-hide' with argument SHOW."
  (interactive)
  (tinymailbox-header-hide 'show))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-hide (&optional show)
  "Hide or SHOW headers according to `tinymailbox-:header-show-regexp'."
  (interactive "P")
  (let* ((re     tinymailbox-:header-show-regexp)
	 (prop   'invisible)
	 (propl  (list 'owner 'timbx
		       'tinymailbox-stat 'hidden
		       prop t
		       'rear-nonsticky t))
	 (prop-stat 'tinymailbox-stat)
	 (opoint (point))
	 point
	 status-property
	 put-property)
    (tinymailbox-header-macro
      (ti::save-buffer-modified
	(goto-char beg)

	;;  The hide on/off information is stored to the message beginning
	;;  - We look if it says 'hidden or 'shown
	;;  - If the user wants hidden headers, but they are already
	;;    hidden, then this function does nothing.

	(setq status-property
	      (memq prop-stat (text-properties-at (point))))

	(cond
	 (show
	  (setq put-property 'shown)
	  (when (or (null status-property)
		    ;; If text is already shown, then do nothing.
		    (and status-property
			 (not (eq (nth 1 status-property) 'shown))))
	    (ti::text-property-search-and-modify '(owner timbx) nil beg end)))
	 (t
	  (setq put-property 'hidden)
	  (when (or (null status-property)
		    (and status-property
			 (not (eq (nth 1 status-property) 'hidden))))
	    (while (< (point) end)
	      (cond
	       ((and (not (looking-at re))
		     ;; If this point has already marked visible, do nothing.
		     ;;
		     (or (null (eq 'timbx (get-text-property (point) 'owner)))
			 (null (get-text-property (point) prop))))
		(setq point (point))
		(tinymailbox-header-next)

		(ti::save-buffer-modified
		  (let (buffer-read-only)
		    (set-text-properties point (point) propl))))
	       (t
		(forward-line 1)))))))
	(put-text-property beg (1+ beg) 'owner 'timbx)
	(put-text-property beg (1+ beg) prop-stat put-property)))
    (goto-char opoint)))

;;}}}
;;{{{ move

;;; ............................................................ &move ...

;;; ----------------------------------------------------------------------
;;;
(eval-and-compile
(defun tinymailbox-fmacro-move-1 (func doc move-func re msg &rest body)
  "Use `tinymailbox-fmacro-move with FUNC DOC MOVE-FUNC RE MSG and BODY."
  (let* ((sym (intern (symbol-name (` (, func))))))
    (`
     (defun (, sym) (&optional arg)
       (, doc)
       (interactive "P")
       (let* ((Opoint  (point))
	      stat)
	 (if (eq (, move-func) 're-search-backward)
	     (beginning-of-line)
	   (end-of-line))

	 (cond
	  ((setq stat (funcall (, move-func) (, re) nil t))
	   (goto-char (match-end 0)))
	  (t
	   (goto-char Opoint)))

	 (tinymailbox-header-show-or-hide)

	 (,@ body)

	 (when (and (null stat) (interactive-p))
	   (message (, msg)))
	 stat)))))

);; eval-and-compile


;;; ----------------------------------------------------------------------
;;;
(defmacro tinymailbox-fmacro-move (func doc move-func re msg &optional body)
  "Create Move function FUNC DOC MOVE-FUNC RE MSG and BODY.
Created function arguments: (&optional arg)"
  (` (, (tinymailbox-fmacro-move-1
	 func doc move-func re msg body))))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload 'tinymailbox-forward "tinymailbox" "Go to next message." t)
(tinymailbox-fmacro-move
 tinymailbox-forward
 "Go to next message."
 're-search-forward tinymailbox-:move-header-regexp
 "TinyMailbox: message forward stop.")

;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload 'tinymailbox-backward "tinymailbox" "Go to previous message." t)
(tinymailbox-fmacro-move
 tinymailbox-backward
 "Go to previous message."
 're-search-backward tinymailbox-:move-header-regexp
 "TinyMailbox: message backward stop.")

;;; ----------------------------------------------------------------------
;;;
(tinymailbox-fmacro-move
 tinymailbox-forward-body
 "Go to next message body."
 're-search-forward "^From "
 "TinyMailbox: body forward stop."
 (and stat
      (setq stat (re-search-forward "^[ \t]*$" nil t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-backward-body ()
  "Go to previous message body."
  (interactive)
  (let* ((opoint (point))
	 stat)

    ;;  We must move to message beginning first.

    (tinymailbox-message-move-beginning)

    (forward-line -1)
    (if (null (re-search-backward "^From " nil t))
	(message "TinyMailbox: body backward stop.")
      (setq stat (re-search-forward "^[ \t]*$" nil t)))

    ;;  If none found, return to original position

    (when (and (null stat) (not (eq (point) opoint)))
      (message "TinyMailbox: body backward stop.")
      (goto-char opoint))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-header-hide-mode (arg &optional verb)
  "Toggle header hiding mode with ARG when moving between messages. VERB."
  (interactive "P")
  (ti::verb)
  (ti::bool-toggle tinymailbox-:header-hide-mode)
  (when verb
    (message "Header hiding mode is %s"
	     (if tinymailbox-:header-hide-mode "on" "off")))
  (tinymailbox-header-show-or-hide))


;;}}}
;;{{{ copy; delete

;;; ................................................... &manipulations ...

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-begin (&optional backward)
  "Move to message begin. Optionally BACKWARD."
  (interactive "P")
  (let* ((re "\nFrom ")			;Unix Mbox supposes this at be of msg.
	 case-fold-search)
    (cond
     (backward
      (if (re-search-backward re nil t)
	  (forward-line 1)
	(ti::pmin)))
     (t
      (unless (re-search-forward re nil t)
	(ti::pmax))))
    (beginning-of-line)))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-delete ()
  "Delete current message. point must be inside message."
  (interactive)
  (buffer-enable-undo)
  (tinymailbox-message-macro
    (forward-line 2)
    (kill-region beg (point))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-copy ()
  "Copy current message. point must be inside message."
  (interactive)
  (tinymailbox-message-macro
    (copy-region-as-kill beg end)
    (tinymailbox-overlay 'show beg end)
    (sit-for 0.5)
    (tinymailbox-overlay 'hide beg end)
    (if (interactive-p)
	(message "TinyMailbox: Message copied as kill."))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-copy-body ()
  "Copy body of current message. point must be inside message."
  (interactive)
  (buffer-enable-undo)
  (tinymailbox-message-macro
    ;;  body starts after all headers.
    (goto-char beg)
    (re-search-forward "^[ \t]$")
    (forward-line 1) (setq beg (point))
    (copy-region-as-kill beg end)
    (tinymailbox-overlay 'show beg end))
  (if (interactive-p)
      (message "TinyMailbox: Message body coied.")))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-message-to-folder (file)
  "File current message by appending it to FILE."
  (interactive
   (list
    (read-file-name
     "Append to folder: "
     (if tinymailbox-:last-file
	 (file-name-directory tinymailbox-:last-file))
     nil
     nil
     (if tinymailbox-:last-file
	 (file-name-nondirectory tinymailbox-:last-file)))))
  (tinymailbox-message-macro
    (setq tinymailbox-:last-file file)
    (append-to-file beg (min (1+ end) (point-max)) file)
    (goto-char opoint)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymailbox-message-to-file (file)
  (interactive
   (list
    (read-file-name
     "Write to file: "
     (if tinymailbox-:last-file
	 (file-name-directory tinymailbox-:last-file))
     nil
     nil
     (if tinymailbox-:last-file
	 (file-name-nondirectory tinymailbox-:last-file)))))
  (tinymailbox-message-macro
    (setq tinymailbox-:last-file file)
    (write-region beg (min (1+ end) (point-max)) file)
    (goto-char opoint)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-occur (regexp)
  "Create Simple `Summary' buffer by running REGEXP `occur'.
Try Subject: or From:"
  (interactive "sOccur Summary: ")
  (save-excursion
    (ti::pmin)
    (occur regexp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-occur-subject ()
  "Generate Subject summary."
  (interactive)
  (tinymailbox-occur "^Subject:.*"))


;;; ----------------------------------------------------------------------
;;;
(defun tinymailbox-mail ()
  "Compose mail using current message."
  (interactive)
  (let* ((buffer   (current-buffer))
	 to
	 subject
	 start)
    (tinymailbox-message-macro
      (ti::narrow-safe beg end
	(ti::pmin)
	(setq to      (mail-fetch-field "From")
	      subject (concat "Re: " (mail-fetch-field "Subject")))
	(ti::pmin) (re-search-forward "^[ \t]*$")
	(setq beg (point)))
     (mail)
     (ti::pmax) (setq start (point))
     (insert-buffer-substring buffer beg end)
     (string-rectangle start (point-max) mail-yank-prefix)
     (goto-char start)
     (ti::save-with-marker-macro
       (ti::pmin)
       (re-search-forward "To: ")
       (insert (or to "Unknown"))
       (ti::pmin)
       (re-search-forward "Subject: ")
       (insert "Re: " (or subject ""))))))

;;}}}

(add-hook  'tinymailbox-:mode-define-keys-hook 'tinymailbox-mode-define-keys)
(provide   'tinymailbox)

(tinymailbox-install)

(run-hooks 'tinymailbox-:load-hook)

;;; tinymailbox.el ends here
