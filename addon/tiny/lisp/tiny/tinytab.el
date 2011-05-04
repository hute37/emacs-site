;;; @(#) tinytab.el --- Programmers TAB minor mode. Very flexible.
;;; @(#) $Id: tinytab.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1995-10
;; Keywords:	    tools
;;
;; To get information on this program use ident(1) or do M-x tinytab-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinytab.el|Jari Aalto|jari.aalto@poboxes.com|
;; TAB handling minor mode, e.g. step 4 chars bck/fwd , insert/del 4 spaces |
;; 2002-08-05|$Revision: 1.1 $|~/misc/tinytab.el.Z|

;; COPYIGHT NOTICE
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
;;   Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file. Rip code with with tinylib.el/ti::package-rip-magic
;;
;;     (require 'tinytab)
;;
;;   or use this; your .emacs loads up a bit quicker
;;
;;* _
;;*  (autoload 'tinytab-mode		"tinytab" t t)
;;*  (autoload 'tinytab-return-key-mode "tinytab" t t)
;;
;;   You also need keybingins to use this module
;;
;;* _
;;*  (global-set-key "\C-c\t"        'tinytab-mode)   ;; Non-window
;;*  (global-set-key [(control shift backtab)] 'tinytab-mode)
;;*  (global-set-key "\C-c\C-m"      'tinytab-return-key-mode)
;;* _
;;
;; - It load this file only when you turn the mode on for the first time.
;;
;; - Here is how you use your ow setup
;;
;;*  (add-hook 'tinytab-:load-hook 'my-tinytab-:load-hook)
;;* _
;;*  (defun my-tinytab-:load-hook ()
;;*    "Add some things after install"
;;*    (add-hook 'tinytab-mode-define-keys-hook 'my-tinytab-keys))
;;* _
;;*  (defun my-tinytab-keys ()
;;*    "My tinytab key additions, override settings."
;;*    ;; ... code here ...
;;*    )
;;* _
;;
;; - If you have any questions, use this functions
;;
;;	M-x tinytab-submit-bug-report	,send bug report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;; Preface, oct 1995
;;
;;	There was a post in gnu.emacs.sources (what an source of
;;	inspiration), where someone asked:
;;
;;	    "Is there anyway to reset the number of spaces that TAB does?
;;	     Like, I want to make it jump four spaces instead of the
;;	     usual whatever.How can I set the tabs to 4?"
;;
;;	and the typical answer was:
;;
;;	    "In .emacs, set the variable tab-stop-list, like so:
;;	     (setq tab-stop-list (list 4 8 12 ...))"
;;
;;	Well, A regular user does not want to touch the original
;;	`tab-stop-list', because the 8 spaces per tab is the norm. But for
;;	programming the 4 tabs is norm, like for shell programming or for
;;	simple memos and text documents. The goal was to write a minor
;;	mode, which you can turn on and off, which handles _only_ tab key.
;;	There also exists tinyindent.el, which is tab-alike mode too, but
;;	there is a subtle, but very important difference: it behaves
;;	according to previous lines, so its job is to "line up" with the
;;	previous text or code. Instead, this mode was supposed to be plain
;;	rigid. The tab goes where you want it, and you can control the
;;	amount of movement to either direction, back or forward.
;;
;;  Overview of features
;;
;;	o   Programmable TAB. If you set the count to to 4,
;;	    you can virtually program "blindly" without any other modes.
;;	o   Selectable: 2, 4, 8 .. space indent.
;;	o   moving commands: tab-forward, tab-backward
;;	o   modify commands: tab-insert, tab-delete
;;	o   indent commands: tinytab-indent-by-tab-width, forward , backward
;;	o   Simple Positioning of braces { } supported when TAB hit.
;;
;;	Extras
;;
;;	o   Special auto-indent function offered for return key,
;;	    switch it on, and you can continue your shell,awk,perl comments...
;;	o   Press C-c TAB and your in constant indentation mode where
;;	    keys "wq" "as" "zx" indent region <--> by 1,2 and 4 until
;;	    esc pressed.
;;
;;  What this package does?
;;
;;	Mimic `tab-stop-list' with minor mode if some analogy can be
;;	drawn. You only set one variable, that controls the amount of
;;	movement, whereas you would have to put many values inside
;;	`tab-stop-list'. The variable for tab widths is:
;;
;;	    tinytab-:width-table
;;
;;	The advantage is, than you don't have to alter the `tab-stop-list'
;;	at all. When the mode is off, the tabs are back again as they were
;;	(or as the other mode thinks how the tab should be) There is a
;;	little more than that: movement and deletion functions, that really
;;	make day shine when you program. They all operate according to the
;;	variable `tinytab-:width'. Try out the functions
;;
;;	    tinytab-indent-by-tab-width
;;	    tinytab-indent-by-tab-width-back
;;
;;	to format region of code forward and backward easily. (This is
;;	different than using command indent-region) When you delete
;;	backward "one indentation level", you can do it when the division
;;	factor is 2/4/8. The nearest column that satisfies the division
;;	factor is used when "indenting back". See the example:
;;
;;      Before:
;;                *		<< cursor here
;;      123 567 9012		<< columns, starting from 1
;;
;;      After:
;;             *		<< cursor here, at div-factor compliant point
;;      123 567 9012		<< columns, starting from 1
;;
;;	Don't forget to look at function
;;
;;	    tinytab-change-tab-width
;;
;;	Which allows you to switch between 2, 4, 8, you name it, tabs
;;	widths with one key press during the curren tminor mode.
;;
;;  Major modes and this minor mode
;;
;;	When you use some programming mode, say C++ mode, it usually
;;	provides function to indent the line right when you press tab key.
;;	If you then turn on this mode, you loose the mode specific
;;	indenting, because turning on minor mode overrides the underlying
;;	major mode bindings. There is included one simple function to deal
;;	with major modes: it preserves the original indenting style in some
;;	extent, so that you can use this minor mode and current major mode
;;	together. In variable `tinytab-:tab-insert-hook' there is function
;;	`tinytab-tab-mode-control' which looks at variable
;;
;;	    tinytab-:mode-table
;;
;;	If the mode is listed in the table _and_ current point is at the
;;	beginning of line, then the line is handled by original major mode
;;	and not by this minor mode. This allows you to indent the line
;;	initially and then use normal tab indent within the current line
;;	according to this minor mode.
;;
;;	However, this minor mode is normally meant to be used as turn
;;	on/off basis in such programming modes that indent lines when you
;;	pressing tab key. Current compatibility function
;;	`tinytab-tab-mode-control' only allows you to get some flexibility
;;	when this mode is temporarily on. Bind this mode to some fast key
;;	which you can use to toggle this mode on/off when you need tab for
;;	a moment in programming modes. If you don't want any support to
;;	major modes, put following into your $HOME/.emacs
;;
;;	    (setq tinytab-:mode-table nil)
;;
;;  Return key addition
;;
;;	In this package there is also this little function which will
;;	make itself a must in no time:
;;
;;	    tinytab-return-key-mode   ;; I have bound this to C-c C-m
;;
;;	When the function is active, you can continue the ndentation from
;;	current position.
;;
;;				// Comment here. Call C-c C-m...and press RET
;;				// And it automatically indents here.
;;
;;	See variable
;;
;;	    tinytab-:auto-indent-regexp
;;
;;	what line prefixes are "copied" along with the indented spaces.
;;
;;  Development note
;;
;;	This package solely concerns TAB key. Nothing outside of it is
;;	counted. I have no intention to make this a full fledged
;;	programming mode, since there is `sh-mode', `ksh-mode', `perl-mode'
;;	and many more for other languages. I almost added
;;
;;	    (make-variable-buffer-local 'tinytab-:width)
;;	    (make-variable-buffer-local 'tinytab-:width-table)
;;
;;	But after thinking I decided not to add it to this package, since
;;	most of the time user is satisfied with the tab "4", at least
;;	that's what I use mostly. And changing the width usually means
;;	a little flick of a finger to reach out the function
;;
;;	    tinytab-change-tab-width
;;
;;	In very rare occasions one wants different tab widths in different
;;	buffers and in those cases you can add the lisp commands
;;	`make-variable-buffer-local' to mode hooks or after `tinytab-:load-hook'.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-when-compile
  (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyTab tinytab-: tools
  "TAB minor mode. The most basic 'programming mode' for any language.
  Overview of features

	o   Programmable TAB. If you set the spacing(div factor) to 4,
	    you can virtually program blindly without any other modes.
	o   moving commands: tab-forward, tab-backward
	o   modify commands: tab-insert, tab-delete
	o   indent commands: indent-by-tab-width, forward , backward
	o   Positioning braces { } supported.

	Extras

	o   special auto-indent function offered for return key,
	    switch it on, and you can continue your shell,awk,perl comments...
	o   Press C-c TAB and your in contant indentation mode where
	    keys \"wq\" \"as\" \"zx\" indent region <--> by 1,2 and 4 until
	    esc pressed.")

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

(eval-and-compile
(ti::macrof-version-bug-report
 "tinytab.el"
 "tinytab"
 tinytf-:version-id
 "$Id: tinytab.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinytab-:version-id
   tinytab-:load-hook
   tinytab-:tab-insert-hook
   tinytab-:tab-delete-hook
   tinytab-:width
   tinytab-:mode-name-base
   tinytab-:verbose
   tinytab-:width-table
   tinytab-:mode-table
   tinytab-:indent-region-key-message
   tinytab-:indent-region-key-list
   tinytab-:auto-indent-regexp)))


;;}}}
;;{{{ setup: variables

;;; .......................................................... &v-mode ...

;;;###autoload (autoload 'tinytab-mode			"tinytab" "" t)
;;;###autoload (autoload 'turn-on-tinytab-mode		"tinytab" "" t)
;;;###autoload (autoload 'turn-off-tinytab-mode		"tinytab" "" t)
;;;###autoload (autoload 'tinytab-commentary		"tinytab" "" t)
;;;###autoload (autoload 'tinytab-version		"tinytab" "" t)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinytab-" " +" nil "Tab" 'TinyTab "tinytab-:"		;1-6


  "Tab movement minor mode. Adjustable movement step.
Below you will see lot bindings that try to cope with event differencies
in non windowed and windowed emacs. If you're running
non/windowed version, Try to figure out which key combinations
work there best, In X, you have more flexible bindings.

References:

  tinytab-:width

Mode description:

\\{tinytab-:mode-map}

"
  "TinyTab"
  (progn
    (if tinytab-mode
	(tinytab-set-mode-name)))


  "Tab mode"
  (list                                  ;arg 10
   tinytab-:mode-easymenu-name
   ["Insert"			    tinytab-tab-key			t]
   ["Delete"			    tinytab-tab-del-key			t]
   ["Indent region forward"	    tinytab-indent-by-tab-width		t]
   ["Indent region backward"	    tinytab-indent-by-tab-width-back    t]
   ["Indent region dynamically"	    tinytab-indent-region-dynamically   t]
   ["Forward"			    tinytab-tab-forward			t]
   ["Backward"			    tinytab-tab-backward		t]
   ["Change step factor"	    tinytab-change-tab-width		t]
   ["Return key indent mode"	    tinytab-return-key-mode		t]
   "----"
   ["Package version"		    tinytab-version			t]
   ["Package commentary"	    tinytab-commentary			t]
   ["Mode help"			    tinytab-mode-help			t]
   ["Mode off"			    turn-off-tinytab-mode		t])

  (progn

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . Non Wwindow . .
    ;;  (define-key	tinytab-:mode-map "\M-\t"		'tinytab-tab-key)

    (define-key   root-map "\t"		'tinytab-tab-key)
    (define-key   root-map "\e\t"	'tinytab-indent-by-tab-width)
    (define-key   root-map "\C-c\e\t"	'tinytab-indent-by-tab-width-back)
    (define-key   root-map "\C-c\C-m"   'tinytab-return-key-mode)

    ;;  In HP, upper left, near the TAB
    ;;  Please relocate these two as you like.

    (define-key   root-map "\e`"	'tinytab-tab-forward)
    (define-key   root-map "\e~"	'tinytab-tab-backward)
    (define-key   root-map "\C-c`"      'tinytab-change-tab-width)
    (define-key   root-map "\C-c\t"     'tinytab-indent-region-dynamically)

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. X keys . .

    (define-key   root-map "\t"		'tinytab-tab-key)

    ;;  Unfortunately this is many times the famous
    ;;  "Alt-tab" -- change windows key in PC/Unix. Try to map it anyway.

    (define-key root-map [(alt tab)]	        'tinytab-tab-forward)
    (define-key root-map [(alt kp-tab)]	        'tinytab-tab-forward)
    (define-key root-map [(control ?\`)]	'tinytab-tab-forward)

    ;; .......................................................... back ...
    ;; Indent one line backward

    (define-key root-map [(shift backtab)]	'tinytab-tab-del-key)
    (define-key root-map [(shift hpBackTab)]    'tinytab-tab-del-key) ;; XEmacs
    (define-key root-map [(shift tab)]	        'tinytab-tab-del-key)
    (define-key root-map [(shift kp-tab)]	'tinytab-tab-del-key)
    (define-key root-map [(shift iso-lefttab)]  'tinytab-tab-del-key)

    (define-key root-map [(control tab)]	'tinytab-indent-by-tab-width)

    ;;  #todo: Hm. Linux KDE uses control-tab to switch desktops ??

    (when (string-match "kde" (or (getenv "WINDOW_MANAGER_TYPE") ""))
      (define-key root-map [(control shift tab)]
	'tinytab-indent-by-tab-width))

    ;; .......................................................... back ...
    ;;  Indent region backward

    (define-key root-map [(alt shift backtab)]
      'tinytab-indent-by-tab-width-back)

    (define-key root-map [(meta shift backtab)]
      'tinytab-indent-by-tab-width-back)


    (define-key root-map [(alt shift kp-tab)]
      'tinytab-indent-by-tab-width-back)

    (define-key root-map [(meta shift kp-tab)]
      'tinytab-indent-by-tab-width-back)


    (define-key root-map [(control meta tab)]
      'tinytab-indent-by-tab-width-back)

    (define-key root-map [(control alt tab)]
      'tinytab-indent-by-tab-width-back)


    (define-key root-map [(control alt iso-lefttab)]
      'tinytab-indent-by-tab-width-back)

    (define-key root-map [(control meta iso-lefttab)]
      'tinytab-indent-by-tab-width-back)


    ;; XEmacs A-S-TAB

    (define-key	root-map [(alt hpBackTab)]
      'tinytab-indent-by-tab-width-back))))



;;; ......................................................... &v-hooks ...

(defcustom tinytab-:load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyTab)

(add-hook 'tinytab-:load-hook 'tinytab-install-mode)

(defcustom tinytab-:tab-insert-hook
  '(tinytab-tab-mode-control
    tinytab-tab-brace-control
    tinytab-tab-forward-insert
    tab-to-tab-stop)
  "*List of functions to call for inserting logical TAB.
If any of these functions return non-nil, it is assumed,
that the tab handling was performed."
  :type  'hook
  :group 'TinyTab)


(defcustom tinytab-:tab-delete-hook
  '(tinytab-tab-backward-del)
  "*List of functions to delete a logical TAB backward.
If any of these functions return non-nil, it is assumed,
that the tab handling was performed."
  :type  'hook
  :group 'TinyTab)



;;; ....................................................... &v-private ...

(defvar tinytab-:width 4
  "Current tab division.")

;;; ........................................................ &v-public ...
;;; User configurable

;;   Simple name is enough. Think this as "Tab +" or "extended tab" -mode

(defcustom tinytab-:mode-name-base " +"
  "*Minor mode base name."
  :type  'string
  :group 'TinyTab)

;;  If I accidentally press key I didn't meant to, I want to know
;;  about it. Like in empty line, where is no visual aids
;;
(defcustom tinytab-:verbose t
  "*Enable Insert/Delete messages."
  :type  'boolean
  :group 'TinyTab)

(defcustom tinytab-:width-table '(2 4 8)
  "*When using `tinytab-change-tab-width', cycles through list of tab positions.
Default values are '(2 4 8)"
  :type  '(repeat integer)
  :group 'TinyTab)


(defcustom tinytab-:mode-table '(c++-mode cc-mode c-mode)
  "*List of mode name symbols where the TAB key call mode's TAB function.
But, only if the point is at the beginning of line."
  :type '(repeat (symbol
		  :tag "Mode name symbol"))
  :group 'TinyTab)


(defcustom tinytab-:indent-region-key-message
  "Indenting...  z=4 a=2 q=1 < >  w=1 s=2 x=4 [Esc - exit]"
  "*Message displayed while in dynamic indent mode."
  :type  'string
  :group 'TinyTab)

(defcustom tinytab-:indent-region-key-list '(?q ?w   ?a ?s   ?z ?x  ?\e)
  "*List of keys to control dynamic indenting. The keys go in pairs.
elt 0 1       ,left and right by 1
elt 2 3       ,left and right by 2
elt 4 5       ,left and right by 4
elt 6         ,exit key"
  :type '(list
	  character character
	  character character
	  character character
	  character)
  :group 'TinyTab)

;;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... return key  ..

(defcustom tinytab-:auto-indent-regexp "[#!;*/]\\|REM\\|//"
  "*If previous line match this regexp, it is copied when you hit RET.
This allows e.g. continuing C++'s // comments.
See function `tinytab-return-key-mode' to turn on this auto-indent feature."
  :type  'string
  :group 'TinyTab)


;;}}}

;;; ########################################################### &Funcs ###

;;{{{ extra functions

;;; ........................................................... &extra ...

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytab-width ()
  "Return TAB advance."
  (if (not (integerp tinytab-:width))
      (setq tinytab-:width 4))
  tinytab-:width)

;;; ----------------------------------------------------------------------
;;; - So that you can bind this to fast key
;;;
(defun tinytab-indent-by-tab-width-back (beg end)
  "Just shortcut to `tinytab-indent-by-tab-width'. Indent BEG END backward."
  (interactive "*r")
  (tinytab-indent-by-tab-width beg end 'back))


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-indent-by-tab-width (beg end &optional back)
  "Indent region BEG END by current tab division.
Optional arg BACK indents backward."
  (interactive "*r\nP")
  (tinytab-width)
  (let* ((div  (if back
		   (- 0 tinytab-:width)
		 tinytab-:width)))
    (indent-rigidly beg end div)))


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-mode-control ()
  "If `mode-name' in in the list `tinytab-:mode-table', call mode's tab key.
But only if
o  point is at the beginning of line.
o  the line is empty

This way you can partly mix e.g. C++ mode and this minor mode."
  (interactive)
  (let* ((sym  (intern (format "%s-map" (symbol-name major-mode))))
	 map
	 func)
    ;;  If we're ar the beginnning of line, see if there is keymap for
    ;;  this current mode. Then try to find function for "\t" key
    ;;  and call it

    (when (and (or (bolp)
		   (save-excursion
		     (beginning-of-line)
		     (looking-at "^[ \t]+$")))
	       (memq major-mode tinytab-:mode-table)
	       (boundp sym)
	       (keymapp (setq map (eval sym)))
	       (setq func (lookup-key map "\t")))
      (call-interactively func)
      t)))

;;; ----------------------------------------------------------------------
;;; - For a little more smarter TAB key to line up { } braces
;;;   in variaous programming modes I made this. It's simple,
;;;   but suffices for most common needs.
;;; - I don't know how the C-mode or cc-mode does this, but, hey,
;;;   this is one way :-)
;;;
;;;
(defun tinytab-tab-brace-control ()
  "When hitting TAB, line up {} braces, otherwise do nothing special.
Remember that opening brace, the {, follows previous line's indentation
and } follows the \"{\".

Return:

  t		,if TAB handled in this function.
  nil		,nothing done."
  (interactive)
  (let* (line
	 rest
	 indent
	 pindent			;previous
	 col
	 ret				;flag
	 handle
	 equal)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... check brace ...
    (save-excursion
      (beginning-of-line)

      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... { . .

      (cond
       ((looking-at "^\\([ \t]*\\)\\({.*\\)")
	(setq indent (or (match-string 1) "")
	      rest   (match-string 2)
	      handle '{ )
	(save-excursion
	  (forward-line -1)		; peek previous line indent
	  (setq pindent (or (ti::buffer-match "^[ \t]+" 0) ""))))

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. } ..

       ((looking-at "^\\([ \t]*\\)\\(}.*\\)")
	(setq indent (or (match-string 1) "")
	      rest   (match-string 2)
	      handle '{)
	(save-excursion
	  (cond
	   ((re-search-backward "^\\([ \t]*\\){" nil t)
	    (setq pindent (or (match-string 1) ""))))))))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... .. adjust brace ...

;;;    (ti::d! "AFTER"  (string= indent pindent) pindent indent rest)

    (setq equal (and indent pindent (string= indent pindent))
          col   (and indent (length (subst-char-with-string indent))))
    (cond
     ((and indent pindent equal
	   (memq handle '({ }))
	   (< (current-column) col))
      ;;  Pressing TAB, before the {, puts cursor to brace.

      (move-to-column col)
      (setq ret t))

     ((and indent pindent
	   (not (string= indent pindent)))

      ;;  This is reindent case: { and } didn't line up.

      (setq line (concat pindent rest)
	    col  (current-column)
	    ret  t)

      (setq col (current-column))
      (delete-region (line-beginning-position) (line-end-position))
      (insert line)

      ;;  If user is in the LEFT side of brace, put cursor to brace
      ;;  If user if in the RIGHT side, then move-to-column will
      ;;  preserve position.

      (move-to-column col)
      (when (< col (length (subst-char-with-string pindent)))
	(re-search-forward "{\\|}")
	(forward-char -1))))

    ret))

;;}}}

;;{{{ code: return key

;;; ----------------------------------------------------------------------
;;; Replaces the RET key
;;; The arg is just due to: (newline &optional ARG1)
;;;
(defun tinytab-auto-indent (&optional arg)
  "Automatically indent according to previous line.
If optional ARG is given, behave exactly like 'newline' function."
  (interactive "P")

  ;;  The RE matches few common comments and empty whitespaces
  ;;  #     = shell
  ;;  ;	    = lisp comments
  ;;  *	    = C comments
  ;;  !	    = .Xdefaults comments
  ;;  //    =  C++ comments
  ;;  REM   = oracle SQL comments

  (let ((re  (concat "^[ \t]*\\(" tinytab-:auto-indent-regexp "\\)*[ \t]*"))
	str)
    (cond
     (arg     ;;  We do not do anything special if user has given arg.
      (newline arg))

     ((not (eolp))   ;; Now let's see if user wanted fresh line

      ;;  User wanted to divide a line.
      ;;  Read the line up till cursor point

      (if (> (current-column) 0)
	  (setq str (buffer-substring (line-beginning-position) (point))))

      ;;  Ignore portion match --> nothing important matched

      (if (or (null str)
	      (not (and (string-match re str)
			(equal (length str)
			       ;;  The position (column in string)
			       (match-end 0)))))
	  (newline)			;something else than re, break line
	(let ((left-margin 0))		;Can't add string right otherwise
	  (newline)
	  (insert str))))
     (t
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. else ...
      ;;  Let's peek current line

      (if (> (current-column) 0)
	  (save-excursion
	    (beginning-of-line)
	    (setq str (ti::buffer-match re 0))))

      (if (null str)			;Nothing important here
	  (newline)
	(let ((left-margin 0))
	  (newline)
	  (insert str)))))))

;;}}}
;;{{{ code: tab


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-set-mode-name ()
  "Set mode name according to tabs in effect."
  (interactive)
  (let* ((base  tinytab-:mode-name-base)
	 (val   (tinytab-width)))
    (setq tinytab-:mode-name (format "%s%d" (or base "") val))
    (ti::xe-modeline-update)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-change-tab-width ()
  "Toggle tab width according to `tinytab-:width-table'."
  (interactive)
  (let* ((verb  (interactive-p))
	 (val   (tinytab-width))
	 (table tinytab-:width-table)
	 elt)
    (cond
     ((not (integerp val))
      (setq val (car table)))		;default value

     ((setq elt (memq val table))
      (if (eq 1 (length elt))		;it's last item
	  (setq val (car table))	;pick first value
	(setq val (nth 1 elt))))	;get next in the list

     (t					;can't find value from table ?
      (setq val (car table))))		;get first then.

    (setq tinytab-:width val)		;update
    (tinytab-set-mode-name)
    (if verb				;this does no harm....
	(message "TinyTab: Tab factor is now %d" val))))


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-backward-del ()
  "Move Tab backward.
Delete whitespace if all characters preceding the point are white spaces
_and_ the final position is in divide-able by current div-factor.

Eg. If you factor is 4, and there is 2 spaces before your cursor \"*\",
    This function will not delete the extra spaces, because it can't reach
    position 8.

	 bar Geezy *
	 12345678901234
            ^   ^     ^

In this case, calling the function is no-op.

References:

  `tinytab-:tab-delete-hook'
  `tinytab-:width'"
  (interactive "*")
  (let* ((div   (tinytab-width))
	 (verb  tinytab-:verbose)
	 (col   (current-column))
	 (dest  (- col (% col div)))
	 MARK
	 str
	 eob				;flag
	 p				;points
	 p2)


      ;; ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^ normal deletion ^ ^

      (setq MARK  (save-excursion
		    (if (eobp)
			(setq eob t)
		      (forward-char 1))	;push marker one forward
		    (point-marker)))

      (if (= col dest )			; would be exact
	  (setq dest (- col div )))
      (if (< dest 0)
	  (setq dest 0))


      (if (= col 0)			;beg of line
	  nil
	(move-to-column dest 'force)	;converts tabs to spaces.

	;; consider following:
	;;    actual         seen
	;;    12345678       123456789
	;;    ----------------------------------
	;;    #\thello      "#       hello"	,suppose cursor is in "h"
	;;    |  |
	;;    |  point 3
	;;    point 1
	;;
	;;    Now you indent back by 4, this is what happens
	;;    12345678       12345678
	;;    #   hello	   "#   hello"
	;;    |   |
	;;    |	 point 5			, Geez!
	;;    point 1
	;;
	;;    The tab is converted and it caused all point to be altered.
	;;    That's why we have to use the marker, because it stays
	;;    releative to text, in this case just _behind_ the letter "h"
	;;

	(setq p  (if eob
		     (marker-position MARK)
		   (1- (marker-position MARK))))
	(setq p2 (point))
	(setq str (buffer-substring p2 p))


;;;       (ti::d! p2 p str dest col (point) )

	(if (not (string-match "^[ \t]+$" str))
	    (progn
	      (if verb (message "Can't reach previous tab position"))
	      (goto-char p))		;do not move. Stay put.
	  (delete-region p2 p)

	  (if verb
	      (message "Tinytab: Deleted"))))

      (setq MARK nil)))			;kill the marker

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-backward ()
  "Logical tab movement backward, until reach beginning of line."
   (interactive)
   (let* ((div   (tinytab-width))
	  (dest  (- (current-column) div)))
     (if (< dest 0)
	 (setq dest 0))

     (move-to-column dest 'force)
     (if (and tinytab-:verbose
	      (looking-at "[ \t]+$"))	;no visible hints
	(message "TinyTab: Moved."))))


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-forward-insert ()
  "Move tab forward, insert spaces or tabs, see variable `indent-tabs-mode'.

References:

  `tinytab-:width'"
  (interactive "*")
  (let* ((col   (current-column))
	 (div   (tinytab-width))
	 (nbr   (- div (% col div)))
	 div
	 eob				;flag
	 MARK				;marker
	 str)

    (if (= 0 nbr)
	(setq str (make-string div ?\ ))
      (setq str (make-string nbr ?\ )))

    (insert str)

    (when indent-tabs-mode

      ;; - When we insert non-tabs, like in mode "tab 4", what happens is
      ;;   that we insert "    " + "    " ie. 4 + 4 spaces.
      ;; - but, we really like them to be like one "\t" code in text,
      ;;   So, let's fix the line every time something is inserted.
      ;; - We have to use markers again due to tabify.
      ;; - The EOB is special case

      (setq MARK (save-excursion
		   (if (eobp)
		       (setq eob t)
		     (forward-char 1))
		   (point-marker)))

      (tabify (line-beginning-position) (point))

      (goto-char (if eob
		     (line-end-position)
		   (1- (marker-position MARK))))

      (setq MARK nil))			;kill it
    t))					;we handled this


;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-forward (&optional verb)
  "Step logical tab forward. Does not insert anything. Stops at EOL. VERB.
Tabs are converted to spaces when needed; because you can't step inside
'\t' character in the line otherwise.."
   (interactive)
   (let* ((div   (tinytab-width))
	  (col   (current-column))
	  (nbr   (- div (% col div)))
	  (ecol  (save-excursion (end-of-line) (current-column)))
	  (dest  (+ col nbr))
	  (verb  (or verb tinytab-:verbose)))
     (cond
      ((> dest ecol)
       (end-of-line)
       (if verb (message "End of line.")))
      (t
       (move-to-column dest 'force)
       (if (and verb
		(looking-at "[ \t]+$"))	;no visible hints in this line...
	   (message "Moved."))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-tab-key ()
  "Main function for TAB key. See variable `tinytab-:tab-insert-hook'."
  (interactive)
  (let* ((sym   'tinymail-:complete-key-hook)
	 (tinymail-:complete-key-hook (if (boundp sym)
					  (symbol-value sym))))

    ;; No-op: byte compiler silencer

    (if (null tinymail-:complete-key-hook)
	(setq tinymail-:complete-key-hook nil))

    (remove-hook sym 'tinymail-complete-guest-packages)

    ;; keep this at the end

    (when (memq 'tab-to-tab-stop tinytab-:tab-insert-hook)
      (remove-hook 'tinytab-:tab-insert-hook 'tab-to-tab-stop)
      (add-hook    'tinytab-:tab-insert-hook 'tab-to-tab-stop 'append))

    ;;  We could use this instead:
    ;;
    ;;  (run-hook-with-args-until-success 'tinytab-:tab-insert-hook)
    ;;
    ;;  But then it would not be possible to debug which function gets
    ;;  called.

    (dolist (function tinytab-:tab-insert-hook)
      (if (funcall function)
	  (return)))))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-tab-del-key ()
  "Main function for TAB key. See variable `tinytab-:tab-delete-hook'."
  (interactive)
  (run-hook-with-args-until-success 'tinytab-:tab-delete-hook))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-return-key-mode (&optional mode verb)
  "Toggle auto indent MODE / regular newline mode. VERB."
  (interactive)
  (let* ((func  'tinytab-auto-indent)
	 (now
	  (or (and
	       ;;  e.g. in fundamental-map this value is nil and
	       ;;  nil cannot be used as an keymap for lookup-key
	       ;;
	       (current-local-map)
	       (lookup-key  (current-local-map) "\C-m"))
	      (lookup-key  (current-global-map) "\C-m")))
	 to)

    ;;  If we redefine return key here, user will nver get out.
    ;;  C-m is exit-minibuffer.

    (if (string-match "minibuf" (buffer-name))
	(error "Not allowed in this buffer."))

    (ti::verb)
    (cond
     ((or (null mode) (not (integerp mode)))
      (setq to (if (eq now 'tinytab-auto-indent) 'newline func)))
     ((< mode 1)
      (setq to 'newline))
     (t
      (setq to func)))

    (local-set-key "\C-m" to)

    (if verb
	(message "Return key: auto indent %s" (if (eq to func) "on" "off")))
    to))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-indent-region-dynamically (beg end)
  "Move region BEG END until exit key is pressed.
Default key setup, see `tinytab-:indent-region-key-list' where default
keys are:

  left  = q by 1, a by 2, z by 4
  right = w by 1, s by 2, x by 4"
  (interactive "*r")
  (let* ((i     1)
	 (k     tinytab-:indent-region-key-list)
	 (msg   tinytab-:indent-region-key-message)
	 ch
	 EXIT)
    (if (not (eq (length k) 7))
	(error "Not enough members in tinytab-:indent-region-key-list."))
    (setq EXIT (nth 6 k))

    (while (not (eq EXIT (setq ch (ti::read-char-safe-until msg))))

      (setq i nil)

      (cond
       ((eq ch  (nth 0 k))
	(setq i -1))

       ((eq ch  (nth 1 k))
	(setq i 1))

       ((eq ch  (nth 2 k))
	(setq i -2))

       ((eq ch  (nth 3 k))
	(setq i 2))

       ((eq ch  (nth 4 k))
	(setq i -4))

       ((eq ch  (nth 5 k))
	(setq i 4)))

      (if i
	  (indent-rigidly (region-beginning) (region-end) i)))))

;;}}}

(add-hook 'tinytab-:mode-define-keys-hook 'tinytab-mode-define-keys)

(provide   'tinytab)
(run-hooks 'tinytab-:load-hook)

;;; tinytab.el ends here
