;;; @(#) tinyef.el --- (E)lectric (f)ile minor mode. Easy C-x C-f filename composing
;;; @(#) $Id: tinyef.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1995-04
;; Keywords:	    extensions
;;
;; To get information on this program use ident(1) or do M-x tinyef-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinyef.el|Jari Aalto|jari.aalto@poboxes.com|
;; Electric file minor mode. Minibuffer. Easy filename composing.|
;; 2002-01-23|$Revision: 1.1 $|~/misc/tinyef.el.Z|

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
;; Put this file on your emacs-lisp load path, add following into your
;; ~/.emacs startup file
;;
;;	(require 'tinyef)
;;
;; Or use this autoload choice and your ~/.emacs will load quicker.
;; This is the preferred method:
;;_
;;*      (ti::when-package 'tinyef nil
;;*        (autoload 'turn-on-tinyef-mode "tinyef" "" t)
;;*        (add-hook 'minibuffer-setup-hook 'turn-on-tinyef-mode))
;;_
;; If you have any questions, use this function to contact maintainer
;;
;;	M-x tinyef-submit-bug-report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Apr 1995
;;
;;      There was a post in gnu.emacs.sources where Anders Lindgren
;;      <andersl@csd.uu.se> presented the basic code that allowed electric
;;      ~ and electric / characters to wipe out full (mini)buffer in certain
;;      cases. What you see here, is complete rewrite and enchancement of
;;      that code. This is a real must for any minibuffer file handling.
;;
;;  Overview of features
;;
;;	o   Easy filename editing. Deletes directories at time, delete line
;;	    backward, electric tilde, electric slash, electric colon etc..
;;	o   Useful for along with `C-x' `C-f' command.
;;	o   Mouse-3 in minibuffers clears the input.
;;
;;  Description
;;
;;      This ins only slightly *electric*, in a sense that it only defines
;;      some keys to be electric and it needs some other keys solely to its
;;      own use (you can't insert these chars to buffer without `C-q'
;;      `CHAR') If you're another non-english speaking, as I am, the
;;      electric means that the character you press behaves differently if
;;      the pressing happens around certain other charcters (some condition
;;      is met which triggers this other behavior). Other than that, the
;;      character behaves normally. Below there is a sample graph to give
;;      you an overview of what the so called "electricity" is is practice.
;;      In these presented cases cursor it at the end of line. Jusr load
;;      this file, press `C-x' `C-f' and experiment with keys "[]\/~".
;;
;;	o   b>> means what's on the line *before*
;;	o   a>> means what's there *after*
;;	o   ""  means what you just pressed
;;	o   []  means which action the character triggered
;;
;;	    b>> http:/www.site.com/~userFoo/dir1/dir2/dir3/ "/" [e-slash]
;;	    a>> http:/
;;	    The e-slash action wiped out the line, because writing
;;	    two slashes normally indicates, that you want to give
;;	    another path
;;
;;	    b>> ~/dir1/dir2/dir3/			"~" [e-tilde]
;;	    a>> ~
;;	    The action wiped the line away, because it assumed
;;	    you want to give "~userFoo" or another "~" relative path
;;
;;	    b>> ~/dir1/dir2/dir3/			"[" [step-delete-back]
;;	    a>> ~/dir1/dir2/
;;	    The action wiped previous directory name or until
;;	    special mark, See code, defaults are  ":/@" (ange-ftp things)
;;
;;	    b>> ~/dir1/dir2/			        "=" [undo]
;;	    a>> ~/dir1/dir2/dir3/
;;	    The action works like normal undo.
;;
;;	    b>> ~/dir1/dir2/				"`" [chunk-delete]
;;	    a>>
;;	    The action deleted whole line. It deletes until special marks
;;	    like "@:". If repeated, it deletes constantly backward
;;
;;  Automatic Isntallation
;;
;;	This file includes function `tinyef-install' which hooks the mode
;;	to the appropriate places. Eg. to your minibuffer. If you're in
;;	trouble, you can always turn this mode off with the supplied
;;	hotkey, which is by default `C-c' `/'. You can't "see" whether mode
;;	is on or off in minibuffer, since it doesn't have its own mode
;;	line. But calling the hotkey will tell you the state change. You
;;	can also remove this mode completely from your emacs if you need to
;;	do that in emergencies. just call following function with some
;;	prefix argument like `C-u' to `tinyef-install'
;;
;;  Mouse bindings in minibuffer
;;
;;	When this package loads, it calls function `tinyef-install-mouse'
;;	which defined following bindings to your minibuffer
;;
;;	    <-- BIG erase backward from point Mouse-3 (because it's free)
;;	    <-- Small delete backward C-mouse-1. Use C-mouse-3 to undo( -->)
;;
;;	This should give your free hands to cut,paste and Delete, without
;;	lifting your hand off the mouse.


;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyEf tinyef-: extensions
    "Electric file minor mode. Designed for minibuffer file prompt editing.
  Overview of features

	o   Easy filename editing. Deletes directories at time, delete line
	    backward, electric tilde, electric slash, electric colon etc..
	o   This is a must for minibuffer's C-x C-f command
    ")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinyef-:load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyEf)

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyef-:mode-key "\C-c/"
  "*Key to toggle function `tinyef-mode' on/off in global and minibuffer map."
  :type  '(string :tag "Key sequence")
  :group 'TinyEf)


;;  Note: In HP-UX the "[]\" keys are unshifted and just above ENTER
;;  key. Pick your favourites
;;
;;  Note: it's more confortable to have undo in some non-shift,
;;  non-control key when you're in minibuffer.

(defcustom tinyef-:mode-key-table
  '(
    (?\[   . step-delete-back)		;KEY -- action symbol
    (?\]   . step-delete-fwd)
    (?\|   . chunk-delete)
    (?\;   . move-back)
    (?\'   . move-fwd)
    (?\~   . e-tilde)			;electric keys
    (?\/   . e-slash)
    (?\$   . e-dollar)
    (?\=   . undo))
  "*Map keys to actions.
Refer source file's default values for action names.
If you change this; you must call function \\[tinyef-mode-map-define-keys]."
  :type '(repeat
	  (list
	   (character :tag "Electric char")
	   (choice
	    :tag "Action"
	    (const step-delete-back)
	    (const step-delete-fwd)
	    (const chunk-delete)
	    (const move-back)
	    (const move-fwd)
	    (const e-tilde)
	    (const e-slash)
	    (const e-dollar)
	    (const undo))))
  :group 'TinyEf)


(defcustom tinyef-:step-delete-chars "-./@:"
  "*When using step-delete action, kill until these chars. This is charset.
The \"-\" character must be first in the string."
  :type '(string "Charset")
  :group 'TinyEf)

;;; ....................................................... &v-private ...

(defcustom tinyef-:mode-defined-maps	;== if you need to change this; report
  (delq nil                             ;== change to maintainer
	(list
	 'global-map

	 'read-expression-map

	 'minibuffer-local-map
	 'minibuffer-local-must-match-map	;eg C-x C-f uses this
	 'minibuffer-local-completion-map

	  ;;  Only in Emacs
	  ;;  the minibuffer when spaces are not allowed

	 (if (boundp 'minibuffer-local-ns-map)
	     'minibuffer-local-ns-map)))
  "*Keymap list where to install Electric file minor mode hotkey-
See `tinyef-:mode-key'."
  :type  '(symbol :tag "Keymap")
  :group 'TinyEf)

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

(eval-and-compile
(ti::macrof-version-bug-report
 "tinyef.el"
 "tinyef"
 tinyef-:version-id
 "$Id: tinyef.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinyef-:version-id
   tinyef-:load-hook
   tinyef-:mode-hook
   tinyef-mode
   tinyef-:mode-map
   tinyef-:mode-defined-maps
   tinyef-:mode-name
   tinyef-:mode-key-table)))


;;}}}

;;{{{ code: misc, keys, install

;;; ########################################################### &Funcs ###

;;; .......................................................... &v-mode ...

;;;###autoload (autoload 'tinyef-mode		"tinyef" "" t)
;;;###autoload (autoload 'turn-off-tinyef-mode	"tinyef" "" t)
;;;###autoload (autoload 'turn-on-tinyef-mode	"tinyef" "" t)
;;;###autoload (autoload 'tinyef-commentary     "tinyef" "" t)
;;;###autoload (autoload 'tinyef-version        "tinyef" "" t)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinyef-" " Tef" nil "Tef" 'TinyEf "tinyef-:"		;1-6

  "Electric file name mode.
This mode helps you composing filename more easily. Some keys
are \"electric\", meaning that they have two behavior. By default
~/$ are electric. Some other keys have special meaning and you
cannot insert them into buffer. They do move/delete/undo operations.

See variable `tinyef-:mode-key-table' which specifies actions
for each electric character. Consult also `tinyef-:step-delete-chars'.
The default action table is as follows:

    (setq tinyef-:mode-key-table
      '(
	(?\[   . step-delete-back)		;KEY -- action symbol
	(?\]   . step-delete-fwd)
	(?\\   . chunk-delete)
	(?\;   . move-back)
	(?\'   . move-fwd)
	(?\~   . e-tilde)			;electric keys
	(?\/   . e-slash)
	(?\$   . e-dollar)
	(?\=   . undo)))

Here is smple graph to give you an overview of what this mode does.
In these presented cases cursor it at the end of line.
Alternatively, just load this file, press C-x C-f and experiment
with keys `[]\/~'.

o   b>> means what's on the line *before*
o   a>> means what's there *after*
o   `'  means what you just pressed
o   []  means which action the character triggered

    b>> http:/www.site.com/~userFoo/dir1/dir2/dir3/ `/` [e-slash]
    a>> http:/
    The e-slash action wiped out the line, because writing
    two slashes normally indicates, that you want to give
    another path

    b>> ~/dir1/dir2/dir3/			`~' [e-tilde]
    a>> ~
    The action wiped the line away, because it assumed
    you want to give `~userFoo or another `~' relative path

    b>> ~/dir1/dir2/dir3/			`[' [step-delete-back]
    a>> ~/dir1/dir2/
    The action wiped previous directory name or until
    special mark, See code, defaults are  `:/@' (ange-ftp things)

    b>> ~/dir1/dir2/			        `=' [undo]
    a>> ~/dir1/dir2/dir3/
    The action works like normal undo.

    b>> ~/dir1/dir2/				`\' [chunk-delete]
    a>>
    The action deleted whole line. It deletes until special marks
    like `@:'. If repeated, it deletes constantly backward

Defined keys:

\\{tinyef-:mode-map}"

  "Tief"
  nil
  "Electric file mode"
  nil
  nil))


;;; ----------------------------------------------------------------------
;;;
(defmacro tinyef-function-macro (action)
  "Define interactive command ACTION."
  (let* ((sym (intern (format "tinyef-%s" (symbol-name (` (, action)))))))
    (`
     (defun (, sym) ()
       (interactive)
       (tinyef-char nil (quote (, action)))))))

(tinyef-function-macro chunk-delete)
(tinyef-function-macro step-delete-back)

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyef-key-p (map key)
  "Test if function `tinyef-mode' is in MAP with KEY."
  (eq 'tinyef-mode (lookup-key map key)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyef-action (char)
  "Return action for CHAR."
  (cdr-safe (char-assq char tinyef-:mode-key-table)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-install-maps (&optional remove force)
  "Define Electric file mode's hot key. Optionally REMOVE.
The install is done only once, but you can FORCE reinstall.

See `tinyef-:mode-defined-maps'."
  (let* ((key	tinyef-:mode-key)
	 (fun	'tinyef-mode)
	 map)
    (dolist (x tinyef-:mode-defined-maps)
      (setq map (eval x))
      (if remove
	  ;; eval or symbol-value function
	  (if (tinyef-key-p (eval x) key)
	      (define-key (eval x) key nil))
	(unless (get 'tinyef-install-maps 'installed)
	  (if (lookup-key map key)
	      (progn
		;;(message "TinyMy: tinyef-:mode-key already taken in %s"
		;;  (symbol-name x))
		nil)
	    (define-key (eval x) key fun)))))
    ;; Mark as installed
    (put 'tinyef-install-maps 'installed t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-mode-map-define-keys ()
  "Defines `tinyef-:mode-map' keys.
Always clears the keymap first and reinstalls the minor mode."
  (interactive)
  (setq tinyef-:mode-map  (make-sparse-keymap)) ;always refresh

  ;;  Minor modes have copy of the keymap. Get rid of it and
  ;;  replace it with new one.

  (ti::keymap-add-minor-mode	'tinyef-mode nil nil	'remove)

  (dolist (elt tinyef-:mode-key-table)
      (define-key tinyef-:mode-map (char-to-string (car elt)) 'tinyef-char))

  (ti::keymap-add-minor-mode	'tinyef-mode
			'tinyef-:mode-name
			tinyef-:mode-map))

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-install (&optional arg)
  "Install package. With optional ARG, cancel installation."
  (interactive)
  (tinyef-install-mouse arg)
  (cond
   (arg
    (remove-hook	'minibuffer-setup-hook	'tinyef-minibuffer-setup)
    (remove-hook	'minibuffer-exit-hook	'turn-off-tinyef-mode)

    (ti::keymap-add-minor-mode 'tinyef-mode nil nil	'remove)
    (tinyef-install-maps  'remove))
   (t
    (add-hook		'minibuffer-setup-hook	'tinyef-minibuffer-setup 'end)
    (add-hook		'minibuffer-exit-hook	'turn-off-tinyef-mode    'end)
    (tinyef-mode-map-define-keys)		;installs also minor-mode
    (tinyef-install-maps))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-install-mouse  (&optional arg)
  "Install default mouse binding. With ARG, remove."
  (dolist (map (list
		minibuffer-local-map
		minibuffer-local-must-match-map
		minibuffer-local-completion-map))
    (cond
     ((emacs-p)
      ;; Have to bind down event; because MSB occupies it.
      (define-key map [C-down-mouse-1] 'tinyef-step-delete-back)
      (define-key map [C-down-mouse-3] 'undo)
      (define-key map [mouse-3]        'tinyef-chunk-delete))
     (t
      (define-key map [(control button1)] 'tinyef-step-delete-back)
      (define-key map [(control button3)] 'undo)
      (define-key map [(button3)]         'tinyef-chunk-delete)))))

;;}}}
;;{{{ code: minibuffer

;;; ----------------------------------------------------------------------
;;; by Anders Lindgren.
;;;
(defun tinyef-minibuffer-setup ()
  "Turn on function `tinyef-mode' when entering minibuffer."
  (setq
   tinyef-mode
   (if (boundp 'minibuffer-completion-table)
       (eq minibuffer-completion-table 'read-file-name-internal)))
  (if (and (boundp 'tinypair-mode)		;Turn off TinyPair.el
	   (fboundp 'turn-off-tinypair-mode))
      (ti::funcall 'turn-off-tinypair-mode)))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-step (&optional back)
  "Position cursor, optionally BACK."
  (let* ((set    tinyef-:step-delete-chars)
	 (rset   (concat "^" set))	;reverse set
	 (func   (if back 'skip-chars-backward 'skip-chars-forward))
	 (point  (point))
	 limit)

    (if back
	(setq limit (line-beginning-position))
      (setq limit (line-end-position)))

    (funcall func rset limit)		;do the movement

    (when (eq (point) point)		;not moved
      (funcall func set  limit)
      (funcall func rset limit))	;try again

    (when (not (eq (point) point))	;moved ok
      (when (and (null back) (not (eolp)))
	;; fix position a little
	(forward-char 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyef-char (&optional character action)
  "Handle Electric file mode's commands.
If there is no action for character insert it as is.
If this command is called interactively outside of minibuffer,
turn off function `tinyef-mode' and insert character as is.

Input:

  CHARACTER  The character is read from input argument or it it is nil, then
	     `last-command-char' is used.
  ACTION     If nil `tinyef-:mode-key-table' is consulted for character.
	     If non-nil, then should ve valid action symbol.

Current keymap:

\\{tinyef-:mode-map}"
  (interactive)
  (let* ((char		(or character last-command-char)) ;char pressed
	 (act           (or action (tinyef-action char)))
	 (re		'(".*@"  ".*:"))
	 (e-list	'(?/  ?@ ?\" ?\'))
	 (pnow          (point))
	 (point		(point))
	 str
	 eolp
	 bolp
	 hits)
    (if (or (null act)			;no action recognized
	    (and (interactive-p)
		 (not (eq (selected-window) (minibuffer-window)))
		 (prog1 t
		   (setq tinyef-mode nil))))
	(insert char)

      (setq bolp (line-beginning-position)  eolp (line-end-position))
;;;      (ti::d! char (char-to-string char) act)

      ;; ... ... ... ... ... ... ... ... ... ... ... ...  e-kill-point . .
      ;; find suitable kill point

      (save-excursion
	(beginning-of-line)

	(dolist (regexp re)
	  (if (and (looking-at regexp)
		   (not (eq eolp (match-end 0))))
	      (push (match-end 0)  hits)))

	(if hits  (setq point (apply 'max hits))) ;;find longest position
	(if (eq point eolp)		          ;;end of line ?
	    (setq point (point)))

	(cond
	 ((eq pnow point)		;no different than current point?
	  (setq str (buffer-substring bolp pnow))

	  ;;  make the end position not to go past string delimiter "

	  (if (not (string-match ".*\"" str))
	      (setq point bolp)
	    (setq point (+ bolp (match-end 0))))))) ;; cond-save-excursion

      (cond

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... undo ..

       ((eq act 'undo)
	(undo))

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. chunk ..

       ((eq act 'chunk-delete)
	(delete-region point (point))) 	;; The kill point is already set

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... step ..

       ((memq act '(step-delete-fwd move-fwd))
	(setq point (point))
	(tinyef-step)
	(if (eq act 'step-delete-fwd)
	    (delete-region point (point))))

       ((memq act '(step-delete-back move-back))
	(setq point (point))
	(tinyef-step 'back)
	(if (eq act 'step-delete-back)
	    (delete-region point (point))))

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... electric ..

       ((and (memq act (list 'e-slash))
	     (char-in-list-case (preceding-char) e-list)
	     ;; permit `//hostname/path/to/file'
	     (not (eq (point) (1+ (point-min))))
	     ;; permit `http://url/goes/here'
	     (not (char= ?: (char-after (- (point) 2)))))
	(delete-region point (point))
	(insert char))

       ((memq act '(e-tilde))
	(cond
	 ((char= (preceding-char) ?~)
	  ;;  /ftp@some:~  pressing "~" now deletes full line
	  (delete-region bolp (point)))
	 ((and (not (win32-p)) (char= (preceding-char) ?:))
	  ;;  In NT, it's best to delete immediately, because you have
	  ;;  those MS-DOS filename C:/ ...
	  ;;
	  ;;  In Unix:
	  ;;  /ftp@some:   allow adding "~"
	  nil)
	 ((let ((filename (buffer-substring bolp (point))))
	    (if (not (string= (file-name-nondirectory filename) ""))
		;;  find file which would have tilde in the name.
		(file-name-completion (file-name-nondirectory filename)
				      (file-name-directory filename))))
	  ;; skip electric: tilde is part of an existing filename
	  nil)
	 (t
	  (delete-region point (point))
	  (if (save-excursion (beginning-of-line) (looking-at "[a-z]:[/\\]?"))
	      ;;  Kill MS-DOS fabsolute path c:/this/dir
	      (delete-region (line-beginning-position) (point))
	    (delete-region point (point)))))
	(insert char))

       ((memq act '(e-dollar))
	(delete-region bolp (point))
	(insert char))

       (t
	(insert char))))))

;;}}}

(tinyef-install)
(provide 'tinyef)

(run-hooks 'tinyef-:load-hook)

;;; tinyef.el ends here
