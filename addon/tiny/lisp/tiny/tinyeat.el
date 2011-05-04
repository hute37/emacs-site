;;; @(#) tinyeat.el --- Eat blocks of text at point, forward and backward
;;; @(#) $Id: tinyeat.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

; This file is not part of Emacs

;;{{{ Documentation

;; Copyright (C)    1995-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1995-03
;; Keywords:	    extensions
;;
;; To get information on this program use ident(1) or C-u M-x tinyeat-version
;; Look at the code with folding.el, tinybm.el


;; LCD Archive Entry:
;; tinyeat|Jari Aalto|jari.aalto@poboxes.com|
;; Eating blocks of text forward, backward. No more ESC-d or or DEL/BACKSPACE|
;; 2002-01-23|$Revision: 1.1 $|~/misc/tinyeat.el.Z|

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
;; Put this file on your Emacs-Lisp load path, add following into your
;; emacs startup file. Rip code with tinylib.el / ti::package-rip-magic
;;
;;* _
;;*  	(require 'tinyeat)
;;
;; Or use autoload, your emacs starts up a bit faster
;;
;;*     (global-set-key [(control backspace)]	'tinyeat-forward-preserve)
;;*     (global-set-key [(control delete)]	'tinyeat-forward-preserve)
;;_
;;*     (autoload 'tinyeat-forward-preserve         "tinyeat" "" t)
;;*     (autoload 'tinyeat-backward-preserve        "tinyeat" "" t)
;;*     (autoload 'tinyeat-delete-paragraph         "tinyeat" "" t)
;;*     (autoload 'tinyeat-kill-line                "tinyeat" "" t)
;;*     (autoload 'tinyeat-zap-line                 "tinyeat" "" t)
;;*     (autoload 'tinyeat-kill-line-back           "tinyeat" "" t)
;;*     (autoload 'tinyeat-kill-buffer-lines        "tinyeat" "" t)
;;*     (autoload 'tinyeat-kill-buffer-lines-min    "tinyeat" "" t)
;;* _
;;
;; Investigating problems
;;
;;	M-x tinyeat-debug-toggle
;;	M-x tinyeat-debug-show
;;
;; If you have any questions, use this function to contact maintainer
;;
;;      M-x tinyeat-submit-bug-report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, overview of features
;;
;;	o   Determine how much text should be eaten around current cursor
;;	    position. Eat extra spaces, extra newlines, next word
;;	    next statement, next comment ... whatever is appropriate
;;	o   When you grow accustomed to this, it probably replace your
;;	    old deleting habbits.
;;	o   Can also eat inside mixed case word: WordsThatAreLikeThis
;;	o   Yank and "overwrite" text under cursor with Meta mouse-2 or
;;	    `Meta' `C-y'. (Std Emacs in `overwrite-mode' doesn't allow you to
;;	    yank and overwrite at the same time.)
;;
;;  Non-windowed and Windowed Emacs
;;
;;	This package works _best_ in windowed Emacs, because in windowed
;;	environment you can use the modifiers *Control*, *Alt* and *Meta*
;;	freely with other keys. The idea of this package is to overload
;;	your single key, `backspace', as much as possible with various
;;	delete functionalities.
;;
;;	In non-windowed Emacs there is no key named `backspace', so
;;	standard Emacs bindings are bound instead. Many of this package's
;;	features are also unused because there are no suitable keys to bind
;;	the commands to. In non-windowed Emacs the extra bindings have been
;;	marked with (*):
;;
;;			    was			now
;;          -------------------------------------------------------------
;;	    Meta s	    <none>		tinyeat-backward-preserve (*)
;;	    Meta d	    kill-word		tinyeat-forward-preserve  (*)
;;	    Meta SPC	    just-one-space	tinyeat-delete-whole-word (*)
;;	    Meta k	    kill-sentence	tinyeat-delete-paragraph  (*)
;;	    Meta C-d	    down-list		tinyeat-kill-line-back    (*)
;;	    Meta ESC	    mark-defun		tinyeat-erase-buffer
;;	    Meta C-y	    <none>		tinyeat-yank-overwrite
;;
;;  Story behind this package
;;
;;	One day the developer got frustrated of moving cursor around the
;;	point and using keys del or backspace to write C++ and LISP
;;	symbols. The start situation was:
;;
;;	    (defun lisp-symbol-name-myname          ()
;;                                  *
;;
;;	He decided to change 'myname' to something else. Normally he
;;	would reach out for ESC-d for `kill-word' to delete `myname' and
;;	type the new name:
;;
;;	    (defun lisp-symbol-name-mynew           ()
;;		                         *
;;
;;	Next, he noticed that there were extra spaces involved.
;;	A call to `fixup-whitespace' would make it go away ... Hmm that was
;;      not bound to any key by default (in this particular Emacs he was
;;	using at the time), so he had to type it the long way round: `M-x'
;;	`fixup-whitespace'. His thoughts were: "Oh, why I hadn't I bound it
;;	to some easily reacheable key". The story continues.
;;      He looked at the function once more and decided that name
;;	`symbol-name-mynew' wasn't a good one after all. He decided to
;;	delete 3 words backward. Now, how do you do that?
;;
;;	    (defun lisp-symbol-name-mynew ()
;;		       		         *
;;
;;	He murmurs, "where is the command to delete backward ...". After
;;	spending valuable minutes to find the `delete-backward-word'
;;      command with the emacs `M-x' `apropos' and hitting the page up and down
;;      keys to find anything that would look like what he wanted, he sits
;;      back with despair, "Rats again, there is no such command". Silently
;;      he ends up tapping the backspace until he reaches correct point:
;;
;;	    (defun lisp- ()
;;	                *
;;
;;	and starts typing a new name...
;;
;;	    (defun lisp-my-func ()
;;
;;	All is perfect for a moment. Then, he notices that there are too
;;	many newlines above the newly created function and says to himself:
;;	"I really should delete those 5 extra empty lines above the
;;	function. Now, how do I kill backward 5 empty lines backward? The
;;	`kill-line' in C-k kills only forward" ...". The story teller
;;      rests here to leave your imagination to continue.
;;
;;  Lesson learned
;;
;;	As you can notice, people often spend most of the time to position the
;;	cursor to the right spot and deleting text over there.. over here
;;	..  typing more .. changing our mind ... and so on.
;;
;;	It was time to do something creative, so that user wouldn't have to
;;	worry about the deletion of text so much. This package provides
;;	atempts to provide _smart_ deleting capabilities: whether you want
;;	to delete forward of backward. Naturally it isn't capable of
;;	miracles, it just does few guesses, and a guess may be wrong. If it
;;	so happens that a lot of text have suddenly retired (vanished,
;;	vaporized) from you buffer, remember, there is no need to panic.
;;	Just send a bug report to that poor maintainer, and hit `undo'.
;;
;;	The maintainer would be surprised if you ever wanted to discard
;;	this package after you have tried it. Can he expect a happy smile
;;	at this point? Now, load this and be happy, spread the word and
;;      help others to get "the job done", whatever you were doing.
;;
;;  Default keybindings
;;
;;	chunk delete: words, spaces, symbols ...
;;
;;	    <<		    >>			<<>> [Delete whole word]
;;	    Alt-Backspace   Control-backspace	Shift-Backspace
;;
;;	Line delete
;;
;;	    <<		    >>			<<>> [zap whole line]
;;	    Alt-Backspace   Control-k (Alt-k) 	Control-k
;;	    + Shift				+ Alt
;;
;;	Buffer delete
;;
;;	    \/		    /\			\//\		 ZAP
;;	    untill pmax	    untill pmin		Paragraph delete Whole buffer
;;	    C-A-backspace   C-A-Backspace 	C-S-backspace	 Esc-Backspace
;;			    + Shift
;;
;;
;;	Joining next line to the end of current line: Esc Control-backspace
;;
;;	[Some minibuffer hotkeys]
;;
;;	    f1	= Kill whole line.
;;	    f2	= Delete line backward (to the left)
;;
;;	[Mouse binding]
;;
;;	    (Alt|meta)-Mouse-2 overwries text when pasting.
;;
;;
;;  Known Bugs
;;
;;	This package heavily relies on various modifiers that can be attached
;;	to the *BACKSPACE* key and that is a difficult subject in Unix.
;;	For example the *Alt* keys usually does not exist and to make
;;	it work, yu have to introduce yourself to `xmodmap(1)' or
;;	`keycaps(1)' and possibly `xev(1)' in order to find the key symbols
;;	correctly.
;;
;;	Worse, In the same Unix the Emacs and XEmacs diagree what a simple key
;;	sequence
;;
;;	    BACKSPACE
;;
;;	Means. Or with various modifiers. To get some taste, here is what
;;	XEmacs 20.4 and Emacs 20.3 in Redhat Linux 6.2 return:
;;
;;				XEmacs		Emacs
;;
;;	    <esc backspace>	M-backspace	ESC DEL
;;	    <shift backspace>	delete		S-delete
;;	    <alt backspace>	<nothing>	<nothing>
;;
;;	There is nothing this package can do to cope with these changes in
;;	key symbols or the environemtn you use. If you can, try to get the
;;	ALT key working and shift-modifier for backspace and everything
;;	is well. If that is not possible, the power of the predefined
;;	keybindings are mostly left unused and you have to look at the
;;	install function and determine how woulf you use your keyboard best
;;	with these functions.



;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: variables

;;; ......................................................... &require ...

(require 'tinylibm)
(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyEat tinyeat-: extension
    "Eating blocks of text forward, backward.
Overview of features

	o   Determine how much text should be eaten around current cursor
	    position. Eat extra spaces, extra newlines, next word
	    next statement , next comment ... whatever is appropriate
	o   Can also eat only 'inside' words: WordsThatAreLikeThis")

;;; ......................................................... &v-hooks ...

(defcustom tinyeat-:load-hook '(tinyeat-install-default-bindings)
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyEat)

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyeat-:verbose-flag t
  "*Non-nil means allow informational messages to be displayed."
  :type  'boolean
  :group 'TinyEat)

(defcustom tinyeat-:bind-install-type '(alt-keys meta-keys)
  "*How to bind the keys: with Alt or Meta.
See source code of function `tinyeat-install-default-bindings.'"
  :type  '(choice
	   (const alt-keys)
	   (const meta-keys))
  :group 'TinyEat)


(defcustom tinyeat-:non-word-chars
  "][=_~+!@#$%&*:;'\"`,.<>(){}$<>?/|\\\\\n \t-"
  "*Characters that _stop_ eating word.
Character ][ be in this order and in the beginning of variable,
because this string is converted into regexp later."
  :type  '(string :tag "Charset")
  :group 'TinyEat)

(defcustom tinyeat-:eat-full-word-charset  "^][ \t\n(){};'\","
  "*Character set to use when determining word boundary.
Normally word is terminated by whitespace or newlines."
  :type  '(string :tag "Charset")
  :group 'TinyEat)

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinyeat-version "tinyeat" "Display commentary." t)
(eval-and-compile
(ti::macrof-version-bug-report
 "tinyeat.el"
 "tinyeat"
 tinyeat-:version-id
 "$Id: tinyeat.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinyeat-:version-id
   tinyeat-:debug
   tinyeat-:load-hook
   tinyeat-:verbose-flag
   tinyeat-:bind-install-type
   tinyeat-:non-word-chars
   tinyeat-:eat-full-word-charset)
 '(tinyeat-:debug-buffer)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ install


;;;###autoload (autoload 'tinyeat-debug-toggle "tinyeat" "" t)
;;;###autoload (autoload 'tinyeat-debug-show   "tinyeat" "" t)

(ti::macrof-debug-standard "tinyeat" "-:")

;;; ......................................................... &install ...


(defun tinyeat-install-default-bindings ()
  "Add default bindings to the backspace key with [control ald meta] modifiers."
  (interactive)
  (let* ((type tinyeat-:bind-install-type))

    (global-set-key "\ek"	  'tinyeat-kill-line-back)

    (unless (ti::xe-window-system)

      ;;  was kill-word
      (global-set-key [(meta d)]	'tinyeat-forward-preserve)

      ;;  Was down-list
      (global-set-key [(meta ?\C-d)]	'tinyeat-kill-line-back)

      ;; has no binding
      (global-set-key [(meta s)]	'tinyeat-backward-preserve)

      ;;  was just-one-space
      (global-set-key [(meta ?\ )]	'tinyeat-delete-whole-word)

      ;; was kill-sentence
      (global-set-key [(meta k)]	'tinyeat-delete-paragraph)


      (message "\
TinyEat: ** Note, in non-windowed Emacs keys M-d M-d M-SPC M-k have
been bound to TinyEat delete functions."))

    ;;  Hmm, howabout these...

    (global-set-key [(meta backspace)]	'tinyeat-erase-buffer)
    (global-set-key [(meta delete)]	'tinyeat-erase-buffer)
    (global-set-key "\e\C-?"		'tinyeat-erase-buffer)
    (global-set-key [(meta backspace)]	'tinyeat-erase-buffer)

    ;; XEmacs says that ESC + BACKSPACE is C-M-h in non-window

    (unless (ti::xe-window-system)
      (global-set-key [(control meta ?h)] 'tinyeat-erase-buffer))

    (global-set-key [(control meta backspace)] 'tinyeat-join-lines)

    ;; Control or Alt backspace: the "delete" is usually recognized
    ;; in PC as backspace.
    ;;
    ;; Usually M- is same as ALT key in HP-UX.

    (global-set-key [(control backspace)]	'tinyeat-forward-preserve)
    (global-set-key [(control delete)]		'tinyeat-forward-preserve)

    (global-set-key [(meta delete)]		'tinyeat-backward-preserve)
    (global-set-key [(meta backspace)]		'tinyeat-backward-preserve)
    (global-set-key [(alt backspace)]		'tinyeat-backward-preserve)

    (global-set-key [(shift backspace)]		'tinyeat-delete-whole-word)
    (global-set-key [(shift delete)]		'tinyeat-delete-whole-word)
    (global-set-key [(control shift backspace)]	'tinyeat-delete-paragraph)

    (global-set-key [(control shift backspace)]	'tinyeat-delete-paragraph)

    (global-set-key [(control meta y)]		'tinyeat-yank-overwrite)

    ;;  Portable way to define this key right

    (when (fboundp 'read-kbd-macro)
      (global-set-key (read-kbd-macro "ESC DEL")
		      'tinyeat-erase-buffer))

    ;; Clearing the input in minibuffer prompt

    (define-key minibuffer-local-completion-map	[(f1)] 'tinyeat-zap-line)
    (define-key minibuffer-local-map		[(f1)] 'tinyeat-zap-line)
    (define-key minibuffer-local-completion-map	[(f2)] 'tinyeat-kill-line-back)
    (define-key minibuffer-local-map		[(f2)] 'tinyeat-kill-line-back)


    (when (memq 'meta-keys type)
      ;;  When the M- is same as ALT key in HP-UX.

      ;; Alt + Control + [Shift]

      (global-set-key [(control meta backspace)]   'tinyeat-kill-buffer-lines)
      (global-set-key [(control meta shift backspace)]
		      'tinyeat-kill-buffer-lines-min)

      ;; Alt + Shift, the opposite of this is plain C-k :=)

      (global-set-key [(meta shift backspace)]  'tinyeat-kill-line-back)
      (global-set-key [(meta control k)]	'tinyeat-zap-line)

      (if (emacs-p)
	  (global-set-key [(meta mouse-2)] 'tinyeat-yank-overwrite)
	(global-set-key [(meta button2)] 'tinyeat-yank-overwrite)))

    (when (memq 'alt-keys type)

      ;;  Using this Alt-k, instead of C-k is sometimes better, because
      ;;  it doesn't wipe out the cut buffer.

      (global-set-key [(alt k)]		'tinyeat-kill-line)
      (global-set-key [(alt control k)]	'tinyeat-zap-line)

      (global-set-key [(alt control backspace)]	'tinyeat-kill-buffer-lines)
      (global-set-key [(alt control shift backspace)]
		      'tinyeat-kill-buffer-lines-min)

      ;; Alt + Shift, the opposite of this is plain C-k

      (global-set-key [(alt shift backspace)]   'tinyeat-kill-line-back)

      (if (emacs-p)
	  (global-set-key [(alt mouse-2)] 'tinyeat-yank-overwrite)
	(global-set-key [(alt button2)] 'tinyeat-yank-overwrite)))))

;;}}}
;;{{{ misc

;;; ------------------------------------------------------------ &misc ---
;;;
;;;###autoload
(defun tinyeat-erase-buffer  ()
  "Erase buffer."
  (interactive)
  ;;  - This stupid; but when I bound erase-buffer directly to a key;
  ;;    it didn't work. I propably have some mysterious setting in my
  ;;    emacs startup file; that causes the behavior..
  (erase-buffer))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-zap-line ()
  "Kill whole line, including the final newline."
  (interactive)
  (beginning-of-line)
  (if (looking-at "\n")
      (kill-line)
    (kill-line 1)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-backward ()
  "Eat backward. See `tinyeat-eat'."
  (interactive)
  (tinyeat-eat t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-backward-preserve ()
  "Eat forward, but handle spaces differently. See `tinyeat-eat'."
  (interactive)
  (tinyeat-eat t t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-forward ()
  "Eat forward. See `tinyeat-eat' function."
  (interactive)
  (tinyeat-eat))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-forward-preserve ()
  "Eat forward, but handle spaces differently. See `tinyeat-eat'."
  (interactive)
  (tinyeat-eat nil t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-join-lines()
  "Join this and next line with one space, and go to the joint."
  (interactive)
  (end-of-line)
  (unless (eobp)
    (kill-line)
    (fixup-whitespace)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-delete-whole-word  ()
  "Delete word at point. Cursor at whitespace, calls `fixup-whitespace'.

References:

  `tinyeat-:eat-full-word-charset'"
  (interactive)
  (let* ((set  tinyeat-:eat-full-word-charset)
	 beg
	 end)
    (if (looking-at "[ \t\n]")
	(fixup-whitespace)
      (skip-chars-backward set) (setq beg (point))
      (skip-chars-forward  set) (setq end (point))
      (delete-region beg end)
      (setq beg (point))
;;      (unless (zerop (skip-chars-forward " \t"))   ; delete white space
;;	(delete-region beg (point)))
      nil)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-line ()
  "Same as `kill-line', except the killed text isn't put into cut buffer.
This way you can retain mouse selection in cut buffer.
This only interests people who can use mouse."
  (interactive)
  (cond
   ((eobp))				;Do nothing
   ((eolp)
    (delete-char 1))
   (t
    (delete-region (point) (line-end-position)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-line-back ()
  "Like `kill-line' but backward."
  (interactive)
  (when (not (bobp))
    (if (bolp)				;Kill previous newline (shift line up)
	(backward-delete-char 1)
      (delete-region (point) (line-beginning-position)))))

;;; ----------------------------------------------------------------------
;;; Just to ease calling it from key
;;;
;;;###autoload
(defun tinyeat-kill-buffer-lines-min (&optional back)
  "Kill until `point-min'. Optionally BACK."
  (interactive)
  (tinyeat-kill-buffer-lines 'back))

;;; ----------------------------------------------------------------------
;;; very trivial, but I need this all the time
;;;
;;;###autoload
(defun tinyeat-kill-buffer-lines (&optional back)
  "Kill to the `point-max' or BACK to the `point-min' with ARG."
  (interactive)
  (cond
   (back
    (delete-region (point) (point-min)))
   (t
    (delete-region (point) (point-max)))))

;;}}}
;;{{{ misc2

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun  tinyeat-delete-paragraph ()
  "Delete current paragraph, separated by empty lines."
  (interactive "*")
  (let* ((re "^[ \t]*$")
	 beg
	 end)
    (cond
     ((save-excursion			;sitting on empty line
	(beginning-of-line)		;kill empty lines around the point
	(looking-at "^[ \t]*$"))
      (skip-chars-backward " \t\n")
      (forward-line 1)
      (setq beg (point))
      (skip-chars-forward " \t\n")
      (forward-line -1)
      (setq end (point)))
     ((save-excursion
	;;  Kill paragraph.
	(if (not (re-search-backward re nil t))
	    (setq beg (point-min))
	  (beginning-of-line)
	  (forward-line 1)		;exlude space
	  (setq beg (point))))
      (save-excursion
	(if (not (re-search-forward re nil t))
	    (setq end (point-min))
	  (beginning-of-line)
	  (setq end (point))))))
    (if (not (and beg end))
	(message "TinyEat: Can't find paragraph bounds (empty line) .")
      (unless (eq beg end)
	(kill-region beg end)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-space-delete-at-point (&optional back preserve)
  "Delete whitespace at point. Optionally BACK.
If optional PRESERVE is given, then deletes towards the BACK only.
if BACK is non-nil the deletion is headed backward."
  (interactive)
  (let* (;; character function selection
	 (charf	  (if back 'skip-chars-backward 'skip-chars-forward))
	 (p	  (point))
	 (verb	  tinyeat-:verbose-flag)
	 (ch	  (ti::buffer-read-char back 0)) ;sitting on it if looking fwd
	 (ch-p	  (ti::buffer-read-char back -1))
	 (ch-n	  (ti::buffer-read-char back 1)))
    (cond
     ((and back (ti::space-p (or ch-p ?\ )) (char= ch ?\n))
      (delete-horizontal-space)
      (if (and verb (null back))
	  (message "TinyEat: line cleared"))
      t)
     ((char= ch ?\n)		;no spaces before, do nothing
      nil)
     ((or (and ch ch-n
	       (ti::space-p ch)
	       (ti::space-p ch-n)) ;at least two spaces
	  (and ch ch-p
	       (ti::space-p ch-p)
	       (ti::space-p ch)))
      (if (null preserve)
	  (fixup-whitespace)
	(funcall charf " \t")
	(delete-region p (point)))
      t)
     (t
      (delete-horizontal-space)
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-word-move-point (&optional back)
  "Move to suitable word kill point. Mixed case words are special.
Optionally BACK. See variable `tinyeat-:non-word-chars' how to delimit word parts.

* = cursor position

ThisIsMixedWord --> ThisIsMixedWord
*		        *
THISmixedWord   --> THISmixedWord
*                       *"
  (interactive)
    (let* ((fid         "tinyeat-word-move-point")
	   (charf	(if back 'skip-chars-backward 'skip-chars-forward))
	   (non-word    tinyeat-:non-word-chars)
	   (nonw-re     (concat "[" non-word "]+"))
	   (ch		(ti::buffer-read-char back))
	   p
	   str
	   mb
	   me			;match beg end
	   mixed)

      (tinyeat-debug fid "ENTRY" 'back back
		     'char ch
		     (if ch
			 (char-to-string ch)
		       "no CHARACTER??"))

      ;;    Check if this is special mixedCase before vaporizing word...

      (save-excursion
	(setq p (point))
	(if back
	    (backward-word 1)
	  (forward-word 1))
	(setq str (buffer-substring p (point)))
	(setq mixed (ti::string-match-case "[A-Z][a-z]" str)))

      (cond
       (mixed
	(tinyeat-debug fid "CASE MIXED" 'point (point))

	(if (eq ch (downcase ch))
	    (funcall charf "a-z")
	  (setq p (point))
	  ;;  Skip all big letters
	  (funcall charf "A-Z")
	  ;;  If this was only one letter, continue deleting. Otw stay put.
	  (if (eq 1 (abs (- p (point))))
	      (funcall charf "a-z")))

	;;  The previous statements only moved 2 first statements
	;;	    ThisIsWord	    start,
	;;		     *
	;;	    ThisIsWord	    after,
	;;		   *
	;;	    ThisIsWord	    correction. This is needed
	;;		  *

	(if (and back
		 (not (bobp)))
	    (backward-char 1)))

       (t

	;; if there is non-word we must remove it.
	;; - There is some problems in backward deltion, eg deleting "...."
	;;   backward in text-mode does not delete all dots. Don't
	;;   know why not.

	(cond
	 ((if back			;select FWD of BCK looking
	      (cond
	       ((string-match nonw-re (char-to-string ch))
		(re-search-backward nonw-re nil t)))
	    (looking-at nonw-re))

	  (setq mb (match-beginning 0)
		me (match-end 0))

	  (tinyeat-debug
	   fid "CASE 1" ch 'point (point)
	   'match-begin mb
	   'match-end   me)

;;;	  (setq N nonw-re MB mb ME me) (ti::d! 1)

	  ;;  1. if there is multiple items like "....", delete only
	  ;;     those
	  ;;  2. if there is only one member like ".member", delete
	  ;;     dot and the word that follows it.
	  ;;

	  (if back (setq p mb) (setq p me)) ;selet direction

	  (if (not (eq 1 (abs (- me mb))))
	      (goto-char p)
	    (goto-char p)
	    (funcall charf (concat "^" non-word))))
	 (t

	  (tinyeat-debug "CASE default ")

	  ;;  The skip-chars-forward _requires_ that the "-"
	  ;;  character is the first item. That's why we have
	  ;;  to add extra "-" to the front of string if user
	  ;;  has defined "-" to be word stopper.

	  (if (ti::string-match-case "-" non-word)
	      (setq non-word (concat  "^-" non-word))
	    (setq non-word (concat "^" non-word)))
	  (tinyeat-debug "CASE default " charf non-word)
	  (funcall charf non-word)))))))

;;}}}
;;{{{ Yanking


;;; ----------------------------------------------------------------------
;;; Don't you hate that the overwrite mode doeesn't support this...
;;;
(defun tinyeat-yank-overwrite ()
  "Yank text by overwriting previous content."
  (interactive)
  (let* ((p  (point))			;insertion point
	 len
	 end)
    (with-temp-buffer
     (yank)
     (setq len (1- (point-max))))	;how many chars in there ?
    (cond
     ((= len 0)
      (message "TinyEat: Nothing to yank"))
     (t
      ;;   we must untabify  the line, otw we get unpleasant results
      (untabify p (line-end-position))
      (setq end (+ p len))
      (if (> end (point-max))
	  (setq end (point-max)))
      (delete-region p end)
      (yank)))))

;;}}}

;;{{{ engine

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-eat (&optional back ti::space-preserve verb)
  "Eat *appropriate* text forward, if BACK then backward.

The optional SPACE-PRESERVE changes the space eating (VERB).

A.  when it is NIL and BACK is anything.   * marks the cursor.
         text1 text1        *     text2  text2
    -->  text1 text1 text2  text2                   ;one space left

B.  when it is NON-NIL and BACK nil
         text1 text1        *     text2  text2
    -->  text1 text1        *text2  text2            ;delete right spaces

C.  when it is NON-NIL and BACK t
         text1 text1        *     text2  text2
         text1 text1*     text2  text2               ;delete left spaces


References:

  `tinyeat-:non-word-chars'"
  (interactive)
  (let ((fid        "tinyeat-eat ")
	(p	    (point))
	(syntaxf    (if back 'skip-syntax-backward 'skip-syntax-forward))
	(charf	    (if back 'skip-chars-backward  'skip-chars-forward))

;;;	ch-nn ch-p
	ch
	ch-n)

    (setq verb (or verb tinyeat-:verbose-flag))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..

    (setq ch    (ti::buffer-read-char back 0))       ;; sitting on it if looking fwd

;;;    (setq ch-p  (ti::buffer-read-char back -1))	;; previous

    (setq ch-n (ti::buffer-read-char back 1))	;; next

    (tinyeat-debug
     fid
     "CHARACTER " ch  (char-to-string ch)
     "NEXT CHARACTER" ch-n (char-to-string ch-n))


;;;    (setq ch-nn (ti::buffer-read-char back 2))	;; following next

;;d     (ti::d!
;;d      (char-to-string ch-p )
;;d      (char-to-string ch )
;;d      (char-to-string ch-n )
;;d      (char-to-string ch-nn))


    (cond
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; BEG of buffer or END of buffer

     ((eq nil ch)
      (tinyeat-debug fid "CHARCTER is nil, maybe bop or eob")
      (if verb
	  (message
	   "TinyEat: "
	   (concat
	    (if (bobp)
		"Beginning"
	      "End")
	    " of buffer"))))

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--

     ((ti::space-p ch)			;one whitespace
      (tinyeat-debug fid
		     "SPACE-P choice" 'back back 'preserve ti::space-preserve)
      (tinyeat-space-delete-at-point back ti::space-preserve)
      (if (and verb
	       (null back)
	       (looking-at "$"))		;it handled this
	  (message "TinyEat: line cleared.")))

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - Multiple  newlines, squeeze to one only

     ((and (char= ch ?\n) (and ch-n (char= ch-n ?\n)))

      (funcall charf "\n")
      (if (null back)
	  (backward-char 1)		;do not join, leave 1 EMPTY newline
	(forward-char 1))
      (tinyeat-debug fid "MULTIPLE newlines" 'was-point p 'now-point (point))
      (delete-region p (point)))

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - at the end of line I suppose add previous line to it.

     ((char= ch ?\n)
      (tinyeat-debug fid "NEWLINE" 'back back 'ti::space-preserve ti::space-preserve)
      (unless (tinyeat-space-delete-at-point back ti::space-preserve)
	(if (null back)			;which direction
	    (delete-char 1)
	  (if (not (eq 0 (funcall syntaxf  "_")))	;try to move
	      (delete-region p (point))	;moveti::d!
	    (backward-char 1)
	    (delete-region p (point))))))

     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; WORD handling (blocks)

     (t					;eat next word
      (funcall syntaxf " ")		;ignore spaces
      (tinyeat-debug fid "default - WORD CASE\n"
		     "CHARACTER " (char-to-string ch)
		     "CHARACTER SYNTAX " (char-to-string (char-syntax ch)))

      ;;   - What is next char after whitespace ??
      ;;   - With these following conditionals we set the point
      ;;     to appropriate position and after COND we run the kill command

      (cond
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       ((and (not  (char-in-list-case ch  '(?- ?_ ?:)))
	     (equal ?w (char-syntax ch)))
	(tinyeat-debug fid "-- CASE 1 syntaxes [-_:]")
	(tinyeat-word-move-point back))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..

       ((and (char-in-list-case ch   '(?- ?_ ?:))
	     ch-n
	     (memq (char-syntax ch-n)  '(?w ?\ )))
	(tinyeat-debug fid "-- CASE 2")
	;;  This is really hard to understand... execpt for the author
	;;  1) Is CH variable start, a delimiter ?
	;;  2) AND is the NEXT-CH word or whitespace

	;; (funcall syntaxf  "_w")
	;; (funcall syntaxf  " w")
;;;	(setq CH ch CH-N ch-n)  (ti::d! "A sym")

	(funcall charf "-_:"))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..

       (t
	;; punctuation, comment, the rest ... skip non important stuff
	(tinyeat-debug fid "-- CASE other")
	(funcall charf "^ \t\na-zA-Z0-9")))
      (delete-region p (point))))))

;;}}}

(provide   'tinyeat)
(run-hooks 'tinyeat-:load-hook)

;;; tinyeat.el ends here
