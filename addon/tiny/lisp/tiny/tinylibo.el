;;; @(#) tinylibo.el --- Library for handling (o)verlays
;;; @(#) $Id: tinylibo.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1995-03
;; Keywords:	    extensions
;;
;; To get information on this program use ident(1) or do M-x tinylibo-version
;; Look at the code with folding.el

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

;;; Intallation:

;; ........................................................ &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file. Yes, you require 'm' lib which publishes
;; this modules interface.
;;
;;     (require 'tinylibm)

;;}}}
;;{{{ Documentation


;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface 1995
;;
;;	The functions were developed to ease the usage of highlighting,
;;	which really seemed "inside" stuff when I first wanted to use
;;	colored text in emacs.
;;
;;	o   This is LIBRARY module, it does nothing on its own.
;;	o   Offers functions for 19.xx overlay handling
;;
;;  Code policy
;;
;;	All functions are written by [jaalto], unless otherwise notified.
;;	If I have found interesting function from some other .el I have
;;	written a reference to that .el. If the code is directly taken
;;	'almost as is' all the credit belongs to the Author of the
;;	original code. Cheers for him, to write so recpectable code, that
;;	it can be used right from the box!
;;
;;	If someone contributes good, general purpose function, all the
;;	credit belongs to him automatically. Persons name and date and so
;;	on is included.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: -- require

;;; ......................................................... &require ...

(require 'tinylibm)			;macro package

(ti::package-use-dynamic-compilation)

(eval-and-compile
  (ti::overlay-require-macro
    (message "\n\
tinylibo: ** XEmacs needs overlay.el package; emulation may not work.")

    ;; Idea by Kyle Jones,  kyle@wonderworks.com, in setnu.el
    ;; 1997-02-27 19.15 HAS overlay emulation

    (unless (fboundp 'overlayp)
      (defalias 'overlayp               'extent-live-p))

    (unless (fboundp 'make-overlay)
      (defalias 'make-overlay		'make-extent))

    (unless (fboundp 'delete-overlay)
      (defalias 'delete-overlay	'delete-extent))

    (unless (fboundp 'overlay-get)
      (defalias 'overlay-get		'extent-property))

    (unless (fboundp 'overlay-put)
      (defalias 'overlay-put		'set-extent-property))

    (unless (fboundp 'move-overlay)
      (defalias 'move-overlay		'set-extent-endpoints))

    (unless (fboundp 'overlay-end)
      (defalias 'overlay-end		'extent-end-position))

    (unless (fboundp 'overlay-start)
      (defalias 'overlay-start	'extent-start-position))

    (unless (fboundp 'overlay-buffer)
      (defalias 'overlay-buffer	'extent-start-position))

    (unless (fboundp 'overlay-buffer)
      (defalias 'overlay-buffer	'extent-start-position))

    (unless (fboundp 'next-overlay-change)
      (defalias 'next-overlay-change  'next-extent-change))

    (unless (fboundp 'overlay-properties)
      (defalias 'overlay-properties   'extent-properties))

    (unless (fboundp 'overlay-length)
      (defalias 'overlay-length	'extent-length))

    (unless (fboundp 'overlays-at)
      (defun overlays-at (point)
	"tinylibo.el -- return overlay at POINT."
	(ti::funcall 'extent-list (current-buffer) point)))))

;;}}}
;;{{{ setup: -- vars

;;; ....................................................... &v-version ...

(defconst tinylibo-version
  (substring "$Revision: 1.1 $" 11 15)
  "Latest version number.")

(defconst tinylibo-version-id
  "$Id: tinylibo.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun  tinylibo-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylibo.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun  tinylibo-feedback ()
  "Submit suggestions, error corrections, impressions, anything..."
  (interactive)
  (ti::package-submit-feedback "tinylibo.el"))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ macros

;;; .......................................................... &macros ...

(defsubst ti::overlay-make (level)
  "Make overlay according to match in buffer at LEVEL.
The match is NOT checked. Returns new overlay."
  (make-overlay
   (match-beginning level)
   (match-end level)))


(defsubst ti::overlay-makec (level)
  "Make overlay according to match in buffer at LEVEL.
The match is checked. Returns new overlay or nil."
  (if (match-end level)
      (make-overlay
       (match-beginning level)
       (match-end level))))

;;}}}
;;{{{ funcs


;;; ----------------------------------------------------------------------
;;;
(defun ti::overlay-make-match  (level plist)
  "Make overlay over the matched text portion. The match level is checked.

Input:
  LEVEL	    match level
  PLIST	    property list '(PRO-NAME PROP-VAL)

Return:
  ov	    overlay or nil"
  (let* ((ov   (ti::overlay-makec level))
	 prop
	 propv)
    (when ov
      (while plist
	(setq prop (nth 0 plist)  propv (nth 1 plist))
	(setq plist (cdr (cdr plist)))  ;go 2 fwd
	(overlay-put ov prop propv)))
    ov))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::overlay-buffer-substring (ov &optional no-properties)
  "Read `buffer-substring' underneath overlay OV.

Input:

  OV			overlay, can also be nil.
  NO-PROPERTIES		flag, if non-nil remove all properties

Return:

  string
  nil"
  (when ov
    (if no-properties
	(buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
      (buffer-substring  (overlay-start ov) (overlay-end ov)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::overlay-mouse-on-p (ov)
"Check if overlay OV has `mouse-face' on.
If `mouse-face' contains 'default, it's treated to mean same as nil.

Return:
  nil or property value of `mouse-face'"
  (let* (prop
	 propl)
    (when ov
      (setq propl (overlay-properties ov)
	    prop  (when (memq 'mouse-face propl)
		    (overlay-get ov 'mouse-face)))
      (unless (or (null prop)
		  (eq prop 'default))
	;;  it had some property
	prop))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::overlay-get-mouse ()
  "Check if the point has 'mouse-face overlay.

Return:

  nil          no overlay at the point found
  t            no mouse face
  ov           overlay"
  (let* (ovl				;overlay list
	 ov)
    (when (setq ovl (overlays-at (point)))
      (setq ov (ti::overlay-get-prop ovl (list 'mouse-face)))
      (if (null ov)
	  (setq ov t)))	;no mouse
    ov))

;; ----------------------------------------------------------------------
;; 'prop'   means parameter form
;; - There should only one unique...
;;
(defun ti::overlay-get-prop (ovl prop-list)
  "Read OVL and return first overlay where is property list PROP-LIST.

Input:

  OVL		overlay list
  PROP-LIST	list of properties (PROP PROP ..)"
  (let ((len (length prop-list))
	ov
	ovx
	propl
	i)
    (unless (and ovl  prop-list)
      (error "Invalid parameters"))

    (while (and ovl			;until list end
		(null ov))		;until found
      (setq ovx   (car ovl)
	    propl (overlay-properties ovx)
	    i     0)

      (dolist (elt prop-list)		;check all properties
	(when (memq elt propl)
	  (incf  i)))			;hit counter

      (if (eq i len)
	  (setq ov ovx))	;found all matches
      (setq ovl (cdr ovl)))
    ov))

;; ----------------------------------------------------------------------
;; 'prop-val'   means parameter form
;; - This is more heavier function
;;
(defun ti::overlay-get-prop-val (ovl prop-list)
  "Read OVL and find overlay(s) which contain PROP-LIST '(PROP VAL PROP VAL..)

Input:

  OVL		overlay list
  PROP-LIST	list of properties (PROP VAL PROP VAL ..)"
  (let (len
	ov
	ovx
	ptr
	propl
	prop
	propv)
    (when ovl
      (setq len (length prop-list))

      (if (or (not (and ovl prop-list))
	      (not (= 0 (% len 2))))	;must go paired
	  (error "Invalid parameters" ovl prop-list)

	(setq len (/ (length prop-list) 2))

        ;; ..................................................... check ...

	(while (and (setq ovx (pop ovl)) ;until list end
		    (null ov))		;until found

	  (setq ptr   prop-list
		propl (overlay-properties ovx))

	  (while ptr
	    (setq prop  (car ptr)   ptr (cdr ptr)
		  propv (car ptr)   ptr (cdr ptr))

;;;	  (ti::d!! '!! prop propv
;;;	       'memq (memq prop propl)
;;;	       'get (overlay-get ovx prop) propv propl "\n")

	    (if (and (memq prop propl)
		     (equal (overlay-get ovx prop) propv))
		(push ovx ov)))

	  (setq ovl (cdr ovl)))) ;; while-if

;;;    (ti::d!! "~out" (prin1-to-string ov))
      ov)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::overlay-re-search
  (re level list &optional max back reuse reuse-t no-prop-l)
  "Search for RE at LEVEL by creating overlay and its property LIST.
Assigning LIST (PROP PROP_VAL) to the overlay. The search is repeated
until no more hits or up till MAX point is reached.

Input:

  RE	regexp
  LEVEL	subexpression level in regexp
  LIST	list of (PROP PROP_VAL)
  MAX   if non-nil, searches up till MAX point.
  BACK  search backward
  REUSE (PROP PROP PROP ..) or (PROP_SYM PROP_VAL ..)

	When re match is found it looks overlays underneath the
	point, and the first overlay that satisfies list, will
	be reused, instead of creating new one. Note that _first_
	overlay matched is used, if none is found, new is created

  REUSE-T

	Specifies the list _type_ that was given in REUSE.
	nil = first type , non-nil = second type.

  NO-PROP-L

	Ig given, then possible overlay starting at the same point must
	not have properties PROP-L (PROL VAL PROP VAL ..). If there is
	susch matching overlay, then do not create overlay.

Return:

  nil                          nothing created or used.
  '(used-list created-list)    two lists, list of used and created overlays."
  (let* ((func (if back 're-search-backward 're-search-forward))
	 (max  (if max
		   max			;it's given
		 (if back
		     (point-min)
		   (point-max))))
	 ret-reused
	 ret-created
	 ov
	 ovl
	 prop
	 propv
	 ptr
	 ;; match pointers
	 mb)
    (unless (and list
		 (listp list)
		 (zerop (% (length list ) 2)))
      (error "Parameter LIST invalid" re level list))

    (save-excursion
      (while (funcall func re max t)
	(setq mb  (match-beginning level))

;;;	(ti::d! level (match-string level) mb)

	(when mb			;match on this level found

          ;; ....................................... find or create ov ...

	  (setq ovl (overlays-at mb)) 	;try finding all overlays

	  (cond
	   ((and ovl
		 (ti::overlay-get-prop-val ovl no-prop-l))
	    ;; Do nothing, overlap happened
	    nil)

	   ((or (null reuse)
		(null ovl))
	    (setq ov (ti::overlay-make level))
	    (push ov ret-created ))

	   (t
;;;	    (ti::d! "r" reuse ovl)  (setq OVL ovl RE reuse)
	    (if reuse-t			;what type the list is ?
		(if ovl
		    (setq ov (car-safe (ti::overlay-get-prop-val ovl reuse))))
	      (if ovl
		  (setq ov (ti::overlay-get-prop ovl reuse))))

;;;	    (ti::d! "after" ov)

	    (if ov
		(push ov ret-reused)
	      (setq ov (ti::overlay-make level)) ;none satisfies us
	      (push ov ret-created)))) ;; cond

          ;; .................................... add properties to ov ...
	  ;; Now we should have overlay in a way or other

	  (when ov
	    (setq ptr list)
;;;	  (ti::d! list)
	    (while ptr
	      (setq prop (nth 0 ptr)  propv (nth 1 ptr))
	      (setq ptr (cdr (cdr ptr)))	;go 2 fwd
;;;	    (ti::d! "put" prop propv ov)
	      (overlay-put ov prop propv))))))

    (when (or ret-reused ret-created)
      (list ret-reused ret-created))))

;;; ----------------------------------------------------------------------
;;; - Try following example, cool!
;;;
;;; (setq OV (ti::overlay-make-overlay (point) (point)))
;;; (overlay-put OV 'face 'highlight)
;;; (ti::overlay-re-search-move OV "ti::o")
;;;
;;;
(defun ti::overlay-re-search-move (ov re &optional level back max)
  "Maove OV to Search match RE at LEVEL.
Default level is 0, full match. if BACK is given, search is done
backward. MAX is last position to search.

If overlay OV is currently in some other buffer, it will be transferred
to the current buffer.

Input:

  OV	overlay
  RE	regexp
  LEVEL	subexpression level in regexp
  BACK	flag, is non-nil, go backward
  MAX	max point of search

Return:

  nil	if not moved.
  nbr   overlay end position [matched portion end]"
  (let* ((max	    (or max
			(if back (point-min) (point-max))))
	 (level	    (or level 0)))		;default is full string
    (unless ov
      (error "invalid overlay, nil"))

    (when (and (if back
		   (re-search-backward re max t)
		 (re-search-forward re max t))
	       (match-end level))
      (move-overlay ov
		    (match-beginning level)
		    (match-end level)
		    (current-buffer))
      (match-end level))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun ti::overlay-get-within-area (propl &optional propl-t beg end)
  "Return all overlays which match property list PROPL.
If PROPL is t then returns all overlays. Default is to search from
current point forward.

Input:
  PROPL		property list, see next
  PROPL-T	if nil the propl is of type    (PROP PROP .. )
		if non-nil                     (PROP VAL PROP VAL ..)
  BEG		region beginning
  END		region end"
  (let* ((p   (or beg (point)))
	 (max (or end (point-max)))
	 (all (eq t propl))
	 ovl
	 ovx
	 list)
    (save-excursion
      (goto-char p)
      (while (< (setq p (next-overlay-change p)) max)
	(goto-char p)
	(setq ovl (overlays-at p))
	(when ovl
;;;	  (ti::d! "~go" p max (prin1-to-string ovl))
	  (if all
	      (mapcar
	       (function
		(lambda (x)
		  (if x
		      (push x list))))
	       ovl)
;;;	    (ti::d! "prop type" propl-t  (prin1-to-string propl) )

	    (if propl-t
		(setq ovx (car-safe (ti::overlay-get-prop-val ovl propl)))
	      (setq ovx (ti::overlay-get-prop ovl propl)))

	    (if ovx
		(push ovx list))))))
    list))

;;; ----------------------------------------------------------------------
;;; If you're in trouble, call this function interactively
;;; and it wipes out all overlays.
;;;
(defun ti::overlay-remove-region (&optional beg end propl propl-t)
  "Remove all matched overlays within area.
Default is from point forward. Ignores buffer read-only status.

Input:

  BEG	     region beginning
  END	     region end
  PROPL	     (PROP PROP ..) or
	     (PROP VAL PROP VAL ..)
	     If this value is t, removes all overlays

  PROPL-T   Specifies the list type given. nil = first list type."
  (interactive "r")
  (let* (buffer-read-only
	 (p     (or beg (point)))
	 (max   (or end (point-max)))
	 (propl (if propl propl t))	;set to t is not given
	 (ovl   (ti::overlay-get-within-area propl propl-t p max))
	 ovx)
;;;    (ti::d! "FULL" (length ovl) (prin1-to-string ovl))
    (while ovl
      (setq ovx (car ovl)  ovl (cdr ovl))
;;;      (ti::d! "del" (prin1-to-string ovx))
      (if ovx
	  (delete-overlay ovx)))))



;;}}}

(provide 'tinylibo)

;;; tinylibo.el ends here
