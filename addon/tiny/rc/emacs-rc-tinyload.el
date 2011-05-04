;; @(#) emacs-rc-tinyload.el -- Delayed loading of files
;; @(#)	$Id: emacs-rc-tinyload.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{ Documentation
;;; Documentation:
;;
;;  File id
;;
;;      Copyright (C) 1997-2001 Jari Aalto
;;      Author:       Jari Aalto <jari.aalto@poboxes.com>
;;      Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;;      Created:      1997
;;      Keywords:     tools
;;
;;      This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that.. Just rename this file to whatever is shown
;;      in the first line. You load this file from .emacs in the following
;;      manner
;;
;;  Installation
;;
;;      ** MODIFY FILE BEFORE USING IT, THIS IS my personal SETUP **
;;
;;      Put following entry entry in your .emacs
;;
;;          (require 'emacs-rc-tinyload)
;;
;;      This file may be under some other name in the current directory
;;      where you found it, but do not mind that.. Just rename this file to
;;      whatever is shown in the first line.
;;
;;  Description
;;
;;      This file configures all packages and files that can be loaded
;;	later when Emacs sits idle for tinyload.el. See for full description
;;	of the usage from there.
;;
;;      `~/elisp/config/emacs-rc-xxx.el' all Emacs resource files of various kind.
;;
;;	`ti::xe-window-system' is Emacs independent window system check
;;	function found from tinylib.el
;;

;;}}}

;;; Change Log:
;;; Code:

;;{{{ Lazy loading

(require 'tinyliba)     ;; Library for autoload interfaces

;;; ............................................................ &load ...

(let* ((w  (ti::xe-window-system))
       (x  (eq w 'x))			;x windowed
       (win32 (win32-p))
       )
(defconst tinyload-:load-list
  (delq
   nil
   (list

   ;;  Those with 'noerr flag are not essential packages, I can live
   ;;  without them if they cannot be found.
   ;;

   ;;  X Windowed emacs
   ;;  - Load non-compiled rc file in XEmacs, because the compiled faces
   ;;    are not compatible with XEmacs.

   (list (if (emacs-p)
	     "emacs-rc-font"
	   "emacs-rc-font.el")
	 'emacs-rc-font
	 nil nil nil
	 ;;  The file defines this function thet I want to call when it
	 ;;  has been loaded. It configures faces for this emacs.
	 '(progn
	    (cond
	     ((eq 'win32 (ti::xe-window-system)) (my-face-change 'pc))
	     (t   (my-face-change 'def))
	     )))

   (list "emacs-rc-kbd")

   (list "emacs-rc-set")

   (list "emacs-rc-tiny")
   (list "emacs-rc-19")



   (list "emacs-rc-hook1"	        nil 'noerr)
   (list "emacs-rc-hooks"	        nil 'noerr)

   (list "emacs-rc-time"	        nil  'noerr)

   (list "emacs-rc-dired"	        nil 'noerr)


   ;;  Tips I have collected from usenet

   (list "emacs-rc-tips" nil 'noerr)

   ;;  kills Shell buffer processes when Emacs Exits.

   (when win32 (list "msdos-shell-fix" nil 'noerr))

   (list "tinyef"	nil 'noerr)
   (list "tinytab"	nil 'noerr)
   (list "tinyeat"	nil 'noerr)
   (list "tinylisp"	nil 'noerr)

   ;;	Load my mail interface

   ;; (list "emacs-rc-tm-mime"	nil 'noerr)
   (list "emacs-rc-tinymail"	nil 'noerr)
   (list "tinymail"	        nil 'noerr)

   ;;  Package contain faces: load non-compiled version for XEmacs

   (if (emacs-p)
       (list "emacs-rc-mail" nil 'noerr))

   (list "emacs-rc-bbdb" 'rc-bbdb 'noerr)


  ;;  All X/popup menus are defined here that I use

   (when (and w (emacs-p))
     (list "emacs-rc-x-menu" 'rc-xmenu))



;;;   (list "tinypgp" nil nil nil
;;;	   '(progn (provide 'tinypgp)) 	 ;I dont' want to see startup screen
;;;	   )
;;;

;;;   (list "emacs-rc-tinypgp")	;Second generation Emacs PGP interface

   ;;  Read the tiny-docs.el that comes with the Tiny Tools distribution

   (list "tinymy"	nil)

   (list "tinylibmenu"	nil)
   (list "tinysearch"	nil 'noerr)
   (list "tinydiff"	nil 'noerr)
   (list "tinyreplace"	nil 'noerr)
   (list "tinytf"	nil 'noerr)

   (list "tinyurl"
	 nil 'noerr nil nil
	 '(progn 8turn-on-tinyurl-mode))



   (list "tinycache"	nil 'noerr)
   (list "tinyigrep"	nil 'noerr)
   (list "tinydired"	nil 'noerr)

   (list "tinylibid"	nil 'noerr)
   (list "tinydesk"	nil 'noerr)
   (list "checkdoc"     nil 'noerr)

   (list "iswitchb"     nil 'noerr)

   (if (emacs-p)
       (list "mldrag" nil 'noerr nil
	     '(progn (setq mldrag-load-hook 'mldrag-default-keys))))


   (list "fa-extras" nil 'noerr nil nil '(progn (my-fa-setup)))

   ;;  My personal lisp function library

   (list "funcs.ja" 'funcs nil nil nil)

   (when (emacs-p)
     (list "tinyadvice" nil 'noerr nil
	   '(progn (add-hook 'tiad-compile-internal-hook
			     'my-compile-font-lock))))

   ;;  I debug lisp a lot during Emacs session. Preload edebug so that
   ;;  I can use the C-u ESC C-x function instrumentation call
   ;;
   ;;  For XEmacs it is mandatory to load debug.el before edebug.el

   '("debug.el"       nil noerr)
   '("edebug.el"      nil noerr)

   ;; TinyIgrep.el customisations

   '("emacs-rc-tinyigrep" nil noerr)

   ))))



;;; ----------------------------------------------------------------------
;;;
(defun my-fa-setup ()
  "Filladapt setup."
  (when (boundp 'filladapt-token-table)
    (defvar filladapt-token-table nil)
    (defconst filladapt-mode-line-string " Fa")
    (let* ((tok  "[*]+")
	   (elt (assoc tok filladapt-token-table))
	   )
      ;;  Clear the old definition

      (cond
       ((setq  filladapt-token-table (delq elt filladapt-token-table))
	(setq  filladapt-token-table
	       (cons (cons tok 'citation->) filladapt-token-table))
	))

      (setq tok ">+")

      ;; (setq tok adaptive-fill-regexp)

      (cond
       ((setq elt (assoc tok filladapt-token-table))
	(setq filladapt-token-table (delq elt filladapt-token-table))
	(setq  filladapt-token-table
	       (list (cons adaptive-fill-regexp 'citation->)))
	)))))



;;}}}


;;  start the delayed loading. This setup will also restart if this
;;  file is loaded again.

(unless (featurep 'tinyload)
  (require 'tinyload))

(tinyload-install)

;;  Put this file under these features, depending on my current system

(provide 'emacs-rc-tinyload)

;;; .emacs-rc-tinyload.el ends here
