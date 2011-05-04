;;; @(#) tinycompile.el --- Compile buffer additions. Minor mode.
;;; @(#) $Id: tinycompile.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1996-09
;; Keywords:	    extensions
;;
;; To get information on this program use ident(1) or do M-x tinycompile-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinycompile|Jari Aalto|jari.aalto@poboxes.com|
;; Compile additions: shorten file names, guess compile command for buffer.|
;; 2002-08-01|$Revision: 1.1 $|~/misc/tinycompile.el.Z|

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
;;  ~/.emacs startup file.
;;
;;      (require 'tinycompile)
;;
;;  or use this autoload; your ~/.emacs loads quicker
;;_
;;*      (autoload 'tinycompile-mode            "tinycompile" "" t)
;;*      (autoload 'turn-on-tinycompile-mode    "tinycompile" "" t)
;;*      (add-hook 'compilation-mode-hook 'turn-on-tinycompile-mode 'append)
;;_
;; If you find any incorrect behavior, please immediately
;;
;;      o   Turn on debug with `M-x' `tinycompile-debug-toggle'
;;      o   Repeat the task
;;      o   Send bug report with included debug buffer contents.

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, mar 1997
;;
;;	When I was doing grepping over multiple files with igrep.el the
;;	results that were inserted into buffer were too long: There were
;;	2-6 directory paths which occupied 40 characters and the actual
;;	grep hits were continued with \ character to the right. That was
;;	awfull to read. I couldn't get clear look at the grep results. I
;;	decided that there must be a way to clarify the results somehow, so
;;	I started writing this package.
;;
;;  Overview of features
;;
;;	o   Shortening directory paths
;;	o   Killing non-interesting files from the buffer
;;	o   Hiding commented lines

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: misc

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyCompile tinycompile-: tools
    "Compile buffers additions.
  Overview of features

	o   Shortening directory paths
	o   Killing non-interesting files from the buffer
	o   Hiding commented lines")

;;; .......................................................... &v-menu ...

(defcustom tinycompile-:menu-use-flag t
  "*Non-nil means to use echo-area menu."
  :type  'boolean
  :group 'TinyCompile)

(defvar tinycompile-:menu-main
  (list
   '(format
     "%sTinyCompile: DEL|k)ill files s)horten SPC)hide toggel r)egexp ')parse x)mode off"
     (if current-prefix-arg
	 (format "%s "  (prin1-to-string current-prefix-arg)) "" ))
   '(
     (?\177 . ( (tinycompile-kill-all-file-lines)))
     (?\b   . ( (tinycompile-kill-all-file-lines)))
     (?k    . ( (tinycompile-kill-all-file-lines)))
     (?s    . ( (tinycompile-shorten-lines)))
     (?\    . ( (tinycompile-show-hide-toggle)))
     (?r    . ( (call-interactively 'tinycompile-hide-by-regexp)))
     (?'    . ( (tinycompile-parse-line)))
     (?x    . ( (turn-off-tinycompile-mode)))))
  "*TinyCompile echo menu.")

;;; ............................................................ &mode ...


;;;###autoload (autoload 'tinycompile-version "tinycompile" "Display commentary." t)
(ti::macrof-version-bug-report
 "tinycompile.el"
 "tinycompile"
 tinycompile-:version-id
 "$Id: tinycompile.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinycompile-:version-id
   tinycompile-:debug
   tinycompile-:menu-use-flag
   tinycompile-:menu-main
   tinycompile-:load-hook
   tinycompile-:table-hide)
 '(tinycompile-:debug-buffer))



;;;### (autoload 'tinycompile-debug-toggle "tinycompile" t t)
(ti::macrof-debug-standard "tinycompile" "-:")


;;;###autoload (autoload 'turn-on-tinycompile-mode	"tinycompile" "" t)
;;;###autoload (autoload 'turn-off-tinycompile-mode	"tinycompile" "" t)
;;;###autoload (autoload 'tinycompile-mode		"tinycompile" "" t)
;;;###autoload (autoload 'tinycompile-commentary        "tinycompile" "" t)

(eval-and-compile
(ti::macrof-minor-mode-wizard
 "tinycompile-" " Tco" ":" "Tco" 'TinyCompile "tinycompile-:" 		;1-6

 "Additional commands to Compile buffer. You can kill lines or
shorten the file names and hide comments.

Defined keys:

Prefix key to access the minor mode is defined in `tinycompile-:mode-prefix-key'

\\{tinycompile-:mode-map}
\\{tinycompile-:mode-prefix-map}"

 "TinyCompile"

 (progn
   (if (and tinycompile-mode verb
	    (not (string-match "compil" (symbol-name major-mode))))
       (message "TinyCompile: Are you sure this is compile buffer?")))


 "Compile buffer extras."

 (list
  tinycompile-:mode-easymenu-name
  ["Kill matching file lines at point"  tinycompile-kill-all-file-lines     t]
  ["Shorten directory names"		tinycompile-shorten-lines	    t]
  ["Show or hide comments (toggle)"	tinycompile-show-hide-toggle	    t]
  ["Hide by regexp"			tinycompile-hide-by-regexp	    t]
  ["Jump to file on line"		tinycompile-parse-line		    t]
  "----"
  ["Keyboard menu"			tinycompile-menu-main               t]
  ["Package version"			tinycompile-version                 t]
  ["Package commentary"			tinycompile-commentary		    t]
  ["Mode help"				tinycompile-mode-help		    t]
  ["Mode off"				tinycompile-mode		    t])

 (progn

   (if (xemacs-p)
       (define-key root-map [(button2)] 'tinycompile-parse-line)
     (define-key root-map [mouse-2]	 'tinycompile-parse-line))

    (cond
     (tinycompile-:menu-use-flag
      ;;  Using menu to remeber commands is easier if you don't use
      ;;  menu bar at all.
      (define-key root-map p 'tinycompile-menu-main))
     (t
      (define-key map  "\177"	'tinycompile-kill-all-file-lines)
      (define-key map  "k"	'tinycompile-kill-all-file-lines)

      (define-key map  "s"	'tinycompile-shorten-lines)
      (define-key map  " "	'tinycompile-show-hide-toggle)
      (define-key map  "r"	'tinycompile-hide-by-regexp)

      (define-key map  "`"	'tinycompile-parse-line)
      (define-key map  "x"	'turn-off-tinycompile-mode)

      (define-key map  "?"	'tinycompile-mode-help)
      (define-key map  "Hm"	'tinycompile-mode-help)
      (define-key map  "Hc"	'tinycompile-commentary)
      (define-key map  "Hv"	'tinycompile-version)

      (define-key map  "\C-v"	'turn-off-tinycompile-mode)

      ;;  Overwrite compilation-minor-mode definitions

      (define-key root-map "\C-m" 'tinycompile-parse-line))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-menu-main (&optional arg)
  "Show echo area menu and pass ARG to `ti::menu-menu'."
  (interactive "P")
  (ti::menu-menu 'tinycompile-:menu-main arg))

;;; ......................................................... &v-hooks ...

(defcustom tinycompile-:load-hook nil
  "*Hook that is run when package is loaded."
  :type 'hook
  :group 'TinyCompile)

;;}}}
;;{{{ setup: public

;;; ........................................................ &v-public ...
;;; User configurable


(defcustom tinycompile-:table-hide
  '(("^.*\\.el:"			;lisp
     "^.*:[ \t]*[;\"'].*")

    ("^.*\\.\\([cC][cC]?\\|[hH][hH]?\\):" ;C/C++
     ":[ \t]*/[/*].*"))
  "*List of FILENAME and HIDE regexps.
If filename in the beginning of line matches elt1 then
show/hide all lines matching elt2.

Format:
 '((FILENAME-REGEXP HIDE-REGEXP)
   (F H)
   ...)"
  :type '(repeat
	  (string :tag "File Regexp")
	  (string :tag "Hide Regexp"))
  :group 'TinyCompile)

;;}}}


;;{{{ code: macros

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycompile-get-files  (&optional max-point)
  "Return all filenames in compile buffer, optionally until MAX-POINT."
  (beginning-of-line)
  (tinycompile-get-error-lines max-point 'car))

;;}}}
;;{{{ code: support functions

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-shorten-lines ()
  "Shorten the filenames in compile buffer.

Line format must be
  FILE:LINE: results"
  (interactive)
  (let* (dir
	 cd
	 path
	 prev
	 file)
    (buffer-enable-undo)
    (save-excursion
      (ti::pmin)
      (setq cd (ti::buffer-match "^cd +\\(.*\\)" 1))

      (while (re-search-forward "^\\([/.][^:]+\\):" nil t)
	(setq path (match-string 1))

	;; ./pie-mail/hypb.el --> {cd}/pie-mail/hypb.el

	(if (char= (aref path 0) ?.)
	    (setq path (concat cd (substring path 2))))

	(when path
	  (setq file (file-name-nondirectory path))
	  (setq path (file-name-directory path))
	  (ti::replace-match 1 file)
	  (when
	      (or (null prev)
		  (null dir)
		  (string= dir prev))
	    (setq dir path))
	   (unless
	       (string= dir prev)
	     (setq prev dir   dir path)
	     (beginning-of-line)
	     (insert "\ncd " dir "\n\n")))
	(end-of-line)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-parse-line  (&optional verb)
  "Find directory from the previous 'cd' command. VERB.
Look current line first and if it has no directory part,
search backward.

If function is called interactively or VERB is set, the found file
is loaded to emacs and cursor put to line. This works like
`compile-goto-error'.

Note:

  If TinyUrl package is present and current point holds TinyUrl overlay,
  then it is called to handle the line.

Line format must be:

  FILE:LINE: results
  FILE LINE results

  Or the format can be following, where tokens can span multiple lines

  line LINE, file LINE results

Return:

  (file . line)		information
  nil			not valid line"
  (interactive)
  (let* ((fid  "tinycompile-parse-line:")
	 (path "")
;;	 (drive  "\\([a-zA-Z]:\\)?")
	 (cd-re1 ".*cd +\\(.*\\)")
	 (cd-re2 "^cd +\\(.*\\)")

	 line
	 file
	 ret
	 func
	 tmp buffer win
	 tinyurl)
    (ti::verb)

    ;;    If TinyUrl is present, try it to resolve the line.
    ;;    If it marks anything, raise flag `tinyurl'

    (when (and verb
	       (ti::overlay-supported-p)
	       (featurep 'tinyurl))
      (cond
       ((null (symbol-value 'tinyurl-mode)) ;Mode not on
	(setq tinyurl (tinyurl-mark-line)))
       ((tinyurl-overlay-get)		;line already marked
	(setq tinyurl  t)))
      (tinycompile-debug fid 'TinyUrl tinyurl (ti::read-current-line)))

    ;; ..................................................... Tinyurl ...

    (cond
     (tinyurl				;Let this handle url first
      (tinyurl-dispatcher "\C-m" 'key)
      nil)

     ;; ................................................ grep-format ...

     ((save-excursion
	(beginning-of-line)
	(or
	 ;; file:nbr:
	 (looking-at
	  (concat "^[ \t]*\\([^:\n]+\\):\\([0-9]+\\):"))
	 ;; Win32  d:/home/path/file.txt
	 (looking-at
	  (concat "^[ \t]*\\([a-zA-Z]:[^:\n]+\\):\\([0-9]+\\):"))
	 ;; file nbr
	 (looking-at "^[ \t]*\\([^ \t\n:]+\\)[ \t]+\\([0-9]+\\)[ \t:]+")
	 (looking-at (concat ".*line[ \t,\n]+\\([0-9]+\\)[ \t,\n]+"
			     "file[ \t,\n]+\\([^ \t\n:)]+\\)"))))

      (setq file (ti::remove-properties (match-string 1))
	    line (ti::remove-properties (match-string 2)))

      (if (string-match "^[0-9]+$" line)
	  (setq line (string-to-int line))
	(setq tmp  file
	      file line			;Swap order
	      line (string-to-int tmp)))


      (tinycompile-debug fid 'cond1 'file file 'line line)

      ;; ..................................................... Paths ...

      (cond				;Unix, Dos paths
       ((save-excursion
	  (and (null (string-match (concat "^/\\|^[a-z]:[\\/]") file))
	       (or (looking-at cd-re1)
		   (re-search-backward cd-re2 nil t)))
	  (setq path (match-string 1))))
       (buffer-file-name		;Another condition
	(tinycompile-debug fid 'buffer-file-name)
	;; If we loaded erorr log file from the same directory: try it
	;;
	;;   weblint file.html > file.err
	;;
	;;   --> then load file.err into emacs and start jumping to errors.

	(setq path (file-name-directory buffer-file-name))))


      (tinycompile-debug fid 'file-now file)

      ;;  ./dir/file --> dir/file
      ;;  #todo: what about windows Nt?

      (if (and (stringp file)
	       (string-match "^\\.[/\\]" file))
	  (setq file (ti::replace-match 0 nil file)))

      (setq ret (cons
		 (if path
		     (ti::file-make-path path file)
		   file)
		 line)))

     (t
      (when (looking-at "^\\([^:]+\\):")
	(message
	 "TinyCompile: Line does not contain line number information")
	(sit-for 0.5))

      ;;  I don't know how to handle this line, Let mode below handle it

      (let (tinycompile-mode)
	(setq func (lookup-key (current-local-map) "\C-m"))
	(message "TinyCompile: Passing control to underlying \C-m key: %s"
		 (symbol-name func))
	(when (fboundp func)
	  (funcall func)))))

    ;; ......................................................... verbose ...

    (when verb
      (cond
       (tinyurl)			;Already handled
       ((null ret)
	(message "TinyCompile: Can't read file/line information."))
       (t
	(setq tmp (car ret))

	(setq buffer (or (find-buffer-visiting tmp)
			 (get-buffer tmp)
			 ;; We may have mistakenly grabbed 'cd' command and
			 ;; stucked it with buffers name.
			 ;; /users/foo/*scratch*  --> *scratch*
			 (get-buffer (file-name-nondirectory tmp))))

	;;  If buffer exists and is diplayed in another frame, use it.

	(if buffer
	    (setq win (get-buffer-window buffer t)))

	(tinycompile-debug fid "interactive" buffer 'tmp tmp)

	(cond
	 ((and buffer win)
	  (select-window win)
	  (raise-frame (window-frame win)))
	 (t
	  (ti::select-frame-non-dedicated)
	  (if (and buffer (not (file-exists-p tmp)))
	      (switch-to-buffer-other-window buffer)
	    (switch-to-buffer-other-window (find-file-noselect tmp)))))
	(goto-line (cdr ret)))))

    (tinycompile-debug fid 'ret ret)
    ret))


;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-get-error-lines  (&optional max-point list-func)
  "Get error lines in compile buffer from current point forward.
Input:

  MAX-POINT	max search point, defaults to `point-max'
  LIST-FUNC	if given apply it to extract data member.
		Eg 'car, gives you only list of filenames

Return:

 '((\"filename\" . NBR) (F . N) ..)
 or whatever format LIST-FUNC says."
  (let* ((max-point   (or max-point (point-max)))
	 table
	 elt)
    (save-excursion
      (while (and (re-search-forward "^\\([^:]+\\):[0-9]+:" nil t)
		  (< (point) max-point))
	(setq elt (tinycompile-parse-line))
	(if list-func
	    (setq elt (funcall list-func elt)))
	(if (null (member elt table))
	    (push elt table)))
      (nreverse table))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-kill-all-file-lines ()
  "Kill all lines associated with the file on the current line."
  (interactive)
  (let* ((fid  'tinycompile-kill-all-file-lines)
	 (elt (tinycompile-parse-line))
	 file
	 re
	 point)
    (if (null elt)
	(message "TinyCompile: Can't find file name in this line.")
      (beginning-of-line)
      (setq file (car elt)
	    re   (format "^%s:\\|^%s:\\|^%s:"
			 (file-name-nondirectory file)
			 (regexp-quote file)
			 (file-name-nondirectory file)))

      (tinycompile-debug fid 'file file 'RE re 'elt)

      ;;  Search previous line that is not the same as the line we want
      ;;  to kill

      (while (re-search-backward re nil t))
      (setq point (point))

      (buffer-enable-undo)
      (ti::pmin)

      (delete-matching-lines re)

      (if (< point (point-max))
	  (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-hide-by-regexp  (regexp)
  "Hide lines matching REGEXP."
  (interactive "sHide strings matching: ")
  (tinycompile-show-hide-toggle regexp))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-show-hide-toggle  (&optional regexp)
  "Hide or show comment lines matching REGEXP.
We can't kill the lines from buffer because Compile expects the
lines to be there.

References:
 `tinycompile-:table-hide'"
  (interactive)
  (let* ((list tinycompile-:table-hide)
	 search
	 show)
    (save-excursion
      (unless regexp			;Find right value
	(setq show (y-or-n-p "Y = show, N = hide "))
	(dolist (elt list)
	  (setq search (car elt))
	  (if (ti::re-search-check search)
	      (setq list  nil  regexp (nth 1 elt)))))
      (ti::pmin)
      (cond
       (show
	(set-text-properties (point-min) (point-max) nil)
	;;  Won't update well otherwise
	(redraw-display))
       (t
	(if (null regexp)
	    (message "TinyCompile: No matching regexp in tinycompile-:table-hide")
	  (ti::text-re-search
	   regexp nil nil nil
	   (if show
	       'null
	     '(owner tinycompile  invisible t)))))))))

;;}}}

;; NOTE:  I have no idea why the `tinycompile-mode' gets set globally
;; to value `t' which is highly annoying. After `find-file'
;; tinycompile-mode' was set to on, meaning that the mouse-2 key
;; was taken away.
;;
;; Make sure that the global value is nil

(if (default-value 'tinycompile-mode)
    (setq-default tinycompile-mode nil))

(add-hook 'compilation-mode-hook	    'turn-on-tinycompile-mode 'append)
(add-hook 'tinycompile-:mode-define-keys-hook	'tinycompile-mode-define-keys)

(provide   'tinycompile)
(run-hooks 'tinycompile-:load-hook)

;;; tinycompile.el ends here
