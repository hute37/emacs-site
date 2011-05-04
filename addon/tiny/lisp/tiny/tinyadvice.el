;;; @(#) tinyadvice.el --- Collection of adviced functions
;;; @(#) $Id: tinyadvice.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1996-04
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinyadvice-version
;; Look at the code with folding.el, tinybookmark.el

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

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file.
;;
;;     (require 'tinyadvice)
;;
;; Loading this package takes lot of time. You might gain more comfortable
;; Emacs startup "feel" using the following autoload suggestion:
;;
;;     (require 'tinylib)
;;     (when (emacs-p)                           ;Do not load in XEmacs
;;    	 (if (fboundp 'run-with-idle-timer)      ;Emacs
;;    	     (run-with-idle-time (* 4 60) nil '(lambda () (require 'tinyadvice)))
;;    	   (run-at-time "4 min" nil '(lambda () (require 'tinyadvice)))))
;;
;; But before you leap into this, make sure you want to do it.
;;
;;      CHECK IF YOUR EMACS IS SUPPORTED
;;      THESE ADVICES ARE FOR Emacs, expect trouble in XEmacs.
;;
;;      Change `tinyadvice-:re' to try advices in non-supported Emacs versions
;;
;; This file modifies original Emacs functions, so read the document
;; carefully to tailor this package for you (enabling/disabling advices)
;; The best up to date documentation can be generated from this file:
;;
;;      M-x eval-current-buffer
;;      M-x load-library tinyliby.el
;;      M-x ti::y-get-file-documentation RET tinyadvice.el RET
;;
;; If you have any questions, use this function
;;
;;      M-x tinyadvice-submit-bug-report      send bug report or feedback
;;

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Apr 1996
;;
;;      What you see here is a selection of adviced functions that have
;;      proven to be extremely useful. Some of them have been written by
;;      the author (if there is no author mentioned) and some of them have
;;      been collected form the emacs newsgroups.
;;
;;      Here is one example how to to fontify automatically, whenever
;;      compilation buffer runs:
;;
;;     	    (add-hook 'tinyadvice-:compile-internal-hook 'my-compile-font-lock)
;;
;;     	    (defun my-compile-font-lock  ()
;;     	      "Compile buffer fontifying immediately."
;;     	      (interactive)
;;     	      (let* ((buffer tinyadvice-:compile-internal-buffer))
;;     		;; According to buffer you could set up different font
;;     		;; keyword parameters, say for
;;     		;; *compilation*
;;     		;; *grep*
;;     		;; *igrep*
;;     		;; ...
;;     		;;  My setup automatically turn on the lazy-lock too, see
;;     		;;  font-lock-mode-hook
;;     		(with-current-buffer
;;     		 buffer
;;     		  (turn-on-font-lock))))
;;
;;  Note: XEmacs
;;
;;      These advices are for Emacs and it would be a surprise if they
;;      worked in XEmacs. Use at your own risk. Send fixed XEmacs
;;      compatible advices to maintained if you try them.
;;
;;  These advises and Emacs releases
;;
;;      Many of these enhancements could have shipped with the Emacs
;;      itself. And there was a time when these were suggested to be added
;;      to the next Emacs release. For some reason the developers
;;      were not interested in the features at that time.
;;
;;  How to use this package
;;
;;      The best way is to load this package, print the whole file and read
;;      the comments about individual functions and how they change things.
;;
;;  Note on adviced file functions
;;
;;      If you set variable `tinyadvice-:file-compress-support' to no-nil then
;;      you can compress files on disk at wish and emacs packages that
;;      read/save data from/to files doesn't even notice that you have
;;      compressed them. This is very valuable feature if you are low of
;;      quota, e.g. you can now compress Gnus `.newsrc.eld' and gnus will
;;      read the .newsrc.eld.gz transparently. See `write-file' and
;;      `find-file' function documentation for full explanation of this
;;      feature.
;;
;;  Overview of features
;;
;;	In general, advices are activated only if Emacs release doesn't have
;;	similar kind of support.
;;
;;      o   `gud' highlights full line
;;      o   no dialogs in X for `y-or-n-p' styled questions. You shouldn't
;;          need to lift your hands from keyboard and grab mouse for these
;;          dialog questions.
;;      o   mouse window delete confirmation (pointing on the mode line)
;;      o   `dired', diables dired mouse properties --> dired runs faster.
;;
;;	o   `call-last-kbd-macro' ends the current macro
;;	    before trying to execute it.
;;      o   `debugger-eval-expression',  Backtrace buffer's
;;           "e" offers current word for prompt
;;      o   `dired-man'       , make sure variables are initialized.
;;      o   `dired-do-rename' , you can edit the old filename
;;      o   `find-file'       , can load .gz and .Z file
;;      o   `goto-line' and `imenu' now widens automatically before executing
;;      o   `rename-buffer'   , offers old buffer name for editing
;;      o   `recover-file'    , offers buffer filename by default
;;      o   `switch-to-buffer-other-frame' , selects some non existing frame
;;      o   `setenv'          , getenv offers completion
;;      o   `write-file'      , confirmation upon overwrite, VAX support too
;;      o   `write-region'    , confirmation upon overwrite, VAX support
;;
;;      o   `C-x' `;'   , `indent-for-comment' negative arg deletes comment.
;;      o   `C-x' `='   , `what-cursor-position' shows the line number too
;;      o   `C-x' `i'   , insert buffer offers other window
;;      o   `C-x' `C-c' , `save-buffers-kill-emacs' asks confirmation
;;                        to prevent accidents
;;      o   `C-x' `b'   , `swich-to-buffer' ask confirmation
;;                        for non-existing buffers.
;;      o   `C-x' `C-b' , list-buffers puts you to "*Buffer List*"
;;
;;      o   compilation: buffer auto scroll (disabled, see 'handling advices')
;;          Smart save feature (only .cc .h files, not
;;          all emacs files). Find-file is done in non dedicated frame.
;;          TAB completes filenames.
;;
;;      o   completion:  case sensitive filename completion
;;
;;      o   ediff:  list only files that have diff over directories.
;;
;;      o   grep:   filename completion with TAB key
;;
;;      o   `vc-print-log' ,put cursor on the buffer's revision number.
;;	    Smarter `vc-mode-line' , shows "b" if version is in the middle.
;;	    `vc-register' creates RCS directory if does not exist and
;;           offers checking as "original" file with existing version
;;           numbers (tracking 3rd party sources).
;;           User to set the initial comment when doing 1st CI.
;;           If `tinyadvice-:cvs-buffer-read-only' is nil, then keep.
;;           CVS files in writable mode (the default CVS behavior)
;;
;;  Handling advices
;;
;;      If you have some other emacs version that is not supported in the
;;      `tinyadvice-:advice-table' you can modify the regexps in the list and try
;;      if the advice works in your emacs. If it does, please drop me a
;;      mail immediately and I update the regexp. If some advice annoys
;;      you, there is simple method how you disable advice(s).
;;
;;          (setq tinyadvice-load-hook '(tinyadvice-install my-tinyadvice-load-hook))
;;
;;          (defun my-tinyadvice-load-hook ()
;;            "Configure 'tiny tool's advices' to my taste."
;;            (interactive)
;;            ;; This diables two advices
;;            (tinyadvice-advice 'disable
;;               '(switch-to-buffer mouse-delete-other-windows)))
;;          (require 'tinyadvice)
;;
;;  Disabling disturbing advice by hand
;;
;;      If some piece of advice disturbs or causes trouble in your current
;;      emacs session, you can deactivate it immediately. First you have to
;;      know the function name that generates problems. Say you used `C-x'
;;      `C-b' `switch-to-buffer' and you don't like the confirmation for
;;      non-existent buffers. You can disable this behavior by calling:
;;
;;          C-u M-x tinyadvice-advice
;;
;;      and giving the function name `switch-to-buffer' to it. To permanently
;;      turn it off in your emacs sessions, see previous lisp code.
;;
;;  Code note
;;
;;      You see this in the code:
;;
;;          (when (tinyadvice-activate-p)
;;              (defadvice ..
;;
;;      If emacs version is wrong, the advice is _never_ actually assembled.
;;      You can't activate or deactivate this function with `tinyadvice-advice'.
;;
;;  Many thanks to, in no particular order:
;;
;;      Vladimir Alexiev        <vladimir@cs.ualberta.ca>
;;      Kevin    Rodgers        <kevinr@ihs.com>
;;      Ilya     Zakharevich    <ilya@math.ohio-state.edu>
;;      Peter    Breton         <pbreton@i-kinetics.com>
;;      T. V.    Raman          <raman@adobe.com>

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'advice)
(require 'tinylibm)

(eval-and-compile
  (defvar vc-parent-buffer)		;Emacs vc.el
  (when (xemacs-p)
    (message "\
** TinyAdvice: You must configure this package manually to XEmacs
               In general, do not use this packaage on XEmacs.")
    (load "overlay" 'noerr)))		;19.15+

;;}}}
;;{{{ setup: public

;;; ......................................................... &v-hooks ...

(defvar tinyadvice-load-hook '(tinyadvice-install)
  "Hook that is run when package is loaded.")

;;; ........................................................ &v-public ...

(defvar tinyadvice-:cvs-buffer-read-only t
  "*nil makes CVS buffers writable.  Value t preserves vc.el's decision.
Many times vc.el sets read-only status to CVS buffers when there is no need.
In default case, CVS itself does not mark files read-only, unlike RCS.
But if you do \"cvs watch on\" on a tree then when you do \"cvs co tree\" it
will check files out read-only. You have to do \"cvs edit\" to make them
writable.

Setting this variable to nil, will override vc.el and
keep CVS buffers always writable. The t value preserves what vc.el does.")

(defvar tinyadvice-:compile-internal-hook nil
  "*Hook run after `compile-internal' funtion.
You can peek variable `tinyadvice-:compile-internal-buffer' too.")

(defvar tinyadvice-:compile-save-re
  "\\(\\.hh?\\|\\.cc?\\|\\.C?\\|\\.java\\)$"
  "*Regexp. Case sensitive. Which buffers to save when compiling.")

(defvar tinyadvice-:gud-overlay
  (when (and (emacs-p)
	     (not (fboundp 'make-extent)))
    (let* ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'face 'highlight)
      ov))
  "*Gud. Current line overlay.")

(defvar tinyadvice-:find-alternate-file-flag  t
  "*Non-nil means : `buffer-name' in \\[find-file] if no `buffer-file-name'.")


;;  Ignore tmp/ dir files
;;  like ~/T  ~/TT ~/T1 ~/T2 ~/T.test ~/T1.xx ...

(defconst tinyadvice-:write-file-no-confirm
  "^/tmp\\|/[Tt][Tt0-9]?\\.?\\|/[Tt]$"
  "*Do not verify overwriting these files. See advice `write-file'.")


(defconst tinyadvice-:file-compress-support  nil ;; #todo: See auto-compression-mode
  "Non-nil add .Z .gz support to `find-file', `write-file' and `write-region'.
If this variable is non-nil the FILE.txt is written
out/read as FILE.txt.gz if such file exists.

See `write-file' and find-files function description for Full
explanation.")


(defvar tinyadvice-:switch-to-buffer-find-file  t
  "*Suggest `find-file' for non-existing buffers in `switch-to-buffer'.")

(defvar tinyadvice-:vc-main-rcs-dir  "~/RCS"
  "Main RCS repository. See advice of function `vc-register'.")


;;; ........................................................ &v-advice ...

(defvar tinyadvice-:re "19\\.2[7-9]\\|19\\.3[0-5]\\|2[01]\\."
  "General regexp for advices that work in variety of (X)Emacs versions.")


;; - Change the REGEXP is you know the advice works in your emacs ver.
;;   Drop me mail if you change any of these, so that I can update list
;;
;; - Functions that have ".", almost always get advice, see the code.
;;   In those rows the regexp value is almost always ignored.
;;
;; - If it says ";; always on", then the regexp has no effect,
;;   you have to disable feature by hand, if you don't want it.

(defconst tinyadvice-:advice-table		;alphabetically ordered
  (list
   (list 'after-find-file		".")			;;always on
   (list 'ange-ftp-dired-run-shell-command ".")			;;always on

   (list 'call-last-kbd-macro
		".")
   (list 'compile			".")
   (list 'compile-internal		"2[7-9]")		;;fixed 19.30+
   (list 'compilation-find-file         ".")
   (list 'shell                         ".")

   (list 'debugger-eval-expression	".")

   (list 'dired-do-rename	        ".")
   (list 'dired-insert-set-properties	".")			;;always on
   (list 'dired-man			".")			;;always
   (list 'display-time-process-this-message "19" 'xe)

   (list 'ediff-intersect-directories	".")			;;always
   (list 'exchange-point-and-mark       ".")
   (list 'find-file			".")

   (list 'grep				".")
   (list 'igrep-read-expression         ".")
   (list 'igrep-read-options            ".")

   (list 'find-alternate-file		".")
   (list 'find-tag                      ".")
   (list 'fill-paragraph                "19\.2[0-8]")

   (list 'getenv			".")			;;always on
   (list 'goto-line			".")
   (list 'grep                          ".")
   (list 'gud-display-line		".")			;;always

   (list 'hkey-help-show		".")			;;hyberbole

   (list 'imenu				".")			;; always
   (list 'indent-for-comment		".")
   (list 'insert-buffer			tinyadvice-:re)
   (list 'Info-build-node-completions   "19\\.\\|20\\.")
   (list 'list-buffers			".")
   (list 'line-move			".")

   (list 'map-y-or-n-p			tinyadvice-:re)
   (list 'mouse-delete-other-windows	tinyadvice-:re)
   (list 'mouse-delete-window		tinyadvice-:re)

   (list 'occur				".")
   (list 'PC-complete			".")                    ;;always on

   (list 'recompile			".")
   (list 'recover-file                  ".")
   (list 'rename-buffer			tinyadvice-:re)

   (list 'save-buffers-kill-emacs	".")
   (list 'save-some-buffers             ".")
   (list 'sendmail-pre-abbrev-expand-hook tinyadvice-:re)
   (list 'setenv			".")			;;always on
   (list 'set-mark-command		".")			;;always on
   (list 'switch-to-buffer		tinyadvice-:re)
   (list 'switch-to-buffer-other-frame	".")

   (list 'tq-process-buffer		".")

   (list 'vc-do-command                 tinyadvice-:re)
   (list 'vc-mode-line			tinyadvice-:re)
   (list 'vc-print-log			"2[89]\\|3[01]")	;;fixed in 19.32
   (list 'vc-register			tinyadvice-:re)
   (list 'vc-rcs-status                 tinyadvice-:re)

   (list 'what-cursor-position		tinyadvice-:re)
   (list 'write-file			".")
   (list 'write-region			".")

   (list 'y-or-n-p			tinyadvice-:re))
  "*Flag table of enabled advices.
It is consulted if particular advice can be used in current emacs. Format is

  ((FUNCTION ALLOW-ADVICE-REGEXP [FLAG])
   (FUNCTION ALLOW-ADVICE-REGEXP)
   ..)

The FLAG is optional and values can be:

  nil   or missing: Only works in Emacs
  'xe   only works in Xemacs
  t     works both Emacs and XEmacs")

;;}}}
;;{{{ setup: private

;;; ....................................................... &v-private ...

(defconst tinyadvice-:advice-re  "^tinyadvice"
  "Prefix name used in advices for TinyAdvice package.")

(defconst tinyadvice-:tmp-buffer  "*tinyadvice*"
  "Temporary working buffer.")

(defvar tinyadvice-:compile-internal-buffer  nil
  "The compilation buffer created by `compile-internal'.")

(defvar tinyadvice-:vc-p nil
  "Variable indicating if file in `vc-do-command' is version controlled.")

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinyadvice-version "tinyadvice" "Display commentary." t)
(eval-and-compile
(ti::macrof-version-bug-report
 "tinyadvice.el"
 "tinyadvice"
 tinyadvice-:version-id
 "$Id: tinyadvice.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinyadvice-version-id
   tinyadvice-:compile-save-re
   tinyadvice-:write-file-no-confirm
   tinyadvice-:file-compress-support
   tinyadvice-:re)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ tinyadvice: misc

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyadvice-elts (elt func re type)
  "Decode ELT to variables FUNC RE TYPE."
  (`
   (setq (, func) (nth 0 (, elt))
	 (, re)   (nth 1 (, elt))
	 (, type) (if (eq 3 (length (, elt)))
		      (nth 0 (, elt))
		    nil))))



;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-match  (re &optional type)
"Check if RE match emacs version according to TYPE.
TYPE :
  nil = Emacs
  t   = XEmacs and Emacs
  'xe = XEmacs"
(let* ((ver   (emacs-version))
	ret)
  (cond
   ((and (eq type 'xe)
	 (xemacs-p)
	 (string-match re ver))
    (setq ret 1))

   ((and (eq type nil)
	 (emacs-p)
	 (string-match re ver))
    (setq ret 2))

   ((and (eq type t)
	 (string-match re ver))
    (setq ret 3)))
  ret))



;;; ----------------------------------------------------------------------
;;; Testing... (tinyadvice-activate-p 'compile-internal)
;;;
(defun tinyadvice-activate-p (func-sym)
  "Determine if we can advice FUNC-SYM."
  (let* ((elt   (assoc func-sym tinyadvice-:advice-table))
 	 func
	 re
	 type)
    (when elt
      (tinyadvice-elts elt func re type)
      ;;  XEmacs 19.14 ByteComp, Shut up "bound but not referenced"
      ;;  the `func' is set above.
      (if func
	(setq func 'ignore))
      (tinyadvice-match re type))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-ad-function-list  (&optional string-format)
  "Return list of tinyadvice ad-functions for current emacs.
Notice: all functions may not be adviced; this merely
return entries in the table. See source file's \"Code note\"

If STRING-FORMAT is non nil, then return string list.

Return:

  '(func    func ..)
  '(\"func\" \"func\" ..)"
 (let* (func
	re
	type
	list)
   (dolist (member tinyadvice-:advice-table)
     (tinyadvice-elts member func re type)
     (when (tinyadvice-match re type)
       (if  string-format
	   (push (symbol-name func) list)
	 (push func list))))
   list))


;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-install  ()
  "Activates advices that are listed in `tinyadvice-:advice-table'."
  (interactive)
  (tinyadvice-advice nil (tinyadvice-ad-function-list)))


;;; ----------------------------------------------------------------------
;;;
;;; This is slow, but returns only tinyadvice adviced functions...
;;;
;;; (ad-do-advised-functions (func)
;;;  (if (ad-find-some-advice func 'any tinyadvice-:advice-re)
;;;      (push func list)))
;;;
;;;
(defun tinyadvice-advice (&optional disable func-or-list)
  "Activate or optionally DISABLE tinyadvice advice for FUNC-OR-LIST."
  (interactive
   (list
    current-prefix-arg

    (let* (var)
      (setq var (completing-read
		 (concat
		  (if current-prefix-arg "un"  "")
		  "advice function: ")
		 (ti::list-to-assoc-menu (tinyadvice-ad-function-list 'strings))
		 nil t))
      (intern-soft var))))

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  args end . .
  ;; This is in fact cheating a little; we check against full advice list,
  ;; not just "tinyadvice" owned functions.

  (when (and (symbolp func-or-list)
	     (not (member (list (symbol-name func-or-list))
			  ad-advised-functions )))

    ;; This makes the call to 'ti::' after this if, unefective

    (setq func-or-list nil)

    (if (interactive-p)
	;; more accurate: "No advice found..." but since we deal with
	;; tinyadvice ones only the following is better.
	(message "\
TinyAdvice: Sorry, the function is not advice controlled by TinyAdvice.")))

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. do the job . .
  (ti::advice-control
   func-or-list tinyadvice-:advice-re disable (interactive-p)))



;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-advice-control  (&optional disable verb)
  "Acivate all TinyAdvice advices. Use extra argument to DISABLE all. VERB."
  (interactive "P")
  (let* ((verb	(or verb (interactive-p)))
	 (re	tinyadvice-:advice-re)
	 (doit  t)
	 msg)

    (if verb   ;;  This is rough! Be sure...
	(setq
	 doit
	 (y-or-n-p (format "advices %s: No mistake here? "
			   (if disable "off" "on")))))

    (when doit
      (cond
       (disable
	(ad-disable-regexp re)		;only sets flag
	(setq msg "All TinyAdvice advices activated"))
       (t
	(ad-enable-regexp re)		;only sets flag
	(setq msg "All TinyAdvice advices deactivated")))
      (ad-update-regexp re)
      (if verb
	  (message msg)))))



;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-convert-filename  (file &optional cautious)
  "Return normal or compressed filename.

Input:

 FILE	    full filename
 CAUTIOUS   if non-nil then when in doubt do not change the filename.
	    (e.g. in clash situation, where there is bot un/compressed  file)

Return:

 string	    possibly modified."
  (interactive)
  (unless (string-match "\\.Z$\\|\\.gz$"  file)
    (when (and (file-exists-p file)
	       (or (file-exists-p (concat file ".gz"))
		   (file-exists-p (concat file ".Z"))))
      (message "TinyAdvice: clash, both un/compressed file found. %s " file)
      (sleep-for 1)

      (if (and
	   (null cautious)		;only if no cautious mode
	   (setq
	    file
	    (or (ti::file-newer-exist file (concat file ".gz"))
		(ti::file-newer-exist file (concat file ".Z")))))
	  ;;  We must load this package too to enable compress support.
	  (require 'jka-compr))))
  file)

;;}}}

;;{{{ ange-ftp

;;; ----------------------------------------------------------------------
;;; 1996-03-24 <rdong@printrak.com> Rui-Tao Dong
;;; in <efs-help-request@cuckoo.hpl.hp.com> mailing list
;;;
;;; I am still using the good & old ange-ftp 4.20 most of the time.  I do have
;;; the need to log into the remote host as a different user (including root).
;;; I found the following to be very useful
;;;
(defadvice ange-ftp-dired-run-shell-command (before tinyadvice-rsh-cmd dis)
  "Launch rsh -l if needed."
  (setq ange-ftp-remote-shell-file-name
        (format "rsh -l %s" (nth 1 (ange-ftp-ftp-path default-directory)))))

;;}}}
;;{{{ built-ins

;;; ........................................................ &built-in ...

;;; ----------------------------------------------------------------------
;;;
(when (tinyadvice-activate-p 'rename-buffer)
  (defadvice rename-buffer (around tinyadvice dis)
    "Gives old buffer name for editing."
    (interactive
     (list
      (read-from-minibuffer
       "Rename buffer (to new name): "
       (buffer-name))))
    ad-do-it))

;;}}}
;;{{{ compile

;;; ......................................................... &compile ...

;;; ----------------------------------------------------------------------
;;; (ad-disable-advice 'compilation-find-file 'before 'tinyadvice)
;;; (ad-activate       'compilation-find-file)
;;;
(defadvice compilation-find-file  (before tinyadvice act)
  "Move to some non dedicated frame."
  (ti::select-frame-non-dedicated))

;;; ----------------------------------------------------------------------
;;;
(defadvice shell (around tinyadvice dis)
  "Rename old shell buffer and ask if user want to create new one.
To change to existing shell without asking, prefer `switch-to-buffer'
or `iswitchb.el'."
  (let* (name)
    (when (and (interactive-p)
	       (comint-check-proc "*shell*")
	       (setq name
		(read-string
		 "Create new shell by renaming old? [RET no] "))
	       (not (ti::nil-p name)))
      (with-current-buffer "*shell*"
	(rename-buffer name))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; See variable `compilation-last-buffer'
;;;
;;; - Handling other frames suggested by James (Jim) R.R. Service
;;;   <jservice@rd.hydro.on.ca> (the extra t's)
;;;
;;; - This has been reported to be corrected in 19.30
;;;
(when (and (not (boundp 'compilation-scroll-output))
	   (tinyadvice-activate-p 'compile-internal))

  (defadvice compile-internal (after tinyadvice-scroll dis comp)
    "Force compile buffer to scroll."
    (let* ((ob (current-buffer))
	   (obw  (get-buffer-window ob t))
	   win)
      (save-excursion
	(unless (or (null (setq win (get-buffer-window ad-return-value t)))
		    (null obw))
	  (select-window win)
	  (goto-char (point-max))
	  (select-window obw))))))

;;; ----------------------------------------------------------------------
;;; "tap" -- listen secretly :-)
;;;
(defadvice compile-internal (around tinyadvice-tap-buffer dis comp)
  "Save compile buffer name to 'tinyadvice-:compile-internal-buffer'.
See `tinyadvice-:compile-internal-hook'."
  (prog1
      ad-do-it
    (setq tinyadvice-:compile-internal-buffer ad-return-value)))


;;; ----------------------------------------------------------------------
;;;
(defadvice compile-internal (after tinyadvice-run-hook last act comp)
  "Run hook 'tinyadvice-:compile-internal-hook'.
E.g. you can add lazy-lock.el fontifying to that hook."
  (run-hooks 'tinyadvice-:compile-internal-hook))



;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-compile-save-buffers ()
  "Check what buffers for current compilation target should be saved."
  (interactive)
  (let* ((case-fold-search	nil)	;case sensitive
	 (re-file               tinyadvice-:compile-save-re))

    ;; Save only interesting buffers, don't care about others.
    (ti::dolist-buffer-list
     (string-match re-file  (buffer-name))
     nil
     nil
     (and (buffer-modified-p)
	  (y-or-n-p (format "Buffer %s modified. Save it? "
			    (buffer-name)))
	  (save-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice igrep-read-expression (around tinyadvice dis)
  "Replace function: TAB key completes file names."
  (setq
   ad-return-value
   (let ((default-expression (igrep-default-arg igrep-expression-default)))
     (if (string= default-expression "")
	 (setq default-expression nil))
     (ti::file-complete-filename-minibuffer
       (read-from-minibuffer (igrep-prefix prompt-prefix "Expression: ")
			     default-expression map nil
			     'igrep-expression-history)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice igrep-read-options (around tinyadvice act)
  "Replace function: TAB key completes file names."
  (setq
   ad-return-value
   (if (or igrep-read-options
	   (and (consp current-prefix-arg)
		(memq (prefix-numeric-value current-prefix-arg)
		      '(4 64))))
       (let ((prompt "Options: "))
	 (ti::file-complete-filename-minibuffer
	   (read-from-minibuffer
	    (igrep-prefix prompt-prefix prompt)
	    (or igrep-options "-")
	    map)))
     igrep-options)))

;;; ----------------------------------------------------------------------
;;;
(defadvice grep (around tinyadvice dis)
  "Modify interactive spec: TAB key completes file names."
  (interactive
   (let (grep-default (arg current-prefix-arg))
     (unless grep-command
       (grep-compute-defaults))
     (when arg
       (let* ((tag-default
	       (funcall (or find-tag-default-function
			    (get major-mode 'find-tag-default-function)
			    ;; We use grep-tag-default instead of
			    ;; find-tag-default, to avoid loading etags.
			    'grep-tag-default))))
	 (setq grep-default (or (car grep-history) grep-command))
	 ;; Replace the thing matching for with that around cursor
	 (if (string-match
	      "[^ ]+\\s +\\(-[^ ]+\\s +\\)*\\(\"[^\"]+\"\\|[^ ]+\\)"
	      grep-default)
	     (setq grep-default (replace-match tag-default t t
					       grep-default 2)))))
     (list (ti::file-complete-filename-minibuffer
	     (read-from-minibuffer "Run grep (like this): "
				   (or grep-default grep-command)
				   map nil 'grep-history)))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; - More smarter buffer saving.
;;;
(defadvice compile (around tinyadvice dis)
  "Replace original function. More smarter buffer saving.
See function `tinyadvice-compile-save-buffers'.
In addition, TAB key completes file names."
  (interactive
   (if compilation-read-command
       (list (ti::file-complete-filename-minibuffer
	       (read-from-minibuffer "Compile command: "
				     compile-command map nil
				     '(compile-history . 1))))
     (list compile-command)))
  (setq compile-command command)

  (if (null compilation-ask-about-save)
      (save-some-buffers (not compilation-ask-about-save) nil)
    (tinyadvice-compile-save-buffers))

  (compile-internal compile-command "No more errors"))



;;; ----------------------------------------------------------------------
;;; Run compile with the default command line
;;;
(defadvice recompile (around tinyadvice dis)
  "Replace original function.
More smarter buffer saving, seefunction `tinyadvice-compile-save-buffers'."
  (interactive)
  (if (null compilation-ask-about-save)
      (save-some-buffers (not compilation-ask-about-save) nil)
    (tinyadvice-compile-save-buffers))
  (compile-internal compile-command "No more errors"))


;;}}}
;;{{{ completion and macros

;;; ...................................................... &completion ...


;;; ----------------------------------------------------------------------
;;; 2000-09-03 <Tom_Roche@ncsu.edu> in NT Emacs mailing list
;;;
(defadvice call-last-kbd-macro (before tinyadvice dis)
  "If still defining a macro, end it before attempting to call-last.
  This prevents whacking the current definition."
  (if defining-kbd-macro
      (end-kbd-macro)))

;;; ----------------------------------------------------------------------
;;;
(defadvice PC-complete  (around tinyadvice dis)
  "In file name prompt, use case sensitive completion.
Set `completion-ignore-case' locally to nil."
  (let* ((completion-ignore-case  completion-ignore-case)
	 word)
    (setq word (or (save-excursion (ti::buffer-read-space-word)) ""))

    (if (string-match "^[/~]" word)
	(setq completion-ignore-case nil))
    ad-do-it))


;;}}}

;;{{{ debugger


;;; -------------------------------------------------------- &debugger ---
;;;
(defadvice debugger-eval-expression (around tinyadvice dis)
  "Chnage interactive so that it offer word from buffer."
  (interactive
    (list
     (read-from-minibuffer
      "(tinyadvice) Eval: "
      (or (ti::buffer-read-space-word) "")
      read-expression-map t
      'read-expression-history)))
  ad-do-it)


;;}}}
;;{{{ dired

;;; ........................................................... &dired ...


;;; ----------------------------------------------------------------------
;;;
(defadvice dired-mark-read-file-name (around tinyadvice dis)
  "Instead of asking directory, offer full filename for editing.
This advice is enabled only upon request."
  (if (and dir (string-match "/" dir))
      (setq dir (dired-get-filename)))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice dired-do-rename  (around tinyadvice act)
  "Offer editing the current filename.
Without this advice you don't get the old filename for editing.
Activates advice 'dired-mark-read-file-name 'my 'around during call."
  (let* ((ADVICE 'dired-mark-read-file-name))
    (ad-enable-advice ADVICE 'around 'tinyadvice)
    (ad-activate ADVICE)
    ad-do-it
    (ad-disable-advice ADVICE 'around 'tinyadvice)
    (ad-activate ADVICE)))

;;; ----------------------------------------------------------------------
;;; - I love to have highlight in emacs, but if it makes thigs slow
;;;   in emacs, then it's not so important. Dired is too slow alredy.
;;; - I think that lazy-lock would be more appropriate here.
;;;
(defadvice dired-insert-set-properties (around tinyadvice dis)  ;; #todo: Check
  "Replace function.
Inhibits setting `mouse-face' properties so that dired executes faster."
  nil)


;;; ----------------------------------------------------------------------
;;; 1996-03-06 <pbreton@i-kinetics.com>
;;;
(defadvice dired-man (before tinyadvice dis)
 "Make sure man variables are initialized."
  (require 'man)
  (Man-init-defvars))


;;}}}

;;{{{ ediff

;;; ----------------------------------------------------------------------
;;; 1996-09-25 Kevin Rodgers <kevinr@ihs.com> gnu.emacs.help
;;; This affects `ediff-directories3' and `ediff-merge-directories' as well.")

(defvar tinyadvice-:ediff-directories-different-files t
  "*See `ediff-intersect-directories'.
If non-nil, `ediff-directories' only includes files that differ
from one another (plus directories) in the session.")

(defadvice ediff-intersect-directories (before tinyadvice-different-files act)
  "Return files with the same name but different contents.
If `tinyadvice-:ediff-directories-different-files' is non-nil and COMPARISON-FUNC
is nil, only return files with the same name but different contents
\(plus directories with the same name\)."
  ;; Warning: The new COMPARISON-FUNC depends on the dynamically scoped
  ;; binding of `auxdir1', `auxdir2', and `auxdir3' established by
  ;; `ediff-intersect-directories'.
  ;;
  (if (and tinyadvice-:ediff-directories-different-files
	   (null (ad-get-arg 6)))	; comparison-func
      (ad-set-arg 6 (lambda (file-1 file-2)
		      (and (string= file-1 file-2)
			   (let ((directory-1 auxdir1)
				 (directory-2 (if (and dir3 common)
						  auxdir3
						auxdir2)))
			     (/= (call-process "cmp" nil nil nil
					       "-s"
					       (expand-file-name file-1
								 directory-1)
					       (expand-file-name file-2
								 directory-2))
				 0)))))))

;;}}}
;;{{{ env

;;; ............................................................. &env ...

;;; ----------------------------------------------------------------------
;;; 1995-06-28 Kevin Rodgers <kevinr@ihs.com> gnu.emacs.sources
;;; provide completion
;;;
(defun tinyadvice-read-envvar (prompt &optional require-match)
  "Read an environment variable name from the minibuffer.
Prompt with PROMPT and complete from `process-environment'.
If optional arg REQUIRE-MATCH is non-nil, only defined variable
names are allowed."
  (completing-read
   prompt
   (mapcar (function
            (lambda (var=value)
              (list (substring var=value 0
                               (string-match "=" var=value)))))
           process-environment)
   nil
   require-match))

;;; ----------------------------------------------------------------------
;;; 1995-06-28 Kevin Rodgers <kevinr@ihs.com> gnu.emacs.sources
;;;
;;; Comment by jari: Hanged for me sometimes, don't know why..
;;; Currently owned by "my" and disabled. Enable this manyally in load-hook
;;; if you want to try it.
;;;
(defadvice getenv (around my dis)
  "Make command more comfortable. Completion."
  (interactive (list (tinyadvice-read-envvar "Get environment variable: " t)))
  ad-do-it
  (if (and (interactive-p)
	   ad-return-value)
      (message "%s" ad-return-value)
    ad-return-value))


;;; ----------------------------------------------------------------------
;;; 1995-06-28 Kevin Rodgers <kevinr@ihs.com> gnu.emacs.sources
;;; 1996-03-29 Jari - Added editing previous content
;;;
(defadvice setenv (around tinyadvice dis)
  "Add interactive completion."
  (interactive
   (if current-prefix-arg
       (list (tinyadvice-read-envvar "Clear environment variable: " t) nil t)
     (let ((var (tinyadvice-read-envvar "Set environment variable: ")))
       (list var
	     (read-from-minibuffer
	      (format "Set %s to value: " var)
	      (or (getenv var) ""))))))
  ad-do-it
  (if (and (interactive-p) value)
      (message "%s" value)
    value))

;;}}}

;;{{{ grep, tag

;;; ------------------------------------------------------------ &grep ---
;;;
(defadvice grep  (around tinyadvice  dis)
  "Complete filenames with TAB.
Read word from the current pointand put it into grep prompt."
  (interactive
   (ti::file-complete-filename-minibuffer
     (list
      (read-from-minibuffer
       "(tinyadvice) Run grep: "
       (concat grep-command  (or (ti::buffer-read-space-word) ""))
       map
       nil
       'grep-history))))
  ad-do-it)


;;; ----------------------------------------------------------------------
;;; 2001-04-10 Richard Y. Kim <ryk@dspwiz.com>
;;;
(defadvice find-tag (after tinyadvice-reposition-window act)
  "Call reposition-window after finding a tag."
  (reposition-window))


;;}}}

;;{{{ files.el , info

;; Richard Y. Kim" <ryk@dspwiz.com>
;; This is a bug in info.el which is fixed in Emacs 21.
;;
;; The command 'g*' in info mode is mentioned in the info nodes
;; "(info)Expert" and "(info)Add" on emacs 20.4 info directory.
;; I quote from these nodes:
;;
;;     The node name `*' specifies the whole file.  So you can
;;     look at all of the current file by typing `g*<RET>' or
;;     all of any other file with `g(FILENAME)<RET>'.
;;
;;     The node name `*' is special: it refers to the entire
;;     file.  Thus, `g*' shows you the whole current file.  The
;;     use of the node `*' is to make it possible to make
;;     old-fashioned, unstructured files into nodes of the
;;     tree.
;;
;; The command 'g*' does not work as documented due to bug in the elisp
;; code.  But bug is that completing-read function is used with
;; REQUIRE-MATCH set to non-nil, but the completion list does not have
;; '("*") entry in it.
;;
;; The solution could be to set REQUIRE-MATCH to nil.  Instead, I prefer
;; to add '("*") to the completion list. Adding one line just before the
;; last line of Info-build-node-completions function makes 'g*' command
;; work.

(defadvice Info-build-node-completions
  (after tinyadvice-add-star-to-completion-list dis)
  "Insert `*' to the node name completion list so that (Info-goto-node \"*\")
works even when it is called interactively."
  (or (assoc "*" ad-return-value)	;no need to add, if it was already done
      (setq ad-return-value (cons '("*") ad-return-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;			    files.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;; ANGE things...
;;; - Ange ftp gets "listing" when it tries to guess if the file
;;;   exists or if it's new file. The listing is produced with the call
;;;   `insert-file-contents'
;;;
;;; find-file-noselect (filename &optional nowarn)
;;;  ...
;;;  ange-ftp-insert-file-contents
;;;    ..file-exists-p
;;;
(defadvice after-find-file (around tinyadvice-file dis)
  "Suppress call if no `buffer-file-name'. This may happen with ange-ftp."
  (if buffer-file-name
      ad-do-it))

;;; ----------------------------------------------------------------------
;;;
(defadvice find-file (around tinyadvice dis)
  "Check if there is compressed file and load it instead.

If there is both files, uncomressed and compressed; then load the
one that is newest and issue warning.

    `find-file' FILE.txt
    FILE.txt
    FILE.txt.gz	    -- this is newer, load it.

    `find-file' FILE.txt.gz  -- do nothing special
    `find-file' FILE.txt.Z   -- do nothing special

You have to set `tinyadvice-:file-compress-support' to non-nil to enable
this feature."
  (if tinyadvice-:file-compress-support
      (ad-set-arg 0 (tinyadvice-convert-filename (ad-get-arg 0))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; 19.30 doesn't offer the filename, so enable this in all emacs versions
;;;
(defadvice find-alternate-file  (around tinyadvice dis)
  "Interactive change: offer buffer filename as default.
Reference:
  `tinyadvice-:find-alternate-file-flag'"
  (interactive
   (list
    (read-file-name
     "find alternate file: "
     (file-name-directory (or (buffer-file-name)
			      default-directory))
     nil
     t
     (if (buffer-file-name)
	 (file-name-nondirectory (buffer-file-name))
       (if tinyadvice-:find-alternate-file-flag
	   (buffer-name) "")))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice recover-file  (around tinyadvice dis)
  "Offer current buffer's filename in prompt."
  (interactive
   (list
    (read-file-name
     "(TinyAdvice) Recocer file: "
     (file-name-directory (or (buffer-file-name)
			      default-directory))
     nil
     t
     (if (buffer-file-name)
	 (file-name-nondirectory (buffer-file-name))
       (if tinyadvice-:find-alternate-file-flag
	   (buffer-name) "")))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; 1995-08-29 idea by Kevin Rodgers <kevinr@ihs.com>
;;; 1996-02-16 Jari - reprogrammed
;;;
(defadvice write-file (around tinyadvice-file dis)
  "File handling additions.

Interactive change:

    Changes the interactive prompt so, that full `buffer-file-name' is given
    for editing.

Confirm overwrite:

    When called interactively, require confirmation if FILENAME already exists.
    If FILENAME matches `tinyadvice-:write-file-no-confirm', no confirmation
    is asked.

Vax support:

    Also modifies the file name so that you can write VAX files by removing
    the versin extension.

    LOGIN.COM;98 --> written as LOGIN.COM

    You cannot normally write with the version extension, the VAX will
    increase the number when using the \"raw\" name only

Compressed file support, .gz and.Z:

    If `write-file' is called to write FILE.txt and there only exists
    FILE.txt.gz, then write to that file instead. This allows you
    to compress files and not needing to make any changes to e.g. lisp
    packages that save files (e.g. in Gnus you can now have
    .newsrc.eld.gz which takes much less space)

    If called with FILE.txt and there exists both files FILE.txt and
    FILE.txt.gz, then warning about possible conflict is showm and
    the original file argument is used (FILE.txt).

    You have to enable this feature by setting
    `tinyadvice-:file-compress-support' to non-nil."
  (interactive
   ;; Change    "Fwrite to file: "
   (list
    (read-file-name
     "write to file: "
     (or (buffer-file-name)
	 "~/"))))


  (let* ((fn           (ad-get-arg 0))
	 ;;  Tmp buffers do not have filename
	 (buffer-file  (or fn (buffer-file-name) ""))
	 (pass         t))

    ;;  Check if this is VAX file, remove the version extension
    ;;  for the buffer if this is

    (if (and  (stringp buffer-file)
	      (string-match "\\(.*\\);[0-9]+$" buffer-file))
	(setq fn (match-string 1 buffer-file)))

    (if tinyadvice-:file-compress-support
	(ad-set-arg 0 (tinyadvice-convert-filename fn 'cautious))
      (ad-set-arg 0 fn))

    (if (stringp fn)
	(setq pass (ti::string-match-case tinyadvice-:write-file-no-confirm fn)))

    (if (or (not (interactive-p))	;only when user call it, do checks
	    (not (file-exists-p fn))
	    pass
	    (y-or-n-p (format "%s already exists; overwrite it? " fn)))
	ad-do-it
      (message "Aborted"))))

;;; ----------------------------------------------------------------------
;;;
(defadvice write-region (around tinyadvice-file dis)
  "See `write-file' which explains the advice behavior."
  (interactive "r\nFwrite region: ")
  (let* ((fn           (ad-get-arg 2))
         (buffer-file  fn)
	 (pass         t))

    ;;  Check if this is VAX file, remove the version extension
    ;;  for the buffer if this is

    (if (and (stringp buffer-file)
	     (string-match "\\(.*\\);[0-9]+$" buffer-file))
	(setq fn (match-string 1 buffer-file)))

    (if tinyadvice-:file-compress-support
	(ad-set-arg 2 (tinyadvice-convert-filename fn 'cautious))
      (ad-set-arg 2 fn))

    (if (stringp fn)
	(setq pass (ti::string-match-case tinyadvice-:write-file-no-confirm fn)))

    (if (or (not (interactive-p))
	    (not (file-exists-p fn))
	    pass
	    (y-or-n-p (format "%s already exists; overwrite it? " fn)))
	ad-do-it
      (message "Aborted"))))


;;; ----------------------------------------------------------------------
;;;
(defadvice save-some-buffers (before tinyadvice dis)
  "Always save changed abbrevs without questions if `save-abbrevs' is set."
  (when (and save-abbrevs abbrevs-changed)
    (write-abbrev-file nil)
    (setq abbrevs-changed nil)))


;;}}}
;;{{{ fill

;;; ............................................................ &fill ...


;;; ----------------------------------------------------------------------
;;; In new cc-mode there variable `c-hanging-comment-ender-p'
;;; which does exactly same than this advice.
;;;
;;; We install this advice for older emacs only.
;;;
(when (tinyadvice-activate-p 'fill-paragraph)

(defadvice fill-paragraph (after tinyadvice dis)
  "Touch C comment filling, otherwise do nothing.
If the fill was done to C comment. It usually levaes it like this,
while this advice corrects it a bit and moves the last asterisk to
the next line.

/* comment ...         /* comment ...
 * ends here. */        * ends here.
                        */

This function does not affect C comments that occupy only one line."
  (let* (col
	 line)
    (when (and (save-excursion
		 (beginning-of-line)
		 (and

		  ;;  If this is continuing line "*", then search back
		  ;;  otw we're at "/*" already
		  ;;

		  (if (looking-at "^[ \t]*[*]")
		      (re-search-backward "^[ \t]*/[*]" nil t)
		    (looking-at "^[ \t]*/[*]"))
		  (re-search-forward  "^[ \t]*/[*]" nil t)
		  (setq col (current-column)  line (ti::current-line-number))))
		 (re-search-forward "[*]/" nil t)

		 ;;  - The "/*" and "*/" must be at different lines,
		 ;;    because only then we want to adjust the last "*/"
		 ;;  - Skip one line comments.

		 (not (eq (ti::current-line-number) line)))
      (delete-backward-char 2) (insert "\n")
      (move-to-column (1- col) 'force)
      (insert "*/")))))

;;}}}

;;{{{ gud

;;; ............................................................. &gud ...

;;; ----------------------------------------------------------------------
;;; See gud.el
;;;
(defadvice gud-display-line (after tinyadvice dis)
  "Highlight current line."
  (when (and tinyadvice-:gud-overlay
	     (fboundp 'move-overlay))
    (let* ((ov tinyadvice-:gud-overlay)
	   (bf (gud-find-file true-file)))
      (save-excursion
	(set-buffer bf)
	(move-overlay
	 ov
	 (line-beginning-position)
	 (line-end-position)
	 (current-buffer))))))

;;}}}

;;{{{ imenu

;;; ........................................................... &imenu ...

;;; ----------------------------------------------------------------------
;;; 1996-03-06 <pbreton@i-kinetics.com>
;;;
(defadvice imenu (before tinyadvice dis)
  "Widen the buffer before activating imenu."
  (widen))

;;}}}
;;{{{ mail

;;; ............................................................ &mail ...


;;; ----------------------------------------------------------------------
;;; 1996-04-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;; See mailabbrev.el
;;;
(defadvice sendmail-pre-abbrev-expand-hook
  (around tinyadvice-no-abbrevs-in-body dis)
  "Do not expand any abbrevs in the message body through `self-insert-command'."
  (if (or (mail-abbrev-in-expansion-header-p)
	  ;; (not (eq last-command 'self-insert-command)) ; can't be used
	  ;; since last-command is the previous, not the current command
	  (not (integerp last-command-char))
	  (eq (char-syntax last-command-char) ?w)) ; this uses that
      ;; the last char in {C-x '} {C-x a '} {C-x a e} is `w' syntax
      ad-do-it
    (setq abbrev-start-location (point) ; this hack stops expand-abbrev
	  abbrev-start-location-buffer (current-buffer))))



;;}}}
;;{{{ map-ynp

;;; ......................................................... &map-ynp ...

;; 19.28
;; - map-ynp.el::map-y-or-n-p  Get's loaded in loadup.el, it pops up
;;   an dialog Box of questions if the input is event type and it is
;;   annoying to answer yes/no dialog boxes. It is much quicker to
;;   hit SPACE/DEL for yes/no.
;; - Hmm actually it looks back what the command was by looking at
;;   `last-nonmenu-event' variable, so I should reset it instead.
;; - *argh* I was wrong, it is the `y-or-n-p' (built-in) command that pops up
;;   the dialog, anyway the advice works for it too: built-in or not
;;   doesn't matter
;;
;; The way to do this in XEmacs is:
;;
;;    (setq use-dialog-box nil)

(when (and (ti::xe-window-system)
	   (emacs-p))
  (defadvice map-y-or-n-p (before tinyadvice dis)
    "Reset any mouse event to key event so that no dialogs are displayed."
    (if (listp last-nonmenu-event)
	;; replace with some harmless value
	(setq last-nonmenu-event ?\n)))
  (defadvice y-or-n-p (before tinyadvice dis)
    "Reset any mouse event to key event so that no dialogs are displayed."
    (if (listp last-nonmenu-event)
	;; replace with some harmless value
	(setq last-nonmenu-event ?\n))))

;;}}}
;;{{{ mouse

;;; ........................................................... &mouse ...

;;; ----------------------------------------------------------------------
;;;  Too many accidents happened...
;;;
(defadvice mouse-delete-other-windows  (around tinyadvice dis)
  "Confirm window delete."
  (if (y-or-n-p "Really delete _all_ windows ")
      ad-do-it
    (message "")))

;;; ----------------------------------------------------------------------
;;;
(defadvice mouse-delete-window  (around tinyadvice dis)
  "Confirms window delete."
  (if (y-or-n-p "Really delete _this_ window ")
      ad-do-it
    (message "")))


;;}}}
;;{{{ replace.el

(defadvice occur  (before tinyadvice act)
  "Iinteractive change: ask if user want the occur to start from `point-min'.
also Possibly unfold/un-outline the code."
  (when (and (interactive-p)
	     (not (eq (point) (point-min)))
	     (y-or-n-p "TinyAdvice: Start occur from point-min? "))
    (if (and (or (and (featurep 'folding)
		      (symbol-value 'folding-mode))
		 (and (and (featurep 'outline)
			   (boundp  'outline-mode))
		      (symbol-value 'outline-mode)))
	     (save-excursion
	       (ti::pmin)
	       (re-search-forward "\r" nil t))
	     (y-or-n-p "TinyAdvice: Open buffer's selective display too? "))
	(ti::buffer-xtra-open-outline))))

;;}}}
;;{{{ simple.el

;;; .......................................................... &simple ...



;;; ----------------------------------------------------------------------
;;; 1996-04-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;; See simple.el
;;;
(defadvice exchange-point-and-mark (around tinyadvice-pop-if-prefix dis)
  "If given prefix, call `set-mark-command' to pop previous mark positions."
  (if (and current-prefix-arg
	   (interactive-p))
      (call-interactively 'set-mark-command))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; 1996-03-06 <pbreton@i-kinetics.com>
;;;
(defadvice goto-line (around tinyadvice dis)
  "Widen the buffer before and after `goto-line' command."
  (widen)
  ad-do-it
  ;;  We do this because, the folding.el sets narrowing in effect,
  ;;  when the goto-line has finished.
  ;;  #todo: should we check featurep 'folding?
  (widen))


;;; ----------------------------------------------------------------------
;;; 1995-11-17 Francesco Potorti <pot@cnuce.cnr.it> gnu.emacs.sources
;;;
(defadvice indent-for-comment (around tinyadvice dis)
  "Kill the comment with negative prefix."
  (if (eq current-prefix-arg '-)
      (kill-comment nil)
    ad-do-it))


;;; ----------------------------------------------------------------------
;;; 1995-12-06 Francesco Potorti <pot@cnuce.cnr.it>  gnu.emacs.sources
;;; Redefine insert-buffer to insert a visible buffer, if there's one.
;;;
(defadvice insert-buffer (before tinyadvice dis)
  "Use a more reasonable default, the other window's content."
    (interactive
     (list
      (progn
	(barf-if-buffer-read-only)
	(read-buffer "Insert buffer: "
		     (if (eq (selected-window)
			     (next-window (selected-window)))
			 (other-buffer (current-buffer))
		       (window-buffer (next-window (selected-window))))
		     t)))))


;;; ----------------------------------------------------------------------
;;; 1995-11-28 Joachim Schrod <schrod@iti.informatik.th-darmstadt.de>
;;; avoid deactivation of region when buffer end or beginning is reached
;;;
(defadvice line-move (around tinyadvice dis)
  "Avoid deactivation of region. in `beginning-of-buffer' or `end-of-buffer'."
  (condition-case ()
      ad-do-it
    ((beginning-of-buffer end-of-buffer)
     (if (bobp)
	 (message "Beginning of buffer.")
       (message "End of buffer.")))))



;;; ----------------------------------------------------------------------
;;; 1996-05-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;;
(defadvice set-mark-command (around tinyadvice-global-if-negative dis)
  "If the argument is negative, call `pop-global-mark'."
  (if (< (prefix-numeric-value current-prefix-arg) 0)
      (pop-global-mark)
    ad-do-it))



;;; ----------------------------------------------------------------------
;;;
(defadvice what-cursor-position (around tinyadvice dis)
  "Displays line number info too."
  ad-do-it
  ;;  we have to use 'princ' because there is percentage mark
  ;;  in returned string and that would run 'message' beserk,
  ;;  since it thinks it's formatting code
  (princ (concat ad-return-value (int-to-string (ti::widen-safe (ti::current-line-number))))))



;;; ----------------------------------------------------------------------
;;; Code by Jari. Jerome Santini <santini@chambord.univ-orleans.fr>
;;  suggested adding `find-file'
;;;
(defadvice switch-to-buffer (around tinyadvice dis)
  "When called interactively: Confirm switch to non-existing buffer.

References:

  `tinyadvice-:switch-to-buffer-find-file'
   if non-nil, suggest `find-file' for non-existing buffers"
  (interactive "Bbuffer name: ")
  (let ((buffer-name (ad-get-arg 0)))
    (if (or (not (interactive-p))	;user didn't call us
	    (get-buffer buffer-name))	;it exists
	ad-do-it
      (cond
       ((y-or-n-p (format "`%s' does not exist, create? " buffer-name))
	ad-do-it)			;ceate new buffer

       (tinyadvice-:switch-to-buffer-find-file ;is this enabled ?
       (find-file (read-file-name "(tinyadvice) Find-file: "
				  nil
				  nil
				  nil
				  buffer-name)))))
    (message "")))			;clear the echo area

;;; ----------------------------------------------------------------------
;;;
(defadvice switch-to-buffer-other-frame  (around tinyadvice dis)
  "Replace function. Don't ever create new frame; reuse some existing frame."
  (let ((free-frames (ti::window-frame-list nil 'exclude-current))
	stat)
    (if (null free-frames)
	(pop-to-buffer buffer)
      (cond
       ((setq stat (ti::window-get-buffer-window-other-frame buffer))
	;;  buffer is displayed already in some OTHER frame; go to it.
       (raise-frame (car stat))
       (select-frame (car stat))
       (select-window (cdr stat)))
       (t
	;;  Go to some free frame and pop up there
       (raise-frame  (car free-frames))
       (select-frame (car free-frames))
       (switch-to-buffer buffer))))))

;;}}}
;;{{{ subr.el

;;; ----------------------------------------------------------------------
;;; 1996-03-06 <pbreton@i-kinetics.com>
;;;
(defadvice save-buffers-kill-emacs (around tinyadvice dis)
  "Redefine `save-buffers-kill-emacs' to prevent accidental logouts."
  (cond
   ((and (interactive-p)
	 (y-or-n-p "TinyAdvice: Really quit emacs? "))
    (message "")
    ad-do-it)
   ((not (interactive-p))
    ad-do-it)))


;;; ----------------------------------------------------------------------
;;; - This puts cursor to generated list. Propably what we
;;;   want 99% of the time.
;;;
(defadvice list-buffers  (after tinyadvice dis)
  "Select buffer list after displaying."
  (if (interactive-p)
      (select-window (get-buffer-window "*Buffer List*"))))

;;}}}
;;{{{ time

;;; ............................................................ &time ...

;;; ----------------------------------------------------------------------
;;; 1996-03-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;; This is for reporter.el by Barry A. Warsaw in the xemacs distribution
;;;
(defadvice display-time-process-this-message (around tinyadvice-no-junk-mail dis)
  "Suppress message in modeline.
If display-time-announce-junk-mail-too is nil, suppress the [Junk mail]
message on the modeline."
  ((let ((modeline display-time-mail-modeline))
     ad-do-it
     (if (and ad-return-value		; junk-p
     (not display-time-announce-junk-mail-too))
	 ;; restore non-junk modeline
	 (setq display-time-mail-modeline modeline))
     ad-return-value)))


;;}}}
;;{{{ tq.el -- 19.30

;;; .............................................................. &tq ...


;;; ----------------------------------------------------------------------
;;; 1996-03-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;;
(defadvice tq-process-buffer (around tinyadvice-be-quiet dis protect)
  "It was overly loud when the webster server times out.
Turns 'pop-to-buffer to 'ignore and 'error to 'message during its execution."
  ;;  Actually I use another webster now, but the old one had
  ;;  thesaurus and stemming
  (flet ((pop-to-buffer (&rest args))
	 (error         (&rest args)))
    ad-do-it
    ad-return-value))

;;}}}
;;{{{ vc

;;; .............................................................. &vc ...


;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-rcs-initial-comment (file)
  "Add initial comment leader to RCS FILE."
  (let* (buffer
	 file-type
	 str)
    (when (and (stringp file)		;if not nil
	       (ti::vc-rcs-file-exists-p file)) ;RCS controlled file

      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type of file ^^^

      (with-current-buffer (get-file-buffer file)
	(setq file-type (or (ti::id-info nil 'variable)
			    (symbol-name major-mode)))
	(setq str comment-start))

      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ default comment ^^^

      (cond
       ((string-match "lisp" file-type)
	(setq str ";; "))
       ((string-match "c[+]+" file-type)
	(setq str "// "))
       ((stringp str)			;original comment, leave it as is
	nil)
       (t
	(setq str "# ")))			;Not set? Suggest shell comment

      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ setting comment ^^^

      (unless (ti::nil-p		;only if given something
	       (setq str
		     (read-from-minibuffer
		      "Set RCS comment leader to:" str)))
	(setq str (format "rcs -c\"%s\" %s" str file)) ;Shell command

	(message "TinyAdvice: setting rcs comment...")
	(shell-command str "*vc*" )

        ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ fixing emacs buffer ^^^

	;;  - Now, the rcs -u only modified the delta file in RCS tree,
	;;    we must take the version out of the tree, so that the new
	;;    comment setting takes place: Do "co" and reread the file
	;;    into emacs.
	;;

	(message "TinyAdvice: refreshing the file comment...")

	(shell-command (format "co %s" file))

	(when (setq buffer (get-file-buffer file))
	  (let* (find-file-hooks	;prevent VC this time
		 buffer-read-only
		 enable-local-eval)
	    (set-buffer buffer)
	    (find-alternate-file file)
	    (pop-to-buffer (current-buffer))))

	(when (setq buffer (get-buffer "*VC-log*"))
	  (with-current-buffer buffer
	    ;;  Fix this variable, because we reread the file
	    ;;  see vc-finish-logentry
	    (setq vc-parent-buffer buffer)))


	(message "TinyAdvice: refreshing the file comment ...done")))))



;;; ----------------------------------------------------------------------
;;; AROUND advice has been left to user, therefor the
;;; combination of BEFORE and AFTER advices.
;;;
(defadvice vc-do-command  (before tinyadvice-vc  dis)
  "Set flag `tinyadvice-:vc-p' if file is version controlled.
Used by TinyAdvice after advice to determine if initial
comment leader needs to be set."
  ;;  - The arg 'file' is nil when vc calls this command with
  ;;    "rcs" nil nil "-V". We are not interested in those cases.
  (if (stringp file)
      (setq tinyadvice-:vc-p (vc-registered file))))


;;; ----------------------------------------------------------------------
;;;
(defadvice vc-do-command  (after tinyadvice-vc dis)
  "Set initial RCS comment leader.
According to flag `tinyadvice-:vc-p', if file was not version controlled,
ie. the CheckIn was done first time, ask from user about the initial
comment leader and set it."
  (if (and (stringp file)
	   (null tinyadvice-:vc-p))		;Initial CheckIn
      (tinyadvice-rcs-initial-comment file)))

;;; ----------------------------------------------------------------------
;;;
(defadvice vc-do-command (around tinyadvice-vc dis)
  "TinyAdvice Changes.
Set initial RCS comment leader.
According to flag `tinyadvice-:vc-p', if file was not version controlled,
ie. the CheckIn was done first time, ask from user about the initial
comment leader and set it.

Add flags that user gave in `vc-register' (like -k) for initial login
which preserver keyword values if needed. User must register file with
C-x v i for this to take in effect."
  (let* ((tinyadvice-args   (ad-get-args 6))
	 (tinyadvice-flags  (get 'vc-register 'tinyadvice-vc-register)))
    (when (stringp tinyadvice-flags)
      ;;  Add initial RCS flags that were set in vc-register
      (setq tinyadvice-args
	    (append tinyadvice-args (split-string tinyadvice-flags)))
      (put 'vc-register 'tinyadvice-vc-register nil)
      (when (and (stringp tinyadvice-flags)
		 (string-match "-k" tinyadvice-flags))
	;;  vc add option -u1.1 for initial version, get rid of version number
	(setq tinyadvice-args
	      (remove-if
	       (function
		(lambda (x)
		  (string-match "^-u" x)))
	       tinyadvice-args))
	(push "-u" tinyadvice-args))
      (ad-set-args 6 tinyadvice-args)))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice vc-register (before tinyadvice-vc dis)
  "Ask if check in as \"original\" file if there is already version number.
If the current file already includes version control information,
ask from user if the check in should happen using -k which preserves
the original keyword attributes."
  (put 'vc-register 'tinyadvice-vc-register nil)
  (let* ((file     (buffer-file-name))
	 (version  (and file
			;;  No previous file
			(not (ti::vc-rcs-file-exists-p file))
			(not (ti::vc-cvs-file-exists-p file))
			(ti::vc-rcs-buffer-version)))
	 ans)
    (when (and version
	       (not
		(ti::nil-p
		 (setq ans
		       (read-string
			(format "(TinyAdvice: found v%s) Ci rcs flags: "
				version)
			"-k")))))
      (put 'vc-register 'tinyadvice-vc-register ans))))



;;; ----------------------------------------------------------------------
;;; vc-hooks.el , vc-mode-line (file &optional label) [28..30]
;;;
;;; - The string displayed is put into vc-mode variable
;;; - This function is called by `vc-rcs-status', and the original
;;;   functions doesn't always determine the revision number right.
;;;
(defadvice vc-mode-line (around tinyadvice-vc dis)
  "Add word 'b' if RCS revision is in the middle of the
\(b)ranch and not the last one.

Change to CVS: never make buffer read-only if
`tinyadvice-:cvs-buffer-read-only' is nil."
  (let* ((vc  (if (and file
		       (vc-registered file))
		  (ti::vc-rcs-guess-buffer-version file)))
	 (backend (and vc
		       buffer-file-name
		       (vc-backend buffer-file-name)))
	 ver)

    (when (and vc  ;under version control ?
	       ;; #todo: a stat() call is expensive, should use
	       ;; #todo: VC properties to read the backend information instead
	       (ti::vc-rcs-file-exists-p file)
	       (setq ver (ti::vc-rcs-head-version file))
	       (not (string=  vc ver))) ;it's the highest out
      (ad-set-arg 1
		  (format "%s%s"
			  (or (ad-get-arg 1)
			      (and backend
				   (symbol-name backend))
			      "")
			  "b")))

    ad-do-it

    (when (and vc
	       (null tinyadvice-:cvs-buffer-read-only)
	       (eq 'CVS backend))
      (setq buffer-read-only nil))))



;;; ----------------------------------------------------------------------
;;;
(defadvice vc-print-log (around tinyadvice-vc dis)
  "Position cursor to current revision."
  (let* (ver)
    (setq ver (ti::string-match  "[.0-9]+" 0 (or vc-mode "")))
    ad-do-it
    (when ver
      ;;  the version must end directly,
      ;;  "1.4" must not match "1.4.1.1"
      ;;
      ;;  Watch out for this statement too, thats why we start
      ;;  searching from the end of buffer.
      ;;  revision 3.4.1.2	locked by: foo;
      ;;
      (ti::pmax)
      (re-search-backward (concat "revision +" ver "[^.]") nil t))))

;;; ----------------------------------------------------------------------
;;; 1996-04-06 <pbreton@i-kinetics.com>, improvements by Jari
;;;
;;; - Normally each dir have an RCS dir.
;;; - But sometimes user want to keep all RCS files in one RCS dir,
;;;   so he just creates symlinks to that main RCS dir.
;;;
;;;	      /dir/RCS	    main RCS dir
;;;   dir1/RCS ----| |      Symlink 1
;;;   dir2/RCS ------|	    Symlink 2
;;;
;;;
(defadvice vc-register (before tinyadvice-create-rcs-dir act)
  "RCS directory must exist. Ask to create one if it does not exist."
  (when buffer-file-name		;let's not take a risk
    (let* (
	   ;;  - Make sure we're looking under right directory:
	   ;;  - It is possible that user has given the `cd' command
	   ;;    in this buffer e.g. due to compilation.

	   (default-directory (file-name-directory buffer-file-name))

	   ;;  Strange things may happen. If there is no RCS directory
	   ;;  and you use `ci' then the file appear in _current_
	   ;;  directory with name file.txt,v

	   (false (concat buffer-file-name ",v"))

	   rcs
	   cmd)

      (when (file-exists-p false)
	(message "TinyAdvice: ** Warning Suspicious rcs file %s" false)
	(sit-for 5))

      (when (not (and (file-exists-p "RCS")
		      (file-directory-p "RCS")))

	(setq rcs (ti::file-make-path default-directory "RCS"))

	(message "[press esc] No RCS tree in %s" default-directory)
	(sit-for 7) ;; Make sure user sees the directory name
	(discard-input)

	(if (y-or-n-p
	     (concat
	      "Y = Create new RCS dir"
	      (if (not (win32-p))
		  ", N = create symlink to main depository (unix only)? "
		"")))
	    (make-directory rcs)
	  ;; -- ELSE --
	  (if (win32-p)
	      (error "TinyAdvice: `vc-register' Can't proceed without RCS dir.")
	    (if (not (file-exists-p tinyadvice-:vc-main-rcs-dir))
		(error
		 (format
		  "TinyAdvice: `vc-register' No main RCS dirextory exist: %s"
		  tinyadvice-:vc-main-rcs-dir)))

	    (setq cmd (format "ln -s %s %s"
			      (expand-file-name tinyadvice-:vc-main-rcs-dir)
			      rcs)))

	  (ti::temp-buffer tinyadvice-:tmp-buffer 'clear)
	  (shell-command cmd buffer)

	  (unless (ti::empty-buffer-p tinyadvice-:tmp-buffer)
	    (pop-to-buffer tinyadvice-:tmp-buffer))

	  (message "TinyAdvice: (vc-register) %s"  cmd))))))


;;; ----------------------------------------------------------------------
;;;
(defadvice vc-rcs-status (around tinyadvice-vc dis)
  "Fix this function so that it always set the correct revision."

  ad-do-it

  (let* ((now  (if (and file (vc-registered file))
		   (ti::vc-rcs-guess-buffer-version file)))
	 ver)
    (if (and (stringp now)
	     ;;  VC doesn't agree with me, hmpf ? I may be wrong too,
	     ;;  but you can always turn off this advice
	     ;;  and trust VC :-)
	     (not (string-match now ad-return-value))

	     ;;  Last cmd, used by kill-match
	     (string-match "[0-9]+[.][0-9.]+" ad-return-value))
	(setq ad-return-value (kill-match 0 now ad-return-value)))))



;;}}}

;;{{{ Other

;;; ........................................................... &other ...

;;; ----------------------------------------------------------------------
;;; 1997-11-17 Vladimir Alexiev <vladimir@cs.ualberta.ca> comp.emacs
;;;
(mapc
 (function
  (lambda (x)
    (eval
     (`
      (defadvice (, x) (around tinyadvice-kill-buffer act)
	"Kill the buffer if there is no process."
	(condition-case error
	    ad-do-it
	  (error
	   (if (equal error '(error "Current buffer has no process"))
	       (kill-buffer (current-buffer))))))))))
 '(term-copy-old-input term-send-input term-send-raw-string))


;;; ----------------------------------------------------------------------
;;; 1996-04-29 Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;; hyberbole package
;;;
(defadvice hkey-help-show (around tinyadvice-shrink-window act)
  "Shrink auxiliary windows to buffer size.
For `help-mode',switch `view-mode' off."
  ;;
  ;; hkey-help-show is part of Bob Wiener's Hyperbole. In pure emacs
  ;; a hook is more appropriate: with-output-to-temp-buffer asks the
  ;; function in the variable temp-buffer-show-function (if non-nil)
  ;; to take care of the showing. That function also must call
  ;; temp-buffer-show-hook. Take your pick.
  ;;
  (if (and (not current-window)       ; second arg
           (get-buffer-window buffer))
      (delete-window (get-buffer-window buffer))) ; force recreation
  ad-do-it
  (if (and (not current-window)       ; second arg
           (not (one-window-p t)))    ; not counting the minibuffer
      (shrink-window-if-larger-than-buffer (get-buffer-window buffer)))
  (if (and (eq major-mode 'help-mode)
           (boundp view-mode) view-mode)
      (view-exit)))



;;}}}
;;{{{ extra "my" advices -- disabled

;;; .............................................................. &my ...

;;; ----------------------------------------------------------------------
;;; From: ilya@math.ohio-state.edu (Ilya Zakharevich)
;;; Newsgroups: gnu.emacs.help
;;; Subject: Re: Mapping two commands to a fctn key
;;;
;;; I use the following piece of code (it is 2 days old, and I suspect it
;;; still has some bugs): (After you apply it, the above command should
;;; work, as well as a lot of others.)
;;;
(defun my-define-key-subst-args (name)
  "Used in advice for `define-key' and friends.
NAME should be the symbol name of the \"action\"
argument of the adviced function."
  (let ((def (eval name))
	doc
	list)
    ;; XEmacs 19.14 Bytecomp, shut up "bound but not referenced"
    (if doc (setq doc doc))

    (if (or list
	    (and (listp def) (symbolp (car def))
		 (not (memq (car def) '(lambda keymap))))) ; Extended syntax
	(progn
	  (if (and list (stringp def))
	      (setq doc (list def))	; Process docstring
	    (setq list (cons def list))) ; Now list contains all the actions
	  (eval (list
		 'setq name '(append '(lambda (n raw))
				    doc '((interactive "p\nP")) list)))))))


;;; ----------------------------------------------------------------------
;;; Ilya Zakharevich <ilya@math.ohio-state.edu>
;;;
(defadvice define-key
  (before my-key-extra-args (keymap key def &rest list) dis)
  "Allow DEF to be a list to be evaled when key is pressed.
Free variables `n' and `raw' contain numerical and raw repeat counts. Also
supports syntax
\(define-key KEYMAP KEY DOC DEF1 DEF2 ...)
and
\(define-key KEYMAP KEY DEF1 DEF2 ...).
All the DEFi are evaluated, DOC can contain a documentation string."
  (my-define-key-subst-args 'def))



;;; ----------------------------------------------------------------------
;;; Ilya Zakharevich <ilya@math.ohio-state.edu>
;;;
(defadvice global-set-key
  (before my-key-extra-args (keys my-function &rest list) dis)
  "Allow DEF to be a list to be evaled when key is pressed.
Free variables `n' and `raw' contain numerical and raw repeat counts. Also
supports syntax
\(global-set-key KEYS DOC DEF1 DEF2 ...)
and
\(global-set-key KEYS DEF1 DEF2 ...).
All the DEFi are evaluated, DOC can contain a documentation string."
  (my-define-key-subst-args 'function))



;;; ----------------------------------------------------------------------
;;; Ilya Zakharevich <ilya@math.ohio-state.edu>
;;;
(defadvice local-set-key
  (before my-key-extra-args (keys function &rest list) dis)
  "Allow DEF to be a list to be evaled when key is pressed.
Freevariables `n' and `raw' contain numerical and raw repeat counts. Also
supports syntax
\(local-set-key KEYS DOC DEF1 DEF2 ...)
and
\(local-set-key KEYS DEF1 DEF2 ...).
All the DEFi are evaluated, DOC can contain a documentation string."
  (my-define-key-subst-args 'function))

;;}}}

(provide   'tinyadvice)
(run-hooks 'tinyadvice-load-hook)

;;; tinyadvice.el ends here
