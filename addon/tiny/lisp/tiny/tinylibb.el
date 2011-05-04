;;; @(#) tinylibb.el --- Library of (b)ackward compatible functions.
;;; @(#) $Id: tinylibb.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;;{{{ Id

;; Copyright (C)    1998-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1998-03
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinylibb-version
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

;; ........................................................ &t-install ...
;; DO NOT LOAD THIS FILE, but load the central library "m". It loads this
;; file and autoload library "a"
;;
;;     	(require 'tinylibm)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;	This is lisp function library, package itself does nothing.
;;	This library defines new [X]Emacs release functions for older
;;      [X]Emacs releases.
;;
;;  Code policy
;;
;;	If there has been found a useful macro from someone's .el or from
;;	news, the proper source and name of author + time is preserved. All
;;	the credit goes to those profilic programmers.
;;
;;  Usage
;;
;;	You must not autoload this package; but always include
;;
;;	    (require 'tinylibm)
;;
;;	Yes, there is no typo, you load "m" lib. It will handle arranging
;;      everything for you. This library is included by "m" library
;;      automatically. Repeat: you DO NOT PUT any of these in your
;;      packages:
;;
;;	    (require 'tinylib)
;;	    (require 'tinyliba)
;;	    (require 'tinylibb)
;;	    (require 'tinylibo)
;;	    (require 'tinyliby)
;;
;;	A single statement will arrange everything:
;;
;;	    (require 'tinylibm)
;;
;;  Notes
;;
;;      2000-09-12 <ttn@revel.glug.org> in gnu.emacs.sources
;;      http://www.glug.org/people/ttn/software/ttn-pers-elisp/ reported that:
;;      New file core/veneration.el allows GNU Emacs 19 support.
;;      We define some functions used by this library that are available
;;	in GNU Emacs 20, but not in GNU Emacs 19: `compose-mail' and
;;	minimal supporting functions (see mail-n-news/compose-mail.el),
;;	`shell-command-to-string', and `char-before'. We also redefine
;;	`match-data' to handle arguments.
;;
;;	1998-10 SEMI's poe*el libraries also emulate various Emacs
;;	versions. There should be only one emulation library for all
;;	package writers. It would be best if all backward compatibility
;;	functions were moved either to SEMI or to another common place.


;;}}}

;;; Change Log:

;;; Code:

;;; .......................................................... provide ...

(require 'tinyliba)
(provide 'tinylibb)

;;{{{ code: Emacs compatibility, aliases, byteCompiler


(eval-and-compile
  (defvar temporary-file-directory)
  (autoload 'ti::replace-match "tinylibm"))

;;; ....................................................... &emulation ...

(defun-maybe force-mode-line-update  ()
  ;; XEmacs, labels this obsolete
  ;; In older Emacs it does not exist
  (set-buffer-modified-p (buffer-modified-p)))

(defun-maybe eval-after-load (arg1 form) ;; XEmacs 19.14 doesn't have this
  ;;  "A simple emulation. Eval FORM immediately."
  (load arg1)
  (eval form))

;; Some XEmacs doesn't have 'buffer-flush-undo
(defalias-maybe 'buffer-disable-undo 'buffer-flush-undo)

(defalias-maybe 'number-to-string 'int-to-string)

(defalias-maybe 'set-text-properties 'ignore)

(defalias-maybe 'string-to-number 'string-to-int)


;; Doesn't exist in Emacs
(defalias-maybe 'read-directory-name 'read-file-name)


(defun-maybe make-local-hook (hook) ;; Exists in 19.30+
  ;;  "Make HOOK local to buffer."
  ;; - I need locals so many times it make sme cry, e.g. post-command-hook
  ;; - And why doesn't the add-hook accepts list by default ??
  ;;
  ;; - This aapplies to 19.29.1 and newer
  ;;       (add-hook HOOK FUNCTION &optional APPEND LOCAL)
  ;;       Do not use `make-local-variable' to make a hook
  ;;       variable buffer-local.  Use `make-local-hook'
  ;;       instead.
  ;;
  ;; the variable may be local already, but we do not do
  ;; any checkings
  (make-local-variable hook)
  ;; Copy this because add-hook modifies the list structure.
  (set hook (copy-sequence (eval hook))))


(defun-maybe find-buffer-visiting (file) ;not in XEmacs 19.14
;;  "Find buffer for FILE."
  ;;   file-truename  dies if there is no directory part in the name
  ;;   Check it first
  (or (and (string-match "^/" file)
	   (get-file-buffer (file-truename file)))
      (get-file-buffer file)))



(defun-maybe backward-line (&optional arg)
  (forward-line (if (integerp arg)
		    (- 0 arg)
		  -1)))

(defun-maybe abs (x)
  ;;  "Absolute value of X."
  (if (< x 0)
      (- x)
    x))

;;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...  split  ..



(unless (fboundp 'split-sting)
  (eval-and-compile
    ;; 1995-08-17 original code by Kevin Rodgers <kevinr@ihs.com>
(defun ti::split-string (string &optional regexp level cont-level)
  "If called with only STRING, then split on white space.

Input:

  STRING
  REGEXP	The delimiter in string, Default is '[\\f\\t\\n\\r\\v]+'
  LEVEL		The sub match in REGEXP to end reading substring.
		Default is 0
  CONT-LEVEL	The sub match end to continue reading the STRING.
		Default is 0 (REGEXP match's end point)

Example:

  (split-string \"-I/dir1 -I/dir2\" \" *-I\")
  --> '(\"/dir1\" \"/dir2\")"
  (let ((start 0)
	str
	ret)
    (or regexp
	(setq regexp "[ \f\t\n\r\v]+"))
    (or level
	(setq level 0))
    (or cont-level
	(setq cont-level 0))

    ;;  If no match, return as is '(string)

    (if (null (string-match regexp string ))
	(setq ret (list string))
      (while (string-match regexp string start)
	(setq str (substring string start (match-beginning level)))
	(setq start (match-end cont-level))
	;; Ignore BOL matches. There is no string for us.
	(if (> (match-beginning level) 0)
	    (push str ret)))
      ;;  Try with " test" --> '("test")
      (if (and (> start 0)
	       (< start (length string)))
	  (push (substring string start) ret)))
    (nreverse ret)))))

(defun-maybe split-string (string &optional separators)
  ;; (split-string STRING &optional SEPARATORS)
  ;; in XEmacs 19.14 subr.el
  ;;  "Split string on whitespace."
  (ti::split-string string separators))

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. higher Emacs . .
;;:  Features found from new emacs only 20.xx


;; In simple.el, old Emacs does not have this.
(and (fboundp 'delete-indentation)
     (defalias-maybe 'join-lines 'delete-indentation))


(defun-maybe replace-char-in-string (ch1 ch2 string)
  ;;  "Search CH1, change it with CH2 in STRING."
  (nsubstitute ch1 ch2 string))

;; 1997-10-24 gnu.emacs.help
;; Philippe Schnoebelen <phs@mirabelle.lsv.ens-cachan.fr>
(defun-maybe string-prefix-p (s1 s2)
  ;;  "True if string S1 is a prefix of S2 (i.e. S2 starts with S1)"
  (equal 0 (string-match (regexp-quote s1) s2)))

(put 'with-temp-buffer 'lisp-indent-function 0)
(put 'with-temp-buffer 'edebug-form-spec '(body))
(defmacro-maybe with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (`
     (let (((, temp-buffer)
	    (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
	   (save-excursion
	     (set-buffer (, temp-buffer))
	     (,@ forms))
	 (and (buffer-name (, temp-buffer))
	      (kill-buffer (, temp-buffer))) )))))

;; gnu.emacs.help, William G. Dubuque <wgd@martigny.ai.mit.edu>
;; #note This already exists in some XEmacs

(put 'with-output-to-string 'edebug-form-spec '(body))
(defmacro-maybe with-output-to-string (&rest body) ;XEmacs has this
  "Please use `shell-command-to-string'. Execute BODY and return string."
  (`
   (save-current-buffer
    (set-buffer (get-buffer-create " *string-output*"))
    (setq buffer-read-only nil)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (,@ body))
    (buffer-string))))


;; `save-excursion' is expensive; use `save-current-buffer instead
(put 'save-current-buffer 'edebug-form-spec '(body))
(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
    Executes BODY just like `progn'."
  (` (save-excursion (,@ body))))


(put 'with-current-buffer 'lisp-indent-function 1)
(put 'with-current-buffer 'edebug-form-spec '(body))
(defmacro-maybe with-current-buffer (buffer &rest body)
  "tinylibm.el
Execute the forms in BODY with BUFFER as the current buffer.
    The value returned is the value of the last form in BODY.
    See also `with-current-buffer'."
  (`
   (save-current-buffer
    (set-buffer (, buffer))
    (,@ body))))

;; 2001-03-05 gnu.emacs.help Hannu Koivisto <azure@iki.fi>
(defmacro-maybe with-output-to-file (file &rest body)
  "Open FILE and run BODY.
\(with-output-to-file \"foo\"
  (print '(bar baz)))."
  `(with-temp-file ,file
     (let ((standard-output (current-buffer)))
       ,@body)))



;; Emacs 19.30 and below don't have this

(defun-maybe match-string (level &optional string)
;;  "Read match from buffer at sub match LEVEL. Optionally from STRING.
;;Return nil, if match at LEVEL doesn't exist.
;;
;;You have to call `looking-at' etc. before using this function.
;;You can use use `ti::buffer-match' or `ti::string-match' directly too."
  (if (match-end level)
      (if (stringp string)
	  (substring
	   string
	   (match-beginning level) (match-end level))
	(buffer-substring
	 (match-beginning level) (match-end level)))))


;; Emacs 20.4

;; (replace-regexps-in-string
;;   REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)

;;  (string regexp rep &optional subexp count)
;;
(defun-maybe replace-regexps-in-string
  (regexp rep string &optional fixedcase literal subexp start)
  (let* ((i  0))
    (or subexp
	(setq subexp 0))

    (while (string-match regexp string)
      (if (> (incf i) 5000)
	  (error "Substituted string causes circular match. Loop never ends.")
	(inline (setq string (ti::replace-match subexp rep string)))))
    string))



(defun-maybe buffer-substring-no-properties (beg end)
  (ti::remove-properties (buffer-substring beg end)))

;; 1998-03-30 comp.emacs Dave Love <d.love@dl.ac.uk>
;; Here's the pre-Emacs 20.3 definition.  Note the optional arg.

(defun-maybe match-string-no-properties (num &optional string)
;;   "Return string of text matched by last search, without text properties.
;; NUM specifies which parenthesized expression in the last regexp.
;;  Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
;; Zero means the entire text matched by the whole regexp or whole string.
;; STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
          (let ((result
                 (substring string (match-beginning num) (match-end num))))
            (set-text-properties 0 (length result) nil result)
            result)
        (buffer-substring-no-properties (match-beginning num)
                                        (match-end num)))))



(defun-maybe file-to-string (file &optional oneline args)   ;; This is from pcl-cvs.
  "Read the content of FILE and return it as a string.
If ONELINE is t, only the first line (no \\n) will be returned.
If ARGS is non-nil, the file will be executed with ARGS as its
arguments.  If ARGS is not a list, no argument will be passed."
  (with-temp-buffer
    (condition-case nil
	(progn
	  (if args
	      (apply 'call-process
		     file nil t nil (when (listp args) args))
	    (insert-file-contents file))
	  (buffer-substring (point-min)
			    (if oneline
				(progn (goto-char (point-min))
				       (end-of-line)
				       (point))
			      (point-max))))
      (file-error nil))))

(defun-maybe file-name-extension (filename)
  (ti::file-get-extension filename))

(defun-maybe file-name-sans-extension (filename)
;;  "Return FILENAME without extension."
  (replace-regexps-in-string "\\.[^.]+$" ""  filename))


;; Is this curse or what? Emacs 20.3 invented own function names
;; `line-beginning-position' `line-end-position'  while
;; XEmacs already had had these point-* function names since 1996..
;; `point-at-eol' and `point-at-bol'.

(defsubst-maybe line-beginning-position (&optional n)
  "Return begin position of line forward N."
  (save-excursion
    (if n
	(forward-line n))
    (beginning-of-line) (point)))

(defsubst-maybe line-end-position (&optional n)
  "Return end position of line forward N."
  (save-excursion
    (if n
	(forward-line n))
    (end-of-line) (point)))

(eval-and-compile
  (if (locate-library "executable") ;; 20.4 defines this
      (autoload 'executable-find "executable")
    (defun-maybe executable-find (program-name)
      ;;  "Find PROGRAM-NAME along `exec-path'."
      (ti::file-get-load-path program-name exec-path))))


(defun-maybe executable-find-in-system (program-name) ;Handle Win32 case too.
;;   "Find PROGRAM-NAME along `exec-path'.
;; The PROGRAM-NAME should not contain system dependent prefixes; an
;; .exe is added automatically on PC."
  (if (win32-p)
      (or (executable-find (concat program-name ".exe"))
	  (executable-find (concat program-name ".com"))
	  (executable-find (concat program-name ".bat"))
	  (executable-find (concat program-name ".cmd")))
    (executable-find program-name)))


;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. XEmacs20 char . .

;;  eshell-2.4.1/esh-mode.el fix
;;  We must load this package and ti::xe-character-define-macro
;;  will fix the rest.


(defmacro ti::xe-character-define-macro (function1 function2)
  "Define XEmacs compatible character FUNCTION2 as alias for FUNCTION1."
  (`
   (when (or (not (fboundp (, function1)))
	     (and (emacs-p)
		  (fboundp (, function1))
		  (or (not (equal (symbol-function (, function1))
				  (, function2)))
		      ;;  If the definition is 'ignore, reassign correct
		      ;;  function.
		      (equal (symbol-function (, function1))
			     'ignore))))
     (defalias (, function1) (, function2)))))


(defun ti::xe-char-int-p (ch)		;Not in Emacs (in XEmacs20 MULE)
  (and (integerp ch)
       (> ch -1)			;valid range 0-255
       (< ch 255)))

(defun ti:xe-define-compatibility-defalias ()
  "Emacs and XEmacs compatibility.
Define XEmacs character functions to work in Emacs.
Function mapping are:

  int-to-char      identity
  char-equal       equal
  char-to-int      identity
  chars-in-string  length
  characterp       integerp
  char-int-p       ti::xe-char-int-p
  char-int         identity"
  ;;   Not in Emacs (exist in XEmacs 20)
  ;;  In Emacs chars are ints
  (dolist (elt '((int-to-char identity)
		 (char-equal  equal)
		 ;;  Not in Emacs (exist in XEmacs 20)
		 (char-to-int identity)
		 ;;  Emacs 20.2/20.3 change
		 (chars-in-string length)
		 ;;  exists only in XEmacs
		 (characterp integerp)
		 (char-int-p ti::xe-char-int-p)
		 (char-int   identity)))
    (multiple-value-bind (original alias) elt
      (ti::xe-character-define-macro original alias))))

(ti:xe-define-compatibility-defalias)

(defun-maybe char= (ch1 ch2 &optional ignored-arg) ;exists in  XEmacs 20.1
  (let* (case-fold-search)		;case sensitive
    (char-equal ch1 ch2)))


;;  eshell-2.4.1/esh-mode.el  mistakenly defines characterp
;;  to 'ignore which breaks things
(eval-after-load "esh-mode"
  '(progn (ti:xe-define-compatibility-defalias)))


;; In XEmacs20, you can't use following
;; (memq ch '(?a ?b ?c ?d ?e ?f)), because 'eq' test against
;; characters is wrong.
;;
;; Neither is this format recommended.
;; (memq (char-int ch) (mapcar 'char-int '(?a ?b ?c ?d ?e ?f)))
;;
;; Vladimir Alexiev <vladimir@cs.ualberta.ca> also suggests
;; cl's (member* ch '(?a ?b) :test 'char=)

(defun-maybe char-in-list-case (ch list)
  "If CH can be found in LIST, return a pointer to it.
The Match is case sensitive."
  (when ch
    (let* (case-fold-search)
      (member* ch list :test 'char=))))


;; See cplus-md.el
(defun-maybe count-char-in-string (c s)
  "Count CHARACTER in STRING."
  (let ((count 0)
	(pos   0))
    (while (< pos (length s))
      (if (char= (aref s pos) c)
	  (incf  count))
      (incf  pos))
    count))


(defun-maybe count-char-in-region  (beg end char)
  "In region BEG END, count all CHAR occurrences.
E.g. to have real line count in buffer that
is running folding.el or outline, you should not call
count-lines function , but (count-char-in-region ?\\n)"
  (interactive "r\ncChar: ")
    (let* ((i 0))
      (setq end (max beg end)
	    char (char-to-string char))
      (save-excursion
	(goto-char (min beg end))
	(while (search-forward char end  t)
	  (incf  i)))
      (if (interactive-p)
	  (message "%d hits in region." i))
      i))


(defun-maybe char-assq (ch alist)
  "If CH can be found in ALIST, return entry. If CH is nil, do nothing."
  (let (case-fold-search
	ret)
    (while (and ch alist)
      (setq ret (car alist))
      (if (char= ch (car ret))
	  (setq alist nil)
	(setq alist (cdr alist)
	      ret nil) ))
    ret))


;;  XEmacs : replace-in-string
;;  Emacs 20.4
(defun-maybe subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
INPLACE is ignored."
  (let ((len   (length string))
	(ret   (copy-sequence string)))	;because 'aset' is destructive
    (while (> len 0)
      (if (char= (aref string (1- len)) fromchar)
	  (aset ret (1- len) tochar))
      (decf len))
    ret))


(eval-and-compile
  (when (or (featurep 'xemacs)
	    (boundp 'xemacs-logo))
    ;;   Just a forward declaration, because byte-compiler cannot see through
    ;;   defun-maybe. If this function already exists, this autoload
    ;;   definition is no-op.
    (autoload 'subst-char-in-string "tinylibb.el")))


;; Emacs and XEmacs differ here. Convert Emacs function --> XEmacs name

(defalias-maybe 'shell-command-to-string 'exec-to-string)

;; shell.el, term.el, terminal.el

(unless (boundp 'explicit-shell-file-name)
  (defvar explicit-shell-file-name nil))

(unless (boundp 'shell-command-output-buffer)
  (defvar shell-command-output-buffer "*Shell Command Output*"))


(when (or (not (boundp 'temporary-file-directory))
	  (not (stringp temporary-file-directory))
	  (not (file-directory-p temporary-file-directory)))
  (let* ((temp (or (getenv "TEMP")
		   (getenv "TEMPDIR")
		   (getenv "TMPDIR"))))
    (defvar temporary-file-directory	;Emacs 20.3
      (or temp
	  (cond
	   ((file-directory-p "/tmp") "/tmp")
	   ((file-directory-p "~/tmp") "~/tmp")
	   ((file-directory-p "C:/temp") "C:/temp")
	   ;; don't know what to do, maybe this exists.
	   (t "/")))
      "*Tinylib: XEmacs and Emacs compatibility.")))


;;; ........................................................... &other ...


;; Emacs 20.7 doesn not have this
(defun-maybe turn-off-font-lock ()
  "Turn of font lock."
  (font-lock-mode -1))

(defalias-maybe 'compose-mail 'mail)

(defun-maybe region-active-p  ()		;XEmacs function
  "Return t if mark (region) is active."
  (cond
   ((and (xemacs-p)
	 (boundp 'zmacs-regions))
    (let* ((zmacs-regions t))        ;XEmacs
      (mark)))
   ((boundp 'mark-active)
    (symbol-value 'mark-active))))          ;Emacs

;; Newer advice "2.15" uses this call, make sure it exist.
(defalias-maybe 'byte-code-function-p 'ignore)

(defun-maybe add-to-list (list-var element)
;;  "Add to symbol LIST-VAR ELEMENT."
  (or (member element (symbol-value list-var))  ;; copy from 19.34
      (set list-var (cons element (symbol-value list-var)))))



(defun-maybe run-hook-with-args-until-success
  (hook-sym &optional &rest args)
;;   "Run all functions in HOOK-SYM. Stop when first one return non-nil.
;;
;; Input:
;;
;;   HOOK-SYM  hook symbol, or list of functions.
;;   ARGS	    arguments to functions. if NIL, functions
;; 	       are called without arguments."
  (let* ((val  (symbol-value hook-sym))
	 (list (if (listp val) val (list val))) ;Make list maybe
	 ret
	 func)
    (while (and (null ret) list)
      (setq func (car list)   list (cdr list))
      (setq ret (apply func args)))
    ret))


(defun-maybe buffer-live-p (buffer)
;;  "Check if BUFFER exist."
  (cond
   ((not (bufferp buffer))
    (error "must be pointer"))
   ((stringp buffer)
    (get-buffer buffer))
   (buffer
    (buffer-name buffer))))

(eval-when-compile
  ;;  don't show "obsolete function warning", because we know what
  ;;  we're doing below.
  (put 'frame-parameters 'byte-compile nil))

(when (not (fboundp 'frame-parameter))  ;Emacs 19.35
  (if (fboundp 'frame-property)
      (defalias 'frame-parameter 'frame-property) ; XEmacs.
    (defun frame-parameter (frame property &optional default)
      "Return FRAME's value for property PROPERTY."
      (or (cdr (assq property (frame-parameters frame)))
	  default))))

(unless (and (fboundp 'find-file-binary)  ;; Emacs function --> XEmacs
	     (boundp 'buffer-file-coding-system))
  (defun find-file-binary (file)
    "Read FILE without conversiosn."
    (let* ((buffer-file-coding-system 'binary))
      (unless buffer-file-coding-system
	(setq buffer-file-coding-system nil)) ;Quiet Bytecompiler "unused  var".
      (find-file file))))

;;}}}
;;{{{ special

;;; ........................................... &compatibility-special ...
;;; These need emacs-p xemacs-p tests

;; not known function in 19.14

(eval-and-compile
  (autoload 'read-kbd-macro "edmacro")
  (when (emacs-p)
    (or (fboundp 'kbd)			;Std in Emacs 20.x
	(defmacro kbd (keys)		;(kbd "C-<delete>")
	  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `insert-kbd-macro')."
	  (let ((f 'read-kbd-macro))
	    (funcall f keys))))))


;;}}}
;;{{{ code: function test

;;; ...................................................... &func-tests ...
;;; We define these here because they are used lated in this library
;;; "define before using"

(eval-and-compile

;;; ----------------------------------------------------------------------
;;;
(defun ti::function-car-test (symbol test-val &optional test-func)
  "Test car of the SYMBOL against TEST-VAL with TEST-FUNC.
Function must be symbol, not a lambda form.

Return:

  symbol      yes, test succeeded
  nil         test failed"
  (if (and (not (sequencep symbol)) ;; list ?
	     (symbolp symbol)	  ;; chokes if not sequencep
	     (fboundp symbol)

	     ;;  Eg. symbol-function 'car  doesn't return list.
	     ;;
	     (listp (symbol-function symbol))
	     (eq test-val
		 (funcall (or test-func 'car)
		  (symbol-function symbol))))
      symbol
    nil))

;;; ----------------------------------------------------------------------
;;; `indirect-function' unfortunately returns the symbol-function, not
;;; the symbol name of the last function in the chain
;;;
(defun ti::defalias-p (symbol)
  "If function SYMBOL is alias, return it's truename. Otw Return nil."
  (let* (sym
	 prev
	 ret)

    (if (or (sequencep symbol)		;lambda form ?
	    (not (symbolp symbol))
	    (not (fboundp symbol)))
	nil
      (setq sym (symbol-function symbol))
      (if (not (symbolp sym))
	  nil
	(while (and (symbolp sym)	;was alias, go into nesting levels
		    (fboundp sym))	;must be function or user made mistake
	  (setq prev sym)
	  (setq sym (symbol-function sym)))
	(setq ret prev)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::defmacro-p (symbol)
  "Test if function SYMBOL is in fact macro, created with defmacro.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  (ti::function-car-test symbol 'macro))

;;; ----------------------------------------------------------------------
;;;
(defun ti::autoload-p (symbol)
  "Test if function SYMBOL is in its autoload form.
Works with aliased symbols too.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  ;;  Get the REAL name if it is alias or use the func's SYMBOL name
  (let* ((func (or (ti::defalias-p symbol) symbol)))
    (ti::function-car-test func 'autoload)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::lambda-p (symbol)
  "Test if function SYMBOL was created with defsubst or is in lambda form.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  (ti::function-car-test symbol 'lambda))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe functionp (obj)   ;; Emacs 20.3+ XEmacs 20.x
  (or (subrp obj)
      (byte-code-function-p obj)
      (and (symbolp obj) (fboundp obj))
      (and (consp obj)   (eq (car obj) 'lambda))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::function-args-p (symbol)
  "Return function SYMBOL argument list string or nil.
Works for byte compiled functions too.

Notes:
  if function is alias, the real function behind it is examined.
  if function is in autoload state, \"(autoload-args)\" is returned."
  (let* ((args-re "[^(]+\\([^)]+)\\)")
	 sym-func
	 str
	 ret)
    (if (ti::autoload-p symbol)
	;;  We can't know the args. And we don't want to find out,
	;;  since it would load the package unnecessarily
	(setq ret "(autoload-args)")

      (setq sym-func
	    (symbol-function
	     (or (ti::defalias-p symbol) symbol)))

      (if (subrp sym-func)
	  (setq str (documentation sym-func))
	(setq str (prin1-to-string sym-func)))

      (cond
       ((emacs-p)
	(if (or (string-match "^(lambda[ \t]+nil" str)
		(string-match "^#\\[nil" str)
		(and (string-match args-re str)
		     (setq ret (match-string 1 str))
		     ;;  Empty arg list
		     (string= ret "")))
	    (setq ret nil)))
       (t
	;;  XEmacs has different Byte compilation format
	;;  #<compiled-function (from "custom.elc") nil "...(7)
	(cond
	 ((string-match
	   (concat "compiled-function +\(from.*\) +" args-re) str)
	  (setq ret (match-string 2)))
	 ((string-match "^(lambda +nil" str)) ;bypass
	 ((and (string-match args-re str)
	       (setq ret (match-string 1 str))))))))
    ret))


;;; --++-- --++-- --++-- --++-- --++-- --++-- --++--  eval-and-compile --
)

;;}}}

;;; ........................................................... cygwin ...


;;; Patch for these functions has been submitted to Emacs 21.2
;;; (w32-fns.el)

(defvar w32-cygwin-mount-table nil
  "Cygwin mount.exe mapping. See `w32-cygwin-mount-table'.")



;;; ----------------------------------------------------------------------
;;;
(put 'w32-cygwin-mount-table-dolist 'lisp-indent-function 0)
(put 'w32-cygwin-mount-table-dolist 'edebug-form-spec '(body))  ;;#todo: not working
(defmacro w32-cygwin-mount-table-dolist (&rest body)
  "Run DOLIST for Cygwin mount table.
`mount' is complete mount element (cygwin . dos).
Variables `cygwin' and `dos' are bound respectively."
  (`
   (dolist (mount w32-cygwin-mount-table)
     ;;  mount => ("/tmp" . "c:\\temp")
     (let* ((cygwin (car mount))
	    (dos    (cdr mount)))
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-parse ()
  ;; "Parse cygwin mount table from current point forward."

  ;;  Search lines with backslash
  ;;  f:\\u\\bin /usr/bin user binmode
  ;;
  ;;  Cygwin 1.3.3 changed format, it is now
  ;;
  ;;  f:\\u\\bin on /usr/bin type user (binmode)
  ;;             ==

  (let (list
	(regexp
	 (save-excursion
	   (if (re-search-forward "^[a-z]:.* on " nil t)
	       (concat
		"^\\([a-zA-Z]:[\\][^ \t\r\n]*\\|[a-zA-Z]:\\)"
		"[ \t]+on[ \t]+"
		"\\(/[^ \t\r\n]*\\)")
	     (concat
	      "^\\([a-zA-Z]:[\\][^ \t\r\n]*\\|[a-zA-Z]:\\)"
	      "[ \t]+"
	      "\\(/[^ \t\r\n]*\\)")))))
    (while (re-search-forward regexp nil t)
      (let ((dos    (match-string 2))
	    (cygwin (match-string 1)))
	(push (cons dos cygwin)
	      list)))

    ;;  sort the entries so that the longest mounts come first and
    ;;  last the shortest. This makes a difference when Cygwin paths are
    ;;  converted back to dos:
    ;;
    ;;    /tmp/other       mapping must be handled before /tmp
    ;;    /tmp
    ;;    ..

    (sort list
	  (function
	   (lambda (a b)
	     (> (length (car a))
		(length (car b))))))))


;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-convert (path &optional flag)
  "Run `cygpath' to find out PATH.
Return:

   The default concersion is CYGWIN => DOS

   If `flag' is set, then the conversion is
   DOS => cygwin."
  (let* ((cmd     (executable-find "cygpath"))
	 (option  "--windows")
	 ret)
    (when cmd
      (when flag
	(setq option "--unix"))
      (with-temp-buffer
	(call-process
	 cmd
	 nil
	 (current-buffer)
	 nil
	 option
	 path)
	(goto-char (point-min))
	(when (looking-at "^.*") ;; Filter newlines
	  (setq ret (match-string 0)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table ()
  ;; "Return Cygwin mount table '((CYGWIN . DOS) ..) using `mount' command."
  (when ;; (memq system-type '(ms-dos windows-nt))
      (win32-p)
    ;; specifically request the .exe which must be along PATH
    ;; if we used only `mount', that could call user's "mount.bat" or
    ;; something.
    (let ((cmd  (executable-find "mount.exe")))
      (when cmd
	(with-temp-buffer
	  (call-process cmd nil (current-buffer))
	  (goto-char (point-min))

	  ;;  It's a serious error if "mount" does not say where
	  ;;  the ROOT "/" is. Should we do somethng?

	  (goto-char (point-min))
	  (let ((ret (w32-cygwin-mount-table-parse)))
	    (unless ret
	      (error "Cygwin mount.exe output parse failed:\n[%s]"
		     (buffer-string)))
	    ret))))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-point-to-dos (path)
  "Convert Cygwin mount filenames like  /tmp to DOS paths."
  (let* (last-choice
	 try)
    (dolist (cygwin w32-cygwin-mount-table)
      (when (string-match (concat "^"  (car cygwin) "\\(.*\\)")
			  path)
	(setq try
	      ;;  expand will ensure that slashes are after glue
	      ;;  to the same direction
	      (expand-file-name
	       (concat (file-name-as-directory (cdr cygwin) )
		       (match-string 1 path))))
	;;  It is difficult to expand the file name correctly because
	;;  user can make any mount points. That's what we compare which
	;;  mount point gives the longest match and return it.
	;;
	;;  E.g. the root / will always match, but it is not necessarily
	;;  the final answer given path /tmp/something where there is
	;;  separate mount point for longer match /tmp
	;;
	(if (null last-choice)
	    (setq last-choice (cons (car cygwin) try))
	  (if (length (> (car cygwin) (car last-choice)))
	      (setq last-choice (cons (car cygwin) try))))))
    (if (null last-choice)
	path
      (cdr last-choice))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-set ()
  ;;   "Run mount.exe and set internal variable `w32-cygwin-mount-table'.
  ;; You should run this function after you have made a change to
  ;; cygwin mount points."
  ;;   (interactive)
  (if (win32-p) ;; (memq system-type '(ms-dos windows-nt))
      (setq w32-cygwin-mount-table
	    (w32-cygwin-mount-table))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-path-to-dos (path)
  "Convert cygwin like //c/temp  or /cygdrive/c/temp path to
  dos notation c:/temp."
  ;; NOTE for cygwin and bash shell prompt
  ;; We can't require a slash after the drive letter, because
  ;; //c   and  /cygdrive/c   are all top level roots.
  ;;
  ;; The bash shell's PS1 setting \w (The current working directory)
  ;; Does not add trailing slash.
  (cond
   ((or (string-match "^//\\([a-z]\\)/?$" path)
	(string-match "^/cygdrive/\\([a-z]\\)/?$" path))
    (concat (match-string 1 path) ":/"))
   ((or (string-match "^//\\([a-z]\\)\\(/.*\\)" path)
	(string-match "^/cygdrive/\\([a-z]\\)\\(/.*\\)" path))
    (concat (match-string 1 path) ":" (match-string 2 path)))
   ((string-match "^(/cygdrive/./\\|//" path)
    ;;  if previous regexps couldn't handle it, this is severe error.
    (error "Invalid path format for cygwin %s" path))
   ((string-match "[\\]" path)
    (error "Invalid backslash path %s" path))
   ((string-match "^/" path)
    ;;  Convert Cygwin /usr/local to DOS path. LOCATION/usr/local.
    ;;  This relies on the fact that the longest paths are first
    ;;  in the mount table.
    (w32-cygwin-mount-table-dolist
      ;;  mount => ("/tmp" . "c:\\temp")
      ;;  variables `cygwin' and `dos' are part of the macro
      (when (string-match (concat "^" cygwin "\\(.*\\)") path)
	(unless (string= cygwin "/")
	  (setq dos (concat dos (match-string 1 path))))
	;; Convert to forward slashes
	(return (subst-char-in-string ?\\ ?/ dos)))))
   (t
    path)))


;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-dos-path-to-cygwin (path)
  "Convert dos PATH to cygwin path.
Be sure to call `expand-file-name' before you pass PATH to the function."
  (cond
   ((string-match "\\([a-z]\\):[\\/]\\(.*\\)" path)
    (let ((drive     (format  "/cygdrive/%s/" (match-string 1 path)))
	  (rest-path (match-string 2 path)))
      (if (not rest-path)
	  drive
	(w32-cygwin-mount-table-dolist
	  ;;  mount => ("/tmp" . "c:\\temp")
	  ;;  variables `cygwin' and `dos' are part of the macro
	  (when (or (string-match (concat "^" dos "\\(.*\\)") path)
		    (string-match (concat "^"
					  ;; Convert to / slashes
					  (expand-file-name dos)
					  "\\(.*\\)") path))
	    (when (match-string 1 path)
	      (setq path (match-string 1 path))
	      (setq cygwin (concat cygwin path)))
	    ;; Convert to forward slashes
	    (return (subst-char-in-string ?\\ ?/ cygwin)))))))
   (t
    (error "Cannot convert to cygwin. path is not absolute %s" path))))

;;  Make it defconst, so that rereading tinylibb.el will always update
;;  the value. If Cygwin is changed, reloading this library.

(setq w32-cygwin-mount-table
      (if (win32-p) ;; (memq system-type '(ms-dos windows-nt))
	  (w32-cygwin-mount-table)))



;;; ########################################################## &custom ###

;;{{{ custom

;;; ----------------------------------------------------------------------
;;; - Oh my, this custom stuff has made my hair turn grey...
;;; - Instead of writing defvar, you can use 19.35's defcustom
;;;   to let user to run M-x customize RET xxx RET. Example:
;;;
;;; (defcustom xxx-value 6
;;;   "*Integer defining interval between..."
;;;    :type 'integer
;;;    :group 'strokes)
;;;
;;; Originally by By Bill Perry; Email address unknown,
;;; idea completely rewritten by [jari]
;;;
;;; 2000-02-17
;;;
;;;	This code is beginning to be obsolete now when everybody Has
;;;	custom.el. Suppresses the internal-info message which checked that
;;;	correct custom version was around.
;;;
;;;	This code is no-op if custom is present, so I let it be here.

(eval-and-compile
  (cond
   ((string-match "2[0-9]\\." (emacs-version))
    (require 'custom))			;Out of the box
   (t					;Arrgh, cope with old Emacs

  (let* ((list  load-path)
	 dir
	 try
	 path)
  (cond

   ;; ..................................................... no custom ...
   ;;  The reason why newest custom.el does not work in prior releases it the
   ;;  new bacquote macro syntax it uses. It needs new emacs lisp parser to
   ;;  read the macros.

   ((or (and (emacs-p)
	     (< emacs-minor-version  34))
	(and (eq 19 (xemacs-p))
	     (< emacs-minor-version  15)))

    ;;  This emacs is too old for new custom. Emulate it.

    (defmacro defgroup (&rest args) nil)

    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc)))))

   ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. custom maybe . .

   (t

    ;; Explanation: When I say (require 'custom) in -batch byte
    ;; compile; and the load-path HAD my private ~/elisp at front,
    ;; but it still loaded old custom.elc from XEmacs 19.14 distribution.
    ;;
    ;; Why? Don't know. That's why I want to load it manually here.

    (while (and (null path)		;Where it is?
		(setq dir (car list)))
      (setq try
	    (if (string-match "/$" dir)
		(concat dir "custom.el")
	      (concat dir "/custom.el")))

      (when (file-exists-p try)
;;;	(message (format "tinylibm: ** Using custom from [%s]" try))
	(setq path (file-name-directory try)))
      (setq list (cdr list)))

    ;; ............................................... load new custom ...

    (condition-case ()
	(progn

	  ;; The new custom won't work in .el format, it must be
	  ;; loaded in .elc format.

	  (unless (featurep 'custom)
	    (load (concat path "custom.elc"))))
      (error
       (message "tinylibm: ** Couldn't find custom.elc [compiled]")))

    (message "tinylibm: ** internal info: Custom [%s] declare [%s]"
	     (if (featurep 'custom) "t" "nil")
	     (if (fboundp 'custom-declare-variable) "t" "nil"))
    (cond
     ((and (featurep 'custom) (fboundp 'custom-declare-variable))

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . case 1 ..
      ;;  19.14 has very old custom.el, and it shouldn't be used any more.
      ;;  This is funny, different custom version, ahhh
      ;;
      ;;  custom-XE19.14    : custom.el::customize()
      ;;  custom-1.96	    : cus-edit.el::(defun customize (symbol)
      ;;  custom-1.9956	    : cus-edit.el:::customize()
      ;;		       cus-edit.el::customize-group (group)

      (cond
       ((and (null (ti::function-args-p 'customize))
	     (not (fboundp 'customize-group)))
	(message "\
tinylibm.el: ** [Ignore, Compilation is still going fine...]
	     ** Hm, loading custom didn't go quite right. Reasons:
	     ** a. You have too old custom.el library, because I can't
	     **    see `customize' function to take ONE argument.
	     **    Be sure to have new cus-edit.el too.
	     ** b. Your load-path is set so that the old custom.el
	     **    was laoded.
	     ** 27 May 1997 http://www.dina.kvl.dk/~abraham/custom/"))
       (t

	;;  The new 1.9956 Custom.el produces warning for defcustom
	;;  variables not beeing defined. This code is only for
	;;  19.34 and won't wortk anywhere else.

	(if (string-match
	     "19.2[0-9]\\|19.3[0-3]\\|19.1[0-4]"
	     (emacs-version))
            (message "\
             ** ...But you don't have [X]Emacs 19.34, 19.15, or 20+
             ** That's why you see lot of undefined variables.
             ** It's a byte compiler issue, nothing to worry about.")

	  ;; This is part of bytecomp.el in 20.1:

	  (put 'custom-declare-variable 'byte-hunk-handler
	       'byte-compile-file-form-custom-declare-variable)

	  (defun byte-compile-file-form-custom-declare-variable (form)
	    (if (memq 'free-vars byte-compile-warnings)
		(setq byte-compile-bound-variables
		      (cons (nth 1 (nth 1 form))
			    byte-compile-bound-variables))) form))))

      nil)
     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . case 2 ..
     (t

      ;;  Useer has new Emacs! Print message that he should get new
      ;;  custom libs.

      (unless (string-match "19.2[0-9]\\|19.3[0-3]\\|19.1[0-4]"
			    (emacs-version))
	(message "\
tinylibm.el: ** Old custom; visit http://www.dina.kvl.dk/~abraham/custom"))

      ;; We have the old custom-library, hack around it!

      (defmacro defgroup (&rest args) nil)

      (defmacro defcustom (var value doc &rest args)
	(` (defvar (, var) (, value) (, doc))))))))))))

;;}}}

;;; ################################################### &byte-optimize ###

;;{{{ misc

(when (and nil				;Disabled now
	   (null (get 'concat 'byte-optimizer)))
  (put  'concat 'byte-optimizer 'tinylibb-byte-optimize-concat)

  ;; 1997-10-30 By Ray Nickson <nickson@MCS.VUW.AC.NZ> in private mail
  ;;
  ;; Like `concat', but this macro expands to optimized form.
  ;; Many times you want to divide complex regexps on separate lines like
  ;; this
  ;; 	(looking-at (concat
  ;; 		      ;; Comment
  ;; 		      \"regexp-1\"
  ;; 		      ;; Comment
  ;; 		      \"regexp-2\"
  ;; 		      ))
  ;;
  ;; This is perfectly good way, but won't be optimized in any way:
  ;; The compiled version contains `concat' command and separate strings.
  ;;
  ;; This optimized `concat' macro will expand the ARGS to single string
  ;; "regexp-1regexp-2\ if they all are strings.
  ;; In other cases it expands to normal `concat' call.
  ;;
  ;;   (defmacro concat-macro (&rest args)
  ;; 	 (if (every 'stringp args)
  ;; 	     (apply 'concat args)
  ;; 	   (cons 'concat args)))
  ;;

  ;; Better yet, <kevinr@airedale.ihs.com> (Kevin Rodgers) suggested
  ;; 1997-10-29 in gnu.emacs.bug

  (defun tinylibb-byte-optimize-concat (form)
    (let ((args (cdr form))
	  (constant t))
      (while (and args constant)
	(or (byte-compile-constp (car args))
	    ;;  Stop there
	    (setq constant nil))
	(setq args (cdr args)))

      (if constant
	  (eval form)
	form))))

;;}}}
;;{{{ Version

;;; ......................................................... &version ...

(defconst tinylibb-version
  (substring "$Revision: 1.1 $" 11 15)
  "Latest version number.")

(defconst tinylibb-version-id
  "$Id: tinylibb.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
  "Latest modification time and version number.")


;;; ----------------------------------------------------------------------
;;;
(defun tinylibb-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylibb.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylibb-submit-bug-report ()
  "Submit bug report."
  (interactive)
  (ti::package-submit-bug-report
   "tinylibb.el"
   tinylibb-version-id
   '(tinylibb-version-id)))


;;}}}

;;; tinylibb.el ends here
