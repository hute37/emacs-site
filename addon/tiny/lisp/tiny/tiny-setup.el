;;; @(#) tiny-setup.el --- Tiny Tools configure center.
;;; @(#) $Id: tiny-setup.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    2001-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         2001-03
;; Keywords:        extensions

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

;; Nothing to install. Load this file.

;;}}}

;;{{{ Documentation

;;; Commentary:

;;  Preface, overview of options
;;
;;	This file will configure all Tiny Tool files. The alternative method
;;	is to look into each package individually and to follow instructions
;;	there to set up the files.
;;
;;	To use this file, see control function `tinypath-setup' for
;;      full description like this:
;;
;;	    M-x RET load-library RET tiny-setup RET
;;	    C-h f tinypath-setup
;;          M-x tinypath-setup-display
;;
;;          M-x tiny-setup RET                       Default 'all setup
;;
;;       To setup all tools from $HOME/.emacs, use:
;;
;;          (load "~/path/to/tinypath.el")   ;; Emacs autosetup, SEE THIS!
;;          (require 'tiny-setup)            ;; control center
;;          (tiny-setup 'all)                ;; configure all at once.
;;
;;  Administration
;;
;;      This part should concern the maintainer only.
;;
;;     Autoload files
;;
;;      If *loaddef* files were not included in the package or if they were
;;      mistakenly deleted. The tiny-setup.el startup is not possible
;;      without the autoload files.
;;
;;      To generate autoloads recursively, call function
;;      `tiny-setup-autoload-batch-update' with the ROOT
;;      directory of your lisp files. The only requirement is that each
;;      directory name is unique, because the generated autoload file name
;;      contains directory name: *tiny-autoload-loaddefs-DIRNAME.el*
;;
;;     Compilation check
;;
;;      To check for possible leaks in code, ran the byte compilation
;;      function from shell by using XEmacs compiler. The Emacs byte
;;      compiler is not that good in findings all errors.
;;      See function `tiny-setup-compile-kit-all'.
;;
;;     Profiling
;;
;;	To check how much time each file load would take, see function
;;      `tiny-setup-test-load-time-libraries'. Here are results as of
;;      2001-03-18 running Win9x/512Meg/400Mhz, Emacs 20.7
;;
;;	    Timing tinyliba,  took     2.025000 secs (autoloads)
;;          Timing tinylibb,  took     0.011000 secs
;;          Timing tinylibm,  took     0.977000 secs
;;          Timing tinylib,   took     0.982000 secs
;;          Timing tinylibxe, took     0.000000 secs
;;          Timing tinylibid, took     0.006000 secs
;;          Timing tinylibo,  took     0.005000 secs
;;          Timing tinylibt,  took     0.011000 secs
;;          total time is 4.027999997138977 seconds

;;}}}

;;; Change Log:

;;; Code:


(eval-when-compile
  (require 'cl))

(require 'tinyliba)

(eval-and-compile
  (autoload 'byte-compile-file                  "bytecomp")

  (autoload 'turn-on-tinylisp-mode              "tinylisp")
  (autoload 'ti::mail-mailbox-p                 "tinylibmail")
  (autoload 'turn-on-tinymailbox-mode           "tinymailbox")
  (autoload 'folding-install-hooks              "folding")
  (autoload 'turn-on-folding-mode               "folding")
  (autoload 'dired-sort-default-keys            "dired-sort")
  (autoload 'tinymy-define-keys-extra		"tinymy")
  (autoload 'tinymy-define-keys			"tinymy")
  (autoload 'turn-on-tinyef-mode		"tinyef")
  (autoload 'turn-on-tinypair-mode		"tinypair")
  (autoload 'turn-off-tinypair-mode		"tinypair")
  (autoload 'turn-on-tinyperl-mode-all-buffers	"tinyperl")
  (autoload 'tinyrmail-install			"tinyrmail")
  (autoload 'turn-on-tinycompile-mode           "tinycompile")
  (autoload 'tinytag-install-sample-databases   "tinytag")
  (autoload 'turn-on-tinytf-mode		"tinytf")
  (autoload 'turn-on-tinyurl-mode		"tinyurl"))


(defvar tiny-setup-load-hook nil
  "*Hook run when package is loaded.")

(defconst tiny-setup-:library-compile-order
  '("tinyliba.el"
    "tinylibm.el"
   "tinylibb.el")
  "Order which the libraries should be compiled.
This variable is list of REGEXPS.")

(defconst tiny-setup-:library-compile-exclude
  '("tinylib-ad.el")  ;; adviced functions
  "Libraries not to compile.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	SETUP CHOICES
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Get file descriptions with this bash script:
;;  head -1 $(ls *el | sort) |grep ';;'

(defconst tiny-setup-:option-table
  '(("dired-sort"
     ("Dired sort package. See `s' command in dired.")
     ("autoload"))

    ("folding"
     ("Folding content management package. Detect {{{ and }}}.")
     ("autoload"))

    ("tinyadvice"
     "Collection of advised functions."
     ("load"))

    ("tinyappend"
     "A simple text gathering to buffer utility."
     ("bind"))

    ("tinybookmark"
     "Keep file in organized sections."
     ("defalias" "bind"))

    ("tinybuffer"
     "Change buffers in current window."
     ("bind" "bindforce"))

    ("tinycache"
     "Maintain a cache of visited files [compile, dired]."
     ("activate"))

    ("tinychist"
     "Command history save/restore utility."
     ())  ;;#todo:

    ("tinycomment"
     "Smart comment setting utility."
     ("autoload" "bind"))

    ("tinycompile"
     "Compile buffer additions. Minor mode."
     ("autoload"))

    ("tinydesk"
     "Save and restore files between Emacs sessions."
     ("activate" "bind" "bindforce"))

    ("tinydiff"
     "Diff and patch minor mode. Browsing, patching."
     ("autoload" "bind" "bindforce"))

    ("tinydired"
     "Dired enhancements. Background Ange ftp support."
     ("autoload"))

    ("tinyeat"
     "Eat blocks of text at point, forward and backward."
     ("bind" "bindforce"))

    ("tinyef"
     "(E)lectric (f)ile minor mode. Easy C-x C-f filename composing."
     ("autoload"))

    ("tinygnus"
     "Gnus Plug-in. Additional functions. UBE fight etc."
     ("autoload"))

    ("tinyhotlist"
     "Hot-list of important buffers, files(ange-ftp), dired."
     ("autoload" "bind" "bindforce" "bindmouse"  "bindmouseforce"))

    ("tinyigrep"
     "Top level interface to igrep.el."
     ("autoload" "bind" "bindforce"))

    ("tinyindent"
     " Like indented-text-mode, but minor-mode."
     ("bind" "bindforce"))

    ;;  there is nothing to setup in libraries. These are already
    ;;  autoloaded in tinyliba.el

    ("tinylib-ad"
     "Library of advised functions. Backward compatibility."
     ())
    ("tinylib"
     "Library of general functions."
     ())
    ("tinyliba"
     "Library for (a)utoload definitions."
     ())
    ("tinylibb"
     "Library of (b)ackward compatible functions."
     ())
    ("tinylibck"
     "Library to (c)onvert (k)eybindings for XEmacs or Emacs."
     ())
    ("tinylibid"
     "Library for (Id)entifying buffer, regardless of mode."
     ())
    ("tinylibm"
     "Library of s(m)all macros or functions."
     ())
    ("tinylibmenu"
     "Library for echo-area menu."
     ())
    ("tinylibmail"
     "Library of (m)ail and news (t)ool functions."
     ())
    ("tinylibo"
     "Library for handling (o)verlays."
     ())
    ("tinylibt"
     "Library for handling text properties."
     ())
    ("tinylibxe"
     "Library for Emacs and XEmacs emulation."
     ())
    ("tinyliby"
     "Library of functions related to Emacs s(y)stem."
     ("defalias"))


    ("tinylisp"
     "Emacs lisp programming help grab-bag."
     ("autoload" "activate"))

    ("tinyload"
     "Load set of packages when Emacs is idle (lazy load)."
     ())

    ;;  This asks lock password at startup, can't define "load" option
    ;;  for this for unattended load.

    ("tinylock"
     "Simple emacs locking utility."
     ())  ;;#todo:

    ("tinylpr"
     "Easy Emacs lpr command handling, pop-up, completions."
     ("bind"))

    ("tinymacro"
     "Fast way to assign newly created macro to a key."
     ("bind" "bindemacs"))

    ("tinymail"
     "Mail add-ons. Report incoming mail, passwd, BBDB complete."
     ("autoload"))

    ("tinymailbox"
     "Berkeley style aka std. mailbox browsing minor mode."
     ("autoload"))

    ("tinymy"
     "Collection of user ('my') functions. Simple solutions."
     ("load" "bind" "bindextra" "defalias" "dosdisplay"))

    ("tinynbr"
     "Number conversion minor mode oct/bin/hex."
     ()) ;; Already autoloaded. M-x turn-on-tinynbr-mode

    ("tinypad"
     "Emulate Windows notepad with extra menu."
     ()) ;;#todo:

    ("tinypage"
     "Handling ^L pages, select, cut, copy, head renumber."
     ("bind"))

    ("tinypair"
     "Self insert character (pa)irs () \"\" '' <>."
     ("bind"))

    ;; Please see the documentation in this file
    ;; LOAD tinypath.el AS VERY FIRST PACKAGE. Before even tiny-setup.pl

    ("tinypath"
     "Manage Emacs startup dynamically."
     ())

    ("tinyperl"
     "Grab-bag of Perl language utilities. Pod documentation browser."
     ("autoload"))

    ("tinypgp"
     "PGP minor mode, remailing, keyring management."
     ())

    ("tinyprocmail"
     "Emacs procmail minor mode. Lint code checker."
     ("autoload"))

    ("tinyreplace"
     "Handy query-replace, area, case preserve, words."
     ("bind" "bindemacs"))

    ("tinyrmail"
     "RMAIL add-ons, pgp, mime labels, Spam complaint."
     ("autoload"))

    ("tinyscroll"
     "Enable or Disable auto-scroll for any buffer."
     ("autoload"))

    ("tinysearch"
     "Grab and search word under cursor."
     ("bind" "bindforce" "bindmousealt" "bindmousemeta"))

    ("tinytab"
     "Programmers TAB minor mode. Very flexible."
     ("bind" "bindforce" "bindextra"))

    ("tinytag"
     "Coding help: e.g. show C++ syntax call while coding."
     ("autoload"))

    ("tinytf"
     "Document layout tool for '(T)echnical text (F)ormat."
     ("autoload"))

    ("tinyurl"
     "Mark and jump to any URL on current line. C/C++/Perl/Elisp."
     ("autoload" "bind"))

    ("tinyvc"
     "CVS and RCS log minor mode. Checout, Check-in."
     ("autoload"))

    ("tinyxreg"
     "Restore points/win cfg stored in reg. via X-popup."
     ("bind")))
  "Packages and options.
Format is:
 '((PACKAGE ((OPTION-STR ..) ..))).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	CONFIGURE SETUP FOR ALL FILES
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tiny-setup (&optional type option-list)
  "Tiny Tools setup controller. See Message buffer for results.

Please make sure you have run the makefile.pl with build option
\"all\" or \"autoload\". You can verify this by finding files which
contain word \"loaddefs\".

Autoload statements are always defined when this function is called,
so even if you do not define any options to be installed, they will be
available in callable functions that trigger loading packages. This
means, that you an call e.g function \\[tinytab-mode] and the call
will trigger loading package tinytab.el

Please notice, that this central setup function configures only the
essential packages, even with TYPE and FEATURE-LIST. The listing
\\[tiny-setup-display] lists many packages that are not loaded
or set up in any default way because a) package's scope is very narrow
and it may not interest the majority b) there is no sensible autoload
and it requires manual settings: tinyload.el and tinypath.el are
good example of this. c) package is a library and it has been
taken cared of by other means.

Remember that all functions are autoloaded and accessible, although
packages marked <no options> may not have default configurations. Here is
sample listing that you may expect from \\[tiny-setup-display] which
displays then content of `tiny-setup-:option-table' when no tiny-setup
configure options are not defined and you should load the package as
instructed in the file itself:

    ..
    tinychist            <no options defined to install>
    ...
                         Command history save/restore utility.
    tinyload             <no options defined to install>
			 Load set of packages when Emacs is idle (lazy load).
    tinylock             <no options defined to install>
			 Simple emacs locking utility.
    ...
    tinynbr              <no options defined to install>
			 Number conversion minor mode oct/bin/hex.
    ...
    tinypath             <no options defined to install>
			 Manage Emacs startup dynamically.

Here is one way to install packages: a) configure paths automatically b)
load default setup and enable some extra features c) define
delayed loading for some packages that you use most of the time.

   (load \"/ABSOLUTE-PATH/tinypath.el\")

   ;;  Define \"ready to use packages\"

   (require 'tiny-setup)

   (tinypath-setup
     'all             ;; Activate default features safely
    '(tinyeat--bind   ;; plus features that you want
      tinydesk--bindforce
      tinydiff--bind
      tinydired--autoload
      tinyeat--bindforce
      tinylisp--activate  ;; Immediately turn on on all .el buffers
      ..))

   ;; Delayed loading of these packages, when Emacs goes idle.

   (setq tinyload-:load-list
     '(\"tinyadvice\"           ;; NOTE: for Emacs only.
       \"tinymy\"
       \"tinymail\"
       \"tinygnus\"
       \"tinyigrep\"
      ..))

  (require 'tinyload)

Here is yet another example. The `tiny-setup' function can configure
only the very basic features, so some defaults for packages
can be changed before they are loaded (look into each file
for interesting things).

    ;; First, configure few packages MANUALLY

    (ti::add-hooks 'tinytf-:mode-define-keys-hook
	           '(tinytf-mode-define-keys tinytf-mode-define-f-keys))

    (setq tinymy-:define-key-force t)
    (setq tinyef-:mode-key \"\\C-cmr\")

    (setq tinylock-:auto-lock-interval1 45)	;in minutes

    (setq tinyef-:mode-key-table
	  '(
	    (?\[   . step-delete-back)		;KEY -- action symbol
	    (?\]   . step-delete-fwd)
	    (?\*   . chunk-delete)
	    (?\;   . move-back)
	    (?\'   . move-fwd)
	    (?\~   . e-tilde)
	    (?\/   . e-slash)
	    (?\$   . e-dollar)))

    ;; After that, let the contral configure tool do the rest

    (require 'tiny-setup)

    (tiny-setup
     'all
     '(tinymy--bind-bindextra
       tinytab--bindforce-bindextra
       tinysearch--bindmousemeta
       tinyreplace--bindemacs
       tinyeat--bindforce))

The major TYPE of installation can be one of the following:

    'autoload

    Setup packages so that they are loaded when the options are needed,
    but do not define any key-bindings that already exist. This will
    bind free keys to trigger loading packages.

    'all

    Configure with all options on. This will affect free key-bindings.

    nil

    Autoload files are loaded (functions are ready for calling), but
    no defaults are configured unless OPTION-LIST is set.

Alternatively, you can select from OPTION-LIST what packages and what
options inside it will be installed. See list of packages and their
options with command \\[tiny-setup-display]

    The syntax for each package is the same and the symbol passed is
    composed from keywords:

	<package>--   Name of package affected, like `tinyeat--'.

        activate    Activate feature in all related buffers.
                    Like turning on tinylisp-mode in all Emacs lisp
                    buffers. Or tinyperl-mode in all perl files.

	bind	    Bind default keys. This will set package
                    to autoload state so that when the binding is called,
                    package gets loaded.

        bindforce   Overwrite any existing binding. This is like bind, but
                    without a check.

        bindemacs   Bind keys that are known to be occupied in Emacs.

        load        Load package. If you're tempted to use this,
                    use more efficient method described in tinyload.el.
		    Packages that have complex setup or which
		    can't be autoloaded easily are categorized as \"load\".

        autoload    Configure package so, that it will get loaded if option
                    related to package is needed.

    For example, to enable options in tinyadvice.el and tinyurl.el, you could
    send option list below. Notice that multiple options for a package
    are separated by single dashes.

        (require 'tiny-setup)
        (tinypath-setup 'all '(tinyadvice--load tinyurl--autoload-bind ...))
                                                |        |        |
						|        |        Option 2.
					        |        Option 1.
						Package."
  (interactive)
  (when (and (interactive-p)
	     (eq type nil)
	     (eq option-list nil))
    (setq type 'all))
  (tiny-setup-autoload-read)
  (cond
   ((eq type 'all)
    (tiny-setup-all nil))
   ((eq type 'autoload)
    (tiny-setup-all 'autoload-bind)))
  (when option-list
    (tiny-setup-option-process option-list))
  (message "TinySetup: Done."))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-option-process (option-list)
  "Process OPTION-LIST described in `tiny-setup'.
OPTION-LIST items items are in form:

   PACKAGE--OPTION-OPTION-OPTION-..

Like

   '(tinymy--bind-bindextra)
             |    |
             |    option 2
             option 1

See also `tiny-setup-:option-table'."
  (dolist (elt option-list)
    (let* ((name (symbol-name elt))
	   (package (if (string-match "\\(^[^ \t-]+\\)--" name)
			(match-string 1 name))))
      (if package
	  (tiny-setup-package package elt)
	(message "TinySetup: Invalid setup option format %s" name)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tiny-setup-all (&optional type)
  "Setup all tools with TYPE."
  (interactive)
  (dolist (elt tiny-setup-:option-table)
    (tiny-setup-package (car elt) type)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tiny-setup-display (&optional no-descriptions)
  "List all packages and available setup options.
With Argument, like, \\[universal-argument], list NO-DESCRIPTIONS."
  (interactive "P")
  (let* ((buffer (get-buffer-create "*tiny-setup*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "package              Supported install options\n"
	      "-----------          "
	      (make-string 30 ?-)
	      "\n")
      (dolist (elt tiny-setup-:option-table)
	(insert (format "%-20s %s\n%-20s %s\n"
			(car elt)
			(if (null (tiny-setup-nth-options elt))
			    "<no options defined to install>"
			  (mapconcat
			   'identity
			   (sort (tiny-setup-nth-options elt) 'string<)
			    " "))
			""
			(tiny-setup-nth-description elt))))
      (insert "
The options can be installed by adding code like this to .emacs:

    (require 'tiny-setup)
    (tinypath-setup nil '(tinyadvice--load tinyurl--autoload-bind ...))
")
      (goto-char (point-min))
      (display-buffer (current-buffer)))))


;;; ----------------------------------------------------------------------
;;;
(put 'tiny-setup-error-macro 'lisp-indent-function 0)
(put 'tiny-setup-error-macro 'edebug-form-spec '(body))
(defmacro tiny-setup-error-macro (&rest body)
  "Show error."
  (` (progn
       (pop-to-buffer (get-buffer-create "*TinySetup Error*"))
       (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tiny-setup-dolist-buffer-list 'lisp-indent-function 0)
(put 'tiny-setup-dolist-buffer-list 'edebug-form-spec '(body))
(defmacro tiny-setup-dolist-buffer-list (&rest body)
  "Run BODY in each buffer."
  (`
   (dolist (buffer (buffer-list))
     (with-current-buffer buffer
      (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tiny-setup-autoload-read ()
  "Read all autoloads. Makefile must have been run for this to work.
Syntax in Tiny Tools kit bin/ directory: perl makefile.pl autoload."
  (condition-case err
      (progn
	;;  It's better to use `load' and not `require' because user may run
	;;  makefile again.
	(load "tiny-autoload-loaddefs-tiny")
	(load "tiny-autoload-loaddefs-other"))
    (error
     (let* ((str
	     (format
	      (concat
	       "\
TinySetup: Error in reading autoload loaddefs. %s

Symptoms: load-path:

    Please check that your `load-path' contains directories
    tiny/lisp/tiny and tiny/lisp/other.

Symptoms: autoload files:

    Check that the tiny-autoload*el files are present in these directories.
    If there is no autoload files, create them by running makefile:

    cd bin/
    perl makefile.pl --verbose 2 autoload.

Symptoms: compiled files

    There may be problem with compiled  tiny-autoload*.elc files.
    Please remove all *.elc files and try again.")
	      (prin1-to-string err))))
       ;;  Write to *Message* buffer
       (message str)
       (tiny-setup-error-macro
	 (insert str
		 "

Symptoms for tinypath.el usage:

    If you use tinypath.el, it may be possible that it didn't find the
    default ~/elisp or ~/lisp directories. Please move all your Emacs setup
    files under one of these directories. Alternatively set the location
    of your private lisp with:

    (setq tinypath-:load-path-root '(\"~/your-lisp-dir-location\"))
    (load \"ABSOLUTE-INSTALLATION-PATH-HERE/tinypath\")

    (require 'tiny-setup)
    (tiny-setup 'all)

    Refer to README.txt and tinypath.el for more instructions how to
    let tinypath.el set your `load-path' automatically."))
       (error str)))))


;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-option-strings (type)
  "Return list of options from symbol TYPE."
  (setq type (symbol-name type))
  (if (not (string-match "--\\(.*\\)" type))
      type
    (split-string (match-string 1 type) "[-]")))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-package-require (package)
  (message "TinySetup: %s loaded." package)
  (unless (featurep (intern package))
    (message "TinySetup: %s LOADED." package)
    (require (intern package))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-package-option-p (package opt option-list)
  "Check if PACKAGE and OPT is part of user requested OPTION-LIST."
  (let (ret)
    (dolist (elt option-list)
      (when (string= elt opt)
	(setq ret t)
	(return)))
    (unless ret
      (message "TinySetup: [%s] No option [] found for `%s'"
	       package
	       (if (eq 1 (length option-list))
		   (car option-list)
		 (prin1-to-string option-list))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-package (package &optional type)
  "Activate PACKAGE with TYPE.
If TYPE is nil, activate all options that do not contain word
`force' or `load'."
  (let* ((req-options (and type
			   (tiny-setup-option-strings type)))
	 (list     (tiny-setup-package-options package)))
    (cond
     ((null list)
      (message "TinySetup: %-15s No options to configure."
	       package))
     (t
      (unless req-options;; nil, activate almost all
	(dolist (option list)
	  (unless (string-match "^load\\|force" option)
	    (push option req-options))))
      (let* (function
	     sym)
	(dolist (option req-options)
	  (cond
	   ((not (member option list))
	    (message "TinySetup: Uknown option [s]. Choose from %s"
		     option
		     (prin1-to-string list)))
	   (t
	    (setq function (format "tiny-setup-%s-%s" package option))
	    (setq sym (intern-soft function))
	    (cond
	     ((and (null sym)
		   (string= option "load"))
	      (tiny-setup-package-require package))
	     ((null sym)
	      (message "TinySetup: ERROR Unknown function %s"
		       function))
	     (t
	      (setq function sym)
	      (message "TinySetup: %-15s configured with `%s'" package option)
	      (funcall function)))))))))))


;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-nth-options (elt)
  "Return option list from ELT."
  (nth 2 elt))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-nth-description (elt)
  "Return option list from ELT."
  (nth 1 elt))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-package-options (package)
  "Return list of options for PACKAGE."
  (let ((elt   (assoc package tiny-setup-:option-table)))
    (when elt
      (tiny-setup-nth-options elt))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-define-key-1
  (key keymap function &optional prefix str force)
  "Define KEY to KEYMAP using FUNCTION if not yet occupied.

Input:

  KEY	    Key definitions
  KEYMAP    The map.
  FUNCTION  function to bind
  PREFIX    Message prefix. Like \"Package:\" who requested change.
  STR	    Human readable key definition. Shown to user.
  FORCE     Override key definition without a check."
  (setq str (if (stringp str)
		(format "(%s)" str)
	      ""))
  (cond
   (force
    (message "%sKey %-10s%s set to `%s' (FORCED)."
	     (or prefix "")
	     (prin1-to-string key)
	     str
	     (symbol-name function))
    (define-key keymap key function))
   (t
    (let ((def (lookup-key keymap key)))
      (cond
       ((or (eq def function)
	    (memq def '(nil ignore))
	    ;; Lookup key returns NBR if the sequence of keys exceed
	    ;; the last keymap prefix
	    ;; C-cck  --> C-cc  is undefined, so there is no C-c c map yet
	    (integerp def))
	(message "%sKey %-10s%s set to `%s'."
		 (or prefix "")
		 (prin1-to-string key)
		 str
		 (symbol-name function))
	(define-key keymap key function))
       (t
	(message
	 "%sKey %-10s%s already has has definition `%s'. Not set to `%s'"
	 (or prefix "")
	 (prin1-to-string key)
	 str
	 (prin1-to-string def)
	 (symbol-name function))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-define-key (key keymap function &optional str force)
  "Define KEY to KEYMAP using FUNCTION. Display STR and FORCE binding."
  (tiny-setup-define-key-1
   key keymap function "TinySetup: " str force))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-alist-search (alist regexp)
  "Search ALIST for REGEXP."
  (dolist (elt alist)
    (if (string-match regexp (car elt))
	(return elt))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-aput (sym regexp key value &optional force)
  "Search SYM's for REGEXP and set KEY to VALUE if not found.
This function effectively compares each key in SYM to REGEXP and
if there is no matches, it adds new (KEY . VALUE) pair.

Useful, if something needs to be added to the `auto-mode-alist', but
previous definitions must be preserved."
  (let* ((found (tiny-setup-alist-search (symbol-value sym) regexp)))
    (cond
     ((and found
	   (eq (cdr found) value))
      (message "TinySetup: `%s' now contains (%s . %s)"
	       (symbol-name sym)
	       key
	       value))
     (found
      (message "TinySetup: `%s' already contains %s. Not set to (%s . %s)"
	       (symbol-name sym)
	       (prin1-to-string found)
	       key
	       value))
     (t
      (message "TinySetup: `%s' now contains (%s . %s)"
	       (symbol-name sym)
	       key
	       value))
     (push (cons key value) (symbol-value sym)))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-defalias (symbol definition)
  "Like `defalias' but with verbose messages."
  (message "TinySetup: defalias `%s' =>  `%s'"
	   (symbol-name symbol)
	   (symbol-name definition))
  (defalias symbol definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	TIMING UTILITIES
;;      These are admistrative utilies for package maintainer(s)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g. if you want to calculate days; you'd do
\(/ (ti::date-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (multiple-value-bind (s0 s1 s2) a
    (setq a (+ (* (float (ash 1 16)) s0)
	       (float s1) (* 0.0000001 s2))))
  (multiple-value-bind (s0 s1 s2) b
    (setq b (+ (* (float (ash 1 16)) s0)
	       (float s1) (* 0.0000001 s2))))
  (- a b))

;;; ----------------------------------------------------------------------
;;;
(defvar tiny-setup-:time nil)
(put 'tiny-setup-time-this 'lisp-indent-function 0)
(put 'tiny-setup-time-this 'edebug-form-spec '(body))
(defmacro tiny-setup-time-this (&rest body)
  "Run BODY with and time execution. Time is in `my-:tmp-time-diff'."
  (`
   (let* ((tmp-time-A (current-time))
	  tmp-time-B)
     (,@ body)
     (setq tmp-time-B (current-time))
     (setq tiny-setup-:time
	   (tiny-setup-time-difference tmp-time-B tmp-time-A)))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-time-load-file (file)
  "Time lisp FILE loading."
  (interactive "fload file and time it: ")
  (tiny-setup-time-this
   (load file))
  (message "Tiny: Timing %-15s took %12f secs" file tiny-setup-:time))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-test-load-time-libraries ()
  "Time package load times."
  (interactive)
  (message "\n\n** Tiny setup: timing test start\n")
  (message "load-path: %s"
	   (prin1-to-string load-path))
  (let* ((path (locate-library "tinylib.el"))
	 (time-a (current-time))
	 time-b)
    (if (not path)
	(message "Tiny: [timing] Can't find tinylib.el along `load-path'")
      (setq path (file-name-directory path))
      (dolist (pkg (directory-files path 'full "^tinylib.*el"))
	(tiny-setup-time-load-file pkg))
      (setq time-b (current-time))
      (message "Tiny: total time is %s seconds"
	       (tiny-setup-time-difference time-b time-a))
      (display-buffer "*Messages*"))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-test-load-all ()
  "Load each package to check against errors."
  (interactive)
  (message "\n\n** Tiny setup: load test start\n")
  (let* ((path (locate-library "tinylib.el")))
    (if (not path)
	(message "Tiny: [load test] Can't find tinylib.el along `load-path'")
      (setq path (file-name-directory path))
      (dolist (pkg (directory-files path 'full "^tiny.*el"))
	(load pkg))
      (display-buffer "*Messages*"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	AUTOLOAD UTILITIES
;;      These are admistrative utilies for package maintainer(s)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-directory-last (dir)
  "Return last directory name in DIR. /dir1/dir2/ -> dir2."
  (if (string-match "[/\\]\\([^/\\]+\\)[/\\]?$" dir)
      (match-string 1 dir)
    ""))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-directory-to-file-name (dir template)
  "Make file name from NAME and TEMPLATE. <template>-<last-dir>.el."
  (concat
   (file-name-as-directory dir)
   template
   (tiny-setup-directory-last dir)
   ".el"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-autoload-file-footer (file &optional end)
  "Return 'provide and optional END of the file marker."
  (concat
   (format
    "\n(provide '%s)\n\n"
    (file-name-sans-extension (file-name-nondirectory file)))
   (if end
       (format ";; End of file %s\n"
	       (file-name-nondirectory (file-name-nondirectory file)))
     "")))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-directories (list)
  "Return only directories from LIST."
  (let* (ret)
    (dolist (elt list)
      (when (and (file-directory-p elt)
		 ;;  Drop . ..
		 (not (string-match
		       "[/\\]\\.+$\\|CVS\\|RCS"
		       elt)))
	(push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
;;; (tiny-setup-autoload-build-functions "~/elisp/tiny/lisp/tiny")
;;; (tiny-setup-autoload-build-functions "~/elisp/tiny/lisp/other")
;;;
(defun tiny-setup-autoload-build-functions (dir &optional regexp)
  "Build all autoloads in DIR-LIST, except for files matching REGEXP.
Store the autoloads to tiny-DIR-autoload.el"
  (let* (make-backup-files                 ;; Do not make backups
	 (backup-enable-predicate 'ignore) ;; Really, no backups
	 (files   (directory-files
		   dir
		   'full
		   "\\.el$"))
	 ;; There is no mistake in name here: it is "tiny-autoload-DIRNAME".
	 ;; the other autoload generater will generate
	 ;; "tiny-autoload-loaddefs-DIRNAME"
	 (to-file (tiny-setup-directory-to-file-name dir "tiny-autoload-"))
	 (name    (file-name-nondirectory to-file)))
    (when files
      (with-temp-buffer
	(insert
	 (format ";;; %s -- " name)
	 "Autoload definitions of program files in Tiny Tools Kit\n"
	 ";;  Generate date: " (format-time-string "%Y-%m-%d" (current-time))
	 "\n\
;;  This file is automatically generated. Do not Change.
;;  Read README.txt in the Tiny Tools doc/ directory for instructions."
	 "\n\n")
	(dolist (file files)
	  (if (and (stringp regexp)
		   (string-match regexp file))
	      (message "Tiny: Ignoring autoload creation for %s" file)
	    (ti::package-autoload-create-on-file
	     file (current-buffer) 'no-show)))
	(insert (tinypath-tmp-autoload-file-footer to-file 'eof))
	(let ((backup-inhibited t))
	  (write-region (point-min) (point-max) to-file))
	to-file))
    (message "TinySetup: Updated ALL autoloads in dir %s" dir)))

;;; ----------------------------------------------------------------------
;;;	This is autoload generator will generate ALL, that means ALL,
;;;	autoloads from EVERY function and macro.
;;;	The implementation is in tinylib.el
;;;
;;; (tiny-setup-autoload-build-functions-all "~/elisp/tiny/lisp/")
;;;
(defun tiny-setup-autoload-build-functions-all (dir)
  "Build all autoloads recursively below DIR."
  (interactive "Dautoload build root dir: ")
  (let* ((dirs (tiny-setup-directories
		(directory-files
		 (expand-file-name dir)
		 'abs)))
	 (regexp "tinylib\\|autoload"))
    (cond
     (dirs
      (tiny-setup-autoload-build-functions dir regexp)
      (dolist (dir dirs)
	(tiny-setup-autoload-build-functions-all dir)))
     (t
      (tiny-setup-autoload-build-functions dir regexp)))))

;;; ----------------------------------------------------------------------
;;; (tiny-setup-autoload-build-loaddefs-tiny-tools "~/elisp/tiny/lisp/" t)
;;; (tiny-setup-autoload-build-loaddefs-tiny-tools "~/elisp/tiny/lisp/other" t)
;;;
(defun tiny-setup-autoload-build-loaddefs-tiny-tools (dir &optional force)
  "Build Tiny Tools autoloads below DIR. FORCE recreates everything."
  (interactive "DAutoload root: \nP")
  (ti::package-autoload-loaddefs-build-recursive
   dir
   "autoload\\|loaddefs"  ;; Exclude these files
   force
   (function
    (lambda (dir)
      (tiny-setup-directory-to-file-name
       (or dir
	   (error "TinySetup: No DIR"))
       "tiny-autoload-loaddefs-")))))

;;; ----------------------------------------------------------------------
;;;	This is autoload generator will generate ONLY functions marked
;;;	with special ### autoload tag. The implementation used is in
;;;	core Emacs package autoload.el
;;;
;;; (tiny-setup-autoload-batch-update "~/elisp/tiny/lisp/" 'force)
;;;
;;; This function is invoked from the perl makefile.pl with the
;;; ROOT directory as sole argument in Emacs command line.
;;;
;;; The build command from prompt is
;;;
;;;    $ perl makefile.pl --verbose 2 --binary emacs  autoload
;;;
(defun tiny-setup-autoload-batch-update (&optional dir force)
  "Update autoloads in batch mode. Argument in command line is DIR. FORCE."
  (interactive "DAutoload dir to update: ")
  (unless dir
    (setq dir (car-safe command-line-args-left))
    (setq force t))
  (if dir				;Require slash
      (setq dir (file-name-as-directory dir)))
  (message "TinySetup: Generating all autoloads under %s" dir)
  (unless dir
    (message "Tiny: From what directory to make recursively autoloads?")
    ;; Self generate error for command line ...
    (error 'tiny-setup-autoload-batch-update))
  (let* ((default-directory (expand-file-name dir)))
    (message "Tiny: tiny-setup-autoload-batch-update %s"  default-directory)
    (when (not (string-match "^[/~]\\|^[a-zA-Z]:[/\\]"
			     default-directory))
      (message "Tiny: Autoload directory must be absolute path name.")
      (error 'tiny-setup-autoload-batch-update))
    (tiny-setup-autoload-build-loaddefs-tiny-tools
     default-directory force)
    ;;  This would generate second set of autoloads. Don't do that any more,
    ;;  rely on Emacs autoload.el instead.
    ;; (tiny-setup-autoload-build-functions-all default-directory)
    (setq command-line-args-left nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	PACKAGE BYTE COMPILATION
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ----------------------------------------------------------------------
;;;
(defsubst tiny-setup-file-list-lisp (dir)
  "Return all lisp files under DIR."
  (directory-files dir 'full "\\.el$"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tiny-setup-file-list-lisp-compiled (dir)
  "Return all compiled lisp files under DIR."
  (directory-files dir 'full "\\.elc$"))

;;; ----------------------------------------------------------------------
;;;
(put 'tiny-setup-directory-recursive-macro 'lisp-indent-function 1)
(put 'tiny-setup-directory-recursive-macro 'edebug-form-spec '(body))
(defmacro tiny-setup-directory-recursive-macro (directory &rest body)
  "Start from DIRECTORY and run BODY recursively in each directories.

Following variables are set during BODY:

`dir'      Directrory name
`dir-list' All directories under `dir'."
  (`
   (flet ((recurse
	   (dir)
	   (let* ((dir-list (tiny-setup-directory-list dir)))
	     (,@ body)
	     (when dir-list
	       (dolist (elt dir-list)
		 (recurse elt))))))
     (recurse (, directory)))))


;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
		 (not (string-match "[\\/]\\.\\.?$" elt)))
	(push elt list)))
    list))


;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory (dir &optional function)
  "compile all isp files in DIRECTORY.
Optional FUNCTION is passed one argument FILE, and it should return
t or nil if file is to be compiled."
    (dolist (file (tiny-setup-file-list-lisp dir))
      (when (or (null function)
		(funcall function file))
	(byte-compile-file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory-recursive (root)
  "Compile all files under ROOT directory."
  (tiny-setup-directory-recursive-macro root
    (message "TinySetup: compiling directory %s" dir)
    (tiny-setup-compile-directory dir)))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory-delete-recursive (root)
  "Delete all compiled files under ROOT directory recursively."
  (tiny-setup-directory-recursive-macro root
    (dolist (file (tiny-setup-file-list-lisp-compiled dir))
      (message "TinySetup: deleting compiled file %s" file)
      (delete-file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-kit-libraries (dir)
  "Compile tiny tools libraries"
  (tiny-setup-directory-recursive-macro dir
    (let ((libs (directory-files dir 'abs-path "tinylib.*\\.el$")))
      (when libs ;;  Found correct directory
	(message "TinySetup: compiling libraries first in right order.")
	(let ((default-directory dir)
	      compile-file)
	  ;; There is certain order of compilation. Low level libraries first.
	  (dolist (regexp tiny-setup-:library-compile-order)
	    (when (setq compile-file   ;; compile these first
			(find-if (function
				  (lambda (elt)
				    (string-match regexp elt)))
				 libs))
	      (setq libs (delete compile-file libs))
	      (byte-compile-file compile-file)))
	  (message "TinySetup: compiling rest of the libraries.")
	  (dolist (file libs)  ;; Rest of the libraries
	    (cond
	     ((find-if (function
			(lambda (regexp)
			  (string-match regexp file)))
		       tiny-setup-:library-compile-exclude)
	      (message "TinySetup: ignoring library %s" file))
	     (t
	      (byte-compile-file file)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-kit-all (&optional dir)
  "Compile tiny tools kit under install DIR.
This function can be called from shell command line, where the
last argument is the DIR from where to start compiling.

cd root-dir
emacs -batch -l load-path.el -l tiny-setup.el -f tiny-setup-compile-kit-all ."
  (interactive "D[compile] installation root dir: ")
  (unless dir
    (setq dir (car-safe command-line-args-left)))
  (if dir				;Require slash
      (setq dir (file-name-as-directory dir))
    (error "Compile under which DIR"))
  (message "tinySetup: byte compiling root %s" dir)
  ;;  Remove compiled files first
  (tiny-setup-compile-directory-delete-recursive dir)
  ;;  Libraries first
  (tiny-setup-compile-kit-libraries dir)
  ;;  The rest follows, it doesn't matter if libs are are compiled twice.
  (tiny-setup-compile-directory-recursive dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;	PACKAGE CONFIGURATION
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tiny-setup-folding-autoload-find-file-hook ()
  "Install folding if file includes {{{ and }}}.
Do nothing if folding is already installed."
  (if (featurep 'folding)
      ;;  Remove ourself from the `find-file-hooks'.
      (remove-hook  'find-file-hooks
		    'tiny-setup-folding-autoload-find-file-hook)
    (let* ((start  (concat "\\("
			   (regexp-quote (or comment-start ""))
			   "\\)+"))
	   (regexp (concat "^" start "{{{ \\|^" start "}}}")))
      (when (ti::re-search-check regexp)
	(folding-install-hooks)
	(turn-on-folding-mode)))))

(defun tiny-setup-folding-autoload ()
  "Autoload."
  (add-hook  'find-file-hooks
	     'tiny-setup-folding-autoload-find-file-hook))

(defun tiny-setup-dired-sort-autoload ()
  "Autoload."
  (add-hook  'dired-mode-hook 'dired-sort-default-keys 'end))

(defun tiny-setup-tinyadvice-load ()
  "Load for Emacs only."
  (if (emacs-p)
      (require 'tinyadvice)
    (message "TinySetup: tinyadvice.el is not for XEmacs. Didn't load.")))

(defun tiny-setup-tinyappend-bind ()
  "Bind."
  ;; non-shift key
  (tiny-setup-define-key  "\C-c=" global-map 'tinyappend-end)
  ;; non-shift key
  (tiny-setup-define-key  "\C-c-" global-map 'tinyappend-beg)
  (tiny-setup-define-key  "\C-c_" global-map 'tinyappend-kill)
  (tiny-setup-define-key  "\C-c|" global-map 'tinyappend-yank))

(defun tiny-setup-tinybookmark-defalias ()
  "Defalias."
  ;; (tiny-setup-defalias 'tinybookmark-insert 'bm)
  nil)

(defun tiny-setup-tinybookmark-bind ()
  "Bind."
  (if (emacs-p)
      (tiny-setup-define-key [(?\e) (control mouse-1)]
			     global-map 'tinybookmark-mouse)
    (tiny-setup-define-key [(control meta button1)]
			   global-map 'tinybookmark-mouse))

  ;; (tiny-setup-define-key [(?\e) (control shift mouse-1)]
  ;;                     global-map 'tinybookmark-mouse-parse)

  (tiny-setup-define-key [(shift left)]
			 global-map 'tinybookmark-backward)
  (tiny-setup-define-key [(shift right)]
			 global-map 'tinybookmark-forward))

(defun tiny-setup-tinycache-activate ()
  "Autoload."
  (add-hook 'compilation-mode-hook '(lambda () (require 'tinycache)))
  (when (emacs-p)
    (add-hook 'dired-mode-hook '(lambda () (require 'tinycache))))
  (eval-after-load "compile" '(progn (require 'tinycache)))
  (eval-after-load "dired"   '(progn (require 'tinycache))))

(defun tiny-setup-tinybuffer-bind (&optional force)
  "Bind."
  (tiny-setup-define-key [(control <)]
			 global-map 'tinybuffer-previous-buffer
			 nil force)
  (tiny-setup-define-key [(control >)]
			 global-map 'tinybuffer-next-buffer
			 nil force)
  (tiny-setup-define-key [(control meta <)]
			 global-map 'tinybuffer-iswitch-to-buffer
			 nil force)
  (tiny-setup-define-key [(control meta >)]
			 global-map 'tinybuffer-sort-mode-toggle
			 nil force))

(defun tiny-setup-tinybuffer-bindforce ()
  "Bind."
  (tiny-setup-tinybuffer-bind 'force))


(defun tiny-setup-tinycomment-autoload ()
  "Autoload."
  (autoload 'tinycomment-indent-for-comment "tinycomment" "" t))


(defun tiny-setup-tinycomment-bind ()
  "Bind."
  (tiny-setup-define-key
   [(meta ?\;)] global-map 'tinycomment-indent-for-comment "M-;" 'force))

(defun tiny-setup-tinycompile-autoload ()
  "Autoload."
  (add-hook 'compilation-mode-hook 'turn-on-tinycompile-mode 'append)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (memq major-mode '(compilation-mode))
	(turn-on-tinycompile-mode)))))

(defun tiny-setup-tinydesk-bind (&optional force)
  "Bind with optional FORCE."
  (tiny-setup-define-key
   "s" ctl-x-4-map
   'tinydesk-save-state nil force)      ;; free in 19.28
  (tiny-setup-define-key
   "r" ctl-x-4-map
    'tinydesk-recover-state nil force)   ;; Non-free, seldom used
  (tiny-setup-define-key
   "e" ctl-x-4-map
    'tinydesk-edit-state-file nil force) ;; free in 19.28
  (tiny-setup-define-key
   "u" ctl-x-4-map
    'tinydesk-unload nil force))         ;; free in 19.28

(defun tiny-setup-tinydesk-bindforce ()
  "Bind."
  (tiny-setup-tinydesk-bind 'force))

(defun tiny-setup-tinydesk-activate ()
  "Activate.")

(defun tiny-setup-tinydiff-autoload ()
  "Autoload."
 (tiny-setup-aput 'auto-mode-alist
		  "diff" "\\.diff\\'" 'turn-on-tinydiff-mode)
 (tiny-setup-aput 'auto-mode-alist
		  "patch" "\\.patch\\'"  'turn-on-tinydiff-mode))

(defun tiny-setup-tinydiff-bind (&optional force)
  "Bind keys."
  (tiny-setup-define-key
   "\C-cD"
   global-map 'tinydiff-diff-show "\C-cD" force)
  (tiny-setup-define-key
   "\C-cP"
   global-map 'tinydiff-patch  "\C-cP" force))

(defun tiny-setup-tinydiff-bindforce ()
  "Bind keys."
  (tiny-setup-tinydiff-bind 'force))

(defun tiny-setup-tinydired-autoload ()
  "Autoload. This is for Emacs only.
You may want to set

  (setq tinydired-:force-add-keys 'override)."
  (if (xemacs-p)
      (message "\
TinySetup: tinydired.el works only with Emacs. Package not loaded.")
    (add-hook 'tinydired-:load-hook    'tinydired-hook-control)
    (add-hook 'dired-mode-hook '(lambda () (require 'tinydired) nil))
    ;;  If dired is already loaded, install immediately
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (memq major-mode '(dired-mode))
	  (require 'tinydired)
	  (return))))))

(defun tiny-setup-tinyeat-bind (&optional force)
  "Bind."

  ;;  These are REAL difficult choices, because almost every keyboard
  ;;  interprets backspace differently.

  (tiny-setup-define-key [(control backspace)]
			 global-map 'tinyeat-forward-preserve
			 nil force)
  (tiny-setup-define-key [(control delete)]
			 global-map 'tinyeat-forward-preserve
			 nil force)

  (tiny-setup-define-key [(control shift delete)]
			 global-map 'tinyeat-delete-paragraph
			 nil force)

  (tiny-setup-define-key [(control shift backspace)]
			 global-map 'tinyeat-delete-paragraph
			 nil force)

  (tiny-setup-define-key [(shift backspace)]
			 global-map 'tinyeat-delete-whole-word
			 nil force)

  (tiny-setup-define-key [(meta delete)]
			 global-map 'tinyeat-erase-buffer
			 nil force)

  (tiny-setup-define-key [(alt control k)]
			 global-map 'tinyeat-zap-line
			 nil force)

  (unless (ti::xe-window-system)
    (tiny-setup-define-key
     [(control meta ?h)]
     global-map 'tinyeat-erase-buffer nil force))

  (when (fboundp 'read-kbd-macro)
    (tiny-setup-define-key
     (read-kbd-macro "ESC DEL")
     global-map 'tinyeat-erase-buffer "ESC DEL" force)))

(defun tiny-setup-tinyeat-bindforce ()
  "Bind with override."
  (tiny-setup-tinyeat-bind 'force))

(defun tiny-setup-tinyef-autoload ()
  "Autoload."
  (add-hook 'minibuffer-setup-hook 'turn-on-tinyef-mode))

(defun tiny-setup-tinygnus-autoload ()
  "Autoload."
  (add-hook 'gnus-startup-hook '(lambda () (require 'tinygnus)))
  (when (featurep 'gnus)
    ;;  Gnus already present
    (require 'tinygnus)))

(defun tiny-setup-tinyhotlist-autoload ()
  "Autoload."
  (add-hook 'tinyhotlist-:load-hook 'tinyhotlist-load-hotlist))

(defun tiny-setup-tinyhotlist-bindmouse (&optional force)
  "Bind."
  (if (not (ti::xe-window-system))
      (message
       (concat
	"TinySetup: tinyhotlist.el Mouse binding skipped."
	"No window system available."))
    (if (emacs-p)
	(tiny-setup-define-key
	 [(control shift mouse-3)]
	 global-map
	 'tinyhotlist-control
	 force)
      (tiny-setup-define-key
       [(control shift button3)]
       global-map
       'tinyhotlist-control
       force))))

(defun tiny-setup-tinyhotlist-bindmouseforce ()
  "Bind."
  (tiny-setup-tinyhotlist-bindmouse 'force))

(defun tiny-setup-tinyhotlist-bind (&optional force)
  "Bind."
  (tiny-setup-define-key
   (read-kbd-macro "\C-cH")
   global-map 'tinyhotlist-control "\C-cH" force))

(defun tiny-setup-tinyhotlist-bindforce ()
  "Bind."
  (tiny-setup-tinyhotlist-bind))

(defun tiny-setup-tinyigrep-autoload ()
  "Autoload."
  (if (featurep 'igrep)
      (require 'tinyigrep)
    (eval-after-load "igrep" '(progn (require 'tinyigrep)))))

(defun tiny-setup-tinyigrep-bind (&optional force)
  "Bind."
  (tiny-setup-define-key
   (read-kbd-macro "\C-cg")
   global-map 'tinyigrep-menu "\C-cg" force))

(defun tiny-setup-tinyigrep-bindforce ()
  "Bind."
  (tiny-setup-tinyigrep-bind 'force))

(defun tiny-setup-tinyliby-defalias ()
  "Defalias."
  ;;  Shorter name.
  (tiny-setup-defalias 'describe-symbols 'ti::y-describe-symbols))

(defun tiny-setup-tinylibt-bind ()
  "Bind."
  ;;#todo:
  ;;   (tiny-setup-define-key "\C-ztm" global-map 'ti::text-mark-region)   ;; e.g. permanent 'mark'
  ;;   (tiny-setup-define-key "\C-ztu" global-map 'ti::text-unmark-region) ;; remove 'mark'
  ;;   (tiny-setup-define-key "\C-ztc" global-map 'ti::text-clear-buffer-properties)
  ;;   (tiny-setup-define-key "\C-ztb" global-map 'ti::text-buffer)
  ;;   (tiny-setup-define-key "\C-ztU" global-map 'ti::text-undo)
  nil)

(defun tiny-setup-tinylisp-autoload ()
  "Autoload."
  (add-hook 'lisp-mode-hook               'turn-on-tinylisp-mode)
  (add-hook 'emacs-lisp-mode-hook         'turn-on-tinylisp-mode)
  (add-hook 'lisp-interaction-mode-hook   'turn-on-tinylisp-mode))

(defun tiny-setup-tinylisp-activate ()
  "Activate on every lisp buffer."
  (tiny-setup-tinylisp-autoload)   ;; Make sure this is called too.
  (tiny-setup-dolist-buffer-list
    (when (or (string-match "\\.el$" (buffer-name))
	      (memq major-mode '(emacs-lisp-mode
				 lisp-interaction-mode)))
      (message "TinySetup: activating tinylisp-mode in %s" (buffer-name))
      (turn-on-tinylisp-mode))))


(defun tiny-setup-tinylpr-bind ()
  "Bind."
  ;;#todo:
  ;; (ti::use-prefix-key "\C-z")          ;; Free C-z for us.
  ;; (tiny-setup-define-key "\C-zp" (definteractive (ti::menu-menu global-map 'tinylpr-:menu)))
  nil)

(defun tiny-setup-tinymacro-bind ()
  "Bind."
  ;;  Windowed fast keys
  (tiny-setup-define-key [(control ?\()]
			 global-map 'start-kbd-macro "Control-(")
  (tiny-setup-define-key [(control ?\))]
			 global-map 'tinymacro-end-kbd-macro-and-assign
			 "Control-)" 'force))

(defun tiny-setup-tinymacro-bindemacs ()
  "Bind."
  ;;  Emacs
  ;; (tiny-setup-define-key "\C-x(" global-map 'start-kbd-macro)
  (tiny-setup-define-key "\C-x)"
			 global-map 'tinymacro-end-kbd-macro-and-assign
			 "C-x("))

(defun tiny-setup-tinymail-autoload ()
  "Autoload."
  (add-hook 'mail-setup-hook     'turn-on-tinymail-mode)
  (add-hook 'message-mode-hook   'turn-on-tinymail-mode)
  (add-hook 'tinymail-:mode-hook 'turn-on-tinytab-mode))


(defun tiny-setup-tinymailbox-find-file-hook ()
  "Activate `tinymail-mode' on mailbox files."
  (let (remove)
    (if (featurep 'tinymailbox)
	(setq remove t)
      (when (ti::mail-mailbox-p)
	(turn-on-tinymailbox-mode)))
    (if remove
	(remove-hook
	 'find-file-hooks
	 'tiny-setup-tinymailbox-find-file-hook))))


(defun tiny-setup-tinymailbox-autoload ()
  "Autoload."

  (add-hook  'find-file-hooks
             'tiny-setup-tinymailbox-find-file-hook)

  ;;  Gnus temporary mailbox files have name "Incoming"
  (tiny-setup-aput 'auto-mode-alist
		   "Incoming" "Incoming"  'turn-on-tinymailbox-mode)

  (tiny-setup-aput 'auto-mode-alist
		   "mbo?x" "\\.mbo?x\\'"  'turn-on-tinymailbox-mode)
  (tiny-setup-aput 'auto-mode-alist
		   "spool" "\\.spool\\'"  'turn-on-tinymailbox-mode))

(defun tiny-setup-tinymy-bind ()
  "Bind."
  (tinymy-define-keys))

(defun tiny-setup-tinymy-bindextra ()
  "Bind."
  (tinymy-define-keys-extra))

(defun tiny-setup-tinymy-defalias ()
  "Bind."
  ;;  Faster prompting for experts
  (tiny-setup-defalias 'yes-or-no-p 'y-or-n-p))

(defun tiny-setup-tinymy-dosdisplay ()
  "Hide ^M by setting display table, minor mode."
  (add-hook  'find-file-hooks  'tinymy-find-file-hook))

(defun tiny-setup-tinypage-bind ()
  "Bind."
  ;;#todo:
  nil)

(defun tiny-setup-turn-off-tinypair-mode ()
  "Safeguard to function `turn-off-tinypair-mode'.
If tinypair.el cannot be found, function `turn-off-tinypair-mode'
cannot be called. Attempt to do so will yield serious error,
preventing user to enter minibuffer at all.

To prevent this serious error, package existence is
verified."
  (when (locate-library "tinypair")
    ;; It's safe to call this. Function is already autoloaded.
    (turn-off-tinypair-mode)))

(defun tiny-setup-tinypair-bind ()
  "Bind."
  (add-hook 'minibuffer-setup-hook 'turn-off-tinypair-mode)
  (message "TinySetup: Key \" (double quote) set to autoload [tinypair].")
  (global-set-key              ;; Make pressing " to autoload the package
   "\"" '(lambda ()
	   (interactive)
	   (turn-on-tinypair-mode)
	   (insert (char-to-string ?\"))
	   (global-set-key "\"" 'self-insert-command))))

(defun tiny-setup-tinyperl-autoload ()
  "Autoload."
  (add-hook 'perl-mode-hook  'turn-on-tinyperl-mode)
  (add-hook 'cperl-mode-hook 'turn-on-tinyperl-mode)
  (when (or (featurep 'cperl)
	    (featurep 'perl))
    (turn-on-tinyperl-mode-all-buffers)))

(defun tiny-setup-tinyprocmail-autoload ()
  "Autoload."
  ;;  old procmail files start with rc.*
  (tiny-setup-aput 'auto-mode-alist
		   "procmailrc"
		   "\\.rc\\'\\|^rc\\.\\|procmailrc"
		   'turn-on-tinyprocmail-mode))

(defun tiny-setup-tinyreplace-bind ()
  "Bind."
  ;;#todo:
  (ti::use-prefix-key global-map "\C-z")
  (tiny-setup-define-key "\C-z5" global-map 'tinyreplace-menu))

(defun tiny-setup-tinyreplace-bindemacs ()
  "Bind. Replace M-%"
  (tiny-setup-define-key [(meta ?%)]
			 global-map 'tinyreplace-menu
			 "Meta-%" 'force))


(defun tiny-setup-tinytag-autoload ()
  "Autoload."
  (tinytag-install-sample-databases)
  (add-hook 'java-mode-hook 'tinytag-install)
  (add-hook 'jde-mode-hook  'tinytag-install)
  (when (boundp 'cc-mode-hook)
    (add-hook 'cc-mode-hook 'tinytag-install))
  (add-hook 'c++-mode-hook  'tinytag-install)
  (add-hook 'c-mode-hook    'tinytag-install))

(defun tiny-setup-tinyvc-autoload ()
  "Autoload."
  ;;  This is bit tricky autoload setup, but it is the only way.
  ;;  Otherwise you would have to say (require 'tinyvc),
  ;;  which is not nice at all
  (defadvice vc-print-log (after tinyvc act)
    "Run hook `tinyvc-:vc-print-log-hook'."
    (require 'tinyvc)
    (run-hooks 'tinyvc-:vc-print-log-hook))
  (eval-after-load "vc" '(progn (require 'tinyvc))))

(defun tiny-setup-tinyrmail-autoload ()
  "Autoload."
  (add-hook 'rmail-mode-hook 'tinyrmail-install)
  (if (featurep 'rmail)
      (tinyrmail-install)))

(defun tiny-setup-tinysearch-bind (&optional force)
  "Bind."
  ;; In Win32 editors the F2 is de facto search key.
  ;;
  (tiny-setup-define-key [(f2)]
			 global-map 'tinysearch-search-word-forward
			 nil force)
  (tiny-setup-define-key [(shift f2)]
			 global-map 'tinysearch-search-word-backward
			 nil force))


(defun tiny-setup-tinysearch-bindforce ()
  "Bind."
  (tiny-setup-tinysearch-bind 'force))

(defun tiny-setup-tinysearch-bindmousealt ()
  "Bind."
  (tiny-setup-define-key [(alt control mouse-1)]
			 global-map 'tinysearch-search-word-forward)
  (tiny-setup-define-key [(alt control shift mouse-1)]
			 global-map 'tinysearch-search-word-backward))


(defun tiny-setup-tinysearch-bindmousemeta ()
  "Bind."
  (tiny-setup-define-key [(meta control mouse-1)]
			 global-map 'tinysearch-search-word-forward)
  (tiny-setup-define-key [(meta control shift mouse-1)]
			 global-map 'tinysearch-search-word-backward))


(defun tiny-setup-tinyscroll-autoload ()
  "Autoload."
  (unless (boundp 'compilation-scroll-output)
    (add-hook 'compilation-mode-hook
	      '(lambda () (require  'tinyscroll) nil))))

(defun tiny-setup-tinytab-bind (&optional force)
  "Bind."
  (tiny-setup-define-key "\C-cT"
			 global-map 'tinytab-mode "C-cT"
			 force)
  (tiny-setup-define-key "\C-c\C-m"
			 global-map 'tinytab-return-key-mode "C-cRET"
			 force))

(defun tiny-setup-tinytab-bindforce ()
  "Bind."
  (tiny-setup-tinytab-bind 'force))


(defun tiny-setup-tinyindent-bind (&optional force)
  "Bind."
  (tiny-setup-define-key "\C-cI"
			 global-map 'tinytab-mode "C-cT"
			 force))


(defun tiny-setup-tinyindent-bindforce ()
  "Bind."
  (tiny-setup-tinyindent-bind 'force))



(defun tiny-setup-tinytab-bindextra ()
  "Bind."
  ;;  make shift-TAB to toggle mode
  (tiny-setup-define-key [(control shift backtab)]
			 global-map 'tinytab-mode nil 'force)
  (tiny-setup-define-key [(control shift tab)]
			 global-map 'tinytab-mode nil 'force)
  (tiny-setup-define-key [(control shift kp-tab)]
			 global-map 'tinytab-mode nil 'force))

;;; .......................................................... &tinytf ...

(defun tiny-setup-turn-on-tinytf-mode-maybe ()
  "Turn on mode function `tinytf-mode' as needed."
  (let (case-fold-search)
    (cond
     ((memq 'turn-on-tinytf-mode-maybe find-file-hooks)
      ;;  tinytf is already loaded, remove ourself.
      (remove-hook 'find-file-hooks 'tiny-setup-turn-on-tinytf-mode-maybe))
     ((and (string-match "\\.txt"
			 (or (buffer-file-name) ""))
	   (or (re-search-forward
		"^Table [Oo]f [Cc]ontents[ \t]*$" nil t)
	       ;; See if we can find level 1 and 2 headings
	       ;;
	       ;; This Heading here
	       ;;
	       ;;     And This Heading here
	       ;;
	       (re-search-forward
		"^[0-9.]*[A-Z][^ \t\n]+.*[\r\n]+    [A-Z][^ \t\n]" nil t)
	       ;;  Try finding wro headers then
	       ;;
	       ;; This is Header
	       ;;
	       ;; And this is header
	       ;;
	       (and (re-search-forward
		     "^[0-9.]*[A-Z][^ \t\n][^ \t\n]+" nil t)
		    (re-search-forward
		     "^[0-9.]*[A-Z][^ \t\n][^ \t\n]+" nil t))))
      (turn-on-tinytf-mode)))
    ;;  Hook must return nil
    nil))

(defun tiny-setup-tinytf-autoload ()
  "Autoload."
  (add-hook 'find-file-hooks 'tiny-setup-turn-on-tinytf-mode-maybe))

;;; ......................................................... &tinyurl ...

(defun tiny-setup-tinyurl-mode-maybe ()
  "Turn on `tinyurl-mode' as needed."
  (if (featurep 'tinyurl)
      ;; TinyUrl has already set up the watchdog.
      (remove-hook 'find-file-hooks 'tiny-setup-tinyurl-mode-maybe)
    ;;  Use simplistic test here. TinyUrl has much better once it's active.
    (if (ti::re-search-check "[fh]t?tp://[a-z]+[a-z.]+")
	(turn-on-tinyurl-mode)))
  ;;  Hook is best to return nil
  nil)

(defun tiny-setup-tinyurl-autoload ()
  "Autoload."
  (add-hook 'find-file-hooks 'tiny-setup-tinyurl-mode-maybe))

(defun tiny-setup-tinyurl-bind ()
  "Bind."
  ;;*     (tiny-setup-define-key "\C-cmuu"  global-map 'tinyurl-mode)
  ;;*     (tiny-setup-define-key "\C-cmu1"  global-map 'tinyurl-mode-1)
  ;;*     (tiny-setup-define-key "\C-cmup"  global-map 'tinyurl-plugged-mode-toggle)
  nil)

(defun tiny-setup-tinyxreg-bind ()
  "Bind."
  (tiny-setup-define-key "\C-x/"
			 global-map 'tinyxreg-point-to-register "C-x/" 'force)
  (tiny-setup-define-key "\C-x\\"
			 global-map 'tinyxreg-remove-register "C-x\\")
  (tiny-setup-define-key "\C-cj"
			 global-map 'tinyxreg-jump-to-register "C-cj" ))

(provide   'tiny-setup)
(run-hooks 'tiny-setup-load-hook)

;;; tiny-setup.el ends here
