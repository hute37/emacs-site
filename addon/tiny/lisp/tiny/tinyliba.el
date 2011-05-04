;;; @(#) tinyliba.el --- Library for (a)utoload definitions
;;; @(#) $Id: tinyliba.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;;{{{ Id

;; Copyright (C)    1998-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1998-03
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinyliba-version
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
;; file and backward compatible library "b"
;;
;;     	(require 'tinylibm)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;	This is lisp function library, package itself does nothing.
;;	This library defines autoload functions and few emacs version
;;      detection functions.
;;
;;	The autoloads are automatically generated and you should not
;;	fix them by hand. To add or update autoloads from a package,
;;	do it like this:
;;
;;	o   Load library tinylisp.el
;;	o   Generate autoloads to separate buffer with
;;	    command C-u M-x tinylisp-autoload-generate-library
;;	o   At the end of buffer *tinylisp-autoloads* cut'n paste
;;	    the definititions to this file.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ code: Init

(provide 'tinyliba)

;; Older byte compiler's don't allow putting these inside
;; `eval-and-compile':
;;
;;   ** The compiler ignores `autoload' except at top level.  You should
;;      probably put the autoload of the macro `with-timeout' at top-level.

(autoload 'with-timeout	     "timer"	    "" nil 'macro)
(autoload 'easy-menu-define  "easymenu"	    "" nil 'macro)


(defun tinyliba-file-version (file)
  "Find Version: string from lisp FILE."
  (let* ((lib    (locate-library file))
	 (buffer (and lib (find-file-noselect lib)))
	 find-file-hooks
	     version)
    (save-excursion
      (if (null find-file-hooks)       ;; No-op, byte compiler silencer
	      (setq find-file-hooks nil))
      (set-buffer buffer)
      (goto-char (point-min))
	  (if (re-search-forward
	       "^;+[ \t]+Version:[ \t]+\\(.+\\)" nil t)
	      (setq version (match-string 1)))
	  (kill-buffer buffer)
	  version)))


(eval-and-compile

  (require 'cl)

  (autoload 'ti::directory-up "tinylib")
  (autoload 'executable-find  "executble")


  (unless (fboundp 'return)
    ;;  cl.el version 3.0 does not define macro `return' cl 2.02(19.34) is ok.
    ;;  This was noticed by Sami Khoury <skhoury@cse.dnd.ca>
    (let ((location (locate-library "cl"))
	  (version  (tinyliba-file-version "cl.el")))
      (error "\
** tinyliba.el: Your 'cl package [%s] is dysfunctional. Get some other version.
                After the (require 'cl), it dind't provide standard CL
                `return'. This may be a problem in your `load-path'.
                Re-arrange? The package `cl' was found at %s"
	     location version)))

  (require 'backquote)


  ;; ..................................................... &presettings ...


  ;; defvar silences Byte Compiler

  (defvar byte-compile-dynamic nil "")   ;; Introduced in 19.29
  (make-local-variable 'byte-compile-dynamic)
  (setq byte-compile-dynamic t)         ;19.29+


  ;; Predeclare functions for byte compiler, so that we can later use
  ;;
  ;; (eval-and-compile
  ;;   (when (emacs-p)
  ;;       ...))

  (defsubst xemacs-p (&optional version-string)
    "Check if running XEmacs. Optionally at least VERSION-STRING.
Version string is like  \"20.4\". Value t is returned if version of
emacs is equal or greater than VERSION-STRING."
    ;; `emacs-version' can't be modified, be bomb sure
    (when (string-match "xemacs" (emacs-version))
      ;;  These could be "mistakenly" get defined. Unlikely,
      ;;  but we won't even get here unless `emacs-version' say so.
      (if (or (boundp 'xemacs-logo)
	      (featurep 'xemacs))		;Appeared in 20.2+
	  (cond
	   ((null version-string)
	    emacs-version)
	   ((not (string< emacs-version version-string ))
	    emacs-version)))))


  (defsubst emacs-p (&optional version-string)
    "Check if running Emacs. Optionally at least VERSION-STRING.
Version string is like  \"20.4\". Value t is returned if version of
emacs is equal or greater than VERSION-STRING."
    (if (not (boundp 'xemacs-logo))
	(cond
	 ((null version-string)
	  emacs-version)
	 ((not (string< emacs-version version-string ))
	  emacs-version))))


  (defsubst emacs-version-number-as-string ()
    "Emacs and XEmacs compatibility. Return plain version number string."
    (if (emacs-p)
	emacs-version			; "19.34"
      ;; XEmacs return "19.14 XEmacs Lucid", get only version
      (and (string-match "^\\([0-9]+\\.[0-9.]+\\)" emacs-version)
	   (substring emacs-version 0 (match-end 1)) )))

  (defsubst emacs-version-number-as-string-major ()
    "Return major version number string. 20.4.1 --> 20.4"
    (and (string-match "^\\([0-9]+\\.[0-9]+\\)" emacs-version)
	 (substring emacs-version 0 (match-end 1))))


  ;;  Note: While Emacs would return 20.4.1 for version number,
  ;;  The installation directory is not emacs-20.4.1 but 20.4 for
  ;;  official releases.
  ;;
  ;;  Win32: (getenv "emacs_dir"))
  ;;  emacs_dir is one of the variables that are taken from
  ;;  the registry and mapped into the environment during startup
  ;;  of the emacs binary.
  ;;
  ;;  See also `invocation-directory', The directory in which the Emacs
  ;;  executable was found
  ;;
  ;;  See also `data-directory' Directory of machine-independent files that
  ;;  come with GNU Emacs. These are files intended for Emacs to use while
  ;;  it runs.

  (defun emacs-install-root ()
    "Return Emacs install ROOT by searching emacs version number from `load-path'."
    (let ((regexp
	   (concat
	    ".*" (regexp-quote (emacs-version-number-as-string-major))
	    "[.0-9]*"))
	  try
	  ret)
      (dolist (elt load-path)
	(when (and (stringp elt)
		   (string-match regexp elt)
		   (setq try (match-string 0 elt))
		   ;;  load-path may contain whatever directories, but
		   ;;  is it on disk too?
		   (file-directory-p (concat try "/lisp" )))
	  (setq ret try)
	  (return)))
      ret))

  (defun emacs-install-root-emacsen (binary)
    "Search `exec-path' to find BINARY (emacs,xemacs) install root."
    (let* ((bin (executable-find binary)))
      (when bin
	(ti::directory-up
	 (file-name-directory bin)))))

  (defun win32-p ()
    "Check if running under Win32 system."
    (cond
     ((memq system-type '(ms-dos windows-nt)))  ;; Emacs
     ((fboundp 'console-type)
      ;; Quiet Emacs byte compiler
      (memq (funcall (symbol-function 'console-type))
	    '(win32 w32 mswindows)))
     ((boundp 'window-system)
      (memq (symbol-value 'window-system) '(win32 w32 mswindows)))
     ((error "Internal alert, contact maintainer of TinyLib."))))


  ;;  Emacs function compatibility in XEmacs
  (unless (fboundp 'w32-system-shell-p)
    (defun w32-system-shell-p (shell-name)
      "Emacs an XEmacs compatibility."
      (or (and (fboundp 'w32-system-shell-p)
	       (w32-system-shell-p shell-name))
	  ;;  This is simplistic alternative if the above function
	  ;;  is not avilable.
	  (string-match "cmdproxy"
			(or shell-name "")))))

  (defun win32-shell-p ()
    "Check if shell filename is traditional win32 shell."
    (and (win32-p)
	 (w32-system-shell-p (or shell-file-name ""))))

  (defsubst win32-9x-p ()
    "Check windows 9x."
    (and (win32-p)
	 ;;#todo: Should use %SystemRoot% or something
	 ;;  User may have multiboot drive, where c: is not the right drive
	 (file-exists-p "c:/windows/command.com")))

  (defsubst win32-nt-p ()
    "Check windows NT."
    (or (and (fboundp 'w32-using-nt)
	     ;;  Emacs has this in w32-fns.el
	     (funcall (symbol-function 'w32-using-nt)))
	(let ((nt-root  (getenv "systemroot")))
	  (and nt-root
	       (win32-p)
	       (or (string-match "windows.*NT"  (or (getenv "OS") "" ))
		   (file-exists-p
		    (concat
		     (file-name-as-directory nt-root)
		     "system32/cnd.exe")))))))


  (defun win32-cygwin-p (&optional use-cache)
    "Return path if cygwin1.dll is found from `exec-path'.
If USE-CACHE is non-nil, retrieve cached value."
    (let (ret)
      (cond
       ((and use-cache
	     (get 'win32-cygwin-p 'cache-set))
	(get 'win32-cygwin-p 'cache-value))
       (t
	(put 'win32-cygwin-p 'cache-set t)
	(dolist (path exec-path)
	  (when (file-exists-p
		 (concat
		  (file-name-as-directory path) "cygwin1.dll"))
	    ;;  The root directory is one DIR. ../bin/cygwin1.dll
	    ;;
	    ;;  1) Drop the trailing slash  ../bin
	    ;;  2) Give one directory up    ..
	    ;;
	    ;;  We have to leave trailing slash, because the resulting
	    ;;  directory may be in the worst case C:/
	    ;;  (which is NOT recommended place for cygwin install)
	    ;;
	    (when (string-match "^\\(.*\\)[/\\]" path)
	      (setq path
		    (match-string 1 path))
	      (setq ret path)
	      (put 'win32-cygwin-p 'cache-value ret)
	      (return))))))
      ret))


  (defun turn-on-emacs-debug ()
    "Turn on Emacs or XEmacs debug."
    (interactive)
    (emacs-debug-mode 1))

  (defun turn-off-emacs-debug ()
    "Turn off Emacs or XEmacs debug."
    (interactive)
    (emacs-debug-mode 0))

  (defun emacs-debug-mode (&optional mode)
    "Toggle XEmacs/Emacs debug on and off."
    (interactive "P")

    ;;  The normal debug flag
    (cond
     ((null mode)
      (setq debug-on-error (not debug-on-error)))
     ((and (integerp mode) (> mode 0))
      (setq debug-on-error t))
     (t
      (setq debug-on-error nil)))

    (when (boundp 'debug-ignored-errors)
      (unless  (get 'debug-ignored-errors 'tinyliba)
	(put 'debug-ignored-errors 'tinyliba t)
	(put 'debug-ignored-errors 'tinyliba-saved debug-ignored-errors)))

    (cond
     (debug-on-error

      ;;   Emacs 20. You want to see all errors
      (when (boundp 'debug-ignored-errors)
	(set 'debug-ignored-errors nil))
      (setq debug-on-error t)

      ;;  Must be nil, otherwise it get's on your nervers
      ;;  too much when yo hit C-g to interrupt inputs.
      ;;  This only exists in New emacs releases.

      (if (boundp 'debug-on-quit)
	  (setq debug-on-quit nil))

      (if (boundp 'debug-on-signal);;  This must *not* be on!
	  (setq debug-on-signal nil))

      (if (boundp 'stack-trace-on-error)         ;; Xemacs
	  (set 'stack-trace-on-error t))
      (message "TinyLib: Emacs debug is ON"))
     (t
      (when (boundp 'debug-ignored-errors)
	(set 'debug-ignored-errors
	     (get 'debug-ignored-errors 'tinyliba-value)))
      (if (boundp 'stack-trace-on-error)         ;; Xemacs
	  (set 'stack-trace-on-error nil))
      (message "TinyLib: Emacs debug is OFF"))))


  )

;;}}}
;;{{{ code: utility functions

;; These are from SEMI::APEL::poe.el

(put 'defun-maybe    'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
	    (and (fboundp name)
		 (string-match
		  "autoload"
		  (prin1-to-string
		   (symbol-function name)))))
    (` (progn
	 (defun (, name) (,@ everything-else))
	 (put (quote (, name)) 'defun-maybe t)))))

(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
	    (and (fboundp name)
		 (string-match
		  "autoload"
		  (prin1-to-string
		   (symbol-function name)))))
    (` (progn
	 (defsubst (, name) (,@ everything-else))
	 (put (quote (, name)) 'defsubst-maybe t)))))


(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
	    (and (fboundp name)
		 (string-match
		  "autoload"
		  (prin1-to-string
		   (symbol-function name)))))
    (` (progn
	 (defmacro (, name) (,@ everything-else))
	 (put (quote (, name)) 'defmacro-maybe t)))))



(defmacro defalias-maybe (sym newdef)
  "Make defalias SYM if it does not exist and NEWDEF exists."
  (`
   (when (and (not (fboundp (, sym)))
	      (fboundp (, newdef)))
     (defalias (, sym) (, newdef)))))

(defmacro defconst-maybe (name &rest everything-else)
  (or (and (boundp name)
           (not (get name 'defconst-maybe)))
      (` (or (boundp (quote (, name)))
             (progn
               (defconst (, name) (,@ everything-else))
               (put (quote (, name)) 'defconst-maybe t))))))

;;}}}

(eval-and-compile

;; XEmacs and Emacs differ here

;; (if (locate-library "rsz-mini")
;;     (autoload 'resize-minibuffer-mode "rsz-mini")
;;   (autoload 'resize-minibuffer-mode "rsz-minibuf"))
;;

;;{{{ code: Autoload easymenu.el

;;  These are from XEmacs 19.14, they should suffice

(autoload 'easy-menu-do-define                  "easymenu" "" nil)
(autoload 'easy-menu-add                        "easymenu" "" nil)
(autoload 'easy-menu-remove                     "easymenu" "" nil)


;;; ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  .. Emacs 19.30  ..

;; (autoload 'easy-menu-define                     "easymenu" "" nil 'macro)
;; (autoload 'easy-menu-do-define                  "easymenu" "" t)
;; (autoload 'easy-menu-create-keymaps             "easymenu" "" nil)
;; (autoload 'easy-menu-change                     "easymenu" "" nil)
;; (autoload 'easy-menu-remove                     "easymenu" "" nil)
;; (autoload 'easy-menu-add                        "easymenu" "" nil)

;;}}}
;;{{{ code: Autoload skeleton.el

(autoload 'define-skeleton                      "skeleton" "" t 'macro)
(autoload 'skeleton-proxy-new                   "skeleton" "" t)
(autoload 'skeleton-proxy                       "skeleton" "" t)
(autoload 'skeleton-abbrev-cleanup              "skeleton" "" nil)
(autoload 'skeleton-insert                      "skeleton" "" nil)
(autoload 'skeleton-read                        "skeleton" "" nil)
(autoload 'skeleton-internal-list               "skeleton" "" nil)
(autoload 'skeleton-internal-1                  "skeleton" "" nil)
(autoload 'skeleton-pair-insert-maybe           "skeleton" "" t)

;;}}}
;;{{{ code: Autoload cl

;; cl-compat.el Emacs 19.34

(autoload 'defkeyword                           "cl-compat" "" nil 'macro)
(autoload 'keywordp                             "cl-compat" "" nil)
(autoload 'keyword-of                           "cl-compat" "" nil)
(autoload 'values                               "cl-compat" "" nil)
(autoload 'values-list                          "cl-compat" "" nil)
(autoload 'multiple-value-list                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-call                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-bind                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-setq                  "cl-compat" "" nil 'macro)
(autoload 'multiple-value-prog1                 "cl-compat" "" nil 'macro)
(autoload 'build-klist                          "cl-compat" "" nil)
(autoload 'extract-from-klist                   "cl-compat" "" nil)
(autoload 'keyword-argument-supplied-p          "cl-compat" "" nil)
(autoload 'elt-satisfies-test-p                 "cl-compat" "" nil)
(autoload 'cl-floor                             "cl-compat" "" nil)
(autoload 'cl-ceiling                           "cl-compat" "" nil)
(autoload 'cl-round                             "cl-compat" "" nil)
(autoload 'cl-truncate                          "cl-compat" "" nil)
(autoload 'safe-idiv                            "cl-compat" "" nil)
(autoload 'pair-with-newsyms                    "cl-compat" "" nil)
(autoload 'zip-lists                            "cl-compat" "" nil)
(autoload 'unzip-lists                          "cl-compat" "" nil)
(autoload 'reassemble-argslists                 "cl-compat" "" nil)
(autoload 'duplicate-symbols-p                  "cl-compat" "" nil)
(autoload 'setnth                               "cl-compat" "" nil)
(autoload 'setnthcdr                            "cl-compat" "" nil)
(autoload 'setelt                               "cl-compat" "" nil)

;; cl-extra.el 19.34

;; (autoload 'cl-push                              "cl-extra" "" nil 'macro)
;; (autoload 'cl-pop                               "cl-extra" "" nil 'macro)
(autoload 'coerce                               "cl-extra" "" nil)
(autoload 'equalp                               "cl-extra" "" nil)
(autoload 'cl-mapcar-many                       "cl-extra" "" nil)
(autoload 'map                                  "cl-extra" "" nil)
(autoload 'maplist                              "cl-extra" "" nil)
(autoload 'mapc                                 "cl-extra" "" nil)
(autoload 'mapl                                 "cl-extra" "" nil)
(autoload 'mapcan                               "cl-extra" "" nil)
(autoload 'mapcon                               "cl-extra" "" nil)
(autoload 'some                                 "cl-extra" "" nil)
(autoload 'every                                "cl-extra" "" nil)
(autoload 'notany                               "cl-extra" "" nil)
(autoload 'notevery                             "cl-extra" "" nil)
(autoload 'cl-map-keymap                        "cl-extra" "" nil)
(autoload 'cl-map-keymap-recursively            "cl-extra" "" nil)
(autoload 'cl-map-intervals                     "cl-extra" "" nil)
(autoload 'cl-map-overlays                      "cl-extra" "" nil)
(autoload 'cl-set-frame-visible-p               "cl-extra" "" nil)
(autoload 'cl-progv-before                      "cl-extra" "" nil)
(autoload 'cl-progv-after                       "cl-extra" "" nil)
(autoload 'gcd                                  "cl-extra" "" nil)
(autoload 'lcm                                  "cl-extra" "" nil)
(autoload 'isqrt                                "cl-extra" "" nil)
(autoload 'cl-expt                              "cl-extra" "" nil)
(autoload 'floor*                               "cl-extra" "" nil)
(autoload 'ceiling*                             "cl-extra" "" nil)
(autoload 'truncate*                            "cl-extra" "" nil)
(autoload 'round*                               "cl-extra" "" nil)
(autoload 'mod*                                 "cl-extra" "" nil)
(autoload 'rem*                                 "cl-extra" "" nil)
(autoload 'signum                               "cl-extra" "" nil)
(autoload 'random*                              "cl-extra" "" nil)
(autoload 'make-random-state                    "cl-extra" "" nil)
(autoload 'random-state-p                       "cl-extra" "" nil)
(autoload 'cl-finite-do                         "cl-extra" "" nil)
(autoload 'cl-float-limits                      "cl-extra" "" nil)
(autoload 'subseq                               "cl-extra" "" nil)
(autoload 'concatenate                          "cl-extra" "" nil)
(autoload 'revappend                            "cl-extra" "" nil)
(autoload 'nreconc                              "cl-extra" "" nil)
(autoload 'list-length                          "cl-extra" "" nil)
(autoload 'tailp                                "cl-extra" "" nil)
(autoload 'cl-copy-tree                         "cl-extra" "" nil)
(autoload 'get*                                 "cl-extra" "" nil)
(autoload 'getf                                 "cl-extra" "" nil)
(autoload 'cl-set-getf                          "cl-extra" "" nil)
(autoload 'cl-do-remf                           "cl-extra" "" nil)
(autoload 'cl-remprop                           "cl-extra" "" nil)
(autoload 'make-hash-table                      "cl-extra" "" nil)
(autoload 'hash-table-p                         "cl-extra" "" nil)
(autoload 'cl-not-hash-table                    "cl-extra" "" nil)
(autoload 'cl-hash-lookup                       "cl-extra" "" nil)
(autoload 'cl-gethash                           "cl-extra" "" nil)
(autoload 'cl-puthash                           "cl-extra" "" nil)
(autoload 'cl-remhash                           "cl-extra" "" nil)
(autoload 'cl-clrhash                           "cl-extra" "" nil)
(autoload 'cl-maphash                           "cl-extra" "" nil)
(autoload 'hash-table-count                     "cl-extra" "" nil)
(autoload 'cl-prettyprint                       "cl-extra" "" nil)
(autoload 'cl-do-prettyprint                    "cl-extra" "" nil)
(autoload 'cl-macroexpand-all                   "cl-extra" "" nil)
(autoload 'cl-macroexpand-body                  "cl-extra" "" nil)
(autoload 'cl-prettyexpand                      "cl-extra" "" nil)


;; cl-seq.el 19.34
;; Hm. Sometimemes you find this message:
;;    "Tried to load `cl-seq' before `cl'!"
;;
;; These are commented for now

(when nil

(autoload 'cl-push                              "cl-seq" "" nil 'macro)
(autoload 'cl-pop                               "cl-seq" "" nil 'macro)
(autoload 'cl-parsing-keywords                  "cl-seq" "" nil 'macro)
(autoload 'cl-check-key                         "cl-seq" "" nil 'macro)
(autoload 'cl-check-test-nokey                  "cl-seq" "" nil 'macro)
(autoload 'cl-check-test                        "cl-seq" "" nil 'macro)
(autoload 'cl-check-match                       "cl-seq" "" nil 'macro)
(autoload 'reduce                               "cl-seq" "" nil)
(autoload 'fill                                 "cl-seq" "" nil)
(autoload 'replace                              "cl-seq" "" nil)
(autoload 'remove*                              "cl-seq" "" nil)
(autoload 'remove-if                            "cl-seq" "" nil)
(autoload 'remove-if-not                        "cl-seq" "" nil)
(autoload 'delete*                              "cl-seq" "" nil)
(autoload 'delete-if                            "cl-seq" "" nil)
(autoload 'delete-if-not                        "cl-seq" "" nil)
(autoload 'remove                               "cl-seq" "" nil)
(autoload 'remq                                 "cl-seq" "" nil)
(autoload 'remove-duplicates                    "cl-seq" "" nil)
(autoload 'delete-duplicates                    "cl-seq" "" nil)
(autoload 'cl-delete-duplicates                 "cl-seq" "" nil)
(autoload 'substitute                           "cl-seq" "" nil)
(autoload 'substitute-if                        "cl-seq" "" nil)
(autoload 'substitute-if-not                    "cl-seq" "" nil)
(autoload 'nsubstitute                          "cl-seq" "" nil)
(autoload 'nsubstitute-if                       "cl-seq" "" nil)
(autoload 'nsubstitute-if-not                   "cl-seq" "" nil)
(autoload 'find                                 "cl-seq" "" nil)
(autoload 'find-if                              "cl-seq" "" nil)
(autoload 'find-if-not                          "cl-seq" "" nil)
(autoload 'position                             "cl-seq" "" nil)
(autoload 'cl-position                          "cl-seq" "" nil)
(autoload 'position-if                          "cl-seq" "" nil)
(autoload 'position-if-not                      "cl-seq" "" nil)
(autoload 'count                                "cl-seq" "" nil)
(autoload 'count-if                             "cl-seq" "" nil)
(autoload 'count-if-not                         "cl-seq" "" nil)
(autoload 'mismatch                             "cl-seq" "" nil)
(autoload 'search                               "cl-seq" "" nil)
(autoload 'sort*                                "cl-seq" "" nil)
(autoload 'stable-sort                          "cl-seq" "" nil)
(autoload 'merge                                "cl-seq" "" nil)
(autoload 'member*                              "cl-seq" "" nil)
(autoload 'member-if                            "cl-seq" "" nil)
(autoload 'member-if-not                        "cl-seq" "" nil)
(autoload 'cl-adjoin                            "cl-seq" "" nil)
(autoload 'assoc*                               "cl-seq" "" nil)
(autoload 'assoc-if                             "cl-seq" "" nil)
(autoload 'assoc-if-not                         "cl-seq" "" nil)
(autoload 'rassoc*                              "cl-seq" "" nil)
(autoload 'rassoc-if                            "cl-seq" "" nil)
(autoload 'rassoc-if-not                        "cl-seq" "" nil)
(autoload 'union                                "cl-seq" "" nil)
(autoload 'nunion                               "cl-seq" "" nil)
(autoload 'intersection                         "cl-seq" "" nil)
(autoload 'nintersection                        "cl-seq" "" nil)
(autoload 'set-difference                       "cl-seq" "" nil)
(autoload 'nset-difference                      "cl-seq" "" nil)
(autoload 'set-exclusive-or                     "cl-seq" "" nil)
(autoload 'nset-exclusive-or                    "cl-seq" "" nil)
(autoload 'subsetp                              "cl-seq" "" nil)
(autoload 'subst-if                             "cl-seq" "" nil)
(autoload 'subst-if-not                         "cl-seq" "" nil)
(autoload 'nsubst                               "cl-seq" "" nil)
(autoload 'nsubst-if                            "cl-seq" "" nil)
(autoload 'nsubst-if-not                        "cl-seq" "" nil)
(autoload 'sublis                               "cl-seq" "" nil)
(autoload 'cl-sublis-rec                        "cl-seq" "" nil)
(autoload 'nsublis                              "cl-seq" "" nil)
(autoload 'cl-nsublis-rec                       "cl-seq" "" nil)
(autoload 'tree-equal                           "cl-seq" "" nil)
(autoload 'cl-tree-equal-rec                    "cl-seq" "" nil)


;; cl-indent.el 19.34

(autoload 'common-lisp-indent-function          "cl-indent" "" nil)
(autoload 'lisp-indent-report-bad-format        "cl-indent" "" nil)
(autoload 'lisp-indent-259                      "cl-indent" "" nil)
(autoload 'lisp-indent-tagbody                  "cl-indent" "" nil)
(autoload 'lisp-indent-do                       "cl-indent" "" nil)
(autoload 'lisp-indent-function-lambda-hack     "cl-indent" "" nil)

) ;; when-nil


;; assoc.el 20.4

(autoload 'asort                                "assoc" "" nil)
(autoload 'aelement                             "assoc" "" nil)
(autoload 'aheadsym                             "assoc" "" nil)
(autoload 'anot-head-p                          "assoc" "" nil)
(autoload 'aput                                 "assoc" "" nil)
(autoload 'adelete                              "assoc" "" nil)
(autoload 'aget                                 "assoc" "" nil)
(autoload 'amake                                "assoc" "" nil)


;;}}}
;;{{{ code: Autoload 'main' lib

;;; ........................................................ &autoload ...

;; tinylibm.el

(autoload 'tinylibm-version                     "tinylibm" "" t)
(autoload 'tinylibm-submit-bug-report           "tinylibm" "" t)
(autoload 'definteractive                       "tinylibm" "" t 'macro)
(autoload 'ti::fboundp-check-autoload           "tinylibm" "" nil 'macro)
(autoload 'ti::narrow-safe                      "tinylibm" "" nil 'macro)
(autoload 'ti::nconc                            "tinylibm" "" nil 'macro)
(autoload 'ti::consp                            "tinylibm" "" nil);;defsubst
(autoload 'ti::listp                            "tinylibm" "" nil);;defsubst
(autoload 'ti::when-package                     "tinylibm" "" nil 'macro)
(autoload 'ti::with-require                     "tinylibm" "" nil 'macro)
(autoload 'ti::with-time-this                   "tinylibm" "" nil 'macro)
(autoload 'ti::with-coding-system-raw-text      "tinylibm" "" nil 'macro)
(autoload 'ti::process-mark                     "tinylibm" "" nil);;defsubst
(autoload 'ti::verb                             "tinylibm" "" t 'macro)
(autoload 'ti::pmin                             "tinylibm" "" nil);;defsubst
(autoload 'ti::pmax                             "tinylibm" "" nil);;defsubst
(autoload 'int-to-float                         "tinylibm" "" nil 'macro)
(autoload 'ti::dotimes                          "tinylibm" "" nil 'macro)
(autoload 'ti::funcall                          "tinylibm" "" nil 'macro)
(autoload 'logtest                              "tinylibm" "" nil)
(autoload 'bin-string-to-int                    "tinylibm" "" nil)
(autoload 'int-to-bin-string                    "tinylibm" "" nil)
(autoload 'int-to-hex-string                    "tinylibm" "" nil)
(autoload 'int-to-oct-string                    "tinylibm" "" nil)
(autoload 'radix                                "tinylibm" "" nil)
(autoload 'bin-to-int                           "tinylibm" "" nil)
(autoload 'oct-to-int                           "tinylibm" "" nil)
(autoload 'hex-to-int                           "tinylibm" "" nil)
(autoload 'int-to-net                           "tinylibm" "" nil)
(autoload 'rmac                                 "tinylibm" "" nil)
(autoload 'ctime                                "tinylibm" "" nil)
(autoload 'rand0                                "tinylibm" "" nil);;defsubst
(autoload 'rand1                                "tinylibm" "" nil)
(autoload 'randij                               "tinylibm" "" nil)
(autoload 'ti::string-value                     "tinylibm" "" nil)
(autoload 'ti::prin1-mapconcat                  "tinylibm" "" nil)
(autoload 'ti::d!                               "tinylibm" "" nil 'macro)
(autoload 'ti::d!!                              "tinylibm" "" nil 'macro)
(autoload 'ti::string-left                      "tinylibm" "" nil);;defsubst
(autoload 'ti::string-right                     "tinylibm" "" nil);;defsubst
(autoload 'ti::string-match-case                "tinylibm" "" nil);;defsubst
(autoload 'ti::month-list                       "tinylibm" "" nil);;defsubst
(autoload 'ti::month-list-regexp                "tinylibm" "" nil)
(autoload 'ti::month-mm-alist                   "tinylibm" "" nil);;defsubst
(autoload 'ti::month-nn-alist                   "tinylibm" "" nil);;defsubst
(autoload 'ti::month-to-number                  "tinylibm" "" nil);;defsubst
(autoload 'ti::month-to-0number                 "tinylibm" "" nil);;defsubst
(autoload 'ti::number-to-month                  "tinylibm" "" nil);;defsubst
(autoload 'ti::date-eu-list                     "tinylibm" "" nil);;defsubst
(autoload 'ti::date-us-list                     "tinylibm" "" nil);;defsubst
(autoload 'ti::date-list-regexp                 "tinylibm" "" nil)
(autoload 'ti::read-char-safe                   "tinylibm" "" nil);;defsubst
(autoload 'ti::read-char-safe-until             "tinylibm" "" nil)
(autoload 'ti::remove-properties                "tinylibm" "" nil);;defsubst
(autoload 'applycar                             "tinylibm" "" nil 'macro)
(autoload 'ti::add-command-line-arg             "tinylibm" "" nil);;defsubst
(autoload 'ti::buffer-modified-p                "tinylibm" "" nil);;defsubst
(autoload 'ti::first-line-p                     "tinylibm" "" nil);;defsubst
(autoload 'ti::last-line-p                      "tinylibm" "" nil);;defsubst
(autoload 'ti::buffer-narrowed-p                "tinylibm" "" nil);;defsubst
(autoload 'ti::empty-buffer-p                   "tinylibm" "" nil)
(autoload 'ti::ck-maybe-activate                "tinylibm" "" nil)
(autoload 'ti::register-live-p                  "tinylibm" "" nil);;defsubst
(autoload 'ti::dos-file-p                       "tinylibm" "" nil);;defsubst
(autoload 'ti::space-p                          "tinylibm" "" nil);;defsubst
(autoload 'ti::xe-face-p                        "tinylibm" "" nil)
(autoload 'ti::color-type                       "tinylibm" "" nil)
(autoload 'ti::colors-supported-p               "tinylibm" "" nil)
(autoload 'font-lock-mode-maybe                 "tinylibm" "" nil)
(autoload 'turn-on-font-lock-mode-maybe         "tinylibm" "" nil)
(autoload 'ti::overlay-supported-p              "tinylibm" "" nil)
(autoload 'ti::idle-timer-supported-p           "tinylibm" "" nil)
(autoload 'ti::replace-match                    "tinylibm" "" nil)
(autoload 'ti::kill-control-characters          "tinylibm" "" t);;defsubst
(autoload 'ti::string-match                     "tinylibm" "" nil);;defsubst
(autoload 'ti::buffer-match                     "tinylibm" "" nil);;defsubst
(autoload 'ti::selective-display-line-p         "tinylibm" "" nil);;defsubst
(autoload 'ti::bool-p                           "tinylibm" "" nil);;defsubst
(autoload 'ti::print-p                          "tinylibm" "" nil 'macro)
(autoload 'ti::char-case-p                      "tinylibm" "" nil)
(autoload 'ti::nil-p                            "tinylibm" "" nil);;defsubst
(autoload 'ti::window-pmin-visible-p            "tinylibm" "" nil);;defsubst
(autoload 'ti::window-pmax-visible-p            "tinylibm" "" nil 'macro)
(autoload 'ti::window-pmax-line-p               "tinylibm" "" nil)
(autoload 'ti::window-pmin-line-p               "tinylibm" "" nil);;defsubst
(autoload 'ti::window-pmax-line-bol             "tinylibm" "" nil)
(autoload 'ti::window-middle-line               "tinylibm" "" nil)
(autoload 'ti::no-action-in-progress-p          "tinylibm" "" nil)
(autoload 'ti::current-line-number              "tinylibm" "" nil)
(autoload 'ti::read-current-line                "tinylibm" "" nil);;defsubst
(autoload 'ti::line-length                      "tinylibm" "" nil);;defsubst
(autoload 'ti::line-wrap-p                      "tinylibm" "" nil);;defsubst
(autoload 'ti::re-search-check                  "tinylibm" "" nil)
(autoload 'ti::re-search-point-list             "tinylibm" "" nil)
(autoload 'ti::assoc-append-inside              "tinylibm" "" nil 'macro)
(autoload 'ti::assoc-replace-maybe-add          "tinylibm" "" nil)
(autoload 'ti::list-make                        "tinylibm" "" nil);;defsubst
(autoload 'ti::list-flatten                     "tinylibm" "" nil)
(autoload 'ti::list-join                        "tinylibm" "" nil)
(autoload 'ti::list-to-assoc-menu               "tinylibm" "" nil)
(autoload 'ti::list-to-cons                     "tinylibm" "" nil);;defsubst
(autoload 'ti::list-remove-successive           "tinylibm" "" nil)
(autoload 'ti::list-merge-elements              "tinylibm" "" nil)
(autoload 'ti::list-print                       "tinylibm" "" t)
(autoload 'ti::list-to-string                   "tinylibm" "" nil);;defsubst
(autoload 'ti::list-elt-position                "tinylibm" "" nil)
(autoload 'ti::list-find                        "tinylibm" "" nil)
(autoload 'ti::non-dedicated-frame              "tinylibm" "" nil);;defsubst
(autoload 'ti::select-frame-non-dedicated       "tinylibm" "" nil);;defsubst
(autoload 'ti::byte-compile-defun-compiled-p    "tinylibm" "" nil 'macro)
(autoload 'ti::byte-compile-running-p           "tinylibm" "" nil 'macro)
(autoload 'ti::byte-compile-defun-maybe         "tinylibm" "" nil 'macro)
(autoload 'ti::package-use-dynamic-compilation  "tinylibm" "" nil 'macro)
(autoload 'ti::function-autoload-file           "tinylibm" "" nil)
(autoload 'ti::package-require-for-emacs        "tinylibm" "" nil 'macro)
(autoload 'ti::package-require-view             "tinylibm" "" nil 'macro)
(autoload 'ti::package-package-require-timer    "tinylibm" "" nil 'macro)
(autoload 'ti::package-require-mail-abbrevs     "tinylibm" "" nil 'macro)
(autoload 'ti::use-file-compression             "tinylibm" "" nil 'macro)
(autoload 'ti::use-file-compression-maybe       "tinylibm" "" nil)
(autoload 'ti::push-definition                  "tinylibm" "" nil)
(autoload 'ti::pop-definition                   "tinylibm" "" nil)
(autoload 'ti::use-prefix-key                   "tinylibm" "" nil);;defsubst
(autoload 'ti::swap-keys-if-not-keymap          "tinylibm" "" nil)
(autoload 'ti::define-buffer-local-keymap       "tinylibm" "" nil 'macro)
(autoload 'ti::define-key-if-free               "tinylibm" "" nil 'macro)
(autoload 'ti::define-in-function-keymap        "tinylibm" "" nil)
(autoload 'ti::copy-key-definition              "tinylibm" "" nil 'macro)
(autoload 'ti::beginning-of-defun-point         "tinylibm" "" nil);;defsubst
(autoload 'ti::digit-length                     "tinylibm" "" nil);;defsubst
(autoload 'ti::add-hook-fix                     "tinylibm" "" nil)
(autoload 'ti::add-hooks                        "tinylibm" "" nil)
(autoload 'subst-char-with-string               "tinylibm" "" nil)
(autoload 'ti::prefix-arg-to-text               "tinylibm" "" nil)
(autoload 'ti::keep-lower-order                 "tinylibm" "" nil 'macro)
(autoload 'ti::bool-toggle                      "tinylibm" "" nil 'macro)
(autoload 'ti::xe-load-user-init-file           "tinylibm" "" nil 'macro)
(autoload 'ti::xe-Info-directory-list           "tinylibm" "" nil 'macro)
(autoload 'ti::buffer-pointer-of-info           "tinylibm" "" nil)
(autoload 'ti::buffer-pointer-of-messages       "tinylibm" "" nil)
(autoload 'ti::last-message-line                "tinylibm" "" nil)
(autoload 'ti::dolist-buffer-list               "tinylibm" "" nil 'macro)
(autoload 'ti::erase-buffer                     "tinylibm" "" nil)
(autoload 'ti::temp-buffer                      "tinylibm" "" nil)
(autoload 'ti::append-to-buffer                 "tinylibm" "" nil);;defsubst
(autoload 'ti::set-buffer-safe                  "tinylibm" "" nil)
(autoload 'ti::kill-buffer-safe                 "tinylibm" "" nil)
(autoload 'cl-clrhash-paranoid                  "tinylibm" "" nil)
(autoload 'ti::vector-table-init                "tinylibm" "" nil 'macro)
(autoload 'ti::vector-table-get                 "tinylibm" "" nil 'macro)
(autoload 'ti::vector-table-property            "tinylibm" "" nil)
(autoload 'ti::vector-table-clear               "tinylibm" "" nil 'macro)
(autoload 'ti::expand-file-name-tilde-in-string "tinylibm" "" nil)
(autoload 'ti::file-name-path-p                 "tinylibm" "" nil);;defsubst
(autoload 'ti::directory-move                   "tinylibm" "" nil)
(autoload 'ti::write-file-with-wrapper          "tinylibm" "" nil)
(autoload 'ti::load-file-with-wrapper           "tinylibm" "" nil 'macro)
(autoload 'ti::write-file-as-is-macro           "tinylibm" "" nil 'macro)
(autoload 'ti::file-name-remote-p               "tinylibm" "" nil);;defsubst
(autoload 'ti::file-name-backward-slashes       "tinylibm" "" nil)
(autoload 'ti::file-name-forward-slashes        "tinylibm" "" nil);;defsubst
(autoload 'ti::file-name-forward-slashes-cygwin "tinylibm" "" nil);;defsubst
(autoload 'ti::file-changed-on-disk-p           "tinylibm" "" nil);;defsubst
(autoload 'ti::file-mode-make-read-only         "tinylibm" "" nil);;defsubst
(autoload 'ti::file-mode-make-read-only-all     "tinylibm" "" nil);;defsubst
(autoload 'ti::file-mode-make-writable          "tinylibm" "" nil);;defsubst
(autoload 'ti::file-mode-make-executable        "tinylibm" "" nil);;defsubst
(autoload 'ti::file-mode-protect                "tinylibm" "" t);;defsubst
(autoload 'ti::file-toggle-read-write           "tinylibm" "" nil);;defsubst
(autoload 'ti::file-owned-p                     "tinylibm" "" nil);;defsubst
(autoload 'ti::file-modify-p                    "tinylibm" "" nil);;defsubst
(autoload 'ti::file-find-file-p                 "tinylibm" "" nil);;defsubst
(autoload 'ti::file-read-only-p                 "tinylibm" "" nil);;defsubst
(autoload 'ti::file-name-run-real-handler       "tinylibm" "" nil)
(autoload 'ti::find-file-literally              "tinylibm" "" t)
(autoload 'ti::file-eval                        "tinylibm" "" nil)
(autoload 'ti::directory-writable-p             "tinylibm" "" nil)
(autoload 'ti::file-delete-safe                 "tinylibm" "" nil)
(autoload 'ti::temp-directory                   "tinylibm" "" nil)
(autoload 'ti::temp-file                        "tinylibm" "" nil)
(autoload 'ti::pop-to-buffer-or-window          "tinylibm" "" nil)
(autoload 'ti::find-file-or-window              "tinylibm" "" nil)
(autoload 'ti::mouse-point                      "tinylibm" "" nil);;defsubst
(autoload 'ti::i-macro-region-ask               "tinylibm" "" nil);;defsubst
(autoload 'ti::i-macro-region-body              "tinylibm" "" nil 'macro)
(autoload 'ti::save-buffer-modified             "tinylibm" "" nil 'macro)
(autoload 'ti::package-defgroup-tiny            "tinylibm" "" nil 'macro)
(autoload 'ti::package-tiny-defgroup-mail       "tinylibm" "" nil)
(autoload 'ti::occur-macro                      "tinylibm" "" nil 'macro)
(autoload 'shell-command-to-string              "tinylibm" "" nil)
(autoload 'ti::momentary-output-macro           "tinylibm" "" nil 'macro)
(autoload 'ti::save-excursion-macro             "tinylibm" "" nil 'macro)
(autoload 'ti::save-with-marker-macro           "tinylibm" "" nil 'macro)
(autoload 'ti::save-line-column-macro           "tinylibm" "" nil 'macro)
(autoload 'ti::widen-safe                       "tinylibm" "" nil 'macro)
(autoload 'ti::package-config-file-directory-default "tinylibm" "" nil)
(autoload 'ti::package-config-file-prefix       "tinylibm" "" nil)
(autoload 'ti::overlay-require-macro            "tinylibm" "" nil 'macro)
(autoload 'ti::pp-variable-list                 "tinylibm" "" nil)
(autoload 'ti::write-file-variable-state        "tinylibm" "" nil)

;;; tinylib.el

(autoload 'tinylib-version                      "tinylib" "" t)
(autoload 'tinylib-submit-feedback              "tinylib" "" t)
(autoload 'ti::string-trim-blanks               "tinylib" "" nil)
(autoload 'ti::string-verify-ends               "tinylib" "" nil)
(autoload 'ti::string-add-space                 "tinylib" "" nil);;defsubst
(autoload 'ti::string-remove-whitespace         "tinylib" "" nil)
(autoload 'ti::string-mangle                    "tinylib" "" nil)
(autoload 'ti::string-regexp-delete             "tinylib" "" nil);;defsubst
(autoload 'ti::string-format-percent            "tinylib" "" nil)
(autoload 'ti::string-url-to-ange-ftp           "tinylib" "" nil)
(autoload 'ti::buffer-upcase-words-to-variable-names "tinylib" "" t)
(autoload 'ti::string-nth-from-number           "tinylib" "" nil);;defsubst
(autoload 'ti::date-time-elements               "tinylib" "" t)
(autoload 'ti::string-char-to-escape-char       "tinylib" "" nil)
(autoload 'ti::string-plain-string-to-regexp    "tinylib" "" nil)
(autoload 'ti::file-access-mode-to-string       "tinylib" "" nil)
(autoload 'ti::vc-rcs-delta-get-revisions       "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-delta-get-file            "tinylib" "" nil)
(autoload 'ti::vc-rcs-delta-lock-status         "tinylib" "" nil)
(autoload 'ti::vc-rcs-delta-lock-status-user    "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-delta-highest-version     "tinylib" "" t);;defsubst
(autoload 'ti::vc-rcs-read-val                  "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-look-id                   "tinylib" "" nil)
(autoload 'ti::vc-cvs-to-cvs-dir                "tinylib" "" nil);;defsubst
(autoload 'ti::vc-cvs-to-cvs-dir-p              "tinylib" "" nil);;defsubst
(autoload 'ti::vc-cvs-to-cvs-file               "tinylib" "" nil)
(autoload 'ti::vc-cvs-to-cvs-file-content       "tinylib" "" nil)
(autoload 'ti::vc-cvs-file-exists-p             "tinylib" "" nil)
(autoload 'ti::vc-cvs-entry-split               "tinylib" "" nil);;defsubst
(autoload 'ti::vc-cvs-entry-type                "tinylib" "" nil);;defsubst
(autoload 'ti::vc-cvs-entry-split-info          "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-file-p                    "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-make-filename             "tinylib" "" nil)
(autoload 'ti::vc-rcs-file-exists-p             "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-normal-file               "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-sort-same-level-list      "tinylib" "" nil)
(autoload 'ti::vc-rcs-files-in-dir              "tinylib" "" nil)
(autoload 'ti::vc-rcs-head-version              "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-guess-buffer-version      "tinylib" "" nil)
(autoload 'ti::vc-rcs-buffer-version            "tinylib" "" nil)
(autoload 'ti::vc-rcs-rlog-get-revisions        "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-all-versions              "tinylib" "" nil);;defsubst
(autoload 'ti::vc-rcs-previous-version          "tinylib" "" nil)
(autoload 'ti::vc-rcs-get-all-branches          "tinylib" "" nil)
(autoload 'ti::vc-rcs-str-find                  "tinylib" "" nil)
(autoload 'ti::vc-rcs-str-find-buffer           "tinylib" "" nil);;defsubst
(autoload 'ti::date-standard-date               "tinylib" "" t)
(autoload 'ti::date-month-to-number             "tinylib" "" t)
(autoload 'ti::date-time-difference             "tinylib" "" nil)
(autoload 'ti::date-time-diff-days              "tinylib" "" nil)
(autoload 'ti::date-parse-date                  "tinylib" "" nil)
(autoload 'ti::string-repeat                    "tinylib" "" nil)
(autoload 'ti::string-syntax-info               "tinylib" "" t)
(autoload 'ti::string-syntax-kill-double-quote  "tinylib" "" t)
(autoload 'ti::string-tabify                    "tinylib" "" nil)
(autoload 'ti::string-match-string-subs         "tinylib" "" nil)
(autoload 'ti::string-match-string-list         "tinylib" "" nil)
(autoload 'ti::string-case-replace              "tinylib" "" nil)
(autoload 'ti::string-index                     "tinylib" "" nil)
(autoload 'ti::string-index-substring           "tinylib" "" nil)
(autoload 'ti::string-1-space                   "tinylib" "" nil)
(autoload 'ti::string-listify                   "tinylib" "" nil)
(autoload 'ti::buffer-get-ange-buffer-list      "tinylib" "" nil);;defsubst
(autoload 'ti::buffer-find-ange-buffer          "tinylib" "" nil)
(autoload 'ti::buffer-find-ange-to-dired-buffer "tinylib" "" nil)
(autoload 'ti::buffer-uu-area                   "tinylib" "" nil)
(autoload 'ti::buffer-uu-line-p                 "tinylib" "" t)
(autoload 'ti::buffer-area-bounds               "tinylib" "" nil)
(autoload 'ti::buffer-join-region               "tinylib" "" t)
(autoload 'ti::buffer-read-if-solid             "tinylib" "" nil)
(autoload 'ti::buffer-read-whitespace           "tinylib" "" nil)
(autoload 'ti::buffer-read-line                 "tinylib" "" nil)
(autoload 'ti::buffer-grep-lines                "tinylib" "" nil)
(autoload 'ti::buffer-looking-back-at           "tinylib" "" nil)
(autoload 'ti::buffer-read-char                 "tinylib" "" nil)
(autoload 'ti::buffer-read-word                 "tinylib" "" nil)
(autoload 'ti::buffer-read-space-word           "tinylib" "" nil)
(autoload 'ti::buffer-read-syntax-word          "tinylib" "" nil)
(autoload 'ti::buffer-read-nth-word             "tinylib" "" nil)
(autoload 'ti::buffer-replace-keywords-with-table "tinylib" "" t)
(autoload 'ti::buffer-replace-region-with       "tinylib" "" nil);;defsubst
(autoload 'ti::buffer-zap-to-regexp             "tinylib" "" t)
(autoload 'ti::buffer-leave-nth-word            "tinylib" "" t)
(autoload 'ti::buffer-kill-line                 "tinylib" "" t)
(autoload 'ti::buffer-strip-control-m           "tinylib" "" nil)
(autoload 'ti::buffer-lf-to-crlf                "tinylib" "" t)
(autoload 'ti::buffer-arrow-control             "tinylib" "" nil)
(autoload 'ti::buffer-insert-line-numbers       "tinylib" "" t)
(autoload 'ti::buffer-remove-line-numbers       "tinylib" "" t);;defsubst
(autoload 'ti::buffer-randomize-lines           "tinylib" "" t)
(autoload 'ti::buffer-make-dup-line             "tinylib" "" t)
(autoload 'ti::buffer-inc-string-nbr            "tinylib" "" t)
(autoload 'ti::buffer-copy-line-and-inc-numbers "tinylib" "" t)
(autoload 'ti::buffer-copy-word                 "tinylib" "" t)
(autoload 'ti::buffer-add-newlines-to-region    "tinylib" "" t)
(autoload 'ti::buffer-cnv-empty-lines           "tinylib" "" t)
(autoload 'ti::buffer-del-dup-lines             "tinylib" "" t)
(autoload 'ti::buffer-delete-until-non-empty-line "tinylib" "" t)
(autoload 'ti::buffer-trim-blanks               "tinylib" "" t)
(autoload 'ti::buffer-replace-regexp            "tinylib" "" nil)
(autoload 'ti::buffer-diff-type-p               "tinylib" "" nil)
(autoload 'ti::buffer-xtra-open-outline         "tinylib" "" t)
(autoload 'ti::buffer-buffer-list-files         "tinylib" "" nil)
(autoload 'ti::buffer-count-words               "tinylib" "" t)
(autoload 'ti::buffer-count-chars-in-delimited-area "tinylib" "" t)
(autoload 'ti::buffer-word-move                 "tinylib" "" t)
(autoload 'ti::buffer-find-duplicate-same-word  "tinylib" "" t)
(autoload 'ti::buffer-move-paragraph-to-column  "tinylib" "" t)
(autoload 'ti::buffer-move-to-col               "tinylib" "" t);;defsubst
(autoload 'ti::buffer-selective-display-copy-to "tinylib" "" t)
(autoload 'ti::buffer-selective-display-print   "tinylib" "" t)
(autoload 'ti::window-frame-list                "tinylib" "" nil)
(autoload 'ti::window-list                      "tinylib" "" nil)
(autoload 'ti::window-get-buffer-window-other-frame "tinylib" "" nil)
(autoload 'ti::window-find-bottom               "tinylib" "" nil)
(autoload 'ti::window-match-buffers             "tinylib" "" nil)
(autoload 'ti::keymap-single-key-definition-p   "tinylib" "" nil)
(autoload 'ti::keymap-define-key-backspace      "tinylib" "" t)
(autoload 'ti::keymap-function-bind-info        "tinylib" "" nil)
(autoload 'ti::keymap-reinstall-minor-mode      "tinylib" "" nil)
(autoload 'ti::keymap-add-minor-mode            "tinylib" "" nil)
(autoload 'ti::keymap-bind-control              "tinylib" "" nil)
(autoload 'ti::keymap-xlat-table                "tinylib" "" nil)
(autoload 'ti::keymap-put-abc-map               "tinylib" "" nil)
(autoload 'ti::keymap-put-map                   "tinylib" "" nil)
(autoload 'ti::keymap-mapkeys                   "tinylib" "" nil)
(autoload 'ti::buffer-text-properties-wipe      "tinylib" "" t)
(autoload 'ti::set-face-try-list                "tinylib" "" nil)
(autoload 'ti::buffer-forward-line              "tinylib" "" t);;defsubst
(autoload 'ti::buffer-surround-with-char        "tinylib" "" t)
(autoload 'ti::buffer-fill-region-spaces        "tinylib" "" t)
(autoload 'ti::buffer-quote-words-in-region     "tinylib" "" t)
(autoload 'ti::buffer-find-longer-line          "tinylib" "" nil)
(autoload 'ti::buffer-scramble-region           "tinylib" "" t)
(autoload 'ti::buffer-add-string-region         "tinylib" "" t)
(autoload 'ti::buffer-sort-regexp-fields        "tinylib" "" nil)
(autoload 'ti::file-passwd-grep-user-alist      "tinylib" "" nil)
(autoload 'ti::file-passwd-build-alist          "tinylib" "" nil)
(autoload 'ti::file-passwd-read-entry           "tinylib" "" nil)
(autoload 'ti::buffer-defun-function-name       "tinylib" "" nil)
(autoload 'ti::file-days-old                    "tinylib" "" nil);;defsubst
(autoload 'ti::file-touch                       "tinylib" "" nil)
(autoload 'ti::file-ange-completed-message      "tinylib" "" nil)
(autoload 'ti::file-ange-status                 "tinylib" "" nil)
(autoload 'ti::file-ange-download-file          "tinylib" "" nil)
(autoload 'ti::file-ange-file-handle            "tinylib" "" nil)
(autoload 'ti::file-chmod-w-toggle              "tinylib" "" nil)
(autoload 'ti::file-find-shadows                "tinylib" "" t)
(autoload 'ti::directory-subdirectory-list      "tinylib" "" nil)
(autoload 'ti::directory-recursive-do           "tinylib" "" nil)
;; (autoload 'ti::directory-up                     "tinylib" "" nil)
(autoload 'ti::directory-subdirs                "tinylib" "" nil)
(autoload 'ti::directory-unix-man-path-root     "tinylib" "" nil)
(autoload 'ti::directory-files                  "tinylib" "" nil)
(autoload 'ti::file-files-only                  "tinylib" "" nil)
(autoload 'ti::file-newer-exist                 "tinylib" "" nil)
(autoload 'ti::file-get-extension               "tinylib" "" nil)
(autoload 'ti::file-path-and-line-info          "tinylib" "" nil)
(autoload 'ti::file-path-to-unix                "tinylib" "" nil)
(autoload 'ti::file-path-to-msdos               "tinylib" "" nil)
(autoload 'ti::file-make-path                   "tinylib" "" nil)
(autoload 'ti::file-get-load-path               "tinylib" "" t)
(autoload 'ti::file-user-home                   "tinylib" "" nil)
(autoload 'ti::file-file-list                   "tinylib" "" nil)
(autoload 'ti::file-complete-file-name          "tinylib" "" nil)
(autoload 'ti::file-complete-file-name-word     "tinylib" "" t)
(autoload 'ti::file-complete-filename-minibuffer "tinylib" "" t 'macro)
(autoload 'ti::file-read-file-list              "tinylib" "" nil)
(autoload 'ti::process-finger-error             "tinylib" "" nil)
(autoload 'ti::process-finger                   "tinylib" "" t)
(autoload 'ti::process-http-request             "tinylib" "" t)
(autoload 'ti::process-zip                      "tinylib" "" t)
(autoload 'ti::process-zip-view-command         "tinylib" "" t)
(autoload 'ti::process-tar-zip-view-maybe-command "tinylib" "" nil)
(autoload 'ti::process-perl-process-environment-macro "tinylib" "" nil 'macro)
(autoload 'ti::process-perl-version             "tinylib" "" nil)
(autoload 'ti::process-java-version             "tinylib" "" nil)
(autoload 'ti::process-tar-view-command         "tinylib" "" t)
(autoload 'ti::process-tar-read-listing-forward "tinylib" "" nil)
(autoload 'ti::query-read-input-invisible       "tinylib" "" nil)
(autoload 'ti::query-read-input-as-password     "tinylib" "" nil)
(autoload 'ti::advice-control                   "tinylib" "" nil)
(autoload 'ti::package-submit-feedback          "tinylib" "" t)
(autoload 'ti::package-submit-bug-report        "tinylib" "" t)
(autoload 'ti::package-version-info             "tinylib" "" t)
(autoload 'ti::package-get-header               "tinylib" "" nil)
(autoload 'ti::package-install-example          "tinylib" "" t)
(autoload 'ti::package-rip                      "tinylib" "" t)
(autoload 'ti::package-rip-magic                "tinylib" "" t)
(autoload 'ti::package-make-mode-magic          "tinylib" "" t)
(autoload 'ti::package-make-mode                "tinylib" "" t)
(autoload 'ti::package-make-var                 "tinylib" "" nil)
(autoload 'ti::package-make                     "tinylib" "" nil)
(autoload 'ti::package-autoload-create-on-file  "tinylib" "" t)
(autoload 'ti::package-autoload-create-on-directory "tinylib" "" nil)
(autoload 'ti::package-autoload-loaddefs-create-maybe "tinylib" "" nil)
(autoload 'ti::package-autoload-loaddefs-dir-files "tinylib" "" nil)
(autoload 'ti::package-autoload-loaddefs-build-dir-1 "tinylib" "" nil)
(autoload 'ti::package-autoload-loaddefs-build-dir "tinylib" "" nil)
(autoload 'ti::package-autoload-directories     "tinylib" "" nil)
(autoload 'ti::package-autoload-loaddefs-build-recursive "tinylib" "" t)
(autoload 'ti::package-install-pgp-tar          "tinylib" "" t)
(autoload 'ti::xe-installation-root             "tinylib" "" nil)
(autoload 'ti::xe-overlay-some                  "tinylib" "" nil)
(autoload 'ti::xe-overlay-properties            "tinylib" "" nil)
(autoload 'ti::xe-overlays-at                   "tinylib" "" nil)
(autoload 'ti::xe-overlay-put                   "tinylib" "" nil)
(autoload 'ti::xe-overlay-move                  "tinylib" "" nil)
(autoload 'ti::xe-activate-region               "tinylib" "" nil)
(autoload 'ti::xe-read-password                 "tinylib" "" nil)
(autoload 'ti::xe-key-local-map                 "tinylib" "" nil)
(autoload 'ti::xe-key-call-original             "tinylib" "" nil)
(autoload 'ti::xe-mouse-key                     "tinylib" "" nil)
(autoload 'ti::xe-mouse-call-original-function  "tinylib" "" nil)
(autoload 'ti::xe-mouse-call-original           "tinylib" "" t)
(autoload 'ti::xe-popup                         "tinylib" "" t)
(autoload 'ti::xe-display-depth                 "tinylib" "" nil)
(autoload 'ti::xe-read-event                    "tinylib" "" nil)
(autoload 'ti::xe-executing-macro               "tinylib" "" nil)
(autoload 'ti::xe-make-x-popup-event            "tinylib" "" nil)
(autoload 'ti::xe-make-fake-event               "tinylib" "" nil)
(autoload 'ti::xe-modeline-update               "tinylib" "" nil)
(autoload 'ti::xe-set-frame-name                "tinylib" "" nil)
(autoload 'ti::xe-set-frame-parameter           "tinylib" "" t)
(autoload 'ti::xe-frame-window-config           "tinylib" "" nil)
(autoload 'ti::xe-window-system                 "tinylib" "" nil)
(autoload 'ti::xe-timer-list-control            "tinylib" "" nil)
(autoload 'ti::xe-timer-control                 "tinylib" "" nil)
(autoload 'ti::xe-timer-elt                     "tinylib" "" nil)
(autoload 'ti::xe-timer-process-status          "tinylib" "" nil)
(autoload 'ti::xe-timer-cancel                  "tinylib" "" nil)
(autoload 'ti::xe-timer-cancel-function         "tinylib" "" nil)
(autoload 'ti::xe-set-mode-line-format          "tinylib" "" nil)
(autoload 'ti::macrov-minor-mode                "tinylib" "" nil 'macro)
(autoload 'ti::macrov-minor-mode-1              "tinylib" "" nil)
(autoload 'ti::macrof-minor-mode                "tinylib" "" nil 'macro)
(autoload 'ti::macrof-minor-mode-1              "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-on             "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-off            "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-help           "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-commentary     "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-viper-attach   "tinylib" "" t)
(autoload 'ti::macrof-minor-mode-install        "tinylib" "" nil 'macro)
(autoload 'ti::macrof-minor-mode-install-1      "tinylib" "" t)
(autoload 'ti::macrof-define-keys               "tinylib" "" nil 'macro)
(autoload 'ti::macrof-define-keys-1             "tinylib" "" nil)
(autoload 'ti::macrof-version-bug-report-1      "tinylib" "" t)
(autoload 'ti::macrof-version-bug-report        "tinylib" "" nil 'macro)
(autoload 'ti::macrof-debug-1                   "tinylib" "" t)
(autoload 'ti::macrof-debug-lowlevel            "tinylib" "" nil 'macro)
(autoload 'ti::macrof-debug-standard            "tinylib" "" nil 'macro)
(autoload 'ti::macrof-install-pgp-tar-1         "tinylib" "" t)
(autoload 'ti::macrof-install-pgp-tar           "tinylib" "" nil 'macro)
(autoload 'ti::macrof-minor-mode-wizard         "tinylib" "" nil 'macro)
(autoload 'ti::macrof-minor-mode-wizard-1       "tinylib" "" nil)

;;}}}
;;{{{ code: Autoload 'mt' lib -- mail tools

;;; tinylibmail.el

(autoload 'ti::mail-pgp-signature-begin-line    "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-signature-end-line      "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-signed-begin-line       "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-signed-end-line         "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-pkey-begin-line         "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-pkey-end-line           "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-msg-begin-line          "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-msg-end-line            "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-any-pgp-line-regexp     "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-ip-raw-p                    "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-ip-top-level-domain         "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-ip-3-level-domain           "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-news-group                  "tinylibmail" "" nil);;defsubst
(autoload 'tinylibmail-version                  "tinylibmail" "" t)
(autoload 'tinylibmail-submit-feedback          "tinylibmail" "" t)
(autoload 'ti::mail-signature-p                 "tinylibmail" "" nil)
(autoload 'ti::mail-body-empty-p                "tinylibmail" "" nil)
(autoload 'ti:mt-body-clear                     "tinylibmail" "" nil)
(autoload 'ti::mail-set-region                  "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-point-in-header-macro       "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-message-length              "tinylibmail" "" nil)
(autoload 'ti::mail-get-2re                     "tinylibmail" "" nil)
(autoload 'ti::mail-required-headers            "tinylibmail" "" nil)
(autoload 'ti::mail-mail-mode-p                 "tinylibmail" "" nil)
(autoload 'ti::mail-mailbox-p                   "tinylibmail" "" nil)
(autoload 'ti::mail-mail-p                      "tinylibmail" "" nil)
(autoload 'ti::mail-header-area-size            "tinylibmail" "" nil)
(autoload 'ti::mail-hmax                        "tinylibmail" "" nil)
(autoload 'ti::mail-text-start                  "tinylibmail" "" nil)
(autoload 'ti::mail-point-at-header-p           "tinylibmail" "" nil)
(autoload 'ti::mail-point-at-body-p             "tinylibmail" "" nil)
(autoload 'ti::mail-narrow                      "tinylibmail" "" nil)
(autoload 'ti::mail-mail-buffer-name            "tinylibmail" "" nil)
(autoload 'ti::mail-generate-buffer-name        "tinylibmail" "" t)
(autoload 'ti::mail-mail-simple-p               "tinylibmail" "" nil)
(autoload 'ti::mail-to-list-p                   "tinylibmail" "" nil)
(autoload 'ti::mail-vm-macro                    "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mh-macro                    "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-gnus-macro                  "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-rmail-macro                 "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-rmail-do-message-macro      "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-rmail-copy-message          "tinylibmail" "" t)
(autoload 'ti::mail-pgp-v3xx-p                  "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-p                       "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signed-conventional-p   "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signature-detached-p    "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signed-conventional-multi-p "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signed-xpgp-p           "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signed-p                "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-public-key-p            "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-remail-p                "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-comment-file-p          "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-encrypted-p             "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-normal-p                "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-headers-p               "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-re                      "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-block-area-kill-forward "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-block-area              "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-re-search               "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-exe-version-string      "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-data-type               "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-trim-buffer             "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-chop-region             "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-header-kill-in-body     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-data-char-to-int        "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-data-string-to-bin-string "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-data-bin-string-to-int-list "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-data-ascii-armor-convert "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-data-study-ctb-byte     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-study-1-ver      "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-pgp-stream-study-1-key-id   "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-pgp-stream-study-1-time     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-study-enc        "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-study-signed     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-study-pring      "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-study            "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-forward-xpgp     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-forward          "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-forward-and-study "tinylibmail" "" t)
(autoload 'ti::mail-pgp-stream-forward-info     "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-stream-data-elt         "tinylibmail" "" nil)
(autoload 'ti::mail-pgpk-id-lines-in-region     "tinylibmail" "" nil)
(autoload 'ti::mail-pgpk-id-0x-lines-in-region  "tinylibmail" "" nil)
(autoload 'ti::mail-pgpk-public-get-region      "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signature-remove        "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signature-normal-do-region "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-get-article-buffer          "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-with-article-buffer         "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-pgp-signature-normal-info   "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-sig-header-info-v2xx    "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signature-header-info-v3xx "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-signature-header-info   "tinylibmail" "" nil)
(autoload 'ti::mail-mime-parse-header           "tinylibmail" "" nil)
(autoload 'ti::mail-pgp-pkey-read               "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-close                  "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-anonymize-headers      "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-reply-type             "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-block                  "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-reply-block            "tinylibmail" "" nil)
(autoload 'ti::mail-pgpr-parse-levien-list      "tinylibmail" "" nil)
(autoload 'ti::mail-email-domain                "tinylibmail" "" nil)
(autoload 'ti::mail-email-domain-canonilize     "tinylibmail" "" nil)
(autoload 'ti::mail-email-find-region           "tinylibmail" "" nil)
(autoload 'ti::mail-email-from-string           "tinylibmail" "" nil)
(autoload 'ti::mail-test-parse-name             "tinylibmail" "" nil)
(autoload 'ti::mail-parse-name                  "tinylibmail" "" nil)
(autoload 'ti::mail-parse-email                 "tinylibmail" "" nil)
(autoload 'ti::mail-parse-received-regexp-list  "tinylibmail" "" nil)
(autoload 'ti::mail-parse-received-line         "tinylibmail" "" nil)
(autoload 'ti::mail-parse-received              "tinylibmail" "" nil)
(autoload 'ti::mail-nslookup-parse              "tinylibmail" "" nil)
(autoload 'ti::mail-nslookup                    "tinylibmail" "" nil)
(autoload 'ti::mail-get-buffer                  "tinylibmail" "" nil)
(autoload 'ti::mail-signature-insert-break      "tinylibmail" "" nil)
(autoload 'ti::mail-yank                        "tinylibmail" "" nil)
(autoload 'ti::mail-trim-buffer                 "tinylibmail" "" nil)
(autoload 'ti::mail-field-space-count           "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-field-start                 "tinylibmail" "" nil)
(autoload 'ti::mail-next-field-start            "tinylibmail" "" nil)
(autoload 'ti::mail-current-field-name          "tinylibmail" "" nil)
(autoload 'ti::mail-field-email-send-p          "tinylibmail" "" nil)
(autoload 'ti::mail-kill-field-in-body          "tinylibmail" "" nil)
(autoload 'ti::mail-kill-field                  "tinylibmail" "" nil)
(autoload 'ti::mail-get-field-1                 "tinylibmail" "" nil)
(autoload 'ti::mail-get-field                   "tinylibmail" "" nil)
(autoload 'ti::mail-add-field                   "tinylibmail" "" nil)
(autoload 'ti::mail-add-to-field-string         "tinylibmail" "" nil)
(autoload 'ti::mail-kill-field-elt              "tinylibmail" "" nil)
(autoload 'ti::mail-kill-non-rfc-fields         "tinylibmail" "" nil)
(autoload 'ti::mail-get-all-email-addresses     "tinylibmail" "" nil)
(autoload 'ti::mail-set-recipients              "tinylibmail" "" nil)
(autoload 'ti::mail-news-buffer-p               "tinylibmail" "" t)
(autoload 'ti::mail-article-regexp-read-line    "tinylibmail" "" nil)
(autoload 'ti::mail-news-reply-p                "tinylibmail" "" nil)
(autoload 'ti::mail-anon-penet-p                "tinylibmail" "" nil)
(autoload 'ti::mail-anon-penet-to-p             "tinylibmail" "" nil)
(autoload 'ti::mail-nymserver-email-convert     "tinylibmail" "" nil)
(autoload 'ti::mail-mime-tm-featurep-p          "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-mime-semi-featurep-p        "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-mime-feature-p              "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-mime-tm-edit-p              "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-mime-semi-edit-p            "tinylibmail" "" nil);;defsubst
(autoload 'ti::mail-mime-tm-edit-mode-macro     "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mime-semi-edit-mode-macro   "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mime-funcall-0-macro        "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mime-funcall-2-macro        "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mime-turn-on-mode           "tinylibmail" "" t)
(autoload 'ti::mail-mime-turn-off-mode          "tinylibmail" "" t)
(autoload 'ti::mail-mime-sign-region            "tinylibmail" "" t)
(autoload 'ti::mail-mime-encrypt-region         "tinylibmail" "" t)
(autoload 'ti::mail-mime-tm-split-macro         "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-mime-maybe-p                "tinylibmail" "" nil)
(autoload 'ti::mail-mime-p                      "tinylibmail" "" t)
(autoload 'ti::mail-mime-qp-decode              "tinylibmail" "" nil)
(autoload 'ti::mail-qp-mime-prepare             "tinylibmail" "" t)
(autoload 'ti::mail-plugged-p                   "tinylibmail" "" nil)
(autoload 'ti::mail-sendmail-reset-send-hooks   "tinylibmail" "" nil)
(autoload 'ti::mail-sendmail-pure-env-macro     "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-sendmail-macro              "tinylibmail" "" nil 'macro)
(autoload 'ti::mail-abbrev-table                "tinylibmail" "" nil)
(autoload 'ti::mail-abbrev-expand-mail-aliases  "tinylibmail" "" t)
(autoload 'ti::mail-abbrev-get-alist            "tinylibmail" "" nil)
(autoload 'ti::mail-mail-abbrevs-email-list     "tinylibmail" "" nil)


;;}}}
;;{{{ code: Autoload 'y' lib -- system

;;; tinyliby.el

(autoload 'ti::y-autoload-function-list         "tinyliby" "" nil)
(autoload 'ti::y-autoload-function-file-list    "tinyliby" "" nil)
(autoload 'tinyliby-version                     "tinyliby" "" t)
(autoload 'tinyliby-submit-feedback             "tinyliby" "" t)
(autoload 'ti::y-package-where-is-source        "tinyliby" "" nil)
(autoload 'ti::y-load-cleanup                   "tinyliby" "" nil)
(autoload 'ti::y-load-history-emacs-lisp-files            "tinyliby" "" nil)
(autoload 'ti::y-load-history-where-exactly               "tinyliby" "" nil)
(autoload 'ti::y-describe-symbol-find-file      "tinyliby" "" nil)
(autoload 'ti::y-load-history-where-1                     "tinyliby" "" nil)
(autoload 'ti::y-doc-where-is-source            "tinyliby" "" nil)
(autoload 'ti::y-load-history-where-is-source             "tinyliby" "" nil)
(autoload 'ti::y-load-history-get                         "tinyliby" "" nil)
(autoload 'ti::y-feature-kill                   "tinyliby" "" nil)
(autoload 'ti::y-unload-symbols                 "tinyliby" "" nil)
(autoload 'ti::y-unload                         "tinyliby" "" nil)
(autoload 'ti::y-unload-feature                 "tinyliby" "" t)
(autoload 'ti::y-unload-feature-list            "tinyliby" "" nil)
(autoload 'ti::y-symbol-dolist-macro                 "tinyliby" "" nil 'macro)
(autoload 'ti::y-remove-from-hooks              "tinyliby" "" nil)
(autoload 'ti::y-match-in-hooks                 "tinyliby" "" t)
(autoload 'ti::y-get-symbols                    "tinyliby" "" nil)
(autoload 'ti::y-get-file-documentation         "tinyliby" "" t)
(autoload 'ti::y-describe-symbols-i-args        "tinyliby" "" nil)
(autoload 'ti::y-describe-symbols               "tinyliby" "" t)
(autoload 'ti::y-describe-symbol-summary                 "tinyliby" "" t)

;;}}}
;;{{{ code: Autoload 'o' lib -- overlays

;;; tinylibo.el

(autoload 'tinylibo-version                     "tinylibo" "" t)
(autoload 'tinylibo-feedback                    "tinylibo" "" t)
(autoload 'ti::overlay-make                           "tinylibo" "" nil);;defsubst
(autoload 'ti::overlay-makec                          "tinylibo" "" nil);;defsubst
(autoload 'ti::overlay-make-match                     "tinylibo" "" nil)
(autoload 'ti::overlay-buffer-substring               "tinylibo" "" nil);;defsubst
(autoload 'ti::overlay-mouse-on-p                     "tinylibo" "" nil)
(autoload 'ti::overlay-get-mouse                      "tinylibo" "" nil)
(autoload 'ti::overlay-get-prop                       "tinylibo" "" nil)
(autoload 'ti::overlay-get-prop-val                   "tinylibo" "" nil)
(autoload 'ti::overlay-re-search                      "tinylibo" "" nil)
(autoload 'ti::overlay-re-search-move                 "tinylibo" "" nil)
(autoload 'ti::overlay-get-within-area                "tinylibo" "" nil)
(autoload 'ti::overlay-remove-region                  "tinylibo" "" t)

;;}}}
;;{{{ code: Autoload 't lib  -- Text property library

;;; tinylibt.el

(autoload 'ti::text-search-face-reset              "tinylibt" "" nil 'macro)
(autoload 'ti::text-search-face-set                "tinylibt" "" nil 'macro)
(autoload 'ti::text-face                           "tinylibt" "" nil 'macro)
(autoload 'ti::text-stack-clear                    "tinylibt" "" nil);;defsubst
(autoload 'ti::text-stack-length                   "tinylibt" "" nil);;defsubst
(autoload 'ti::text-stack-full-p                   "tinylibt" "" nil);;defsubst
(autoload 'ti::text-stack-p                        "tinylibt" "" nil);;defsubst
(autoload 'ti::text-save-data                      "tinylibt" "" nil)
(autoload 'ti::text-undo                           "tinylibt" "" t)
(autoload 'ti::text-clear-buffer-properties        "tinylibt" "" t)
(autoload 'ti::text-clear-region-properties        "tinylibt" "" t)
(autoload 'ti::text-get-mouse-property             "tinylibt" "" nil)
(autoload 'ti::text-match-level                    "tinylibt" "" nil)
(autoload 'ti::text-re-search                      "tinylibt" "" t)
(autoload 'ti::text-property-search-and-modify     "tinylibt" "" nil)
(autoload 'ti::text-read-regexp                    "tinylibt" "" nil)
(autoload 'ti::text-looking-at                     "tinylibt" "" t)
(autoload 'ti::text-buffer                         "tinylibt" "" t)
(autoload 'ti::text-re-search-forward              "tinylibt" "" t)
(autoload 'ti::text-re-search-backward             "tinylibt" "" t)
(autoload 'ti::text-mouse-mark-region              "tinylibt" "" t)
(autoload 'ti::text-mouse-unmark-region            "tinylibt" "" t)
(autoload 'ti::text-unmark-region                  "tinylibt" "" t)
(autoload 'ti::text-mark-region                    "tinylibt" "" t)

;;}}}

;;{{{ code: Autoload other 'tiny tools'

;;; tinylibck.el

(autoload 'ti::ck-advice-control		"tinylibck")

;;; tinylibid.el

(autoload 'ti::id-info				"tinylibid")
(autoload 'ti::id-cnv-txt2comment		"tinylibid")

;;; tinylibmenu.el

(autoload 'ti::menu-help			"tinylibmenu")
(autoload 'ti::menu-menu			"tinylibmenu")

;;; tinytab.el

(autoload 'tinytab-mode				"tinytab" "" t)
(autoload 'turn-on-tinytab-mode                 "tinytab" "" t)
(autoload 'turn-off-tinytab-mode                "tinytab" "" t)

;;; tinyurl.el

(autoload 'turn-on-tinyurl-mode-maybe           "tinyurl" "" nil)
(autoload 'turn-on-tinyurl-mode-mail            "tinyurl" "" nil)
(autoload 'turn-on-tinyurl-mode-1               "tinyurl" "" t)
(autoload 'turn-off-tinyurl-mode-1              "tinyurl" "" t)
(autoload 'tinyurl-mode-1                       "tinyurl" "" t)
(autoload 'turn-on-tinyurl-mode                 "tinyurl" "" t)
(autoload 'turn-off-tinyurl-mode                "tinyurl" "" t)
(autoload 'tinyurl-mode                         "tinyurl" "" t)
(autoload 'tinyurl-mode-action                  "tinyurl" "" nil)
(autoload 'tinyurl-install                      "tinyurl" "" t)
(autoload 'tinyurl-mark-line                    "tinyurl")
(autoload 'tinyurl-overlay-get                  "tinyurl")
(autoload 'tinyurl-dispatcher                   "tinyurl")
(autoload 'tinyurl-agent-funcall                "tinyurl")


;;}}}

;;{{{ code: autoload other

(autoload 'byte-compile			    "bytecomp")
(autoload 'occur			    "replace" "" t)

(autoload 'folding-open-buffer              "folding" "" t)

(autoload 'mail-yank-original		    "sendmail")
(autoload 'mail-send-and-exit		    "sendmail")
(autoload 'mail-setup			    "sendmail")
(autoload 'mail-mode			    "sendmail")

(autoload 'mail-fetch-field		    "mail-utils")

(autoload 'hexl-hex-string-to-integer	    "hexl")


(autoload 'browse-url                       "browse-url")
(autoload 'browse-url-w3                    "browse-url")
(autoload 'browse-url-netscape              "browse-url")
(autoload 'browse-url-lynx-emacs            "browse-url")

(autoload 'display-time                     "time")
(autoload 'shuffle-vector                   "cookie1")
(autoload 'name-last-kbd-macro              "macros")
(autoload 'mail-extract-address-components  "mail-extr")


;;  This is special case. if there is Igrep package available, it
;;  will define autoload to "grep" and we must reflect the
;;  situation accordingly. See `igrep-insinuate'

(unless (fboundp 'grep)
  (if (locate-library "igrep")
      (autoload 'grep "igrep" "" t)
    (autoload 'grep "grep" "" t)))

(autoload 'compile           "compile" "" t)
(autoload 'compile-internal  "compile")


;; Emacs 20.6 sort.el

(autoload 'sort-subr                            "sort" "" nil)
(autoload 'sort-build-lists                     "sort" "" nil)
(autoload 'sort-reorder-buffer                  "sort" "" nil)
(autoload 'sort-lines                           "sort" "" t)
(autoload 'sort-paragraphs                      "sort" "" t)
(autoload 'sort-pages                           "sort" "" t)
(autoload 'sort-numeric-fields                  "sort" "" t)
(autoload 'sort-fields                          "sort" "" t)
(autoload 'sort-fields-1                        "sort" "" nil)
(autoload 'sort-skip-fields                     "sort" "" nil)
(autoload 'sort-regexp-fields-next-record       "sort" "" nil)
(autoload 'sort-regexp-fields                   "sort" "" t)
(autoload 'sort-columns                         "sort" "" t)
(autoload 'reverse-region                       "sort" "" t)

;; tabify.el

(autoload 'tabify                           "tabify" "" t)
(autoload 'untabify                         "tabify" "" t)

;; pp.el

(autoload 'pp-to-string                         "pp" "" nil)
(autoload 'pp                                   "pp" "" nil)
(autoload 'pp-eval-expression                   "pp" "" t)
(autoload 'pp-eval-last-sexp                    "pp" "" t)

;; thingatpt.el

(autoload 'forward-thing			"thingatpt" "" nil)
(autoload 'bounds-of-thing-at-point             "thingatpt" "" nil)
(autoload 'thing-at-point                       "thingatpt" "" nil)
(autoload 'beginning-of-thing                   "thingatpt" "" nil)
(autoload 'end-of-thing                         "thingatpt" "" nil)
(autoload 'in-string-p                          "thingatpt" "" nil)
(autoload 'end-of-sexp                          "thingatpt" "" nil)
(autoload 'forward-whitespace                   "thingatpt" "" t)
(autoload 'forward-symbol                       "thingatpt" "" t)
(autoload 'forward-same-syntax                  "thingatpt" "" t)
(autoload 'word-at-point                        "thingatpt" "" nil)
(autoload 'sentence-at-point                    "thingatpt" "" nil)
(autoload 'read-from-whole-string               "thingatpt" "" nil)
(autoload 'form-at-point                        "thingatpt" "" nil)
(autoload 'sexp-at-point                        "thingatpt" "" nil)
(autoload 'symbol-at-point                      "thingatpt" "" nil)
(autoload 'number-at-point                      "thingatpt" "" nil)
(autoload 'list-at-point                        "thingatpt" "" nil)


;; rect.el

(autoload 'operate-on-rectangle                 "rect" "" nil)
(autoload 'delete-rectangle-line                "rect" "" nil)
(autoload 'delete-extract-rectangle-line        "rect" "" nil)
(autoload 'extract-rectangle-line               "rect" "" nil)
(autoload 'spaces-string                        "rect" "" nil)
(autoload 'delete-rectangle                     "rect" "" t)
(autoload 'delete-extract-rectangle             "rect" "" nil)
(autoload 'extract-rectangle                    "rect" "" nil)
(autoload 'kill-rectangle                       "rect" "" t)
(autoload 'yank-rectangle                       "rect" "" t)
(autoload 'insert-rectangle                     "rect" "" nil)
(autoload 'open-rectangle                       "rect" "" t)
(autoload 'open-rectangle-line                  "rect" "" nil)
(autoload 'string-rectangle                     "rect" "" t)
(autoload 'string-rectangle-line                "rect" "" nil)
(autoload 'clear-rectangle                      "rect" "" t)
(autoload 'clear-rectangle-line                 "rect" "" nil)

;; jka-compr.el

(autoload 'jka-compr-info-regexp                "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-message      "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-program      "jka-compr"   "" nil)
(autoload 'jka-compr-info-compress-args         "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-message    "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-program    "jka-compr"   "" nil)
(autoload 'jka-compr-info-uncompress-args       "jka-compr"   "" nil)
(autoload 'jka-compr-info-can-append            "jka-compr"   "" nil)
(autoload 'jka-compr-info-strip-extension       "jka-compr"   "" nil)
(autoload 'jka-compr-get-compression-info       "jka-compr"   "" nil)
(autoload 'jka-compr-error                      "jka-compr"   "" nil)
(autoload 'jka-compr-partial-uncompress         "jka-compr"   "" nil)
(autoload 'jka-compr-call-process               "jka-compr"   "" nil)
(autoload 'jka-compr-make-temp-name             "jka-compr"   "" nil)
(autoload 'jka-compr-delete-temp-file           "jka-compr"   "" nil)
(autoload 'jka-compr-write-region               "jka-compr"   "" nil)
(autoload 'jka-compr-insert-file-contents       "jka-compr"   "" nil)
(autoload 'jka-compr-file-local-copy            "jka-compr"   "" nil)
(autoload 'jka-compr-load                       "jka-compr"   "" nil)
(autoload 'jka-compr-byte-compiler-base-file-name "jka-compr" "" nil)
(autoload 'jka-compr-handler                    "jka-compr"   "" nil)
(autoload 'jka-compr-run-real-handler           "jka-compr"   "" nil)
(autoload 'toggle-auto-compression              "jka-compr"   "" t)
(autoload 'jka-compr-build-file-regexp          "jka-compr"   "" nil)
(autoload 'jka-compr-install                    "jka-compr"   "" nil)
(autoload 'jka-compr-uninstall                  "jka-compr"   "" nil)
(autoload 'jka-compr-installed-p                "jka-compr"   "" nil)

;; Advice.el (partial autoloads only)

(autoload 'ad-disable-advice			"advice")
(autoload 'ad-enable-advice			"advice")
(autoload 'ad-activate				"advice")

;; finder.el


(autoload 'finder-compile-keywords              "finder" "" nil)
(autoload 'finder-compile-keywords-make-dist    "finder" "" nil)
(autoload 'finder-insert-at-column              "finder" "" nil)
(autoload 'finder-mouse-face-on-line            "finder" "" nil)
(autoload 'finder-list-keywords                 "finder" "" t)
(autoload 'finder-list-matches                  "finder" "" nil)
(autoload 'finder-find-library                  "finder" "" nil)
(autoload 'finder-commentary                    "finder" "" t)
(autoload 'finder-current-item                  "finder" "" nil)
(autoload 'finder-select                        "finder" "" t)
(autoload 'finder-mouse-select                  "finder" "" t)
(autoload 'finder-by-keyword                    "finder" "" t)
(autoload 'finder-mode                          "finder" "" t)
(autoload 'finder-summary                       "finder" "" t)
(autoload 'finder-exit                          "finder" "" t)

;; lisp-mnt.el

(autoload 'lm-get-header-re                     "lisp-mnt" "" nil);;defsubst
(autoload 'lm-get-package-name                  "lisp-mnt" "" nil);;defsubst
(autoload 'lm-section-mark                      "lisp-mnt" "" nil)
(autoload 'lm-code-mark                         "lisp-mnt" "" nil);;defsubst
(autoload 'lm-commentary-mark                   "lisp-mnt" "" nil);;defsubst
(autoload 'lm-history-mark                      "lisp-mnt" "" nil);;defsubst
(autoload 'lm-header                            "lisp-mnt" "" nil)
(autoload 'lm-header-multiline                  "lisp-mnt" "" nil)
(autoload 'lm-summary                           "lisp-mnt" "" nil)
(autoload 'lm-crack-address                     "lisp-mnt" "" nil)
(autoload 'lm-authors                           "lisp-mnt" "" nil)
(autoload 'lm-maintainer                        "lisp-mnt" "" nil)
(autoload 'lm-creation-date                     "lisp-mnt" "" nil)
(autoload 'lm-last-modified-date                "lisp-mnt" "" nil)
(autoload 'lm-version                           "lisp-mnt" "" nil)
(autoload 'lm-keywords                          "lisp-mnt" "" nil)
(autoload 'lm-adapted-by                        "lisp-mnt" "" nil)
(autoload 'lm-commentary                        "lisp-mnt" "" nil)
(autoload 'lm-insert-at-column                  "lisp-mnt" "" nil)
(autoload 'lm-verify                            "lisp-mnt" "" t)
(autoload 'lm-synopsis                          "lisp-mnt" "" t)
(autoload 'lm-report-bug                        "lisp-mnt" "" t)


;; reporter.el

(autoload 'reporter-update-status               "reporter" "" nil)
(autoload 'reporter-beautify-list               "reporter" "" nil)
(autoload 'reporter-lisp-indent                 "reporter" "" nil)
(autoload 'reporter-dump-variable               "reporter" "" nil)
(autoload 'reporter-dump-state                  "reporter" "" nil)
(autoload 'reporter-calculate-separator         "reporter" "" nil)
(autoload 'reporter-mail                        "reporter" "" nil)
(autoload 'reporter-compose-outgoing            "reporter" "" nil)
(autoload 'reporter-submit-bug-report           "reporter" "" nil)
(autoload 'reporter-bug-hook                    "reporter" "" nil)
(autoload 'define-mail-user-agent               "reporter" "" nil)


;; vc-hook.el from Emacs 20.7


(autoload 'vc-mistrust-permissions              "vc-hooks" "" nil)
(autoload 'vc-error-occurred                    "vc-hooks" "" nil 'macro)
(autoload 'vc-file-setprop                      "vc-hooks" "" nil)
(autoload 'vc-file-getprop                      "vc-hooks" "" nil)
(autoload 'vc-file-clearprops                   "vc-hooks" "" nil)
(autoload 'vc-match-substring                   "vc-hooks" "" nil)
(autoload 'vc-lock-file                         "vc-hooks" "" nil)
(autoload 'vc-parse-buffer                      "vc-hooks" "" nil)
(autoload 'vc-insert-file                       "vc-hooks" "" nil)
(autoload 'vc-parse-locks                       "vc-hooks" "" nil)
(autoload 'vc-simple-command                    "vc-hooks" "" nil)
(autoload 'vc-parse-cvs-status                  "vc-hooks" "" nil)
(autoload 'vc-fetch-master-properties           "vc-hooks" "" nil)
(autoload 'vc-consult-rcs-headers               "vc-hooks" "" nil)
(autoload 'vc-backend-subdirectory-name         "vc-hooks" "" nil)
(autoload 'vc-name                              "vc-hooks" "" nil)
(autoload 'vc-backend                           "vc-hooks" "" nil)
(autoload 'vc-checkout-model                    "vc-hooks" "" nil)
(autoload 'vc-cvs-status                        "vc-hooks" "" nil)
(autoload 'vc-master-locks                      "vc-hooks" "" nil)
(autoload 'vc-master-locking-user               "vc-hooks" "" nil)
(autoload 'vc-lock-from-permissions             "vc-hooks" "" nil)
(autoload 'vc-user-login-name                   "vc-hooks" "" nil)
(autoload 'vc-file-owner                        "vc-hooks" "" nil)
(autoload 'vc-rcs-lock-from-diff                "vc-hooks" "" nil)
(autoload 'vc-locking-user                      "vc-hooks" "" nil)
(autoload 'vc-latest-version                    "vc-hooks" "" nil)
(autoload 'vc-your-latest-version               "vc-hooks" "" nil)
(autoload 'vc-master-workfile-version           "vc-hooks" "" nil)
(autoload 'vc-fetch-properties                  "vc-hooks" "" nil)
(autoload 'vc-workfile-version                  "vc-hooks" "" nil)
(autoload 'vc-registered                        "vc-hooks" "" nil)
(autoload 'vc-sccs-project-dir                  "vc-hooks" "" nil)
(autoload 'vc-search-sccs-project-dir           "vc-hooks" "" nil)
(autoload 'vc-find-cvs-master                   "vc-hooks" "" nil)
(autoload 'vc-buffer-backend                    "vc-hooks" "" nil)
(autoload 'vc-toggle-read-only                  "vc-hooks" "" t)
(autoload 'vc-after-save                        "vc-hooks" "" nil)
(autoload 'vc-mode-line                         "vc-hooks" "" t)
(autoload 'vc-status                            "vc-hooks" "" nil)
(autoload 'vc-follow-link                       "vc-hooks" "" nil)
(autoload 'vc-find-file-hook                    "vc-hooks" "" nil)
(autoload 'vc-file-not-found-hook               "vc-hooks" "" nil)
(autoload 'vc-kill-buffer-hook                  "vc-hooks" "" nil)

(autoload 'vc-backend-dispatch                  "vc" "" nil 'macro)
(autoload 'vc-backend-release                   "vc" "" nil)
(autoload 'vc-release-greater-or-equal          "vc" "" nil)
(autoload 'vc-backend-release-p                 "vc" "" nil)
(autoload 'vc-trunk-p                           "vc" "" nil)
(autoload 'vc-branch-p                          "vc" "" nil)
(autoload 'vc-branch-part                       "vc" "" nil)
(autoload 'vc-minor-part                        "vc" "" nil)
(autoload 'vc-previous-version                  "vc" "" nil)
(autoload 'vc-clear-context                     "vc" "" t)
(autoload 'vc-file-clear-masterprops            "vc" "" nil)
(autoload 'vc-head-version                      "vc" "" nil)
(autoload 'vc-latest-on-branch-p                "vc" "" nil)
(autoload 'with-vc-file                         "vc" "" nil 'macro)
(autoload 'edit-vc-file                         "vc" "" nil 'macro)
(autoload 'vc-ensure-vc-buffer                  "vc" "" nil)
(autoload 'vc-find-binary                       "vc" "" nil)
(autoload 'vc-do-command                        "vc" "" nil)
(autoload 'vc-position-context                  "vc" "" nil)
(autoload 'vc-find-position-by-context          "vc" "" nil)
(autoload 'vc-context-matches-p                 "vc" "" nil)
(autoload 'vc-buffer-context                    "vc" "" nil)
(autoload 'vc-restore-buffer-context            "vc" "" nil)
(autoload 'vc-revert-buffer1                    "vc" "" t)
(autoload 'vc-buffer-sync                       "vc" "" nil)
(autoload 'vc-workfile-unchanged-p              "vc" "" nil)
(autoload 'vc-next-action-on-file               "vc" "" nil)
(autoload 'vc-next-action-dired                 "vc" "" nil)
(autoload 'vc-next-action                       "vc" "" t)
(autoload 'vc-checkout-writable-buffer          "vc" "" nil)
(autoload 'vc-register                          "vc" "" t)
(autoload 'vc-resynch-window                    "vc" "" nil)
(autoload 'vc-resynch-buffer                    "vc" "" nil)
(autoload 'vc-start-entry                       "vc" "" nil)
(autoload 'vc-admin                             "vc" "" nil)
(autoload 'vc-checkout                          "vc" "" nil)
(autoload 'vc-steal-lock                        "vc" "" nil)
(autoload 'vc-finish-steal                      "vc" "" nil)
(autoload 'vc-checkin                           "vc" "" nil)
(autoload 'vc-comment-to-change-log             "vc" "" t)
(autoload 'vc-finish-logentry                   "vc" "" t)
(autoload 'vc-previous-comment                  "vc" "" t)
(autoload 'vc-next-comment                      "vc" "" t)
(autoload 'vc-comment-search-reverse            "vc" "" t)
(autoload 'vc-comment-search-forward            "vc" "" t)
(autoload 'vc-diff                              "vc" "" t)
(autoload 'vc-version-diff                      "vc" "" t)
(autoload 'vc-version-other-window              "vc" "" t)
(autoload 'vc-insert-headers                    "vc" "" t)
(autoload 'vc-clear-headers                     "vc" "" nil)
(autoload 'vc-merge                             "vc" "" t)
(autoload 'vc-resolve-conflicts                 "vc" "" t)
(autoload 'vc-dired-toggle-terse-mode           "vc" "" t)
(autoload 'vc-dired-mark-locked                 "vc" "" t)
(autoload 'vc-fetch-cvs-status                  "vc" "" nil)
(autoload 'vc-dired-state-info                  "vc" "" nil)
(autoload 'vc-dired-reformat-line               "vc" "" nil)
(autoload 'vc-dired-hook                        "vc" "" nil)
(autoload 'vc-dired-purge                       "vc" "" nil)
(autoload 'vc-directory                         "vc" "" t)
(autoload 'vc-add-triple                        "vc" "" nil)
(autoload 'vc-record-rename                     "vc" "" nil)
(autoload 'vc-lookup-triple                     "vc" "" nil)
(autoload 'vc-snapshot-precondition             "vc" "" nil)
(autoload 'vc-create-snapshot                   "vc" "" t)
(autoload 'vc-retrieve-snapshot                 "vc" "" t)
(autoload 'vc-print-log                         "vc" "" t)
(autoload 'vc-revert-buffer                     "vc" "" t)
(autoload 'vc-cancel-version                    "vc" "" t)
(autoload 'vc-rename-file                       "vc" "" t)
(autoload 'vc-update-change-log                 "vc" "" t)
(autoload 'vc-annotate-mode-variables           "vc" "" nil)
(autoload 'vc-annotate-mode                     "vc" "" t)
(autoload 'vc-annotate-display-default          "vc" "" t)
(autoload 'vc-annotate-add-menu                 "vc" "" t)
(autoload 'vc-annotate                          "vc" "" t)
(autoload 'vc-annotate-car-last-cons            "vc" "" nil)
(autoload 'vc-annotate-time-span                "vc" "" nil)
(autoload 'vc-annotate-compcar                  "vc" "" nil)
(autoload 'vc-annotate-display                  "vc" "" nil)
(autoload 'vc-backend-admin                     "vc" "" nil)
(autoload 'vc-backend-checkout                  "vc" "" nil)
(autoload 'vc-backend-logentry-check            "vc" "" nil)
(autoload 'vc-backend-checkin                   "vc" "" nil)
(autoload 'vc-backend-revert                    "vc" "" nil)
(autoload 'vc-backend-steal                     "vc" "" nil)
(autoload 'vc-backend-uncheck                   "vc" "" nil)
(autoload 'vc-backend-print-log                 "vc" "" nil)
(autoload 'vc-backend-assign-name               "vc" "" nil)
(autoload 'vc-backend-diff                      "vc" "" nil)
(autoload 'vc-backend-merge-news                "vc" "" nil)
(autoload 'vc-backend-merge                     "vc" "" nil)
(autoload 'vc-check-headers                     "vc" "" t)
(autoload 'vc-log-mode                          "vc" "" t)
(autoload 'vc-file-tree-walk                    "vc" "" nil)
(autoload 'vc-file-tree-walk-internal           "vc" "" nil)


;; font-lock from Emacs 20.6

(autoload 'font-lock-mode                       "font-lock" "" t)
(autoload 'turn-on-font-lock                    "font-lock" "" nil)

;; Not necessarily in XEmacs font-lock.el
;; (autoload 'global-font-lock-mode                "font-lock" "" t)
;; (autoload 'font-lock-add-keywords               "font-lock" "" nil)

(autoload 'font-lock-change-major-mode          "font-lock" "" nil)
(autoload 'turn-on-font-lock-if-enabled         "font-lock" "" nil)
(autoload 'font-lock-turn-on-thing-lock         "font-lock" "" nil)
(autoload 'font-lock-turn-off-thing-lock        "font-lock" "" nil)
(autoload 'font-lock-after-fontify-buffer       "font-lock" "" nil)
(autoload 'font-lock-after-unfontify-buffer     "font-lock" "" nil)
(autoload 'font-lock-fontify-buffer             "font-lock" "" t)
(autoload 'font-lock-unfontify-buffer           "font-lock" "" nil)
(autoload 'font-lock-fontify-region             "font-lock" "" nil)
(autoload 'font-lock-unfontify-region           "font-lock" "" nil)
(autoload 'font-lock-default-fontify-buffer     "font-lock" "" nil)
(autoload 'font-lock-default-unfontify-buffer   "font-lock" "" nil)
(autoload 'font-lock-default-fontify-region     "font-lock" "" nil)
(autoload 'font-lock-default-unfontify-region   "font-lock" "" nil)
(autoload 'font-lock-after-change-function      "font-lock" "" nil)
(autoload 'font-lock-fontify-block              "font-lock" "" t)
(autoload 'font-lock-prepend-text-property      "font-lock" "" nil)
(autoload 'font-lock-append-text-property       "font-lock" "" nil)
(autoload 'font-lock-fillin-text-property       "font-lock" "" nil)
(autoload 'font-lock-apply-syntactic-highlight  "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactic-anchored-keywords "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactic-keywords-region "font-lock" "" nil)
(autoload 'font-lock-fontify-syntactically-region "font-lock" "" nil)
(autoload 'font-lock-apply-highlight            "font-lock" "" nil);;defsubst
(autoload 'font-lock-fontify-anchored-keywords  "font-lock" "" nil);;defsubst
(autoload 'font-lock-fontify-keywords-region    "font-lock" "" nil)
(autoload 'font-lock-compile-keywords           "font-lock" "" nil)
(autoload 'font-lock-compile-keyword            "font-lock" "" nil)
(autoload 'font-lock-eval-keywords              "font-lock" "" nil)
(autoload 'font-lock-value-in-major-mode        "font-lock" "" nil)
(autoload 'font-lock-choose-keywords            "font-lock" "" nil)
(autoload 'font-lock-set-defaults               "font-lock" "" nil)
(autoload 'font-lock-unset-defaults             "font-lock" "" nil)
(autoload 'font-lock-match-c-style-declaration-item-and-skip-to-next "font-lock" "" nil)
(autoload 'font-lock-match-c++-style-declaration-item-and-skip-to-next "font-lock" "" nil)


;; imenu.el 20.6, Not in XEmacs.

(when (locate-library "imenu")
  (autoload 'imenu--subalist-p                    "imenu" "" nil)
  ;; ** The compiler ignores `autoload' except at top level.
  ;; (autoload 'imenu-progress-message               "imenu" "" nil 'macro)
  (autoload 'imenu-example--name-and-position     "imenu" "" nil)
  (autoload 'imenu-example--lisp-extract-index-name "imenu" "" nil)
  (autoload 'imenu-example--create-lisp-index     "imenu" "" nil)
  (autoload 'imenu-example--create-c-index        "imenu" "" nil)
  (autoload 'imenu--sort-by-name                  "imenu" "" nil)
  (autoload 'imenu--sort-by-position              "imenu" "" nil)
  (autoload 'imenu--relative-position             "imenu" "" nil)
  (autoload 'imenu--split                         "imenu" "" nil)
  (autoload 'imenu--split-menu                    "imenu" "" nil)
  (autoload 'imenu--split-submenus                "imenu" "" nil)
  (autoload 'imenu--truncate-items                "imenu" "" nil)
  (autoload 'imenu--make-index-alist              "imenu" "" nil)
  (autoload 'imenu--cleanup                       "imenu" "" nil)
  (autoload 'imenu--create-keymap-2               "imenu" "" t)
  (autoload 'imenu--create-keymap-1               "imenu" "" nil)
  (autoload 'imenu--in-alist                      "imenu" "" nil)
  (autoload 'imenu-default-create-index-function  "imenu" "" nil)
  (autoload 'imenu--replace-spaces                "imenu" "" nil)
  (autoload 'imenu--generic-function              "imenu" "" nil)
  (autoload 'imenu--completion-buffer             "imenu" "" nil)
  (autoload 'imenu--mouse-menu                    "imenu" "" nil)
  (autoload 'imenu-choose-buffer-index            "imenu" "" nil)
  (autoload 'imenu-add-to-menubar                 "imenu" "" t)
  (autoload 'imenu-add-menubar-index              "imenu" "" t)
  (autoload 'imenu-update-menubar                 "imenu" "" nil)
  (autoload 'imenu--menubar-select                "imenu" "" nil)
  (autoload 'imenu-default-goto-function          "imenu" "" nil)
  (autoload 'imenu                                "imenu" "" t))

;;}}}
;;{{{ code: SEMI libraries: APEL::alist.el

(if (locate-library "alist")
    (progn
      (autoload 'put-alist                            "alist" "" nil)
      (autoload 'del-alist                            "alist" "" nil)
      (autoload 'set-alist                            "alist" "" nil)
      (autoload 'remove-alist                         "alist" "" nil)
      (autoload 'modify-alist                         "alist" "" nil)
      (autoload 'set-modified-alist                   "alist" "" nil)))

;; set-modified-alist                   sym modifier
;; modify-alist                         modifier default
;; remove-alist                         symbol item
;; set-alist                            symbol item value
;; del-alist                            item alist
;; put-alist                            item value alist

;;}}}

;;{{{ code: Xemacs emulation.

(when (locate-library "timer")
  ;; XEmacs provides xemacs-packages\lisp\fsf-compat\timer.el
  ;;
  ;; These functions are the "common denominator" of XEmacs 21.2
  ;; And Emacs 20.4
  ;;
  (autoload 'cancel-function-timers               "timer" "" t)
  (autoload 'cancel-timer                         "timer" "" nil)
  (autoload 'run-at-time                          "timer" "" t)
  (autoload 'run-with-idle-timer                  "timer" "" t)
  (autoload 'run-with-timer                       "timer" "" t)
  (autoload 'timer-activate                       "timer" "" nil)
  (autoload 'timer-activate-when-idle             "timer" "" nil)
  (autoload 'timer-duration                       "timer" "" nil)
  (autoload 'timer-inc-time                       "timer" "" nil)
  (autoload 'timer-relative-time                  "timer" "" nil)
  (autoload 'timer-set-function                   "timer" "" nil)
  (autoload 'timer-set-idle-time                  "timer" "" nil)
  (autoload 'timer-set-time                       "timer" "" nil)
  (autoload 'timer-set-time-with-usecs            "timer" "" nil)
  (autoload 'with-timeout-handler                 "timer" "" nil)
  (autoload 'y-or-n-p-with-timeout                "timer" "" nil)
  )

(when (xemacs-p)

  (autoload 'set-cursor-color			"tinylibxe" "" t)
  (autoload 'set-foreground-color		"tinylibxe" "" t)
  (autoload 'set-background-color		"tinylibxe" "" t)
  (autoload 'transient-mark-mode		"tinylibxe" "" t)

  (unless (fboundp 'run-at-time)
    (autoload 'run-at-time			"tinylibxe"))

  (unless (fboundp 'cancel-timer)
    (autoload 'cancel-timer 			"tinylibxe"))

  (autoload 'posn-window			"tinylibxe")
  (autoload 'posn-point				"tinylibxe")
  (autoload 'posn-timestamp			"tinylibxe")
  (autoload 'window-edges			"tinylibxe")

  (autoload 'event-start			"tinylibxe")
  (autoload 'event-x				"tinylibxe")
  (autoload 'event-y				"tinylibxe")
  (autoload 'posn-x-y				"tinylibxe")

  (autoload 'frame-parameters			"tinylibxe")


  (eval-when-compile
    ;;  emulation in xe library
    (put 'frame-parameters 'byte-obsolete-variable nil))


  (autoload 'dired-unmark			"tinylibxe")
  (autoload 'dired-mark				"tinylibxe")
  (autoload 'dired-get-marked-files		"tinylibxe")
  (autoload 'dired-map-over-marks		"tinylibxe")

  )

;;}}}
;;{{{ code: XEmacs and Emacs autoloads

(defvar   view-mode nil)

(cond
 ;; XEmacs 21.x changed package name
 ((locate-library "view-less")
  (autoload 'view-exit  "view-less" "" t)
  (autoload 'view-mode  "view-less" "" t))
 (t
  (autoload 'view-exit "view" "" t)
  (autoload 'view-mode "view" "" t)))

(when (locate-library "overlay")   ;; Xemacs has emulation lib
  ;; overlay.el
  ;; xemacs-packages/lisp/fsf-compat/overlay.el
  (autoload 'overlayp                             "overlay" "" nil)
  (autoload 'make-overlay                         "overlay" "" nil)
  (autoload 'move-overlay                         "overlay" "" nil)
  (autoload 'delete-overlay                       "overlay" "" nil)
  (autoload 'overlay-start                        "overlay" "" nil)
  (autoload 'overlay-end                          "overlay" "" nil)
  (autoload 'overlay-buffer                       "overlay" "" nil)
  (autoload 'overlay-properties                   "overlay" "" nil)
  (autoload 'overlays-at                          "overlay" "" nil)
  (autoload 'overlays-in                          "overlay" "" nil)
  (autoload 'next-overlay-change                  "overlay" "" nil)
  (autoload 'previous-overlay-change              "overlay" "" nil)
  (autoload 'overlay-lists                        "overlay" "" nil)
  (autoload 'overlay-recenter                     "overlay" "" nil)
  (autoload 'overlay-get                          "overlay" "" nil)
  (autoload 'overlay-put                          "overlay" "" nil))

;;}}}


) ;; eval-and-compile


;;; tinyliba.el ends here
