;;; @(#) tinydebian.el --- Linux (Debian) utilities.
;;; @(#) $Id: tinydebian.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;;{{{ Id

;; Copyright (C)    2001-2001 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         2001-12
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinydebian-version
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
;;   Put this file on your Emacs-Lisp load path, add following into your
;;   $HOME/.emacs startup file
;;
;;	(require 'tinydebian)
;;
;;   or use the autoload feature. Notice that the automatic "file
;;   state backup feature" gets enables only when this file is loaded.
;;   If you want that feature, then use require.
;;
;;   If you have any questions or bug report to send, use this function:
;;
;;      M-x tinydebian-submit-bug-report       send question, feedback,bug report
;;
;;  To read the documentation after file has been loaded, call
;;
;;	M-x tinydebian-version

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;	This package contains utilities for the Debian System Administarator,
;;      to help administring Debian in daily tasks and submitting bug
;;      reports from Emacs. Learn more about debian at
;;      http://www.debian.org/
;;
;;	o   Report Debian bug with M-x
;;
;;  Quick start

;;}}}

;;; Change Log:

;;; Code:

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))


(eval-and-compile
  ;;  Just forward declarations to shut up byte compiler.
  (defvar font-lock-keywords)
  (defvar font-lock-defaults))


(ti::package-defgroup-tiny TinyDebian tinydebian-: extensions
  "Debian System administrator's grabbag of utilities.")

;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...


(defcustom tinydebian-:load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDebian)


;;}}}
;;{{{ setup: user config

;;; ................................................... &v-user-config ...

(defcustom tinydebian-:dummy nil
  "*"
  :type  'string
  :group 'TinyDebian)


(defface tinydebian-:warn-face
  '((((class color) (background light))
     (:background "green"))
    (((class color) (background dark))
     (:background "sea green"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face used for warnings."
  :group 'TinyDebian)

;;; Color loading section  This is messy *Blech!*
;;
(defface tinydebian-:item-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green3")))
  "Face used for noticing important items."
  :group 'TinyDebian)


;;}}}
;;{{{ setup: -- private

;;; ....................................................... &v-private ...


(defvar tinydebian-:font-lock-keywords-adaptive-date nil
  "Flag to signal that current time Is used to display today's log.
For exmple in /etc/syslog today's log log entries are highlighted
differently that previous days. However this must be changed in
next day, because the daye changes.

This flags says, that adaptive-date regexs must be used.")

(make-variable-buffer-local 'tinydebian-:font-lock-keywords-adaptive-date)


(defvar tinydebian-:bin-dpkg (executable-find "dpkg")
  "*Location of `dpkg' binary.")

(defvar tinydebian-:severity-list
  '(("critical"
     "Makes unrelated software on the system (or the whole system) break,
or causes serious data loss, or introduces a security hole on systems where
you install the package.")
    ("grave"
     "Makes the package in question unuseable or mostly so, or causes data
loss, or introduces a security hole allowing access to the accounts of users
who use the package.")
    ("serious"
     "Severe violation of Debian policy (that is, it violates a
\"must\" or \"required\" directive), or, in the package maintainer's
opinion, makes the package unsuitable for release.")
    ("important"
     "A bug which has a major effect on the usability of a package,
without rendering it completely unusable to everyone.")
    ("normal"
     "The default value, applicable to most bugs.")
    ("minor"
     "A problem which doesn't affect the package's usefulness, and is
presumably trivial to fix.")
    ("wishlist"
     "For any feature request, and also for any bugs that are very
difficult to fix due to major design considerations.")
    ("fixed"
     "For bugs that are fixed but should not yet be closed. This is an
exception for bugs fixed by non-maintainer uploads. Note: the "fixed"
tag should be used instead."))
  "The bug system records a severity level with each bug report.
This is set to normal by default, but can be overridden either by supplying a Severity line in the pseudo-header when the bug is submitted Severity or error.
http://www.debian.org/Bugs/Developer#severities")


(defvar tinydebian-:severity-selected nil
  "Function `tinydebian-severity-select-*' sets this to user selection.")

(defconst tinydebian-:menu-severity
  '("\
Severity: ?)help c)rit g)rave s)erious i)import RET-n)orm m)inor w)ish f)ixed"
    ((?c .	( (tinydebian-severity-select-critical)))
     (?g .	( (tinydebian-severity-select-grave)))
     (?s .	( (tinydebian-severity-select-serious)))
     (?i .	( (tinydebian-severity-select-important)))
     (?n .	( (tinydebian-severity-select-normal)))
     (?\C-m .	( (tinydebian-severity-select-normal)))
     (?m .	( (tinydebian-severity-select-minor)))
     (?w .	( (tinydebian-severity-select-wishlist)))
     (?f .	( (tinydebian-severity-select-fixed)))))
  "Severity menu.

The bug system records a severity level with each bug report. This is set
to normal by default, but can be overridden either by supplying a Severity
line in the pseudo-header when the bug is submitted (see the instructions
for reporting bugs), or by using the severity command with the control
request server.

critical
    makes unrelated software on the system (or the whole system) break, or
    causes serious data loss, or introduces a security hole on systems where
    you install the package.
grave
    makes the package in question unuseable or mostly so, or causes data loss,
    or introduces a security hole allowing access to the accounts of users who
    use the package.
serious
    is a severe violation of Debian policy (that is, it violates a \"must\" or
    \"required\" directive), or, in the package maintainer's opinion, makes the
    package unsuitable for release.
important
    a bug which has a major effect on the usability of a package, without
    rendering it completely unusable to everyone.
normal
    the default value, applicable to most bugs.
minor
    a problem which doesn't affect the package's usefulness, and is presumably
    trivial to fix.
wishlist
    for any feature request, and also for any bugs that are very difficult to
    fix due to major design considerations.
fixed
    for bugs that are fixed but should not yet be closed. This is an exception
    for bugs fixed by non-maintainer uploads. Note: the \"fixed\" tag should be
    used instead.  Certain severities are considered release-critical, meaning
    the bug will have an impact on releasing the package with the stable
    release of Debian. Currently, these are critical, grave and serious.")


(defvar tinydebian-:tags-list
  '(("patch"
     "A patch or some other easy procedure for fixing the bug is included
in the bug logs. If there's a patch, but it doesn't resolve the bug
adequately or causes some other problems, this tag should not be used.")
    ("wontfix"
     "This bug won't be fixed. Possibly because this is a choice between
two arbitrary ways of doing things and the maintainer and submitter prefer
different ways of doing things, possibly because changing the behaviour
will cause other, worse, problems for others, or possibly for other reasons.")
    ("moreinfo"
     "This bug can't be addressed until more information is provided by
the submitter. The bug will be closed if the submitter doesn't provide
more information in a reasonable (few months) timeframe. This is for
bugs like "It doesn't work". What doesn't work?.")
    ("unreproducible"
     "This bug can't be reproduced on the maintainer's system.
Assistance from third parties is needed in diagnosing the cause of the problem.")
    ("help"
     "The maintainer is requesting help with dealing with this bug.")
    ("pending"
     "The problem described in the bug is being actively worked on,
i.e. a solution is pending.")
    ("fixed"
     "This bug is fixed or worked around (by a non-maintainer upload,
for example), but there's still an issue that needs to be resolved.
This tag replaces the old \"fixed\" severity.")
    ("security"
     "This bug describes a security problem in a package (e.g., bad
permissions allowing access to data that shouldn't be accessible;
buffer overruns allowing people to control a system in ways they
shouldn't be able to; denial of service attacks that should be fixed, etc).
Most security bugs should also be set at critical or grave severity.")
    ("upstream"
     "This bug applies to the upstream part of the package.")
    ("potato"
     "This bug particularly applies to the potato release of Debian.")
    ("woody"
     "This bug particularly applies to the (unreleased) woody distribution.")
    ("sid"
     "This bug particularly applies to an architecture that is
currently unreleased (that is, in the sid distribution)."))
  "Each bug can have zero or more of a set of given tags.
These tags are displayed in the list of bugs when you look at a
package's page, and when you look at the full bug log.
http://www.debian.org/Bugs/Developer")



;;}}}
;;{{{ setup: -- version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinydebian-version "tinydebian" "Display commentary." t)
(eval-and-compile
(ti::macrof-version-bug-report
 "tinydebian.el"
 "tinydebian"
 tinydebian-:version-id
 "$Id: tinydebian.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinydebian-version-id
   tinydebian-:load-hook
   tinydebian-:font-lock-keywords-adaptive-date
   tinydebian-:bin-dpkg
   tinydebian-:severity-list
   tinydebian-:severity-selected
   tinydebian-:tags-list)))


;;}}}

;;; ########################################################### &Funcs ###

;;{{{ Install: bindings

;;; ........................................................ &bindings ...

;; #todo:
(defun tinydebian-default-bindings ()
  "Define default key bindings to `tinydebian-mode-map'.")

;;}}}

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install-severity-functions ()
  "Generate `tinydebian-severity-select-*' user functions."
  ;; Generate functions on run-time.
  (mapcar
   (function
    (lambda (x)
      (let ((sym (intern (format "tinydebian-severity-select-%s"  x)))
	    def)
	(setq def
	      (` (defun (, sym) ()
		   "Set Severity level `tinydebian-:severity-selected'."
		   (interactive)
		   (setq  tinydebian-:severity-selected (, x)))))
	(eval def))))
   '("critical"
     "grave"
     "serious"
     "important"
     "normal"
     "minor"
     "wishlist"
     "fixed")))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-find-file-hooks ()
  "INstall `font-lock-keywords' for log files."
  (tinydebian-font-lock-keywords))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload (defalias 'tinydebian-bug-report-mail 'debian-reportbug-package)
;;;###autoload
(defun tinydebian-install (&optional uninstall)
  "Install or UNINSTALL package."
  (interactive "p")
  (tinydebian-install-severity-functions)


  ;;  This just hides from byte compiler fucntion definition
  ;;  so that it does not remember how amny arguments it takes
  ;;
  ;;  function tinydebian-bug-report-mail used to take 0+ arguments, now takes 1
  ;;  function tinydebian-bug-report-mail defined multiple times in this file
  ;;
  (let ((sym 'tinydebian-bug-report-mail))
    (defalias sym 'debian-reportbug-package))

  (cond
   (uninstall
    ;;(remove-hook 'write-file-hooks 'tinydebian-auto-save)
    (remove-hook 'find-file-hooks  'tinydebian-find-file-hooks)
    nil)
   (t
    ;; (add-hook 'write-file-hooks 'tinydebian-auto-save)
    (add-hook 'find-file-hooks  'tinydebian-find-file-hooks)
    nil)))


;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-string-delete-newlines (string)
  "Delete newlines from STRING."
  (ti::string-regexp-delete "[\r\n]" string))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-font-lock-keywords ()
  "Add color support to various log files by setting
`font-lock-keywords'."
  (interactive)
  (let* ((today  (ti::date-standard-rfc-regexp "mon-date"))
	 ;; (cs     (or comment-start-skip "[ \t]+"))
	 (file   "")
	 keywords)

    (when (stringp buffer-file-name)
      (setq file (or buffer-file-name "no-name?")))

    (setq
     keywords
     (cond

      ;; ............................................. Linux log files ...
      ;; /var/log/

      ((string-match "/log/messages$" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
	     (list
	      (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
		    0 'font-lock-function-name-face)
	      (list
	       (concat
		"^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
	       0 'font-lock-reference-face)
	      (list
	       (concat "restarted\\|started"
		       "\\|ignoring"
		       "\\|Linux version.*")
	       0 'font-lock-comment-face))))

      ((string-match "mail\\.log\\|mail\\.info" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
	     (list
	      (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
		    0 'font-lock-function-name-face)
	      (list
	       (concat
		"^... +[0-9]+ ++[0-9]+:+[0-9]+:+[0-9]+")
	       0 'font-lock-reference-face)
	      '("timed out\\|did not.*"
		0 tinydebian-:warn-face)
	      (list
	       (concat "\\(from\\|to\\)=\\([^ ,\t\r\n]+\\)")
	       2 'font-lock-comment-face))))


      ((string-match "daemon\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
	     (list
	      (list
	       (concat
		"^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
	       0 'font-lock-reference-face)
	      (list
	       (concat "connection attempt" ;;  See "iplogger" package
		       )
	       0 'tinydebian-:warn-face)
	      (list
	       (concat "signal +[0-9]+\\|no such user"
		       "\\|connect from .*")
	       0 'font-lock-comment-face))))


      ((string-match "auth\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
	     (list
	      (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
		    0 'font-lock-function-name-face)
	      (list
	       (concat
		"^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
	       0 'font-lock-reference-face)
	      (list
	       (concat "opened +for +[^ \t\r\n]+")
	       0 'tinydebian-:warn-face)
	      '( "for user \\(root\\)"
		 1 font-lock-string-face)
	      '( "from \\([^ \t\r\n]+\\)"
		 1 font-lock-type-face)
	      '( "for +\\([^ \t\r\n]+\\) +from"
		 1 font-lock-comment-face)
	      '( "for user +\\([^ \t\r\n]+\\)"
		 1 font-lock-comment-face))))

      ((string-match "syslog" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
	     (list
	      (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
		    0 'font-lock-function-name-face)
	      (list
	       (concat
		"^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
	       0 'font-lock-reference-face)
	      (list
	       (concat "Invalid.*"
		       ;; portmap[135]: cannot bind udp: Address already in use
		       "\\|cannot"
		       "\\|Connection timed out"
		       ;;  See iplogger(1)
		       "\\|connection attempt"
		       ;;  See portsentry(1)
		       "\\|attackalert:.* +to +.*port.*"
		       "\\|did not .*")
	       0 'tinydebian-:warn-face)
	      '("to=\\([^ \t\r\n]+\\)"
		1 font-lock-comment-face)
	      '("(\\([^ )\t\r\n]+\\)) CMD "
		1 font-lock-comment-face)
	      '("CMD .*"
		0 font-lock-constant-face)
	      '("inetd"
		0 font-lock-type-face)
	      '("program exit.*\\|.*started.*"
		0 font-lock-keyword-face))))

      ))
    (when (font-lock-mode-maybe 1)
      (if keywords
	  (setq font-lock-keywords keywords)))))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content-1 ()
  "Read content. Point must be positionioned at Field:-!-."
  (let* ((str (if (looking-at " +\\(.*\\)")
		  (match-string 1))))
    (while (and (not (eobp))
		(zerop (forward-line 1))  ;; Did it
		(looking-at "^\\( +.*\\)"))
      (setq str (concat (or str "") (match-string 1))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content (&optional field)
  "Read FIELD forward. FIELD ust be name like `Package'.
Be sure to call `tinydebian-package-narrow-to-region' first."
  (when (re-search-forward (format "^%s:" field) nil t)
    (tinydebian-package-read-field-content-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-parse-info-all ()
  "Parse all fields forward. Return '((field . info) (field . info) ..)."
  (let* (field
	 alist)
    (while (re-search-forward "^\\([^ \t\r\n]+\\):" nil t)
      (setq field (match-string 1))
      (push (cons field (tinydebian-package-read-field-content-1))
	    alist))
    (reverse alist)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-package-narrow-to-region (&rest body)
  "Search dpkg -s result from current point forward and narrow around it.
Point is put at the beginning of region.
Variable `package' contains the package name."
  (`
   (let* (beg-narrow
	  package)
     (when (re-search-forward "^Package: +\\([^ \t\r\n]+\\) *$" nil t)
       (setq beg-narrow (line-beginning-position))
       (setq package (match-string 1))
       (when (re-search-forward "^[ \t]*$" nil t)
	 (ti::narrow-safe beg-narrow (point)
	   (ti::pmin)
	   (,@ body)))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends-1 ()
  "Parse `Depends' field content from current point forward.
There must nothing else in the buffer."
  (let* (name
	 op
	 ver
	 list)
    (while (re-search-forward "\\([a-z][^ ,()\t\r\n]+\\)" nil t)
      (setq name (ti::remove-properties (match-string 1))
	    op   nil
	    ver  nil)
      (cond
       ((looking-at " +(\\([=><]+\\) +\\([^ ,()\t\r\n]+\\))")
	(setq op   (ti::remove-properties (match-string 1))
	      ver  (ti::remove-properties (match-string 2))))
       ((looking-at " *,?")))
      (goto-char (match-end 0))
      (push (list name op ver) list))
    (reverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends (depends)
  "Parse `Depends' field from DEPENDS string.
Example of the DEPENDS string:

    \"libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)\"

Returned list is

   '((\"libc6\" \">=\" \"2.2.4-2\")
     (\"cron\"  \">=\" \"3.0pl1-42\"))."
   (with-temp-buffer
      (insert depends)
      (ti::pmin)
      (tinydebian-package-status-parse-depends-1)))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-call-process (binary buffer args)
  "CAll BINARY with list of ARGS and print output to current buffer or BUFFER."
  (apply 'call-process
	 binary
	 nil
	 (or buffer (current-buffer))
	 nil
	 args))

;;; ----------------------------------------------------------------------
;;;
;;; Package: autolog
;;; Status: install ok installed
;;; Priority: extra
;;; Section: admin
;;; Installed-Size: 45
;;; Maintainer: Nicolás Lichtmaier <nick@debian.org>
;;; Version: 0.35-10
;;; Depends: libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)
;;; Recommends: mail-transport-agent
;;; Conffiles:
;;;  /etc/autolog.conf a3fcae584ed74543a4a943e722593ff6
;;;  /etc/cron.d/autolog 805d268ea44c645299defc1c14495282
;;; Description: Terminates connections for idle users
;;;  Autolog terminates connections considered to be idle based on a large
;;;  variety of parameters.
;;;
(defun tinydebian-package-status (package)
  "Parse PACKAGE details."
  (let* ((dpkg  tinydebian-:bin-dpkg))
    (cond
     ((not dpkg)
      (message "TinyDebian: no `dpkg' found along PATH (emacs `exec-path').")
      nil)
     (t
      (with-temp-buffer
	(tinydebian-call-process dpkg nil (list "-s" package))
	(ti::pmin)
	(when (re-search-forward "^Use dpkg" nil t)
	  (error "TinyDebian: `dpkg`-s %s' returned error [%s]"
		 package
		 (buffer-string)))
	(tinydebian-package-parse-info-all))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info-from-buffer (buffer)
  "Parse dpkg -s from BUFFER. Tge buffer must contain nothing else."
  (with-current-buffer buffer
    (ti::pmin)
    (tinydebian-package-parse-info-all)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info-from-user (package &optional prompt)
  "Ask user to supply `dpkg -s PACKAGE' to *scratch* and parse it."
  (let* ((buffer "*scratch*"))
    (read-string
     (or prompt
	 (format
	  (concat "[TinyDebian] insert `dpkg -s %s' to "
		  buffer
		  " and press RET when ready: ")
	  package)))
    (display-buffer (or (get-buffer buffer)
			(get-buffer-create buffer)))
    (tinydebian-package-info-from-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info (&optional package prompt)
  "Get PACKAGE information. See`tinydebian-package-status'.
If PACKAGE is nil and `tinydebian-:bin-dpkg' is not available,
ask with PROMPT."
    (let* ((dpkg  tinydebian-:bin-dpkg))
      (or package
	  (setq package (read-string
			 (or prompt
			     "[TinyDebian] Package name: "))))
      (or (and dpkg
	       (tinydebian-package-status package))
	  (tinydebian-package-info-from-user package prompt))))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-general ()
  "Return relevan System information."
  (interactive)
  (let* (
         )
    "" ;; #todo:
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-depends (info &optional depend-key)
  "Return additional Dependency INFO from item `Depends'.
DEPEND-KEY can be \"Depends\" or \"Pre-Depends\".

Example:

  Versions of packages autolog depends on:
  ii  cron            3.0pl1-72  management of regular background p
  ii  libc6           2.2.5-3    GNU C Library: Shared libraries an."
  (let* ((depends (cdr-safe (assoc
			     (or depend-key "Depends")
			     info)))
	 str)
    (when depends
      (setq str "")
      (dolist (dep-info
	       (tinydebian-package-status-parse-depends depends))
	(multiple-value-bind (package op version)
	    dep-info

	  ;; Not used yet, quiet byte compiler
	  (if op
	      (setq op op))
	  (if version
	      (setq version version))

	  (let* (info2
		 desc
		 ver)
	    (setq info2
		  (tinydebian-package-info
		   package
		   (format "\
\[TinyDebian] Depend. Insert `dpkg -s %s' to *scratch* and press RET: "
			   package)))
	    (setq ver  (cdr-safe (assoc "Version" info2)))
	    ;; cut first few characters
	    (when (setq desc (cdr-safe (assoc "Description" info2)))
	      (setq desc (ti::string-left desc 45)))
	    (setq str
		  (concat
		   str
		   (format "%-15s %-15s %s\n" package ver desc)))))))
    str))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-architecture ()
  "Read architecture."
  (if (not tinydebian-:bin-dpkg)
      ""
    (with-temp-buffer
    (tinydebian-call-process
     tinydebian-:bin-dpkg  nil '("--print-installation-architecture"))
    (tinydebian-string-delete-newlines
     (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-version ()
  "Read Debian version number."
  (let* ((file  "/etc/debian_version")
	 (ret    (format "%s not found or readable." file)))
    (when (and (file-exists-p   file)
	       (file-readable-p file))
      (with-temp-buffer
	(insert-file-contents-literally file)
	(setq ret
	      (tinydebian-string-delete-newlines
	       (buffer-string)))))
    ret))


;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os ()
  "Return OS information.
Debian Release: 3.0
Architecture: i386
Kernel: Linux terra 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US."
  (let* ((kernel       (tinydebian-string-delete-newlines
			(ti::process-uname)))
	 (architecture (tinydebian-bug-system-info-os-architecture))
	 (release      (tinydebian-bug-system-info-os-version))
	 (locale  ""))
    (format "\
Debian Release: %s
Architecture: %s
Kernel: %s
Locale: %s"
	    release
	    architecture
	    kernel
	    locale)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-severity ()
  "Select bug severity."
  (setq tinydebian-:severity-selected nil)
    (while (null tinydebian-:severity-selected)
      (ti::menu-menu 'tinydebian-:menu-severity)
      (unless tinydebian-:severity-selected
	(message "TinyDebian: Please select severity.")
	(sit-for 1)))
    tinydebian-:severity-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-report-mail-insert-details (info)
  "Insert Details for apckage INFO into Mail."
  (ti::mail-text-start 'move)
  (insert "Package: " (cdr (assoc "Package" info)) "\n")
  (insert "Version: " (cdr (assoc "Version" info)) "\n")
  (insert "Severity: " (tinydebian-bug-severity)   "\n\n")
  (let* ((point       (point))
	 (depends     (tinydebian-bug-system-info-depends info "Depends"))
	 (pre-depends (tinydebian-bug-system-info-depends info "Pre-Depends")))
    (insert "\n\n-- System Information\n"
	    (tinydebian-bug-system-info-os)
	    (format "\n\n-- Versions of packages `%s depnds on'.\n"
		    (cdr (assoc "Package" info)))
	    (if pre-depends
		(concat "Pre-Depends:\n" pre-depends)
	      "")
	    (if depends
		(concat "Depends:\n" depends)))
    (goto-char point)))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-mail (info)
  "Submit Debian bug report. INFO is alist of attributes for package.
An example ´reportbug(1)' looks like

To: submit@bugs.debian.org
Subject: autolog ....
--text follows this line--
Package: autolog
Version: 0.35-10
Severity: wishlist



-- System Information
Debian Release: 3.0
Architecture: i386
Kernel: Linux foo 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US

Versions of packages autolog depends on:
ii  cron                          3.0pl1-72  management of regular background p
ii  libc6                         2.2.5-3    GNU C Library: Shared libraries an


Subject: autolog based on DNS and IP names
Package: autolog
Version: 0.35-10
Severity: wishlist



-- System Information
Debian Release: 3.0
Architecture: i386
Kernel: Linux terra 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US

Versions of packages autolog depends on:
ii  cron                          3.0pl1-72  management of regular background p
ii  libc6                         2.2.5-3    GNU C Library: Shared libraries an."
  (interactive
   (progn
     (if (y-or-n-p "[TinyDebian] Submit bug report? ")
	 (list (tinydebian-package-info))
       nil)))
  (let ((status  (or (cdr-safe (assoc "Status" info)) ""))
	(package (or (cdr-safe (assoc "Package" info)) "")))
    (cond
     ((null info)
      (message "TinyDebian: no INFO available to send bug report."))
     ((string-match "not-installed" status)
      (message "TinyDebian: bug report skipped. ´%s' status is [%s]"
	       package status))
     (t
      (let* ((name   (format "*mail* Debian Bug %s" package))
	     buffer)
	(cond
	 ((and (setq buffer (get-buffer name))
	       (null (y-or-n-p
		      "[TinyDebian] delete previous bug report? ")))
	  (pop-to-buffer buffer))
	 (t
	  (pop-to-buffer (get-buffer-create name))
	  (erase-buffer)
	  (let ((subject (read-string "[TinyDebian] bug Subject: ")))
	    (mail-setup
	     "submit@bugs.debian.org" subject nil nil nil nil))
	  (mail-mode)
	  (tinydebian-bug-report-mail-insert-details info))))))))

(tinydebian-install)

(provide   'tinydebian)
(run-hooks 'tinydebian-:load-hook)

;;; tinydebian.el ends here
