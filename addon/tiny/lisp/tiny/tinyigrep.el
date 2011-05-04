;;; @(#) tinyigrep.el --- Top level interface to igrep.el
;;; @(#) $Id: tinyigrep.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1996-12
;; Keywords:	    tools
;;
;; To get information on this program use M-x tinyigrep-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinyigrep|Jari Aalto|jari.aalto@poboxes.com|
;; toplevel igrep.el, completions, user defined dir structures..|
;; 2002-07-23|$Revision: 1.1 $|~/misc/tinyigrep.el|

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
;;     (require 'tinyigrep)
;;
;;      ** YOU NEED igrep.el **
;;
;; Or prefer autoload: your emacs loads this package only when you need it.
;; Following assumes that you want the package to be installed behind your
;; "\C-zg" key. You can pick any other key as well.
;;
;; 	(ti::use-prefix-key global-map "\C-z")      ;; Release reserved key
;; 	(global-set-key "\C-zg" 'tinyigrep-menu)    ;; put anywhere you like
;; 	(autoload 'tinyigrep-menu "tinyigrep" "" t)
;;
;;      (setq tinyigrep-:load-hook
;; 	      '(lambda ()
;;              ;;  Load your additional databases
;; 	        (require 'emacs-rc-tinygrep "~/elisp/rc/emacs-rc-tinyigrep")))
;;
;; Put your customizations to separate file emacs-rc-tinyigrep.el and add
;; (provide 'emacs-rc-tinyigrep) to the end of the resource file.
;;
;; If you have any questions, suggestions, bug reports, use function
;;
;;      M-x tinyigrep-submit-bug-report
;;
;; If you find any incorrect behavior, please immediately
;;
;;	o   M-x tinyigrep-debug-toggle
;;	o   Clear debug buffer (kill-buffer tinyigrep-:debug)
;;	o   Repeat the task
;;	o   Send a bug report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Dec 1996
;;
;;	Somewhere at Summer 1996 Kevin Rodgers <kevinr@ihs.com> decided to put
;;	together all grep calls to one package named igrep.el: `agrep',
;;	`egrep', `fgrep' and `zgrep'. It also could search trees
;;      recursively.
;;
;;	The package draw attention and many people picked up the package
;;	from the gnu.emacs.sources newsgroup. The default `M-x' `grep'
;;	command that came with emacs was a pale shadow compared to
;;	`igrep.el' package's possibilities and advanced features. The birth
;;	of tinyigrep.el was the need to integrate searches to some common
;;	directories or grouped files, like news articles, info pages,
;;	project directories, lisp sources, Emacs startup files. A package
;;	that would allow so called "databases" (directories to search).
;;	igrep.el interface seemed to offer great deal of flebility if you
;;	did not have locate(1) or glimpse(1) and their indexes up to date
;;	all the time.
;;
;;  Description
;;
;;	o   Toplevel interface to `igrep.el': Easy command menu access.
;;	o   You can toggle igrep options while you're in the
;;	    echo-area menu.
;;	o   Do directory searches easily: grep all your
;;	    news files, your Emacs news files, your text files, your lisp
;;	    files, grep all manual paths... just configure one variable.
;;	    You can jump to matches from the *compile* buffer where
;;	    the results will appear.
;;	o   The default installation includes many default directories
;;	    to search for: Perl .pod files, perl installation .pm files,
;;	    Emacs lisp tree_: cl, vm, tm, semi, mc, gnus and Emacs Info
;;	    tree files, Grep all Manual pages installed in your system,
;;	    grep your ~/bin ... more.
;;
;;  Do you need this package?
;;
;;      If you use Emacs "grep" then I suggest you to move to *igrep.el*
;;      and evaluate tinyigrep.el as well. It simplifies your grep tasks
;;      much more. Additionally, if you have several directories where you
;;      keep some persistent data where you want to do lookup from time to
;;      time, then you propably appreciate this package. Package defines
;;      all the parameters for you and all you need to to is supply the
;;      SEARCH-STRING.
;;
;;  Suggestion
;;
;;	You may find it useful to keep the igrep buffer in a special frame
;;	when working in windowed environment. See if you like this:
;;
;;	    (if window-system   ;; Use variable `console-type' in XEmacs
;;	       (setq special-display-buffer-names
;;		 '("*compilation*" "*grep*" "*igrep*")))
;;
;;  A simple example how to use search database
;;
;;	Suppose you want to search 1) emacs cl*el files 2) all your ~/Mail
;;	(recurse into dirs) 3) your ~/News files. The sample database
;;	definitions would look like this:
;;
;;          (require 'tinyigrep)  ;; gives macro `tinyigrep-db-lisp-elt'
;;
;;	    (tinyigrep-db-push-elt
;; 	      (tinyigrep-db-lisp-elt
;; 		 "cl.el"        ;; Find root based on this file
;;               "lisp-cl"      ;; the name of search "database"
;;               "egrep"        ;; Program to do the work (remember zgrep)
;;               ;;  Grep only cl libraries
;; 		 '(list (concat dir "cl*el"))))
;;	    ;;
;;	    ;; Notice '(nil) which turns on recursive search.
;;	    ;;
;;	    (tinyigrep-db-push-elt '("Mail" ("egrep" ("~/Mail/*") (nil))))
;;
;;	    ;; This greps only ~/News/*
;;	    (tinyigrep-db-push-elt '("News" ("egrep" ("~/News/*"))))
;;
;;  Special database for current file directory
;;
;;	Normally you add databases to variable `tinyigrep-:database'.
;;	There is also a special database whose name is `.', which refers to
;;	files that are under current buffer's filedirectory. Eg. say you
;;	are editing:
;;
;;	    /user/foo/txt/file.txt
;;
;;	Selecting a special database `.' will give you a prompt to limit
;;	search for files under that directory. You can modify the file
;;	pattern as you like:
;;
;;	    Search: /user/foo/txt/*.txt
;;
;;	You can use this search criteria for successive searches by selection
;;	`last' database.
;;
;;  Selecting igrep command from command menu
;;
;;	When you call TinyIgrep, you get prompted for a database selection,
;;	which could be "lisp-cl", "Mail", "News" anything you defined. The
;;	igrep interface menu looks like this:
;;
;;	    igrep: i)grep g)uess l)ast time d)ired [c)ase r)ecur u)ser]
;;
;;	Pess key ? to see more help on the command line interface. You can
;;      change igrep.el options listed in [], e.g. key `c' toggles case
;;      sensitivity of the search by adding or removing the -i option for
;;	grep, `r' can be used to toggle recursive mode on or off, and
;;      `u' toggles user switches on and off. The user options are stored
;;      to history list `tinyigrep-:history-igrep-user-options' from where
;;      they can be recalled.
;;
;;  List of predefined databases
;;
;;	For your convenience, function `tinyigrep-install-default-databases'
;;	is run from `tinyigrep-:load-hook', which defines several databases
;;	Here is summary of available default database selections:
;;
;;     Home:
;;
;;	o   .		    Search current directory
;;	o   home-bin-sh	    Search ~/bin/ for *.sh *.awk
;;	o   home-bin-pl	    Search ~/bin/ for Perl *.pl
;;
;;     Operating system:
;;
;;	o   man		    Search manpages
;;	o   c-usr-include   Search C header *.h files in /usr/include
;;
;;     Perl pages:
;;
;;	o   perl-modules    Search Perl modules *.pm in @INC
;;	o   perl-pod	    Search Perl installation *.pod manpages
;;
;;     Emacs and Emacs lisp:
;;
;;	o   elisp-home	    Search ~/lisp or ~/elisp for *.el
;;	o   lisp-dot-files  Search all .emacs* or emacs-rc- files in `load-path'
;;	o   load-path	    Search `load-path' *.el files
;;	o   lisp-emacs-distribution Search Emacs Lisp installation root
;;	o   emacs-root	    Search all of Emacs installation root
;;
;;     Seach Emacs packages: (there are many more, these are only examples)
;;
;;	o   lisp-pcl-cvs
;;	o   lisp-elib
;;	o   lisp-cl
;;	o   lisp-mc
;;	o   lisp-irchat
;;	o   lisp-bbdb
;;	o   lisp-w3
;;	o   lisp-vm
;;	o   lisp-semi
;;	o   lisp-apel
;;	o   lisp-flim
;;	o   lisp-mel
;;	o   lisp-tiny
;;

;;      In addition to the above, if you have any of these files in the
;;      directories along the `load-path', you can search those directories
;;      recursively. Please create these empty files "to mark" these directories for automatic
;;      scanning. In Unix, simple `touch(1)' command create a file.

;;
;;      o   *lisp-rc* database. If file `emacs-rc-flag.el' exists. Typically
;;          in ~/elisp/rc/ where you keep all your Emacs startup settings
;;          and directory is usually under CVS, RCS version control. From
;;          this file you would load or set up loading all other Emacs
;;          startup rc files.
;;
;;      o   *lisp-site-lisp* database. If file `site-lisp-flag.el' exists.
;;          Typically in /usr/share/site-lisp/ or under /opt hirarchy.
;;          This is the whole site wide lisp installation root directory.
;;
;;      o   *lisp-site-lisp-emacs* database. If file `site-lisp-emacs-flag.el'
;;          exists. Here you keep Emacs specific files that do not work
;;          with XEmacs. Typically in /usr/share/site-lisp/emacs/ or under
;;          /opt/.
;;
;;      o   *lisp-site-lisp-xemacs* database. if file `site-lisp-xemacs-flag.el'
;;          exists. Here you keep XEmacs specific files that do not work
;;          with Emacs. Typically in /usr/share/site-lisp/xemacs/ or under
;;          /opt/.
;;
;;      A Typical Emacs lisp package installation structure (site wide) might
;;      look like this. Create the appropriate dummy files as needed,
;;      like creating site-lisp-flag.el to directory /usr/share/site-lisp/
;;
;;	    /usr/share/site-lisp/
;;				common/		    for Both XEmacs and Emacs
;;				emacs		    Emacs only packages
;;				net/		    Net packages
;;				    cvs-packages/   CVS method
;;                                  packages/       Big packages
;;				    users/	    ftp or http directories
;;				xemacs/		    XEmacs only packages
;;
;;	If you want to define a short name for any of these defaults, add
;;	additional entry e.g. for "vm". The last parameter could be '(nil) which
;;	instead if ´nil' to  enable recursive search.
;;
;;	    (tinyigrep-db-push-elt-lisp-package "vm" "vm.el" "grep" nil)
;;
;;	Here is small piece of lisp code which adds multiple short names
;;	(defaults are lisp-*} to the search database:
;;
;;          (require 'cl)
;;
;;	    (dolist (package '("vm" "gnus" "irchat" "semi-def" "mc" "tinylib"))
;;		(tinyigrep-db-push-elt-lisp-package
;;		   package (concat package ".el") "egrep"))
;;	    ;; end of code
;;


;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

;;  When tinyigrep.el is compiled, this strange error occurs:
;;  ** the function `igrep-read-args' is not known to be defined
;;

;;  But that function is not used anywhere? The idea to suppress the
;;  message was to tell the byte compiler beforehand where that functions
;;  is and the `eval-and-compile' trick makes the unnecessary message go
;;  away.
;;
;;  Interestingly, this error message is not displayed by XEmacs 19.14
;;  byte compiler. Maybe this is "used before defined" syndrome in
;;  igrep.el.

(eval-and-compile

  ;;  These are used only if tinyperl.el is available.
  ;;  Just introduce variables for byte compiler.

  (defvar tinyperl-:pod-path)
  (defvar tinyperl-:inc-path)

  ;;  From NTEmacs 20.x FAQ "igrep 2.82 needs to have the variable
  ;;  grep-null-device defined; add the following to your startup file"

  (unless (boundp 'grep-null-device)
    (if (boundp 'null-device)
	(defvar grep-null-device (symbol-value 'null-device))
      (message "\
  ** tinyigrep.el: This Emacs does not have `null-device'. Igrep needs it.")))


  (autoload 'igrep-read-args "igrep"))



(eval-and-compile
  (unless (locate-library "igrep")
    (message "\
  ** tinyigrep.el: Hm, no igrep.el along `load-path'.
                   Get it from Kevin Rodgers <kevinr@ihs.com>
                   You may find it at ftp://ftp.cis.ohio-state.edu/pub/emacs-lisp/
                   This file will abort now...")))

(require 'igrep)

;;  If you try to load 2.82 in Xemacs 21.2 beta; it cries that
;;  somebody (igrep) tried to require ange-ftp. Instruct users
;;  to get newest version.

(eval-and-compile
  (if (and (xemacs-p)
	   (string< igrep-version "2.83"))
      (message "  ** TinyIgrep: [XEmacs] Ensure you have igrep.el 2.83+.")))

;;  When you load a buffer from compile-buffer for viewing (mouse-click),
;;  you need this cache utility
;;
;;  2001-09, commented out. Let user choose if he want to activate the package.
;; (require 'tinycache)

(eval-and-compile
  (if (string< igrep-version "2.56")
      (error
       "TinyIgrep: Please ftp igrep version 2.56 or newer. You have %s"
       igrep-version)))

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyIgrep tinyigrep-: tools
    "Top level interface to igrep.el")

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinyigrep-:load-hook
  '(tinyigrep-perl-pod-support  ;; This must be first
    tinyigrep-install-default-databases)
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyIgrep)

;;}}}
;;{{{ setup: private

;;; ....................................................... &v-private ...
;;; Private variables


(defconst tinyigrep-:igrep-previous-args nil
  "List of variables used for calling igrep.")

(defvar tinyigrep-:history-igrep nil
  "History.")

(defvar tinyigrep-:history-database nil
  "History.")

(defvar tinyigrep-:history-igrep-user-options nil
  "History.")

(defvar tinyigrep-:last-database nil
  "Last selected database.")

(defvar tinyigrep-:debug-buffer "*tinyigrep-debug*"
  "Debug data buffer.")

;;}}}
;;{{{ setup: user config

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyigrep-:special-database "."
  "*Special database: grep files under file's directory.
If user seelcts this database, then the current search is suggested
by looking at the buffer's current directory and file extension.

Eg. if you're in list buffer /dir1/dir2/foo.el, then the suggested
files to search are

  /dir1/dir2/*el"
  :type 'string
  :group 'TinyIgrep)

(defcustom tinyigrep-:debug nil
  "*If non-nil, Record program flow to debug buffer."
  :type 'boolean
  :group 'TinyIgrep)

(defcustom tinyigrep-:flag-file-list
  '(("lisp-site-lisp"		    "site-lisp-flag.el" 'rec)
    ("lisp-site-lisp-net"	    "site-lisp-net-flag.el" 'rec)
    ("lisp-site-lisp-net-cvs"	    "site-lisp-net-cvs-flag.el" 'rec)
    ("lisp-site-lisp-net-packages"  "site-lisp-net-packages-flag.el" 'rec)
    ("lisp-site-lisp-net-users"	    "site-lisp-net-users-flag.el" 'rec)
    ("lisp-site-lisp-common"	    "site-lisp-common-flag.el" 'rec)
    ("lisp-site-lisp-emacs"	    "site-lisp-emacs-flag.el" 'rec)
    ("lisp-site-lisp-xemacs"	    "site-lisp-xemacs-flag.el" 'rec)
    ("lisp-rc"			    "emacs-rc-flag.el" 'rec))
  "*List of lisp files that are searches to FLAG the directory.
When the file is found, the database for that directory is created.
The isea is that you have site-lisp structure, possibly under you
~/elisp  or /usr/share/site-lisp, where the files are grouped
according to their characteristics. Here is one possible site-lisp
organization chart:

    site-lisp
    |
    +-common	    Files common to Emacs and XEmacs
    |
    +-emacs		    Fieles than only work in Emacs
    +-xemacs	    Fieles than only work in XEmacs
    +-net		    Packages available directly from internet
	|
	+-cvs-packages  by CVS pserver directories (Gnus, Mailcrypt ..)
	+-packages	    complete kits, like Tamp, Notes, etc (multiple files)
	+-users	    by User, Emacs Lisp developers
	  |
	  +-firedman-noah
	  +-zakharevich-llya
	  ..

If the entry is '(\"lisp-site-lisp\" \"site-lisp-flag.el\" 'rec),
then you would create file directly to the SITE-LISP ROOT,
 /usr/share/site-lisp/site-lisp-flag.el and TinyIgrep.el will flag that
directory as searchable dir, effectively searching all of your lisp.

Similarly, you can drop 'flags' to other directories, like database entry
'(\"lisp-site-lisp-net-users\" \"site-lisp-net-users-flag.el\" 'rec)



Format:

  '((DB-NAME lisp-flag-file-name recursive-search)
    (DB-NAME lisp-flag-file-name recursive-search)
    ..)."
  :type '(list sexp)
  :group 'TinyIgrep)



(defcustom tinyigrep-:perl-pod-path  nil ;Will be set later
  "*Perl installation POD directory."
  :type  '(repeat directory)
  :group 'TinyIgrep)

(defcustom tinyigrep-:perl-inc-path nil	 ;Will be set later
  "*Perl @INC path list."
  :type  '(repeat directory)
  :group 'TinyIgrep)

;; #todo: use MANPATH ?


(defcustom tinyigrep-:man-path-root (ti::directory-unix-man-path-root)
  "*Man path root in the system. usually /usr/man/ or /opt/local/man."
  :type  'directory
  :group 'TinyIgrep)


(defvar tinyigrep-:man-path-sections
  '("cat1.Z" "cat1m.Z"
    "cat2.Z" "cat3.Z"
    "cat4.Z" "cat5.Z"
    "cat6.Z" "cat7.Z" "cat8.Z"
    "man1" "man3" "man5" "man7" "man8")
  "*Possible manual sections under `tinyigrep-:man-path-root'.
You can list non existing section here; they are automatically ignored
if they do not exist.")



;;; ................................................. &user-copy-these ...
;;; These are examples only -- Please copy/paste to your ~/.emacs
;;; and modify to your needs.


(defcustom tinyigrep-:database nil
  "*Igrep database for group of files.
Rule: The directories _must_ have trailing slashes.

There is one special entry named `tinyigrep-:special-database' which
is treated differently.

You can use following entry to tell that it should be skipped, the
DB-NAME here is string \"nil\".

  '(\"nil\")

This is useful when you build the database in a variable and you test if
certain directories exist. Like this, which builds dynamically one
entry to the `tinyigrep-:database' at evaluation time.

  (list
    (if (not (file-exists-p \"/not/in/this/host\"))
       \"nil\" \"my-path-db\")
     (list
      ...go and build the correct ENTRY))

Note [igrep find fag]:

    This optional argument is very important is you grep over many
    directories and many files. It is impossible to tell in the program
    if your defined criteas generate huge listing or not.

    Defining 3rd argument as list, says that we should call `igrep-find' and
    not igrep function to prevent \"Arg list too long\" error. This
    variable reflects `igrep-find-use-xargs', and because nil is valid
    value, you must express it in list format.

    Valid values and their intepretation is presented below. You may gain
    performance benefit with xargs since it will invoke fewer grep
    processes. In the other hand the -exec choise gives you feeback for
    every found file as it seaches them. In xargs case you have to wait
    untill the whole list has been generated.

    These values are same as in `igrep-find-use-xargs', only in list format:

    '(gnu)      find is called with ...  -print0 | xargs -0 -e grep
    '(not-nil)	find is called with ...  -print  | xargs -e grep
    '(nil)	find is called with ...  -exec grep -n -e grep

Adding an entry to the database

    There is a function that handles inserting entries to the database
    for you. It will replace existing entry or add a new one. The argument
    isa same as is described in Format below.

      (tinyigrep-db-push-elt '(\"Elisp\" (\"egrep\" (\"~/elisp/*el\"))))

Format:

    '((DB-NAME
      '(GREP-PROGRAM
	(DIR DIR ..)
	[(igrep-find flag)]))
      ..)"
  :type  'sexp
  :group 'TinyIgrep)

;;}}}
;;{{{ setup: code

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt (elt)
  "Replace existing ELT in the `tinyigrep-:database' or add new one.
If you want to denote a directory, make sure the last character is slash.

Examples:

   ;;  With recursion, see (nil) argument

   (tinyigrep-db-push-elt '(\"my-perl\"	(\"egrep\" (\"~/bin/perl/\") (nil) )))
   (tinyigrep-db-push-elt '(\"my-bin\"	(\"egrep\" (\"~/bin/\") (nil) )))

   ;; Without recursion, filename spec mst be included: `*'

   (tinyigrep-db-push-elt '(\"news-all\"      (\"egrep\" (\"~/News/*\"))))
   (tinyigrep-db-push-elt '(\"news-Incoming\" (\"egrep\" (\"~/Mail/Inc*\"))))

   ;;  Easy and free web server http://www.xitami.com/

   (tinyigrep-db-push-elt
    '(\"Xitami-root\"
      (\"egrep\"
       (\"d:/bin/server/xitami/*cfg\"
	\"d:/bin/server/xitami/*txt\"
	\"d:/bin/server/xitami/*aut\"))))"
  (when (and elt (not (equal "nil" (car-safe elt))))
    (let* ((member (assoc (car elt) tinyigrep-:database)))
      (if member
	  (setq tinyigrep-:database (delete member tinyigrep-:database)))
      (push elt tinyigrep-:database))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyigrep-db-push-elt-lisp-package (name file &optional grep method)
  "Push NAME into `tinyigrep-:database' if FILE found. Use GREP for search.
This means, that if FILE exists, rthe directory where it was found
is searched for *el files.

Input:

  NAME	    name of the database entry.
  FILE	    file to find.
  GREP	    program used to find. Default is `egrep'.
  METHOD    additional recursive grep method.

Examples:

  ;;	Define shorter names. The default database names are prefixed with
  ;;	lisp- These don't need recursice search.

  (dolist (package '(\"vm\" \"irchat\" \"semi-def\" \"mc\" \"tinylib\" \"bbdb\"))
    (tinyigrep-db-push-elt-lisp-package package (concat package \".el\")))

  ;;	Recursively seached

  (tinyigrep-db-push-elt-lisp-package \"gnus\" \"gnus.el\" \"egrep\" '(nil) )"
  (tinyigrep-db-push-elt
   (tinyigrep-db-lisp-elt
    file name (or grep  "egrep")
    '(list (concat dir "*el")) method)))


;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt-package (name package &optional recursive grep)
  "Find PACKAGE and create NAME entry to database for RECURSIVE '(nil).

Example:

  (dolist (elt '(
		 (\"lisp-bbdb\"	    \"bbdb.el\")
		 (\"lisp-ede\"	    \"ede.el\")
		 (\"lisp-efs\"	    \"efs-auto.el\")
		 (\"lisp-eieo\"	    \"eieo.el\")))
  (tinyigrep-db-push-elt-lisp-package
   (nth 0 elt)
   (nth 1 elt)
   \"egrep\"
   (nth 2 elt) ))"
  (let* ((path  (locate-library package)))
    (when path
      (setq path (file-name-directory path))
      (tinyigrep-db-push-elt
       (list
	name
	(list (or grep "grep")
	      (list (concat path "*el"))
	      (if recursive
		  '(nil))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt-package-texi
  (name package &optional recursive grep)
  "Find PACKAGE texi and create NAME entry to database for RECURSIVE '(nil).

Examples:

  (dolist (elt '((\"texi-bbdb\"	    .  \"bbdb.el\")
		 (\"texi-ede\"	    .  \"ede.el\")
		 (\"texi-eieio\"    .  \"eieio.el\")))
    (tinyigrep-db-push-elt-package-texi (car elt) (cdr elt) nil \"egrep\"))"
  (let ((root     (locate-library package))
	(choices  '("texi/" "tex/" "")))
    (catch 'done
      (when root
	(setq root (file-name-directory root))
	(dolist (try (list root
			   (file-name-as-directory (ti::directory-up root))))
	  (dolist (dir choices)
	    (setq dir (concat try dir))
	    (when (and (file-directory-p dir)
		       (directory-files dir nil "\\.te?xi"))
	      (tinyigrep-db-push-elt
	       (list name (list grep (list (concat dir "*.texi*") ))))
	      (throw 'done dir))))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyigrep-countdown (message count &optional msg)
  "Show (format MESSAGE COUNT MSG) and decrease COUNT."
  (` (progn
       (decf (, count))
       (message (format (, message) (, count) (or (, msg) "") )))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-perl-pod-support ()
  "Add support for Perl POD manual pages."
  ;;  Ask from perl what paths are in @INC. That's what we want
  ;;  to search.
  (when (or (executable-find "perl")
	    (executable-find "perl5"))
    (message "TinyIgrep: Perl detected, consulting tinyperl.el...")
    (cond
     ((or (featurep 'tinyperl)
	  (load "tinyperl" 'noerr))
      (setq tinyigrep-:perl-pod-path	tinyperl-:pod-path)
      (setq tinyigrep-:perl-inc-path	tinyperl-:inc-path))
     (t
      (message
       "TinyIgrep: Sorry, tinyperl.el not found. Can't add perl suport.")))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-default-databases (&optional grep)
  "Install default Emacs, Info, Perl: Man entries to `tinyigrep-:database'.
GREP is program to used for grepping. Default is `egrep'."
  (interactive)
  (let* ((count 26)
	 (msg "TinyIgrep: Wait, initialising default databases... %d %s"))

    ;; Safety check. If perl support is asked, make sure it has been
    ;; initialized. User had hooks in wrong order by putting
    ;; `tinyigrep-perl-pod-support' last?

    (when (and (memq 'tinyigrep-perl-pod-support
		     tinyigrep-:load-hook)
	       (or (null tinyigrep-:perl-inc-path)
		   (null tinyperl-:pod-path)))
      ;; This must be run now, before databases are searched.
      (tinyigrep-perl-pod-support))


    (or grep
	(setq grep "grep"))

    (tinyigrep-countdown msg count "[start]" )

    ;; .................................................... &current-dir ...
    ;; Make sure this entry is included.
    ;; Copy this exactly like below, you may only change
    ;; the GREP program name. It greps from the current file directory,
    ;; where buffer is.

    (tinyigrep-db-push-elt
     (list tinyigrep-:special-database (list grep '(nil))))

    ;; ...................................................... &man-pages ...

    (tinyigrep-countdown msg count "[man pages]" )

    (when (and tinyigrep-:man-path-root
	       (file-exists-p tinyigrep-:man-path-root))
      (list
       "man"
       (list
	"zgrep"
	(union

	 (mapcar;; These are system's man paths
	  (function
	   (lambda (x)
	     (setq x (concat (file-name-as-directory
			      tinyigrep-:man-path-root) x))
	     (when (and (stringp x)
			(file-directory-p x))
	       (concat (file-name-as-directory x) "*"))))
	  tinyigrep-:man-path-sections)

	 (delq nil
	       (mapcar
		(function
		 (lambda (x)
		   (when (and (stringp x)
			      (file-directory-p x))
		     (concat (expand-file-name x) "*" ))))
		'(;; Add also extra man paths
		  "~/man/"
		  "/usr/local/man/"
		  "/usr/contrib/man/"))))
	;;  Must be recursive
	'(non-nil))))


    ;; ........................................................... &home ...
    ;; shell programs

    (tinyigrep-countdown msg count "[home bin]" )

    (when (file-directory-p "~/bin")
      (tinyigrep-db-push-elt (list "home-bin-sh"
				   (list grep
					 (list
					  "~/bin/*sh" "~/bin/*awk"))))
      (tinyigrep-db-push-elt (list "home-bin-pl"
				   (list grep (list "~/bin/*pl"))))      )


    (tinyigrep-countdown msg count "[home Mail]" )

    (when (file-directory-p "~/Mail")
      (tinyigrep-db-push-elt
       (list "home-Mail"
	     (list grep (list "~/Mail/" '(nil))))))

    (tinyigrep-countdown msg count "[home News]" )

    (when (file-directory-p "~/News")
      (tinyigrep-db-push-elt
       (list "home-News"
	     (list grep (list "~/News/" '(nil))))))

    ;; .................................................... &usr-include ...

    (tinyigrep-countdown msg count "[usr include]" )

    (tinyigrep-db-push-elt
     (list
      (if (file-exists-p "/usr/include/")
	  "c-usr-include"  "nil")
      (list
       grep
       (list
	"/usr/include/")
       '(nil))))

    ;; ........................................................... &perl ...

    (when tinyigrep-:perl-inc-path
      (let* ((path  tinyigrep-:perl-inc-path))
	(tinyigrep-countdown msg count "[perl modules]" )
	(tinyigrep-db-push-elt
	 (list "perl-modules"
	       (list grep
		     (mapcar
		      (function
		       (lambda (x)
			 (format "%s*pm" x)))
		      path)
		     '(nil))))))

    (when tinyigrep-:perl-pod-path
      (let* ((path  tinyigrep-:perl-pod-path))
      (tinyigrep-countdown msg count "[perl pod]" )
      (when path
	(tinyigrep-db-push-elt
	 (list "perl-pod"
	       (list grep (list (format "%s*pod"
					(file-name-as-directory path)))))))))

    ;; .......................................................... &elisp ...
    ;; Private home lisp directory

    (tinyigrep-countdown msg count "[lisp personal]" )

    (let ((path (if (file-directory-p "~/elisp")
		    "~/elisp/"
		  (if (file-directory-p "~/lisp")
		      "~/lisp/"))))
      (when path
	(tinyigrep-db-push-elt
	 (list "lisp-home"
	       (list grep (list (concat path "*el")) '(nil)  )))))

    ;; ................................................. &elisp-rc-files ...
    ;;  find directories that contain files starting with .emacs*
    ;;  These are Emacs initialisation or setup files.

    (tinyigrep-countdown msg count "[lisp dot files]")

    (let* (list)
      (dolist (path load-path)
	(when (and (stringp path)
		   (file-directory-p path))
	  (dolist (file (directory-files path))
	    (when (string-match "^\\." file)
	      (push (concat (file-name-as-directory path)
			    ".*")
		    list)
	      (return)))))
      (when list
	(tinyigrep-db-push-elt (list "lisp-dot-files" (list grep list)))))

    ;; ................................................. &elisp-rc-files ...
    ;;  find directories that contain files starting with emacs-rc-*.el
    ;;  These are Emacs initialisation or setup files.

    (tinyigrep-countdown msg count "[lisp rc files]")

    (let* (list)
      (dolist (path load-path)
	(when (and (stringp path)
		   (file-directory-p path))
	  (dolist (file (directory-files path))
	    (when (string-match "-rc-" file)
	      (push (concat (file-name-as-directory path)
			    "*-rc-*el")
		    list)
	      (return)))))
      (when list
	(tinyigrep-db-push-elt (list "lisp-rc-files" (list grep list)))))

    ;; .......................................................... &lisp ...

    (tinyigrep-countdown msg count "[lisp load-path]" )

    (tinyigrep-db-push-elt
     (list
      "load-path"
      (list grep
	    (delq nil
		  (mapcar
		   (function
		    (lambda (x)
		      (when (and (stringp x)
				 (file-directory-p x))
			(concat (file-name-as-directory x) "*.el"))))
		   load-path)))))


    ;; ............................................. &emacs-distribution ...


    (tinyigrep-countdown msg count "[emacs all current]" )

    (let* ((root (emacs-install-root)))

      (when root
	(tinyigrep-db-push-elt
	 (list "emacs-install-root-current"
	       (list
		grep
		(list (concat (emacs-install-root) "/*"))
		'(nil))))

	(tinyigrep-countdown msg count "[emacs all others/up dir]" )

	;; See if thre are more Emacs version installed in the same
	;; level and add search to install database as well

	(setq root (file-name-as-directory (ti::directory-up root)))

	(let* ((dirs (ti::directory-subdirs root))
	       name)
	  (dolist (path dirs)
	    (when (string-match
		   "^\\([Xx]?[Ee]macs-\\)[0-9]+\\.[0-9.]+$"
		   path)
	      (setq name (concat "emacs-install-root-" path))
	      (tinyigrep-db-push-elt
	       (list name
		     (list
		      grep
		      (list (concat root path "/*"))
		      '(nil)))))))))

    ;; ............................................................ cl ...

    (tinyigrep-countdown msg count "[lisp cl]" )


    ;;  Find the Emacs lisp root directory dynamically

    (let* ((path-cl  (locate-library "cl.el"))
	   root)

      (when path-cl
	(setq path-cl (file-name-directory path-cl)
	      root    path-cl)

	;;  Emacs 20.7 cl.el is one directory down from root
	;;  in emacs-20.6/lisp/emacs-lisp/cl.el, but we want the root

	(when (string-match ".*[0-9]/lisp/" path-cl)
	  (setq root (match-string 0 path-cl)))

	(tinyigrep-db-push-elt
	 (list
	  "lisp-emacs-distribution"
	  (list grep
		(list (concat root "*el"))
		'(nil))))

	(tinyigrep-db-push-elt
	 (list
	  "lisp-cl"
	  (list grep
		(list (concat path-cl "cl*el"))
		'(nil))))))

    ;; .................................................... flag files ...

    (tinyigrep-countdown msg count "[lisp tinyigrep-:flag-file-list]" )

    ;;  Create these files with touch(1) to the lisp
    ;;  root directories

    (dolist (elt tinyigrep-:flag-file-list)
      (tinyigrep-db-push-elt-package (nth 0 elt)
				     (nth 1 elt)
				     (nth 2 elt)))


    ;; ........................................................ Cygwin ...

    (tinyigrep-countdown msg count "[cygwin]" )

    (let ((root (win32-cygwin-p)))
      (when root
	(tinyigrep-db-push-elt
	 (list
	  "cygwin-doc"			; /usr/doc tree
	  (list grep
		(list (format "%s/usr/doc/*" root))
		'(nil))))))

    ;; ............................................................ MAIL ...

    (tinyigrep-countdown msg count "[Mail]" )

    (when (file-directory-p "~/Mail/")
      (tinyigrep-db-push-elt (list "Mail" (list grep '("~/Mail/*") '(nil)))))

    (tinyigrep-countdown msg count)

    (tinyigrep-countdown msg count "[News]" )

    (when (file-directory-p "~/News/")
      (tinyigrep-db-push-elt (list "Mail" (list grep '("~/News/*") '(nil)))))

    (tinyigrep-countdown msg count)

    ;; ............................................................ docs ...

    (tinyigrep-countdown msg count "[lisp texi]" )

    (dolist (elt '(("texi-bbdb"		.  "bbdb.el")
		   ("texi-edb"		.  "db-file-io.el")
		   ("texi-ede"		.  "ede.el")
		   ("texi-eieio"	.  "eieio.el")
		   ("texi-elib"		.  "elib-node.el")
		   ("texi-gnus"		.  "gnus.el")
		   ("texi-irchat"	.  "irchat.el")
		   ("texi-mailcrypt"	.  "mailcrypt.el")
		   ("texi-pcl-cvs"	.  "pcl-cvs.el")
		   ("texi-psgml"	.  "psgml.el")
		   ("texi-w3"		.  "w3.el")))
      (tinyigrep-db-push-elt-package-texi (car elt) (cdr elt) nil grep))

    ;; ................................................. &elisp-packages ...


    (tinyigrep-countdown msg count "[lisp ediff]" )


    (let* ((path  (locate-library "ediff.el")))
      (when path
	(setq path (file-name-directory path))
	(tinyigrep-db-push-elt
	 (list "lisp-ediff"
	       (list grep (list (concat path "ediff*el"))  nil )))))



    (tinyigrep-countdown msg count "[lisp packages]" )

    (dolist (elt '(("lisp-apel"	      "poe.el")
		   ("lisp-bbdb"	      "bbdb.el")
		   ("lisp-edb"	      "db-file-io.el")
		   ("lisp-ede"	      "ede.el")
		   ("lisp-efs"	      "efs-auto.el")
		   ("lisp-eieo"	      "eieo.el")
		   ("lisp-elib"	      "elib-node.el")
		   ("lisp-eshell"     "eshell.el")
		   ("lisp-flim"	      "FLIM-VERSION")
		   ("lisp-gnus"	      "gnus.el")
		   ("lisp-irchat"     "irchat-main.el" )
		   ("lisp-jde"	      "jde.el")
		   ("lisp-mailcrypt"  "mailcrypt.el")
		   ("lisp-mel"	      "MEL-CFG")
		   ("lisp-notes-mode" "notes-mode.el")
		   ("lisp-psgml"      "psgml-mode.el")
		   ("lisp-pcl-cvs"    "pcl-cvs.el")
		   ("lisp-semi"	      "semi-def.el")
		   ("lisp-speedbar"   "speedbar.el")
		   ("lisp-tiny"	      "tinylibm.el")
		   ("lisp-vm"	      "vm" '(nil))
		   ("lisp-w3"	      "w3")))
      (tinyigrep-db-push-elt-lisp-package
       (nth 0 elt)
       (nth 1 elt)
       grep
       (nth 2 elt) ))




    ;;   If you have SEMI, you propably have installed it so that there is
    ;;   ROOT directory under which you have put SEMI, APEL, FLIM, CHAO
    ;;
    ;;   elisp
    ;;     semi-mime-root
    ;;     |
    ;;     --- flim
    ;;     --- apel
    ;;     --- mel
    ;;     --- semi-1.9.2

    (tinyigrep-countdown msg count "[lisp SEMI]" )

    (let* ((path  (locate-library "semi-def.el")))
      (when path
	(setq path  (if path (ti::directory-up (file-name-directory path))))
	(tinyigrep-db-push-elt
	 (list "lisp-semi-root"
	       (list grep   (list (concat path "*el"))  '(nil) )))))


    (tinyigrep-countdown msg count "[lisp TM]" )


    (let* ((path  (locate-library "tm-def.el")))
      (when path
	(setq path  (if path (ti::directory-up (file-name-directory path))))
	(tinyigrep-db-push-elt
	 (list "lisp-tm-root"
	       (list grep   (list (concat path "*el"))  '(nil) )))))

    ;; ..................................................... &emacs-info ...

    (tinyigrep-countdown msg count "[info elisp]" )

    (tinyigrep-db-push-elt
     (list
      "info-elisp"
      (list grep
	    (mapcar
	     (function
	      (lambda (x)
		(when (and (stringp x)
			   (file-directory-p x))
		  (concat (file-name-as-directory x) "*elisp*"))))
	     (ti::xe-Info-directory-list)))))

    (tinyigrep-countdown msg count "[info emacs]" )

    (tinyigrep-db-push-elt
     (list
      "info-emacs"
      (list grep
	    (mapcar
	     (function
	      (lambda (x)
		(when (and (stringp x)
			   (file-directory-p x))
		  (concat (file-name-as-directory x) "*emacs*"))))
	     (ti::xe-Info-directory-list)))))


    (tinyigrep-countdown msg count "[info all]" )


    (tinyigrep-db-push-elt
     (list
      "info-all"
      (list grep
	    (mapcar
	     (function
	      (lambda (x)
		(when (and (stringp x)
			   (file-directory-p x))
		  (concat (file-name-as-directory x) "*info*"))))
	     (ti::xe-Info-directory-list)))))


    (tinyigrep-countdown msg count "[info Gnus]" )

    (tinyigrep-db-push-elt
     (list
      "info-gnus"
      (list grep
	    (mapcar
	     (function
	      (lambda (x)
		(when (and (stringp x)
			   (file-directory-p x))
		  (concat (file-name-as-directory x) "*gnus*"))))
	     (ti::xe-Info-directory-list)))))

    (message "TinyIgrep: Wait, initialising default databases...done.")))

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

(eval-and-compile

;;;###autoload (autoload 'tinyigrep-version "tinyigrep "Package Description." t)

(ti::macrof-version-bug-report
 "tinyigrep.el"
 "tinyigrep"
 tinyigrep-:version-id
 "$Id: tinyigrep.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinyigrep-:version-id
   tinyigrep-:debug
   igrep-version
   tinyigrep-:load-hook
   tinyigrep-:igrep-previous-args
   tinyigrep-:history-igrep
   tinyigrep-:history-database
   tinyigrep-:special-database
   tinyigrep-:database)
 '(tinyigrep-:debug-buffer)))


;;;### (autoload 'tinyigrep-debug-toggle "tinyigrep" t t)

(ti::macrof-debug-standard "tinyigrep" ":-")

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ Install

(defvar tinyigrep-:menu
  '((format
     "%s%sigrep: i)grep g)uess l)ast d)ired v)er [c)ase r)ecur u)ser] %s"
     (if (eval (tinyigrep-recursive-var))  "R " "")
     (if igrep-options (concat igrep-options " ") "")
     (if (stringp tinyigrep-:last-database)
	 tinyigrep-:last-database
       ""))

    ((?i  . ( (call-interactively 'igrep)))
     (?d  . ( (call-interactively 'dired-do-igrep)))
     (?g  . ( (call-interactively 'tinyigrep-guess)))
     (?l  . ( (call-interactively 'tinyigrep-as-last-time)))

     (?c  . (t
	     (progn
	       (if (string= "-i" (or igrep-options ""))
		   (setq igrep-options nil)
		 (setq igrep-options "-i")))))


     (?u  . (t
	     (let (opt)
	       (setq
		opt
		(read-from-minibuffer
		 "Set igrep options: "
		 nil nil tinyigrep-:history-igrep-user-options))

	       ;;  Should I check something here before doing the assignment?
	       (setq igrep-options opt))))

     (?r  . (t (let ((sym (tinyigrep-recursive-var)))
		 ;;  toggle value
		 (if (eval sym)
		     (set sym nil)
		   (set sym t)))))

     (?v . (progn (tinyigrep-version)))))
  "TinyIgrep echo area menu.
The complete package manual is available at M-x tinyigrep-version

Commands:

i = Run `igrep'. This is the standard grep interface that Emacs
    has had for ages. Gives you a command line prompt where you
    can write the command and parameters.

g = Guess using databases. You can select from predefined
    databases that were set up at package load time. It is possible
    to define your own custom search directories and give it a
    search \"name\". See more with M-x tinyigrep-version.

l = Use same database for searching as last time. The name of the last
    database is rightmost string that us displayd after brackets [].

d = Run `dired-do-grep' which see.

v = Run `tinyigrep-version' which will print the package's manual.

Options:

c = Toggle case sensitive option `-i' in grep searches.
u = User option. Prompt for custom grep options.
r = Toggle recursive option `igrep-find'.")

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyigrep-menu (&optional arg)
  "Igrep command menu."
  (interactive "P")
  (ti::menu-menu 'tinyigrep-:menu arg))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyigrep-check-files (files)
  "Check that FILES can be grepped."
  (when files
    (dolist (elt (ti::list-make files))
      (if (ti::file-name-remote-p elt)
	  (error "TinyIgrep: Remote file name is not supported: %s" elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-lisp-elt (file name prg list &optional method)
  "Return tigr entry if FILE was found.

Input:

  FILE		package.el or Directory. This variable is evaluated.
  NAME	        the database completions name
  PRG		grep program to use
  LIST		files definitions and directories to grep
  METHOD	additional recursive grep method

You can refer to variable `dir' if path was found.

Example:

  (igr-push-elt
   (tinyigrep-db-lisp-elt
    \"bbdb.el\" \"bbdb\" \"zgrep\"
    '(list (concat dir \"*\"))))

This will dynamically find the directory where bbdb.el is stored and
assign local variable `dir' to it (which you see used here).
If bbdb.el is not found, then this return valid 'null' entry."
  (let* ((some  (eval file))
	 (path  (when (stringp some)
		  (if (file-directory-p some)
		      some
		    (locate-library some))))
	 (dir  (if path (file-name-directory path))))
    (if (null dir)
	(list "nil")
      (list name (list prg (eval list) method)))))

;;}}}
;;{{{ Igrep

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-recursive-var ()
  "Return igrep variable name."
  (if (boundp 'igrep-recursively)
      'igrep-recursively
    ;;  Newer, 2.55
    'igrep-find))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-fix-word (word)
  "Set WORD to '' if it contain only repeated chars.
Fix other things too."

  ;;  Be a bit nice to user; if he sits on repeated line like
  ;;  '------------------------' there is no point of
  ;;  offerering that as initial string.

  (if (and (> (length word) 5)
	   (string=
	    (make-string (length word)
			 (string-to-char (substring word 0 1)))
	    word))
      (setq word ""))

  ;;	Remove grabbed parenthesis and symbol(') ticks

  (when (stringp word)
    (setq word (replace-regexps-in-string "[?!`()'\"\r\n]" "" word)))

  (ti::remove-properties word))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-as-last-time (pattern arg-list)
  "Call `igrep' like last time, with same args. Allow editing.
The word to be grepped and the passed args can be changed.
PATTERN is new search patter and ARG-LIST is original argument list."
  (interactive
   (let* ((fid     "tinyigrep-as-last-time: ")
	  (args    tinyigrep-:igrep-previous-args)
	  (list    (mapcar 'prin1-to-string args))
	  (alist   (ti::list-to-assoc-menu list))
	  (word    (tinyigrep-fix-word (or (ti::buffer-read-space-word) "")))
	  dir
	  sel
	  elt)

     (tinyigrep-debug fid "interactive in:" args word)

     (if (null args)
	 (error "TinyIgrep: Sorry, no previus call arguments in memory"))

     (setq word
	   (read-from-minibuffer
	    "Igrep pattern: "
	    (ti::string-left word 40)
	    nil nil
	    'tinyigrep-:history-igrep))

     (setq sel				;Previous args, allow changing
	   (completing-read
	    "select: "
	    alist
	    nil nil
	    (car list)
	    'list))

     ;;  Read possibly modified entry

     (setq elt (read sel))

     (tinyigrep-debug fid "interactive out: " word elt)
     (list word elt)))

  (let* ((fid			"tinyigrep-as-last-time: ")
	 (default-directory	default-directory)
	 (igrep-program		igrep-program) ;we may change these
	 use-find
	 files)

    (tinyigrep-debug fid "in: " pattern arg-list)

    (if (not (ti::listp arg-list))
	(error "Tinyigrep: No args"))

    (setq default-directory	(nth 0 arg-list)
	  igrep-program		(nth 1 arg-list)

	  ;;   2 is the pattern we don't care now

	  files			(nth 3 arg-list)

	  use-find		(nth 4 arg-list))

    (if (ti::listp use-find)
	(let ((igrep-find-use-xargs (car use-find)))
	  (igrep-find igrep-program pattern files))

      (tinyigrep-debug fid "out: " igrep-program pattern files)
      (igrep igrep-program pattern files))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-guess-read-args ()
  "Ask args to igrep.
If you press RETURN when this function asks for database, then you
should give directory and file list shell regexp what to match.

References:
  `tinyigrep-:database'"
  (let* ((fid			"tinyigrep-guess-read-args:")
	 (table			tinyigrep-:database)
	 (file                  (buffer-file-name))
	 (extension             (and file (file-name-extension file)))

	 ;;  Remove entries named "nil", do not offer them when
	 ;;  completing the DB name

	 (car-list              (delete "nil" (mapcar 'car table)))

  	 (table-completions	(ti::list-to-assoc-menu car-list))

	 (word	 (tinyigrep-fix-word (or (ti::buffer-read-space-word) "")))
	 (bfn    (or (buffer-file-name) ""))
	 (ext    (cond
		  ((string-match "\\.[Cch][ch]?$" bfn)
		   "*[ch]")

		  ((string-match "\\.java$" bfn)
		   "*.java")

		  ((string-match "\\.el$" bfn)
		   (concat
		    (char-to-string
		     (aref (file-name-nondirectory bfn) 0)) ;first char
		    "*el"))

		  (t
		   (or (and extension (concat "*." extension))
		       (and file (ti::list-find auto-mode-alist file))
		       "*"))))

;;;	 (info   (ti::string-match "^[^-]+" 0
;;;			    (symbol-name
;;;			     (or (ti::id-info 'symbol) major-mode))))

	 (program-completions		;You really don't need others
	  (ti::list-to-assoc-menu
	   '("zgrep"
	     "egrep"
	     "grep")))
	 completion-ignore-case         ; Be case sensitive
	 use-find
	 dir
	 prg
	 pattern files
	 ans
	 elt
	 ret)

    (tinyigrep-debug fid "in:" bfn ext word car-list)

    (setq tinyigrep-:last-database "nil")
    (or
     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  db ...

     (progn
       (setq ans (completing-read
		  "use database: "
		  table-completions
		  nil
		  'match
		  nil
		  'tinyigrep-:history-database))

       (tinyigrep-debug fid "db selected answer:" ans)

       ;; Did we get valid database ?

       (if (or (ti::nil-p ans)
	       (null (setq elt (nth 1 (assoc ans table)))))
	   nil

	 (setq tinyigrep-:last-database ans)

	 (setq files	(nth 1 elt)
	       prg	(nth 0 elt)
	       use-find (and (> (length elt) 2)	;If there is 3rd element, get it
			     (nth 2 elt)))
	 (tinyigrep-debug fid "prog db ans selected:" files prg "use find" use-find)
	 t))				;progn ret val

     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ask ...

     (progn
       ;;  Read each directory and file to grep
       (setq files (ti::file-read-file-list))
       (tinyigrep-debug fid "prog db ask files:" files)
       (if (null files)
	   (error "Tinyigrep: Need file list"))
       (setq prg
	     (completing-read
	      "Program: " program-completions nil nil "egrep"))))


    (setq ret t)

    ;; ... ... ... ... ... ... ... ... ... ... ... ...  special / db . .

    (cond
     ((string= ans tinyigrep-:special-database)

      (setq dir
	    (read-from-minibuffer
	     "TinyIgrep Search: "
	     (if (buffer-file-name)
		 (concat
		  (file-name-directory  (buffer-file-name))
		  ext)
	       (car (ti::file-read-file-list)))))

      (setq files (list dir))

      (setq pattern
	    (read-from-minibuffer
	     (format "%s TinyIgrep: " (car files))
	     word nil nil
	     'tinyigrep-:history-igrep))

      (tinyigrep-debug fid "cond special db files:"))
     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... rest ..
     (t
      (setq pattern (read-from-minibuffer
		     "TinyIgrep: "
		     (ti::string-left word 40) ;limit length
		     nil
		     nil
		     'tinyigrep-:history-igrep))
      (setq files (mapcar 'expand-file-name files))))

    (tinyigrep-debug fid "out:" default-directory
		     "-" prg pattern files ret use-find)

    (tinyigrep-check-files files)

    (list
     (ti::remove-properties prg)
     (ti::remove-properties pattern)
     files
     ret
     use-find)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-guess (&optional prg pattern files do-it use-find)
  "Front-end to igrep.
Try to guess what directories to search according to buffer content.

If you give empty prompt (do not select any database completion),
then you can specify all arguments.

The grep is never case sensitive.

Input:

 PRG PATTERN FILES DO-IT USE-FIND"
  (interactive (tinyigrep-guess-read-args))

  (tinyigrep-debug "tinyigrep-guess in:"  default-directory "-"
	      prg pattern files do-it use-find)

  (let* ((fid			"tinyigrep-guess")
	 (default-directory	default-directory))
     (when do-it

       ;;  CD to the first directory. If it exists.
       ;;  If it doesn't the call-process in igrep will tell it to user.

       (when (file-directory-p (or (file-name-directory (car files)) ""))
	 (setq  default-directory (or (file-name-directory (car files))
				      default-directory))
	 (setcar files (file-name-nondirectory (car files))))


       (push (list
	      default-directory
	      prg
	      pattern
	      files
	      use-find)
	     tinyigrep-:igrep-previous-args)

       ;; (save-some-buffers)

       (tinyigrep-debug fid "Calling IGREP"
		   'default-directory default-directory
		   'prg prg
		   'pattern pattern
		   'files files)

       (if (ti::listp use-find)
	   (let ((igrep-find-use-xargs (car use-find)))
	     (igrep-find prg pattern files))
	 (igrep prg pattern files)))
     do-it))

;;}}}

(provide   'tinyigrep)
(run-hooks 'tinyigrep-:load-hook)

;;; tinyigrep.el ends here
