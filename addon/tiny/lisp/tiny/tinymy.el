;;; @(#) tinymy.el --- Collection of simple solutions.
;;; @(#) $Id: tinymy.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1995-11
;; Keywords:	    tools
;;
;; To get information on this program use ident(1) or do M-x tinymy-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinymy|Jari Aalto|jari.aalto@poboxes.com|
;; user ('my') functions. Simple shar/tar/uu/compilation, periodic save|
;; 2002-08-02|$Revision: 1.1 $|~/misc/tinymy.el.Z|

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
;; ~/.emacs startup file. Rip code with with tinylib.el/ti::package-rip-magic
;;
;;* _
;;*     (add-hook  'find-file-hooks  'tinymy-find-file-hook) ;; DOS display
;;*     (require 'tinymy)
;;* _
;;
;; If you get key binding conflict when you load this package, either
;; relocate keys, modify `tinymy-:define-key-table' or use forced bindings
;; by adding this statement prior `require' command.
;;
;;*     (setq tinymy-:define-key-force t)
;;* _
;;
;; AUTOLOAD SETUP INSTRUCTIONS
;;
;; This package can't be autoloaded easily, because it installs timers
;; and many global bindings. One possible way to autoload this package is
;; to rely on the fact that you will most likely use function to match
;; parens: like "(this)". The autoload below is quite tricky, see if
;; you can learn from it. What if effective does, is a) put temporary
;; function under key "%", when you press it b) function gets called
;; and tinymy.el is loaded c) it wipes itself away and assigns
;; function `tinymy-vi-type-paren-match' to the "%" key.
;;
;;* (global-set-key "%"
;;*		 (definteractive
;;*		   (let ((function (lookup-key global-map "%")))
;;*		     (global-unset-key "%") ;; tinymy.el doesn't complain
;;*		     (require 'tinymy)
;;*		     ;;  Now run whatever user had there.
;;*		     (if function
;;*			 (funcall function)
;;*		       (self-insert-command 1))
;;*		     ;;  Second time, direc calls here
;;*		     (global-set-key
;;*		      "%"
;;*		      'tinymy-vi-type-paren-match))))
;;* _
;;
;; There are some scripts included in this module and you can unpack them
;; with following commands. You need `pgp' and `tar' executable in path for
;; this to work. The extra scripts are for compile command command C-z c c,
;; but you don't need them necessarily.
;;
;;      M-x load-library RET tinymy RET
;;      M-x tinymy-install-files
;;
;; Modify the following variable and put your own installation there if the
;; default setting is interfering your setup. Please remember to look the
;; _source_ code of `tinymy-define-keys' which is run when package loads.
;; Function overrides some default Emacs key bindings.
;;
;;	tinymy-:define-key-table
;;
;;*     ;;  Redefine hook so that it doesn't
;;* 	;;  override Emacs keys.  Define them somewhere else.
;;*
;;* 	(defconst tinymy-:load-hook '(tinymy-install tinymy-alias))
;;* _
;;
;;   If you have any questions, use 'submit' function. In case of error
;;   or misbehavior, turn on the debug too and send the debug result and
;;   describe what you did and where went wrong.
;;
;;      M-x tinymy-debug-toggle
;;      M-x tinymy-submit-bug-report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, Nov 1995
;;
;;      Emacs startup files started to look quite interesting:
;;
;;          emacs-rc-tips     emacrs-rc-el      emacrs-rc-el      emacrs-rc-el.sun
;;	    emacs-rc-18	      emacs-rc-19	emacs-rc-abb      emacs-rc-compile
;;          emacs-rc-debug    emacs-rc-default  emacs-rc-font     emacs-rc-ding
;;          emacs-rc-font.b   emacs-rc-gnus     emacs-rc-hooks
;;          emacs-rc-init     emacs-rc-init2    emacs-rc-mail
;;          emacs-rc-o        emacs-rc-o-19     emacs-rc-out
;;          emacs-rc-path     emacs-rc-pc       emacs-rc-prog     emacs-rc-set
;;          emacs-rc-test     emacs-rc-time     emacs-rc-tips     emacs-rc-vc
;;          emacs-rc-w3       emacs-rc-x-menu   emacs-rc-xe
;;	    emacs-rc-dired
;;          ..
;;
;;      Private functions seemed to grow all the time, most of which were
;;      turned into packages, but sometimes it was just function or two
;;      that made a life with Emacs easier.  What you see here is a
;;      selection of so called general *my* functions. The term *my* does
;;      not refer to *mine*, but has a background in function
;;      naming. Remember? All user functions are recommended to be named
;;      so, that the first word is `my-', like `my-FUNC-NAME-HERE'
;;
;;  Overview of features
;;
;;      Timer processes
;;
;;      o   RMAIL/other buffers saved in regular intervals.
;;      o   Revert buffer in background and ask confirmation, if file
;;          has changed on disk. By <duthen@cegelec-red.fr>
;;          (Jacques Prestataire) This feature is automatically disabled
;;          if autorevert.el is present and running.
;;      o   Mail lock watchdog. If you have this lock in your file system,
;;	    you cannot receive mail.
;;
;;      Buffer
;;
;;	o   Cursor changes shape according to `overwrite-mode'
;;      o   Rename any buffer with one key `C-z' `n' to be able to launch
;;          e.g. new *shell* or *mail* buffer.
;;      o   Scroll command goes to window end/beginning and does not scroll
;;          immediately. See variable `tinymy-:scroll-mode' for more.
;;      o   Trim trailing whites paces from the buffer when file
;;          is saved.
;;      o   Stamp file when buffer is saved by looking into RCS-like tags
;;          "Docid:", "Contactid:" and LCD package entry fields.
;;      o   Gzip or unzip current file buffer.
;;      o   Add up numbers in rectangle area
;;
;;      Compile
;;
;;      o   Guess compile command by looking at the buffer content
;;          Configure variable `tinymy-:compile-table' and
;;          `tinymy-:compile-command-c-code'. The compile command you
;;          chose is buffer local and lasts until you change it.
;;          This is different than hitting M-x compile, because compile
;;          Does not "remember" each buffer's correct compile command.
;;
;;	Files
;;
;;	o   Toggle write/read-only file permissions on disk with
;;	    C-x q or `M-x' `tinymy-buffer-file-chmod'
;;	o   If file saved had #!, it is automatically made chmod u+x.
;;	    Idea and code by Jesper Pedersen <blackie@imada.ou.dk>
;;
;;      Gnus, mail
;;
;;      o   Save lisp package in buffer like *mail* to file: find
;;	    package regions.
;;      o   Copy current buffer's contents to new mail buffer and
;;	    set subject line. You can send diff buffers and file buffers
;;	    conveniently this way: `C-z' `m' (Zend buffer as Mail)
;;
;;      Keys
;;
;;      o   Jumps to matching paren {([". _Bound_ to key "%".
;;      o   Better word movement: LikeThisInC++Mode.
;;          Moving forward/backward always keeps cursor at the
;;          beginning of word. See also `c-forward-into-nomenclature'
;;          _Bound_ to keys `C-left', `C-right' in X and `Esc-b', `Esc-f'
;;          in non-windowed Emacs.
;;      o   PgUp and PgDown behave differently; they jump to
;;          window's beg/end first and only next key hit scrolls.
;;          _Bound_ to keys `prior' and `next'. Check if your keyboard
;;          produces another pgUp and PgDown events.
;;
;;	Line formatting
;;
;;	o   Fixing all backslash(\) lines in current paragraph to the
;;	    same column as the starting line. Very useful in makefile mode,
;;	    shell mode or when writing C/C++ macros. It even inserts missing
;;	    backslashes.
;;
;;	Mouse
;;
;;	o   Point window and it gets cursor focus: The frame is
;;	    raised and window selected. No need to click window any more.
;;	o   Show File information in echo-area: Point mouse near
;;	    the end of window and Displayed info contains
;;	    BUFFER MODES SIZE PATH. You do not consume your mode line
;;	    or frame title any more for buffer specific information.
;;	    Example output:
;;
;;		TinyMy: -rw-r--r-- 108k /users/jaalto/elisp/tinymy.el
;;
;;      Shell
;;
;;      o   Easy shar/tar/UU commands. configure variables
;;	    `tinymy-:shar-command' and `tinymy-:tar-command'
;;
;;	vc
;;
;;	o   Key C-x C-q now won't call vc blindly. To prevent mistakes,
;;	    a confirmation will be asked. You can also just toggle the
;;	    buffer's read-only flag, without engaging vc.
;;
;;      Window
;;
;;      o   Flip the order of two windows
;;
;;  Minor modes in this package
;;
;;     Sort minor mode
;;
;;	If you have data in columns, use `C-cmS' or `M-x' `tinymy-sort-mode'
;;	to toggle sort mode on and off. With it you can sort columns 1-9
;;	easily. Mode line indicator is "S"
;;
;;     Display minor mode
;;
;;	Hot key `C-cmD' or `M-x' `tinymy-display-mode'. If you have DOS file and
;;	you load it into Emacs, you usually see the ^M characters. With the
;;	display mode active, it hides them from the screen, but they are
;;	still in the buffer. This display mode will hide other non-ascii
;;	characters too. See the source if you want to find out the full
;;	repertoire.
;;
;;	This display mode is _automatically_ turned on when you load an
;;      dos-like file that has ^M character at the end of line.
;;      Mode line indicator is "D".
;;
;;      Another display made selection also let's you view MS Word binary
;;      files in some extent.
;;
;;  Features immediately activated when package loads
;;
;;	o   You own Emacs lisp package LCD stamp is updated every time
;;	    you save the package. Configure `tinymy-:stamp-lcd-regexp'
;;	    to detect your packages.
;;	o   Your most valuable buffers are periodically saved.
;;	    Configure variable `tinymy-:save-buffer-modes' and
;;	    `tinymy-:save-buffer-regexp'
;;	o   You mailbox lock is kept on eye on, if the lock remains,
;;	    you won't be able to receive mail. (safety measure).
;;	o   If you use procmail you want to configure
;;	    `tinymy-:mail-check-inbox-file-permissions'
;;	    otherwise, your mailbox's mode permissions are kept eye on:
;;	    "Permission error: -rw-------" warning will be show if the
;;	    mailbox doesn't have right modes.
;;	o   Automatic window selection when you point it with mouse cursor.
;;	    See `tinymy-:install-select-window-auto'.
;;	o   When buffer that has `#!' to indicate shell
;;	    script, is save, the +x flag is set on for the file.
;;
;;  What commands are defined when you load this file?
;;
;;      It's better to look at the code of this file, than to explain all the
;;      key definitions here, because I may not remember update this
;;      text section every time I add new interactive commands to the file.
;;
;;      All the new interactive commands can be found from these two
;;      functions:
;;
;;          tinymy-define-keys
;;          tinymy-mail-common-keys
;;
;;      See their description, or alternatively hit
;;
;;	    C-h m				 ;; to view all bindings
;;	    M-x delete-non-matching-lines tinymy ;; show bound keys
;;
;;  Key bindings
;;
;;      When you load this package, you can also install global key-bindings that
;;      if you set the load hook:
;;
;;	    (setq tinymy-:load-hook
;;              '(tinymy-install
;;                tinymy-define-keys
;;                tinymy-define-key-extra
;;                tinymy-alias)
;;
;;      If you want to use your own bindings, use it like this:
;;
;;	    (setq tinymy-:load-hook '(tinymy-install my-tinymy-keys tinymy-alias)
;;                                                   ^^^^^^^^^^^^^^
;;          (defun my-tinymy-keys ()
;;            ;; .. different global key mappings)
;;
;;      There is table of global bindings which you can modify if the
;;      bindings clash: the auto install will warn you about this
;;      automatically and your own bindings are not replaced by default.
;;      See variable: `tinymy-:define-key-table'

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-and-compile
  (ti::package-package-require-timer)
  (autoload 'compile-internal               "compile")
  (autoload 'operate-on-rectangle           "rect")

  (defvar gnus-article-buffer)
  (defvar gnus-original-article-buffer)
  (defvar gnus-summary-buffer))

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyMy tinymy-: tools
  "Collection of small so called 'my' utility functions.
The full feature list is in the source code documentation, read it well.")


;;}}}
;;{{{ setup: variables

;;; .......................................................... &v-bind ...
;;; Change this table if you have conflicting bindings.
;;;

(defcustom tinymy-:define-key-force nil
  "*If non-nil; assign keys without any check."
  :type 'boolean
  :group 'TinyMy)


(defcustom tinymy-:define-key-table
  '(
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. C-x . .
    ;;  The 'rectangle' map. This sould be free

    ("\C-xrA"	. tinymy-add-rectangle)

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. C-c . .
    ;; minor modes in 'm' map

    ("\C-cmv"   . tinymy-view)
    ("\C-cmS"   . tinymy-sort-mode)
    ("\C-cmD"   . tinymy-display-mode)

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. C-z . .
    ;; Pick "c" for all (c)ompile commads, now define additional
    ;; "c" for this particular command

    ("\C-zcc"   . tinymy-compile-run-command)

    ("\C-z\\"   . tinymy-backslash-fix-paragraph)
    ("\C-zm"    . tinymy-mail-buffer)
    ("\C-zn"    . tinymy-toggle-buffer-name)
    ("\C-zS"	. ti::buffer-surround-with-char)

    ;;  Oher miscellaneout to "x" extra map

    ("\C-zxc"   . tinymy-copy-file)   ;; Make backup (RCS version included)

    ("\C-zxf"   . tinymy-package-save-to-file)
    ("\C-zxt"   . tinymy-trim-blanks)

    ("\C-zxw"   . tinymy-flip-windows)
    ("\C-zxz"	. tinymy-gzip-buffer-file)

    ;;  's' for shell commands

    ("\C-zxss"  . tinymy-shar)
    ("\C-zxst"  . tinymy-tar))
  "*Define command to `global-map' keys.
See also source code for `tinymy-define-keys' which will overwrite
default Emacs keybindings if installed in `tinymy-:load-hook'.

Format '((KEY . FUNCTION) (K . F) ..)"

  :type '(repeat
	  (cons
	   (string :tag "Key Bind sequence")
	   function))
  :group 'TinyMy)

;;; ......................................................... &v-hooks ...

(defcustom tinymy-:load-hook '(tinymy-install)
  "*Hook that is run when package is loaded.
The default value is '(tinymy-install)"
  :type 'hook
  :group 'TinyMy)

(defcustom tinymy-:mail-buffer-hook nil
  "*This hook run last in `tinymy-mail-buffer' function."
  :type 'hook
  :group 'TinyMy)

;;; ....................................................... &v-private ...

(defvar tinymy-:package-exist-mview (locate-library "mview.el")
  "Location of mview.el if exists -- view minor mode.")

;;; ....................................................... &vu-config ...
;;; all "vu" -- "variable user" sections are meant for user configurable

(defcustom tinymy-:install-select-window-auto 'yes
  "*Variable is used only in window system.
The automatic window selection function selects window by pointing
at it with mouse . No clicking is needed. However if you use menu bar, it is
a bit difficult to use this automatic selection feature, because the
menu bar reflects the current window: When you are at lower window and reach
for the menu bar, the upper window gets selected and the menu bar reflects
that window. You never get menu bar for the other windows but for the
topmost one. (Well, you can go round of Emacs, and then reach for
menu-bar, but that's a bit awkward)

Values in this variable:

  'yes
  'no
  'ask

To turn off the menu bar use: (menu-bar-mode -1) in your ~/.emacs."
  :type '(choice
	  (const yes)
	  (const no)
	  (const ask))
  :group 'TinyMy)


(defcustom tinymy-:register ?r
  "*An Emacs register where to put results of commands.
User can then afterwards yank the result into desired buffer."
  :type 'character
  :group 'TinyMy)


(defcustom tinymy-:scroll-mode 'window
  "*If non-nil, then `tinymy-scroll' does not immediately scroll.
The following happen if variable is non-nil.
o  up: if the cursor is not at the window's start line, go there
o  up: if cursor is at window's top, line, now scroll
o  down: --''-- behaves same as up"
  :type 'boolean
  :group 'TinyMy)


(defcustom tinymy-:copy-file-suffix ".orig"
  "Suffix to add when making copy of file with `tinymy-copy-file'.
This variable is only used in interactive call. Default extension
is \".orig\", same as used by Unix 'patch' program to save original
working file.

If the version number can be found from file, that is suggested instead
of this suffix."
  :type 'string
  :group 'TinyMy)

;;; ...................................................... &v-matching ...

(defcustom tinymy-:vi-type-paren-match-special-list '( ?\" ?\' ?\$ )
  "*List of special character to matched in \\[tinymy-vi-type-paren-match].
If the sentence delimited by these chars spread multiple lines,
the missing part is searched backward.

If you call \\[tinymy-vi-type-paren-match] with optional arg, then
the search is forced FORWARD."
    :type '(repeat character)
    :group 'TinyMy)


;;  This could have been (CH . CH) list but because XEmacs20
;;  has different character handling that Emacs; we prefer to check
;;  strings.
;;
;;  This is not configurable variable right now, because the match
;;  function uses hard coded regexps.

(defconst tinymy-:vi-type-paren-match-list
  '( ( "(" . ")" )
     ;; NOPE, DO NOT add these. It won't work - the reason is currently unknown.
     ;;
     ;;     ( "<" . ">" )
     ( "{" . "}" )
     ( "[" . "]" ))
  "List of character string pairs to match.

Format:

  ((BEGIN-CHARACTER-PAIR-STR . END-CHARACTER-PAIR-STR)
   (B . E)
   ..)

Example:

  '( ( \"(\" . \")\" )
     ( \"{\" . \"}\" )
     ( \"[\" . \"]\" )))")


;;; ......................................................... &vu-word ...

(defcustom tinymy-:move-word-set "-[]_$%@#&*\":;,.{}()<>/\\ \t\n"
  "*How to move forward/backward word. This is character set."
  :type '(string :tag "Charset")
  :group 'TinyMy)

(defcustom tinymy-:move-word-case-set "-[]_$%@#&*\":{}()<>/\\ \t\na-z"
  "*How to move forward/backward word. This is character set.
used only over mixed case words."
  :type '(string :tag "Charset")
  :group 'TinyMy)

(defcustom tinymy-:move-word-case-modes
    '(c-mode
      c++-mode
      cc-mode
      java-mode
      sh-mode
      bash-mode
      csh-mnode
      ksh-mode
      jde-mode
      jdee-mode
      perl-mode
      cperl-mode
      php-mode
      jsp-mode
      text-mode)
  "*Modes where `tinymy-:move-word-case-set' is used."
  :type '(repeat function)
  :group 'TinyMy)


;;; ........................................................ &vu-shell ...

(defcustom tinymy-:tar-command "tar -cf"		;; -c Create a new archive
  "*Tar create command, e.g. used in `tinymy-tar'."
  :type '(string :tag "Shell command")
  :group 'TinyMy)


(defcustom tinymy-:shar-command "shar -a -c -C -e -t -u"
  "*Shar command used by `tinymy-shar'.
In HP-UX:

 -a       ,do not protect them specially (uu)
 -c       ,data-integrity check using wc
 -C       ,Insert a line of the form --- cut here ---
 -e       ,code that prevents shar .. overwrite existing files.
 -t       ,Write diagnostics to stdout
 -u       ,Assume that the remote site has uudecode"
  :type '(string :tag "Shell command")
  :group 'TinyMy)


;;; ...................................................... &vu-compile ...


(defcustom tinymy-:compile-table
  (list
   '("perl"	         . "perl -w %s")
   '("code-shell-sh"     . "sh -x %s")
   '("code-shell-bash"   . "bash -x %s")
   '("code-shell-t?csh-" . "csh -x %s")
   '("code-shell-ksh"	 . "ksh -x %s")
   '("awk"		 . "awk -f %s")
   '("c[+]+\\|^cc?-\\|code-c"  . tinymy-compile-cc-command)
   '("bat"    . "%s")

    (cons "lisp"
	  (concat
	   (if (emacs-p)
	       "emacs"
	     "xemacs")
	   " -batch -f batch-byte-compile %s"))

    '("java" . "javac %s")

    (cons "php"
	  (let ((php  (executable-find "php"))
		(php4  (executable-find "php4")))
	    (if (or php php4)
		(concat (or php php4) " %s"))))

    (cons "sql"
	  (let ((mysql (executable-find "mysql"))
		(psql  (executable-find "psql")))
	    (cond
	     (mysql
	      "mysql DB < %s")
	     (psql
	      "psql < %s")
	     (t
	      "%s")))))
  "*Compilation table, how to run the code through interpreters.
The command is put into %s in the COMPILE-COMMAND part.

format:

  '((REGEXP-for-buffer-type  . COMPILE-COMMAND)
    (REGEXP-for-buffer-type  . COMPILE-COMMAND)
    ..)

REGEXP

  The regexp is like 'code-c' 'code-pascal' or alternatively a
  `mode-name' if buffer content can't be identified. See
  tinylibid.el and function `ti::id-info' for more.

COMPILE-COMMAND

  STRING with %s where `buffer-file-name' is inserted.

  -- If string, then this command is suggested for file.
  -- if something else, the content is evaled and it should return
     compile command STRING with %s for file name.
  -- If function, function must return complete compile command,
     with _no_ %s.

Example:

  Suppose you have several perl interpreters and you want to use the
  shebang interpreter (first line in the script) for your project perl
  scripts. The following code:

  -- Looks up the existing perl compile command and stores it to ELT
  -- Changes the right hand COMPILE-COMMAND to Lisp form that
     determines the perl command according to file name. Function
     `ti::buffer-shebang' reads the command interpreter from the first line.

    (add-hook 'tinymy-load-hook 'my-tinymy-compile-customisations)
    (autoload 'aput \"assoc\")

    (defun my-tinymy-compile-customisations ()
      (aput 'tinymy-:compile-table
            \"perl\"
	    '(if (string-match \"project\" buffer-file-name)
	       (concat (or (ti::buffer-shebang) \"perl\") \" -w  %s\")
	     \"perl -w %s\")))

  If you always want to use the shebang command interpreter, then you
  would simply write

   (add-hook 'tinymy-load-hook 'my-tinymy-compile-customisations)
   (autoload 'aput \"assoc\")

    (defun my-tinymy-compile-customisations ()
      (aput 'tinymy-:compile-table
            \"perl\"
	    '(concat (or (ti::buffer-shebang) \"perl\") \" -w %s\")))

  After this package has been loaded. (Place customizations like this
  to `tinymy-:load-hook'."
  :type '(retpeat
	  (string :tag "Regexp")
	  (string :tag "Shell command"))
  :group 'TinyMy)

;;; ................................................ &vu-periodic-save ...

(defcustom tinymy-:save-buffer-modes
  '(rmail-mode
    vm-mode
    ;;  Gnus saves buffers to draft backend
    message-mode)
  "*Buffers that have this mode on are periodically saved."
  :type '(repeat function)
  :group 'TinyMy)

(defcustom tinymy-:save-buffer-regexp nil
  "*If variable `buffer-file-name' match regexp, buffer is periodically saved."
  :type '(string :tag "Regexp")
  :group 'TinyMy)


;;; ........................................................ &vu-stamp ...

(defcustom tinymy-:stamp-lcd-regexp (concat "[|,]" user-mail-address "[|]")
  "*When you save package.el, the LCD entry's time is stamped too.
But only if this regexp is found from the file. This regexp should
match the first line after line

  ;; LCD Archive Entry:

See `tinymy-file-stamp-elisp-lcd'."
  :type '(string :tag "Regexp")
  :group 'TinyMy)


(defvar tinymy-:stamp-user-mail-address
  ;;  Reflect changes in user-mail-address this way automatically.
  (function (lambda () user-mail-address))
  "Email address that is inserted into Contactid: field.
The value can be a plain string or function to return an email address
See `tinymy-file-stamp'.")

;;}}}
;;{{{ setup: version

;;; ......................................................... &version ...

;;;###autoload (autoload 'tinymy-version "tinymy" "Display commentary." t)

(eval-and-compile
(ti::macrof-version-bug-report
 "tinymy.el"
 "tinymy"
 tinymy-:version-id
 "$Id: tinymy.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinymy-:version-id
   tinymy-:debug
   tinymy-:vi-type-paren-match-list
   tinymy-:define-key-force
   tinymy-:define-key-table
   tinymy-:load-hook
   tinymy-:mail-buffer-hook
   tinymy-:package-exist-mview
   tinymy-:install-select-window-auto
   tinymy-:register
   tinymy-:scroll-mode
   tinymy-:copy-file-suffix
   tinymy-:vi-type-paren-match-special-list
   tinymy-:move-word-set
   tinymy-:move-word-case-set
   tinymy-:move-word-case-modes
   tinymy-:tar-command
   tinymy-:shar-command
   tinymy-:compile-table
   tinymy-:save-buffer-modes
   tinymy-:save-buffer-regexp
   tinymy-:stamp-lcd-regexp
   tinymy-:stamp-user-mail-address
   tinymy-:force-revert
   tinymy-:revert-in-progress
   tinymy-:revert-buffer-info-list
   tinymy-:window-previous)
 '(tinymy-:debug-buffer)))

;;;### (autoload 'tinymy-debug-toggle "tinymy" t t)

(ti::macrof-debug-standard "tinymy" "-:")

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ install: main

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymy-define-keys ()
  "Install keys."
  (interactive)

  (when (boundp 'shared-lisp-mode-map)
    (defvar shared-lisp-mode-map nil)  ;; Byte compiler silencer
    (define-key shared-lisp-mode-map    "%" 'tinymy-vi-type-paren-match))

  (define-key emacs-lisp-mode-map       "%" 'tinymy-vi-type-paren-match)
  (define-key lisp-mode-map             "%" 'tinymy-vi-type-paren-match)

  ;;  was C-xq was kbd-macro-query, but I don't believe people use it

  (global-set-key "\C-xq"    'tinymy-buffer-file-chmod)


  ;;  Redefine scroll keys, we don't confirm these...

  (global-set-key [(prior)]             'tinymy-scroll-up)
  (global-set-key [(next)]              'tinymy-scroll)

  ;;  In XEmacs these already have default bindings, but we override them.

  (global-set-key [(control right)]     'tinymy-forward-word)
  (global-set-key [(control left)]      'tinymy-backward-word)
  (global-set-key [(control up)]	'tinymy-beginning-of-defun)
  (global-set-key [(control down)]	'tinymy-end-of-defun)

  (unless (ti::xe-window-system)
    (global-set-key [(meta f)] 'tinymy-forward-word)
    (global-set-key [(meta b)] 'tinymy-backward-word))

  ;; Use C-z prefix because it is most user friendly to pinky
  ;; Pretty useless in X-windowed Emacs, and in windowed
  ;; Emacs you seldom use suspend-emacs because emacs has M-x shell

  (ti::use-prefix-key global-map "\C-z")

  ;;  Set global keys, confirm these

  (mapcar
   (function
    (lambda (x)
      (if tinymy-:define-key-force
          (define-key global-map (car x) (cdr x))
        (ti::define-key-if-free global-map
          (car x)
	  (cdr x)
	  'tinymy-define-key-error))))
   tinymy-:define-key-table)

  ;; .................................................... &emacs-modes ...

  (add-hook 'makefile-mode-hook 'tinymy-makefile-mode-hook)

  (defun tinymy-makefile-mode-hook ()
    "Define key C-\\ to adjust \\ continuing lines."
    (define-key
      (symbol-value 'makefile-mode-map) "\C-\\"
      'tinymy-backslash-fix-paragraph)))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymy-define-keys-extra ()
  "Define extra global keys."
  (interactive)
  (global-set-key "%"         'tinymy-vi-type-paren-match)
  (global-set-key "\C-x\C-q"  'tinymy-read-only))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload  'tinymy-install-files "tinymy" "" t)
(ti::macrof-install-pgp-tar tinymy-install-files "tinymy.el")

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymy-install ()
  "Configures Emacs variables and bindings."
  (interactive)

  ;; ................................... automatic mouse window cursor ...

  (when (ti::xe-window-system)
    (let ((ok
	   (or (eq tinymy-:install-select-window-auto 'yes)
	       (and
		 (eq tinymy-:install-select-window-auto 'ask)
		 (null
		  (y-or-n-p
		   (concat
		    "TinyMy: Do you use menubar? "
		    "Auto window select not available if you use it")))))))
      (cond
       ((and ok
	     (win32-p)         ;; Bug in Win32, works in Unix Emacs
	     (emacs-p "21.1"))
	;;  Bug in 21.1 make Emacs completely to behave starangely
	;;  if you enable mouse-movement tracking. Use this code to
	;;  check your Emacs.
	;;
	;;  (progn
	;;    (defun test (event)
	;;    	(interactive "e")
	;;    	(message "mouse movement ok")
	;;    	(discard-input))
	;;    (setq track-mouse t)
	;;    (global-set-key [(mouse-movement)] 'test))
	;;
	(message "Tinymy: [NOTICE] tinymy-mouse-movement-handler \
has changed in Emacs 21.x. Unable to install handler."))
       ((and ok (emacs-p))
	(defvar track-mouse nil)	;ByteComp silencer in XEmacs
	(setq track-mouse t)		;This is essential

	;;  Make sure that this handler is not occupied yet

	(if (memq (lookup-key global-map [(mouse-movement)])
		  '(tinymy-mouse-movement-handler
		    ignore
		    nil))
	    (global-set-key [(mouse-movement)]
			    'tinymy-mouse-movement-handler)
	  (message "\
** tinymy.el: can't install mouse-movement handler, already occupied.")))

       ((and ok (xemacs-p))
	;; See also package mode-motion+.el
	;;
	;; setting mode-motion-hook it buffer local. Hm. And it is called from
	;; default-mouse-motion-handler inside save-window-excursion.
	;; Not good. I can't change window with that hook.
	;;
	;; --> we have to pull out bigger hammer.

	(defvar mouse-motion-handler nil) ;ByteComp silencer in Emacs

	(if  (eq (symbol-value 'mouse-motion-handler)
		 'default-mouse-motion-handler)
	    (defconst mouse-motion-handler
	      'tinymy-default-mouse-motion-handler)
	  (message "\
** tinymy.el: Can't install auto-select: mouse-motion-handler not default."))

	(defun tinymy-default-mouse-motion-handler (event)
	  (prog1 (ti::funcall 'default-mouse-motion-handler event)
	    (tinymy-mouse-movement-handler event)))))))

  ;; ............................................ periodic buffer save ...

  ;; save buffers every 20 minutes.
  ;; To delete timer, use this command in XEmacs and Emacs
  ;;
  ;;  (ti::xe-timer-cancel
  ;;    (car-safe (ti::xe-timer-elt 'tinymy-save-buffers-periodically)))

  (unless (ti::xe-timer-elt 'tinymy-save-buffers-periodically)
    (run-at-time "60 sec" (* 20 60) 'tinymy-save-buffers-periodically))


  (unless (locate-library "autorevert")	;Emacs 20.3
    (unless (ti::xe-timer-elt 'tinymy-revert-some-buffers)
      (run-at-time "60 sec" (* 15 60) 'tinymy-revert-some-buffers)))

  ;; .............................................. mail lock watchdoc ...


  (ti::add-hooks 'write-file-hooks 'tinymy-write-file-hook))


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-define-key-error (key def)
  "Call back function. Warn about conflicting key binding for KEY and DEF."
  (message "TinyMy: Cannot auto-install, key already occupied: %s %s"
         key def))


;;}}}
;;{{{ buffer: stamp

;;; ............................................................ &file ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-write-file-hook ()
  "Stamp file."
  (ti::widen-safe
    (tinymy-trim-blanks)
    (tinymy-file-stamp)			;any file
    (tinymy-file-stamp-elisp-lcd)	))	;.el

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymy-buffer-file-chmod (&optional verb)
  "Toggle current buffer's Read-Write permission permanently on disk. VERB.
Does nothing if buffer is not visiting a file or file is not owned by us."
  (interactive)
  (let* ((file  (buffer-file-name))
	 stat)
    (ti::verb)
    (when (and file (file-modes file))	;File modes is nil in Ange-ftp
      (setq stat (ti::file-chmod-w-toggle file))
      (when verb
	(cond
	 ((eq stat 'w+)
	  (message "TinyMy: chmod w+")
	  (setq buffer-read-only nil))
	 ((eq stat 'w-)
	  (message "TinyMy: chmod w-")
	  (setq buffer-read-only t))
	 (t
	  (message "TinyMy: couldn't chmod")))
	(ti::xe-modeline-update)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-file-stamp ()
  "Help tracking document change.
Your `save-buffer' function now stamps following
fields every time you save your document. If they don't exist, it
behaves exactly like normal `save-buffer'.

  $Docid\:$ 		the date/time and full user name.
  $Contactid\:$		the tis-contactid-info string.

If there are multiple stamps, only the first one met, starting from
the beginning of file, will be touched. This approach has been chosen
so that it would take least time possible to do stamping before file
is saved.
  You might wonder why there is $ marks surrounding the words. The
format for revision control system 'GNU rcs' requires $ marks if you
run their program ident\(1\) over the file. Ident detects only rigidly
formatted Directives  so that any keyword of the following form can be
printed out.

  $keyword:<space>.......desc text<space>$

You cannot modify contents of these fields by hand, because function
kills old stamping and writes new ones every time it's called. It is
also recommended that you _put_ directive '@(#)' at the beginning
of these stamps; that way the UNIX 'what'-command can display these
fields.
  If there were any errors during stamping the user will be notified.
More information is available in tinystamp.el where this function
belongs."
  (interactive)
  (let* ((case-fold-search  nil)

	 (re1"[$]Docid")		;optimyze search
	 (re1-1  ": +\\([^'\"\\$\r\n]*\\)[$]")

	 (re2    "[$]Contactid" )
	 (re2-1  ": +\\([^'\"\\$\r\n]*\\)[$]")

	 (time   (ti::date-standard-date 'short))
	 (user   (or (user-full-name)
		     (error "TinyMy: Please define `user-full-name'.")))
	 (doc1   (concat time " " user " "))

	 (email  tinymy-:stamp-user-mail-address)

	 (doc2   (or (and (stringp email)
			  email)
		     (and (functionp email)
			  (funcall email))
		     (error
		      "TinyMy: Invalid `tinymy-:stamp-user-mail-address' %s"
		      tinymy-:stamp-user-mail-address)))

	 (no-re  "RMAIL\\|VM\\|GNUS\\|article")	;do not touch these
	 str)
;;;    (setq R1 re1 R11 re1-1 R2 re2 R22 re2-1)
    (cond
     ((and (not (string-match no-re (symbol-name major-mode)))
	   (not (string-match no-re (buffer-name))))
      (ti::widen-safe
       (save-excursion
	 (ti::pmin)				; case 1
	 (when (and (re-search-forward re1 nil t)
		    (looking-at re1-1))
	   (setq str (save-match-data (ti::string-remove-whitespace (match-string 1))))
;;;           (ti::d! "Docid" str doc1)
	   (unless (string= str doc1)
	     (ti::replace-match 1 doc1)))

	 (ti::pmin)				; case 2
	 (when (and (re-search-forward re2 nil t)
		    (looking-at re2-1))
	   (setq str (save-match-data (ti::string-remove-whitespace (match-string 1))))
;;;           (ti::d! "Contactid" str doc2)
	   (unless (string= str doc2)
	     (ti::replace-match 1 (concat doc2 " " ))))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-file-stamp-elisp-lcd ()
  "Updated .el file's LCD entry for file if it's owned by user.
See `tinymy-:stamp-lcd-regexp'."
  (let* ((email-re    tinymy-:stamp-lcd-regexp)
	 (lcd-re      "^;; +LCD.*entry:")
	 end
	 list
	 date)
    (cond
     ((string-match "\\.el" (or (buffer-name) ""))
      (save-excursion
	(ti::pmin)

	;;  Try to find LCD indication and my mail address.

	(cond
	 ((and (re-search-forward lcd-re nil t)
	       (save-excursion		;Limit the next search
		 (setq end
		       (or (re-search-forward "^[ \t]*$" nil t) (point))))
	       (re-search-forward email-re end t))
	  (setq list (ti::date-time-elements 'zero-form))

;;;	  (setq date ;; Was old 28-Sep-1997 format
;;;		(format "%s-%s-%s" (nth 0 list) (nth 5 list) (nth 6 list)))

	  ;; Prefer ISO, universal date format

	  (setq date
		(format "%s-%s-%s" (nth 6 list) (nth 1 list) (nth 0 list)))

	  ;; Now replace the contents

	  (when (re-search-forward "^;+ \\([-0-9a-z]+-[-0-9a-z]+\\)" end t)
	    (ti::replace-match 1 date)))))))))

;;}}}
;;{{{ buffers: gzip

;;; ......................................................... &buffers ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-gzip-buffer-file ()
  "Compress or uncompress current file buffer with gzip."
  (interactive)
  (save-buffer)

  (let* ((gzip "gzip"))

    (cond
     ((or (not (stringp buffer-file-name))
	  (null (file-modes buffer-file-name))) ;Ange ftp
      (message "timy. Can't gzip this buffer."))

     ((or (ti::vc-rcs-file-exists-p buffer-file-name)
	  (and (fboundp 'vc-registered)
	       (ti::funcall 'vc-registered buffer-file-name)))
      (message "TinyMy: This file is VC controlled. No gzip allowed."))

     ((string-match "\\.gz$" buffer-file-name)
      (call-process gzip nil nil nil "-d" buffer-file-name)
      (setq buffer-file-name (replace-regexps-in-string
			      "\\.gz$" "" buffer-file-name))
      (rename-buffer (file-name-nondirectory buffer-file-name))
      (set-visited-file-modtime))

     (t
      (call-process gzip nil nil nil "-9" buffer-file-name)
      (unless (string-match "\\.gz$" buffer-file-name)
	(setq buffer-file-name (concat buffer-file-name ".gz")))
      (rename-buffer (file-name-nondirectory buffer-file-name))
      (set-visited-file-modtime)))))


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-read-only ()
  "Puts buffer in `view-mode' if read-only is turned on.

Important, If file is vc controlled:

    This function is ment for changing the
    buffer characteristics without changing the RCS state

    Normally \\[toggle-read-only] would do CheckOut if the file was
    read-only, but sometimes we just want to put buffer to read-only
    state to prevent changing anything in there for a while."
  (interactive)
  (let* ((fid      "tinymy-read-only")
	 (key-func (if (or (featurep 'vc)
			   (featurep 'vc-hooks))
		       'vc-toggle-read-only
		     'toggle-read-only))
	 state
	 call
	 turn-mode)

    (tinymy-debug fid "VC"    (featurep 'vc)
		"mode"	    major-mode
		key-func
		"FILE"	    buffer-file-name)

    (ti::save-line-column-macro nil nil
      (cond
       ((memq major-mode '(dired-mode)) ;plain C-x C-q for these modes...
	(toggle-read-only))
       (t
	(cond
	 ((and (eq key-func 'vc-toggle-read-only)
	       buffer-file-name		;maybe *temp* buffer ?
	       (vc-name buffer-file-name)) ;is file registered ?

	  (if (y-or-n-p "Call vc? ")
	      (call-interactively 'vc-toggle-read-only)
	    (toggle-read-only)))
	 (t
	  (call-interactively key-func)))

	(tinymy-debug fid "STATE after" buffer-read-only)
	(setq state buffer-read-only)	;what happened ?

	(setq turn-mode			;can't use nil, because it toggles
	      (if  state 1 0))


	(setq call (tinymy-view turn-mode))
	(tinymy-debug fid "Turn-mode" turn-mode call)

	(if (and (eq 'mview-mode call)
		 (eq state nil))

	    ;;  It's too smart.. the vc-toggle-read-only already turned on
	    ;;  the read-only and now mview turned itself off,
	    ;;  while retaining
	    ;;  the *original* read-only status ... force it back to basics

	    (setq buffer-read-only nil)))))))



;;; ----------------------------------------------------------------------
;;;
(defun tinymy-view (&optional arg)
  "Pick the right view mode using mode ARG. Let it be a minor one if possible.
This is Emacs version independent function.

Note:

    Some minor modes occupy view key, e.g. expand.el Those interfering modes
    are also turned off.

Return;

  SYMBOL     which mode turned on."
  (interactive "P")
  (let* (ret)

    ;;  In Emacs 19.30+ the std view.el is minor mode, but not in lower ones..

    (ti::save-line-column-macro nil nil
      (cond

       ;; ................................................. minor-mode ...

       ((assq 'view-mode minor-mode-alist);; is this minor mode?
	(view-mode arg)			;use the new minor mode
	(when (and tinymy-:package-exist-mview ; turn this off.
		   (featurep 'mview)
		   (symbol-value 'mview-mode))
	  (require 'mview)		;No autoloads allowed
	  (ti::funcall 'mview-mode 0))
	(setq ret 'view-mode-minor))

       ;; ...................................................... mview ...

       (tinymy-:package-exist-mview	;get this from OHIO mview.el
	(require 'mview)
	(ti::funcall 'mview-mode arg)
	(when view-mode			;make sure this is off
	  (setq view-mode 0))

	(when (eq major-mode 'view-mode)
	  (view-exit))

	(setq ret 'mview-mode))

       ;; ....................................................... else ...

       (t				;no other choice, use the major-mode
	(if (and (eq major-mode 'view-mode)
		 (fboundp 'view-exit))
	    (view-exit)
	  (view-buffer (current-buffer)))
	(setq ret 'view-mode-major)))

      (if (and (featurep 'expand)	;Turn this mode off, occupies SPC key
	       (boundp 'expand-mode)
	       (symbol-value 'expand-mode))
	  (ti::funcall 'expand-mode 0))

      ret)))


;;; ----------------------------------------------------------------------
;;;  07 Jun 1996, Bart Robinson <lomew@cs.utah.edu>, gnu.emacs.sources
;;;  13 Jun 1996, Slightly modified by [jari]
;;;  27 Jan 1997  duthen@cegelec-red.fr (Jacques Prestataire),
;;;               wrote the current 3 buffer revert functions.
;;;
(defvar tinymy-:force-revert       nil
  "No docs.")

(defvar tinymy-:revert-in-progress nil
 "Set to t when `tinymy-revert-some-buffers' is called.
This variable prevent's recursive revert calls if there us already
revert in progress.")

(defvar tinymy-:revert-buffer-info-list nil
  "Some buffers need special attention to decide if revert is needed.
This variable holds buffers extra info. '((dired-buffer md5-INFO) ..).")


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-dired-md5 (dir)
  "Calculate md5 hash for Dired's files.
The files have changed underneath if this value is different than
previous one."
  (when (and (or (featurep 'md5)
		 (locate-library "md5"))
	     (executable-find "md5sum"))
    (autoload 'md5-encode "md5")
    (ti::funcall 'md5-encode (ti::list-to-string (directory-files dir)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-dired-p (dir)
  (let* ((md5-prev (cdr-safe (assoc dir tinymy-:revert-buffer-info-list)))
	 (md5-now  (tinymy-revert-dired-md5 dir)))
    (unless (equal md5-prev md5-now)
      (aput 'tinymy-:revert-buffer-info-list  dir md5-now)
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-dired-buffer ()
  "Revert current dired buffer if content underneath has changed."
  ;;  Do not revert remote file buffer. #todo: should we?
  (let ((dir (and (boundp 'dired-directory)
		  (symbol-value  'dired-directory))))
    (when (and (stringp dir)
	       (not (ti::file-name-remote-p dir))
	       (tinymy-revert-dired-p dir))
      (message "TinyMy: Reverted dired %s" dir)
      (revert-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-buffer-prompter (buffer)
  "Called from `tinymy-revert-some-buffers' with BUFFER. Confirm revert."
  (save-window-excursion
    (when (buffer-live-p (get-buffer buffer))
      (with-current-buffer buffer
	(cond

	 ((memq major-mode '(dired-mode))
	  (tinymy-revert-dired-buffer))

	 ((and (buffer-file-name)
	       (file-exists-p (buffer-file-name))
	       (not (verify-visited-file-modtime buffer))
	       (tinymy-revert-allowed-p)

	       ;; Skip this time if user has prompt in minibuffer
	       ;; 1997-03-02 this is not a good idea, user uses too much
	       ;; minibuffer...

;;;	     (ti::no-action-in-progress-p)


	       ;;  Make sure buffer in Emacs is not modified, We don't
	       ;;  want to wipe out changes user did

	       (not (buffer-modified-p)))
	  (if tinymy-:force-revert
	      ;;  dynamic var bound in `tinymy-revert-some-buffers'
	      ;;  non-nil means act on that object without asking the user
	      'tinymy-force-revert
	    ;; a string means use it as the prompt for that object
	    (format "Revert buffer %s " (buffer-name buffer))))
	 (t
	  ;;  nil means ignore that object
	  nil))))))
;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-buffer-actor (buffer)
  "Called from `tinymy-revert-some-buffers' with BUFFER."
  (with-current-buffer buffer
    (revert-buffer nil 'no-confirm)
    (set-visited-file-modtime)))

;;; ----------------------------------------------------------------------
;;;
(defun timy-revert-plugged-p (file)
  "Check win32 plugged status for remote FILE."
  (when (stringp file)
    (if (not (win32-p))
	t
      ;;  In Win32; we're not "plugged" to net all the time,
      ;;  So do not request ange-ftp buffers fore revert necessarily
      (or (not (ti::file-name-remote-p file))
	  (ti::mail-plugged-p)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-allowed-p ()
  "Check is reverting is allowed for current buffer."
  (and (not (featurep 'autorevert))

       ;;  live-find-file.el from OHIO LCD
       (not (eq major-mode 'Live-Find-File))

       (if (not (boundp 'live-mode))
	   t
	 ;;  - If we got this far, the live-mode.el pacakge
	 ;;  - has been loaded.
	 ;;  - Check that mode is not active in this buffer
	 (null (symbol-value 'live-mode)))

       (if (not (win32-p))
	   t
	 ;;  In Win32; we're not "plugged" to net all the time,
	 ;;  So do not request ange-ftp buffers fore revert necessarily
	 (timy-revert-plugged-p (buffer-file-name)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-revert-some-buffers (&optional force-revert)
  "Revert some buffers whose underlying file has changed underneath them.
Asks user about each one.  Optional arg FORCE-REVERT (interactive prefix
arg) means to suppress any questions.

Does not touch changed buffers."
  (interactive "P")

  ;;  Emacs 20.3 has autorevert.el package, do not
  ;;  run this code if that package is loaded. Interactive call is fine.
  ;;
  ;;  ** you see, if you loaded this package and thre was no 'autorevert,
  ;;     then the timer process was created to call us
  ;;  ** If you later loaded autorevert, the timer was running there still
  ;;     and calling us, but now the code below doesn't activate any more.

  (when (or (interactive-p)
	    (and (null tinymy-:revert-in-progress)
		 (tinymy-revert-allowed-p)))
    (unwind-protect
	(progn
	  (setq tinymy-:revert-in-progress t)
	  (setq tinymy-:force-revert force-revert)
	  (map-y-or-n-p
	   'tinymy-revert-buffer-prompter
	   'tinymy-revert-buffer-actor
	   (buffer-list)
	   '("buffer" "buffers" "revert")))
      (setq tinymy-:revert-in-progress nil))))

;;}}}
;;{{{ buffers: revert buffers

;;; ----------------------------------------------------------------------
;;; 2000-05-29 gnu.emacs.help Colin Walters <levanti@verbum.org>
;;;
(defun tinymy-flip-windows ()
  "Switch window order. There must be only 2 windows."
  (interactive)
  (when (> (count-windows) 1)
    (let ((first-buffer (window-buffer (selected-window)))
	  (second-buffer (window-buffer (next-window (selected-window)))))
      (set-window-buffer (selected-window) second-buffer)
      (set-window-buffer (next-window (selected-window)) first-buffer))))

;;; ----------------------------------------------------------------------
;;; 25 Mar 1995, Michael Cook <mcook@cognex.com>
;;; - This is very handy function, consider the following:
;;;    M-x shell     --> *shell*
;;;    M-x shell     --> doesn't give you new shell, but puts you in *shell*
;;;
;;;    Instead you should have done this:
;;;    M-x shell                        --> *shell*
;;;    M-x my-toggle-buffer-name        --> *shell*<2>
;;;    M-x shell                        --> *shell*
;;;
;;;    Now you have 2 shell buffers and you can very easily get more.
;;;    The same is true for mail sending, just rename the buffer with
;;;    this command if you decide you need to send another
;;;    mail message while you're in *mail*
;;;
(defun tinymy-toggle-buffer-name ()
  "If the current buffer is named like `foo<2>', rename it to `foo'.
If there is already a buffer `foo', first rename it like `foo<3>'."
  (interactive)
  (let ((i (string-match ".<[1-9][0-9]*>$" (buffer-name))))
    (if (null i)
        (rename-uniquely)
      (let* ((name (substring (buffer-name) 0 (1+ i)))
             (buf (get-buffer name)))
        (if buf
            (save-excursion
              (set-buffer buf)
              (rename-uniquely)))
        (rename-buffer name)))))

;;}}}
;;{{{ Line: formatting

;;; ............................................................ &line ...

;;; ----------------------------------------------------------------------
;;; todo: there seems to be c-backslash-region
;;;
(defun tinymy-backslash-fix-paragraph
    (&optional target-column stop-func verb)
  "Fix \\ lines in current paragraph.
The first \\ Tells what the target column is.  If there is no \\ anywhere
in the paragraph, this function does nothing.

Input:

  TARGET-COLUMN		position backslashes to this column, if possible.
			if \\[universal-argument] or negative number,
			remove backslashes.
  STOP-FUNC		If this function returns non-nil, then stop adding
			backslashes. It is called prior the line is handled.
  VERB			Verbose messages.

Example 1:

  This is \\
  Here is another       ;; Note missing \\
  and \\		;; Note, extra \\, should not be there

  Will be formatted as:

  This is \\		;; Target column, when TARGET-COLUMN is nil
  Here is another \\	;; Because the target-cool couldn't be set.
  end

Example 2:

  This is 		;; Ignored, no \\
  Here is another \\	;; Target starts here
  And still..
  end

  Will be formatted as:

  This is
  Here is another \\
  And still..	  \\	;; Added
  end

Example 3:

All the lines in this procmail example are together, but it would be wrong
to add final \\ to the end of ')'. The STOP-FUNC can make sure about that.

{
   :0 h # this is procmail code
   * condition
   | ( formail -rt    \\
       MORE-OPTIONS | \\
       $SENDMAIL -t)
}"
  (interactive "*P")
  (let* ((point   (point))
	 (cs      (or comment-start "[ \t]*"))
	 (stop-re (format "^\\(%s\\)?[ \t]*$" cs)) ;Paragraph end
	 (kill-it (or (ti::listp target-column)
		      (and (integerp target-column)
			   (< target-column 0))))

	 indent-tabs-mode		;No tabs allowed
	 beg
	 col-target
	 col-now
	 col-word
	 ad-it)
    (ti::verb)

    ;; ............................................... paragraph start ...

    (beginning-of-line)
    (while (and (not (eobp))
		(not (looking-at stop-re)))
      (forward-line -1))

    ;;    (ti::d! "PARA" stop-re  (ti::read-current-line))

    ;; .................................... forward to first backslash ...
    ;;	Skip comment lines and emtuy line forward.

    (while (and (not (eobp))
		(looking-at stop-re))
      (forward-line 1))
;;;    (ti::d! (ti::read-current-line))

    (when (eq major-mode 'makefile-mode)
      (if (looking-at ".*:")        ;;  Go path the TARGET: RULE
	  (forward-line 1)))

    ;; ... ... ... ... ... ... ... ... ... ... .. &starting target-col ...

    (save-excursion			;Find the starting \\
      (beginning-of-line)
      (while (and (not (eobp))
		  (not (looking-at ".*[\\][ \t]*$"))
		  (not (looking-at stop-re)))
	(forward-line 1))
      (setq beg (point)))

    (goto-char beg)			;We landed here

;;;    (ti::d! "landed" stop-re (ti::read-current-line))


    (cond
     ((not (looking-at ".*[\\]"))
      (message "\
TinyMy: mak, Nothing to do; no \ mark at the paragraph beginning."))
     (t
      (goto-char (match-end 0))
      (backward-char 1)
      (setq col-target (or (and
			    ;; User gave this value
			    (integerp target-column)
			    (>= target-column 0)
			    target-column)
			   (current-column)));; use column from code them

      (when kill-it
	(delete-char 1)
	(delete-horizontal-space))


;;;      (ti::d! (point) (current-column) col-target (ti::read-current-line))
      ;;  there was old starting \\, but not in the right column. Fix it,
      ;;  but only if it was far left.
      ;;
      ;;    txt txt \     ;; this line is too far right
      ;;      T \         ;; The target column user wanted was T

      (when (and (null kill-it)
		 (not  (eq (current-column) col-target)))
	(delete-region (point) (line-end-position))
	(move-to-column col-target)
	(when (or (null stop-func)
		  (and stop-func
		       (null (funcall stop-func))))
	  (insert "\\")))


      (unless (looking-at "$")		;Remove garbage
	(delete-region (point) (line-end-position)))

      (beginning-of-line)

      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. loop-lines . .
      ;;  Empty line terminates

      (while (and
	      (not (eobp))
	      (not (looking-at stop-re))
	      (or (null stop-func)
		  (and stop-func
		       (null (funcall stop-func)))))
	(save-excursion			;Peek next line
	  (forward-line 1)
	  (setq ad-it (not (looking-at stop-re))))


;;;	(ti::d! "loop" ad-it (ti::read-current-line) (looking-at ".*[\\]"))

        ;; ... ... ... ... ... ... ... ... ... ... ... fix backslashes ...

	(cond
	 (kill-it
	  (when (looking-at ".*[\\]")
	    (goto-char (match-end 0)) (backward-char 1)
	    (delete-char 1)
	    (delete-horizontal-space)))

	 ((looking-at ".*[\\]")
	  (goto-char (match-end 0)) (backward-char 1)
	  (setq col-now (current-column))

	  ;;  Where is the word start?

	  (skip-chars-backward " \t\\")
	  (untabify (point) (line-end-position))
	  (setq col-word (current-column))

;;;	  (ti::d!  "fix" col-now col-word (ti::read-current-line))

	  (cond
	   ((and (eq col-now col-target)
		 (null ad-it))
	    ;;  remove Extra  \\
	    (move-to-column col-now)
	    (delete-char 1)
	    (delete-horizontal-space))

	   ((not (eq col-now col-target))

	    ;;    This \
	    ;;    GFile.here \   < This is further right

;;;	    (ti::d! ">>"  col-word col-now col-target (ti::read-current-line))
	    (cond
	     ((> col-word col-target))	;Do nothing, can't "line up"
	     (t
	      (move-to-column (min col-target col-now))
	      (delete-region (point) (line-end-position))
	      (when ad-it
		(ti::buffer-move-to-col col-target)
		(insert "\\")))))))

	 ;; ... ... ... ... ... ... ... ... ... ... .. no-continuation  ..

	 (ad-it				;No previous "\" and next line exist
	  (end-of-line)
	  (delete-horizontal-space)	;Clear the EOL

	  ;;  Only if there is no text, T is target, but next line has
	  ;;  longer line.
	  ;;
	  ;;       T
	  ;;  This rule here \

	  (if (<= (current-column) col-target)
	      (ti::buffer-move-to-col col-target))
	  (insert "\\")))
	(forward-line 1))))

    (goto-char point)			;Restore user position
    (when verb
      (cond
       (kill-it
	(message "TinyMy: Backslashes killed."))
       (col-target
	(message "TinyMy: Backslashes fixed to column %d" col-target))))))

;;}}}

;;{{{ Mouse, cursors

;;; ----------------------------------------------------------------------
;;; Steve Kemp <skx@tardis.ed.ac.uk>
;;;
(defun tinymy-cursor-set-type (cursor &optional frame)
  "Set the CURSOR type for the named FRAME."
  (if (not frame)
      (setq frame (selected-frame)))
  ;; Do the modification.
  (modify-frame-parameters
   frame
   (list (cons 'cursor-type cursor))))


;;; ----------------------------------------------------------------------
;;; Steve Kemp <skx@tardis.ed.ac.uk>
;;;
(defun tinymy-cursor-overwrite-mode ()
  "Set the cursor-type according to the insertion mode"
  (cond
   (overwrite-mode
    (let ((cursor (or (frame-parameter (selected-frame) 'cursor-type)
		      'block)))
      (put 'tinymy-cursor-overwrite-mode 'saved-cursor-type cursor)
      ;;  The type is going to change to 'bar, but if user has it
      ;;  on by default, pick the opposite.
      (tinymy-cursor-set-type (if (equal cursor 'bar)
				  'block
				'bar))))
   (t
    (tinymy-cursor-set-type
     (get 'tinymy-cursor-overwrite-mode 'saved-cursor-type)))))

;;; ----------------------------------------------------------------------
;;;
(if (fboundp 'overwrite-mode-hook)
    (add-hook 'overwrite-mode-hook 'tinymy-cursor-overwrite-mode-hook)
  (defadvice overwrite-mode (around tinymy act)
    "Change cursor to 'block or 'bar according to `overwrite-mode'."
    ad-do-it
    (tinymy-cursor-overwrite-mode)))

;;; ........................................................... &mouse ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-buffer-info ()
  "Display buffer information:
If buffer is associated to file:  -rwx-rw-r-- 20k /absolute/path/file.txt
If no file: SIZEk SIZE-IN-BYTES"
  (interactive)
  (let* ((file  buffer-file-name)
	 (ssize (buffer-size))
	 (size  (/ ssize 1000))   ;; well, it's 1024 to exact but this suffices
	 (modes "")
	 lines)

    ;;  Eg. Gnus defines buffer-file-name for Draft messages,
    ;;  but the file is not actually written, so we test for existense
    ;;  to prevent suprises from happening.

    (cond
     ((and (memq major-mode '(dired-mode vc-dired-mode))
	   (boundp 'dired-directory))
      (setq lines (- (count-lines (point-min) (point-max)) 2))
      (message "Tinymy: count %d %s"
	       lines
	       (symbol-value 'dired-directory)))
     (file
      (or (string-match "@" file)  ;; Ange-ftp file is ok.
	  (and (file-exists-p file)
	       (setq modes
		     (ti::file-access-mode-to-string (file-modes file)))))
      (message "TinyMy: %s %dk %s"  (or modes "") size file))
     (t
      (message "TinyMy: buffer size %dk (%d bytes)"  size ssize)))))

;;; ----------------------------------------------------------------------
;;; Terry E Eminhizer (teminhi@hubcap.clemson.edu) wrote:
;;; >How can I get the selected window to change as I move the mouse cursor
;;; >into that window?  In other words, I don't want to have to click the
;;; >mouse in the new window every time I move between windows (windows, not
;;; >frames, this is not a click-to-focus window manager question).
;;;
;;; 1996-03-04  <kevinr@ihs.com> (Kevin Rodgers), gnu.emacs.help
;;;		expressed idea and presented some code for automatic
;;;		window changing.
;;;


;;; [jari] I elp'ed this function to test how heavy it is as mouse-handler.
;;; In byte compiled format the results in HP 10.20/9000/715
;;;
;;; Function Name                Call Count  Elapsed Time  Average Time
;;; ===========================  ==========  ============  ============
;;; tinymy-mouse-movement-handler  29          0.0571780000  0.0019716551

(defvar tinymy-:window-previous nil
  "Used in `tinymy-mouse-movement-handler'.")

(defun tinymy-mouse-movement-handler (event)
  "Nice mouse movement EVENT handler.

Change window automatically:

    If you point a nother window where cursor was, the new window is
    automatically made active.

Show information on echo-area:

    If you point mouse near the end of botton line (right hand corner),
    a brief file information is shown in echo area. If window is bigger
    than the text that is at the beginning of it, pointing to the end
    of text is sufficient. Example output:

    TinyMy: -rw-r--r-- 108k /users/jaalto/elisp/tinymy.el"

  (interactive "e")
  (let* ((case-fold-search  t)
	 frame
	 win
	 mini
	 bottom point
	 p)

    (cond
     ((and (fboundp 'event-window)
	   (eventp  event))
      ;;  XEmacs calls us from motion hook
      ;;  #<motion-event 644, 221>
      (setq win (ti::funcall 'event-window event)))
     ((and (fboundp 'posn-window)
	   (fboundp 'event-start)
	   (eventp  event))
      (setq win (posn-window (event-start event))))
     (t
      ;;  Unknown Emacs or interface chnaged radically
      (message "Tinymy: tinymy-mouse-movement-handler error.\
Contact maintaner with M-x tinymy-submit-bug-report.")))




    (setq bottom    (and win (window-end))
	  point     (posn-point
		     (if (emacs-p)
			 (event-start event)
		       event)))

    ;; ............................................ auto window select ...
    ;;  The WIN could be frame pointer too, that's why we check it.

    (cond
     ((null win))  ;; WE HAVE NO WINDOW INFORMATION, stop.

     ;; ............................................. different window ...

     ((and (windowp win)
           (window-live-p win)
           ;;  Motion in same window as prereviously?
           (not (eq tinymy-:window-previous win)))
      (setq tinymy-:window-previous win
	    mini                  (window-minibuffer-p win))

      ;;    1. We select window if it's not minibuffer
      ;;    2. if it's minibuffer, select it _only_ if it's active
      ;;       otherwise Emacs cries.

      (when (or (not mini)
		(minibuffer-window-active-p win))
	(setq frame (window-frame (select-window win)))
	(raise-frame frame)
	;; Is this Needed?
	(select-frame frame)))

     ;; ....................................... Special 'info' handler ...

     ((and (not (window-minibuffer-p (selected-window)))
	   (not (eq (point-min) (point-max)))) ;Not empty buffer?

      ;; ........................................... pointing with mouse ...

      (when (integerp point)		;point could be 'mode-line
	(setq p point)			;When you cross window border

;;	(message "%d %d %d "  p bottom (- bottom p) )

	;;  Threshold of NN characters, near the right hand lower corner.
	;;  Make the call `inline' because `tinymy-mouse-movement-handler'
	;;  is called very often

	(when (and p (< (- bottom p) 50))
	    (inline (tinymy-buffer-info))

	    ;;   mic paren: If your cursor is at end of defun
	    ;;   parenthesis, and this function is called, the
	    ;;   mic-paren will still display the beginning of function
	    ;;   info. Out info is not show...

	    (defvar mic-paren-backw-overlay nil) ;No-op, ByteComp

	    (if (and (featurep 'mic-paren)
		     ;; This overlay exists if cursor was on paren
		     mic-paren-backw-overlay)
		(sit-for 2))))))))

;;}}}
;;{{{ elisp: package saving from mail, gnus


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-package-save-get-file-name ()
  "See `tinymy-package-save-to-file'. Find out package file name.
Return '(file-name  point)."
  (let* ((fid "tinymy-package-save-get-file-name:")
         ;;  - the file start and it's name
         ;;  - The regexp will jump until there is a-zA-Z0-9

	 (com "^\\(#\\|;;+\\)")

         (re1 (concat com "[ \t]+\\([^ \t]+\\.el\\)[ \t]+[-][-]+"))
         (re2 (concat com "[ \t]+\\([^ \t]+\\)[ \t]+[-][-]+[ \t]"))
         (re3 (concat com "[ \t]+\\(.*\\)[ \t]+[-][-]+"))
         (re4 (concat com "[ \t]+\\(.*\\)[ \t]+[-]+"))
         (re5 "^\\(;;;*\\)[ \t]+\\([^ \t\n]+\\.el\\)[ \t]+")

	 file
	 point)
    (save-excursion
      (ti::pmin)

      ;;  See if we can detect the package name in this buffer

      (when (dolist (re (list re5 re1 re2 re3 re4))
	      (when (re-search-forward re nil t)
		(tinymy-debug fid 'MATCH re 'LINE (ti::read-current-line) "\n")
		(return t)))

        (setq file  (match-string 2)
	      com   (match-string 1)
	      point (line-beginning-position))

	;;  Suppose this is a lisp file, because comment mark is colon(;)
	;;  make sure the filename has .el at the end

	(tinymy-debug fid 'BUFFER (buffer-name) 'FILE file "\n")

	(beginning-of-line)

	(when (looking-at "^[ \t]*;")
	  (setq file (ti::string-verify-ends file "\\.el" ".el")
		file (or (locate-library file) file)))))
    (tinymy-debug fid 'BUFFER (current-buffer) 'RET file 'POINT point)
    (cond
     (file
      (list file point))
     (t
      (message "TinyMy: (package save) No proper File header found.")
      nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-package-save-to-file-buffer-beginning (file)
  "Find proper file beginning point.

Return:

  point or nil."
  (let* ((fid   "tinymy-package-save-to-file-buffer-beginning:")
	 (fname (file-name-sans-extension (file-name-nondirectory file)))
	 (ext   (file-name-extension file))
	 (regexp
	  ;;  file\\(.ext\\)? -- description
	  ;;  ;;; @(#) file.ext --- description
	  ;;      |
	  ;;      see unix SunOS what(1) command
	  (format "^\\([^ \t\n:,.-]+\\) +\\(%s[ \t]*\\)?%s[ \t]+-+[ \t]+"
		  (regexp-quote "@(#)")
		  (concat
		   (regexp-quote fname)
		   "\\(\\." (regexp-quote ext) "\\)?")))
	 point)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward regexp nil t)
	(setq point   (line-beginning-position))))

    (tinymy-debug fid
		  'FILE file
		  'BUFFER (current-buffer)
		  'REGEXP regexp
		  'POINT  point)
    point))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-package-save-to-file-buffer-ending ()
  "Find proper file ending.
Return point or nil."
  (let* ((fid "tinymy-package-save-to-file-buffer-ending:")

         ;;  - the file start and it's name
         ;;  - The regexp will jump until there is a-zA-Z0-9

	 (com    "^\\(#\\|;;+\\)")

         (regexp (concat
		  com
		  "[ \t]+\\(end[ \t]+of[ \t]\\(file\\)?\\|^;.*&eof\\)"
		  "\\|^;;+[ \t]+.*ends here"))

	 ;; Yes, it really does have trailing space
	 ;; "- -- \n" is for PGP signed message which breaks the
	 ;; dashes.

	 (signature-end "^\\(- \\)?-- \n")

	 end-point)
    (save-excursion
      (ti::pmin)
      (cond
       ((re-search-forward regexp nil t)
	(beginning-of-line)
	(tinymy-debug fid 'REGEXP regexp (point) (ti::read-current-line))
	(setq end-point (line-beginning-position)))
       ((progn
	  (ti::pmax)
	  (re-search-backward signature-end nil t))
	(tinymy-debug fid 'SIGNATURE (point))
	(setq end-point (line-beginning-position)))))
    end-point))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-package-save-to-file-buffer ()
  "Return correct code buffer, usually `current-buffer'.
For Gnus this is `gnus-original-article-buffer'."
  (cond
   ((and (featurep 'gnus)
	 (or (string= (buffer-name)
		      gnus-article-buffer)
	     (and (equal (current-buffer) gnus-summary-buffer)
		  (not (string-match
			"Dead "
			(buffer-name gnus-summary-buffer))))))
    (if gnus-original-article-buffer
	;;  For Gnus, use the unformatted buffer
	gnus-original-article-buffer
      (error
       "TinyMy: Can't find code buffer. Selet Article.")))
   (t
    (current-buffer))))


;;; ----------------------------------------------------------------------
;;; - Imagine that you're reading gnu.emacs.sources and want to get
;;;   that package in the post.
;;; - Or you receive a package in private mail message...
;;; - This does the job of saving that package to file very easily.
;;;
(defun tinymy-package-save-to-file (file &optional save-start)
  "Save FILE in current buffer starting at optional SAVE-START.

The file is supposed to have special heading and when the heading
is found the file ends at `point-max' or when the footer is found
The following are valid heading. See unix what(1) for the second line.

    ;; file.el -- description
    # @(#) file.txt -- description

If function can't find footer

     End of XXX.txt
     End of file XXX.txt
     &eof
     XXX ends here

it'll add one and include everything to the end of buffer,
before writing."
  (interactive
   (with-current-buffer
       (tinymy-package-save-to-file-buffer)
     (multiple-value-bind (file point)
	 (tinymy-package-save-get-file-name)

       (unless file
	 (error
	  "TinyMy: Can't find filename. Use manual saving."))

       (list
	(read-file-name "Save to file: "
			(file-name-directory file)
			nil   ;; users null string
			(not 'must-match)
			(file-name-nondirectory file))
	point))))
  (let* ((fid           "tinymy-package-save-to-file:")
	 (orig-point	(point))
         (code-buffer   (tinymy-package-save-to-file-buffer))

         p1
	 p2
         ans
	 str
	 point)

    ;;  See if we can detect the package name in this buffer

    (when file
      (with-current-buffer code-buffer
	(ti::pmin)

	(setq p1     (or save-start
			 (tinymy-package-save-to-file-buffer-beginning file)
			 (point))
	      p2     (point-max))

        ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ end ^^^

	(setq point (tinymy-package-save-to-file-buffer-ending))

	(cond
	 (point
	  (setq p2 point))
	 (t
	  (setq str "TinyMy: Hm, No proper save ending. Using point-max ")
	  (tinymy-debug fid str)
	  (message str)
	  (sit-for 1)))

	;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ existing file ^^^

	(when (file-exists-p file)
	  (setq ans (read-from-minibuffer "overwrite?: " file))
	  (cond
	   ((string= ans file)
	    (delete-file file))
	   ((ti::nil-p ans)
	    (error "TinyMy: Aborted."))
	   (t
	    (setq file ans))))

	(tinymy-debug fid 'SAVE-FROM code-buffer p1 p2 'TO file)

	(when (or (eq p1 p2)
		  (> p1 p2))
	  (error "TinyMy: ERROR, nothing to save. Please contact maintainer."))

        ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ saving ^^^

	 (with-temp-buffer
	   (insert-buffer-substring code-buffer p1 p2)
	   (ti::pmin)

	   (when (string-match "\\.\\(zip\\|gz\\)$" file)
	     (ti::use-file-compression))

	   (write-file file)		;jka handles compressing
	   (not-modified)
	   (message (concat "TinyMy: Package filed to " file)))
	 ;; Restore point
	 (goto-char orig-point)))))

;;}}}
;;{{{ file

;;; ............................................................ &file ...


;;; ----------------------------------------------------------------------
;;; By Jesper Pedersen <blackie@imada.ou.dk> in private mail
;;;
(defun tinymy-maybe-make-file-executable ()
  "This function make saved #! file executable. Called from hook.
Ignores file whose `file-modes' can't be read, e.g. ange-ftp buffer."
  (let* ((file (buffer-file-name))
	 (mode (and file (file-modes file))))
    (when (and file mode
	       (save-excursion (ti::pmin) (looking-at "\\s-*#!")))
      (unless (eq 64 (logand 64 mode))
	(set-file-modes file (ti::file-mode-make-executable mode))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-trim-blanks ()
  "Delete trailing blanks from all lines; including lines from end of buffer."
  (interactive)
  (save-excursion
    (unless buffer-read-only
      (ti::buffer-trim-blanks (point-min) (point-max))
      ;;  Now delete extra lines from the end of buffer
      (goto-char (point-max))
      (when (not (zerop (skip-chars-backward " \t\n")) )
	(forward-char 1)		;Leave newline
	(unless (eq (point) (point-max))
	  (delete-region (point-max) (point))))))
  (if (interactive-p)
      (message "TinyMy: Blanks trimmed"))
  nil)					;Clean return code


;;; ----------------------------------------------------------------------
;;; - Especially when I'm making diff to the Author I find this
;;;   very useful.
;;;
;;;
(defun tinymy-copy-file (file1 file2 &optional arg)
  "Make copy of current buffer FILE1 to FILE2 (FILE1.orig or FILE1.VER).
Function tries to find possible RCS version.
You usually make backup if you make a change and send diff to author.

If you supply PREFIX ARG, then

  C - u	remove the copy files; namely,     (buffer-file-name).*
  nbr   Copy back: this like doing
        FILE.VER  --> FILE
        FILE.orig --> FILE

        If you had made a safe copy previously, this restores
	the safe copy to original file."
  (interactive
   (let* ((suf   tinymy-:copy-file-suffix)
	  (ver   (or (ti::vc-rcs-buffer-version)

		     ;;  No rcs string found, then try Regular lisp package
		     ;;  syntax.
		     ;;
		     ;;  Version:       2.37

		     (ti::re-search-check
		      "^;+[ \t]+Version:[ \t]*\\([0-9.]+\\)" 1 nil 'read)))

          (file1 (or (buffer-file-name)
		     (error "Buffer does not visit a file.")))
          (ext   (if ver
		     (concat "." ver)
		   suf))
          file2)

     (if current-prefix-arg
	 (list file1 nil current-prefix-arg)
       (setq file2 (read-from-minibuffer "Make copy to: " (concat file1 ext)))
       (list file1 file2))))

  (let* ((re	    (format "^%s\\." (file-name-nondirectory file1)))
	 (file-list (ti::directory-files (file-name-directory file1)
				     re 'abs t)))
    (cond

     ((null arg)
      (cond
       ((or (not (file-exists-p file2))
	    (and (file-exists-p file2)
		 (y-or-n-p (format "%s exists. Remove? " file2))
		 (progn
		   (delete-file file2)
		   t)))
	(ti::file-delete-safe file2)
	(copy-file file1 file2)
	(message "TinyMy: Safe copy done."))
       (t
	(message "TinyMy: Sorry; cannot decide how to do the copying."))))

     ((equal arg '(4))
      (if (null file-list)
	  (message "TinyMy: There are no safe copy files matching %s" re)
	(dolist (file1 file-list)
	  (if (y-or-n-p (format "Delete %s ? " file1))
	      (delete-file file1)))))

     ((integerp arg)
      (cond
       ((null file-list)
	(message "TinyMy: There is no safe copy for %s" file1))

       ((eq 1 (length file-list))
	(when (y-or-n-p
	       (message "TinyMy: Found safe copy %s; copy it over original? "))
	  (delete-file file1)		;copy-file barfs otherwise
	  (copy-file (car file-list) file1)
	  (message "TinyMy: Safe copy restored.")))

       ((> (length file-list) 1)
	(setq file2
	      (completing-read
	       "Don't know which one to use as source, complete: "
	       (ti::list-to-assoc-menu (mapcar 'file-name-nondirectory file-list))
	       nil 'must-match))
	(setq file2 (concat (file-name-directory file1) file2))
	(delete-file file1)
	(copy-file file2 file1)
	(message "TinyMy: Safe copy restored: %s --> %s"
		 (file-name-nondirectory file2 )
		 (file-name-nondirectory file1))))))))

;;}}}

;;{{{ key: % matching


;;; ----------------------------------------------------------------------
;;; idea                by michael@pbinfo.UUCP  or  michael@uni-paderborn.de
;;; Totally rewritten   by [jari]
;;;
;;; All the posts so far in the internet to make the "%" match parens
;;; right in every possible _mode_ failed. That's why I started writing
;;; my own function, which you see here.
;;;
(defun tinymy-vi-type-paren-match (&optional arg)
  "Match engine: find {[( or )]} pairs. ARG is character repeat count.
See also 'tinymy-:vi-type-paren-match-special-list

References:
  `tinymy-:vi-type-paren-match-list'
  `tinymy-:vi-type-paren-match-special-list'"
  (interactive "P")
  (let* ((p	    (point))
	 (ptable    (syntax-table))	;previous, the original

	 (ch	    (following-char))
	 (ch-next   (ti::buffer-read-char nil 1))
	 (ch-prev   (preceding-char))

	 (pairs	    tinymy-:vi-type-paren-match-list)
	 (left	    (car-safe (assoc  (char-to-string ch) pairs)))
	 (right	    (car-safe (rassoc (char-to-string ch) pairs)))

         (m-list    tinymy-:vi-type-paren-match-special-list) ;match list
         (spread-limit   (* 10 60))	;approx 10 lines of code.

	 table
         s-func add-func max-func bigger-func
	 self-insert
         go
         max)

    (catch 'terminate


      ;;  check if the parens are "closed", ie. there is nothing beween them

      (cond
       ((and (char-in-list-case ch m-list) ch-next)
	(setq self-insert
	      (not (string= (char-to-string ch-next) (char-to-string ch)))))
       (right
	;;  If NEXT == RIGHT
	(setq self-insert
	      (string= (char-to-string ch-prev) left)))
       (left
	(setq self-insert
	      (string= (char-to-string ch-next) right))))

      (when (and ch self-insert)
	(self-insert-command (or arg 1))
	(throw 'terminate t))

      ;;  already calculated ? No ?

      (unless (setq table (get 'tinymy-:vi-type-paren-match-list 'syntax-table))
	(setq table (make-syntax-table))

	;;   We want everything to look like word
	(ti::dotimes counter 0 255 (modify-syntax-entry counter "w" table))

	(mapcar
	 (function
	  (lambda (x)
	    (modify-syntax-entry (string-to-char (car x)) "(" table)
	    (modify-syntax-entry (string-to-char (cdr x)) ")" table)))
	 pairs)

	(put 'tinymy-:vi-type-paren-match-list 'syntax-table table))

      ;;  In lisp; only () are matched.

      (when (not (string-match "lisp" (symbol-name major-mode)))
	(set-syntax-table table))

      (unwind-protect
	  (condition-case nil
	      (cond

	       ;; ........................................ handle quotes ...
	       ((char-in-list-case ch m-list)
		(setq ch (regexp-quote (char-to-string ch)))

		(setq s-func   're-search-forward
		      add-func '+
		      max-func 'point-max
		      bigger-func '>)

		(cond
		 ((looking-at (concat ch "[ \t]*$"))

		  ;; Only search backward if no ARG given.
		  ;; if the " char is at the end of line,
		  ;; then it propably is the 'closing' one.

		  (if (null arg)
		      (setq s-func   're-search-backward
			    add-func '-
			    max-func 'point-min
			    bigger-func '<))))

		;; Do not go too far away....

		(setq go (funcall add-func p  spread-limit))
		(setq max (funcall max-func))
		(setq max
		      (if (funcall bigger-func go max)
			  max go))

		;; ... ... ... ... ... ... ... ... ... ... ... do search . .

		(if (eq s-func 're-search-forward)
		    (forward-char 1))	;move out of way

		(funcall s-func ch go t)

		;; This is funny, it both a) restores the position
		;; if search failed, b) adjusts the "after" search
		;; point back to char.

		(if (eq s-func 're-search-forward)
		    (forward-char -1)))

	       ;; ..................................... handle BEG pairs ...

	       ((looking-at "[[({<]")
		(forward-sexp 1)
		(backward-char)

		(cond ((not (looking-at "[])}>]"))
		       (error "..booomerang"))))

	       ;; ..................................... handle END pairs ...

	       ((looking-at "[])}>]")
		(forward-char 1)
		(forward-sexp -1)

		(when (not (eq p (point))) ;moved ?

		  ;;  In lisp, jumping from closing ) to starting
		  ;;  "'(lambda" puts cursor at "'"?? Correct it.

		  (if (and (not (eq (following-char) ?\( ))
			   (looking-at ".[]({<]"))
		      (forward-char 1))))

	       ;; ...................................... no special char ...
	       (t
		(self-insert-command (or arg 1))))
	    (error
	     (goto-char p)		;restore position
	     (message "TinyMy: No match.")))
        ;; make sure we restore this
        (set-syntax-table ptable)))))

;;}}}
;;{{{ mail

;;; ............................................................ &mail ...


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-mail-subject-get ()
  "Look buffer content and return subject for mail message.

diff buffer:

  'context diff file.txt'

rcsdiff buffer:

  'diff 1.23 --> 1.25 file.txt'

Regular buffer:

   '1.25 file.txt'       ;; possibly without version information

buffer with no filename:

   nil"
  (let* ((rcs-re    "retrieving revision +\\(.*\\)")
	 (v1	    "")
	 (v2	    "")
	 type
	 ver
	 file
	 msg)
    (save-excursion
      (ti::pmin)
      (cond

       ;;  See if this is rcsdiff
       ;;
       ;;  RCS file: RCS/tinylib.el,v
       ;;  retrieving revision 1.95
       ;;  retrieving revision 1.97
       ;;  diff -c -r1.95 -r1.97
       ;;  *** 1.95	1997/03/22 12:26:59
       ;;  --- 1.97	1997/03/22 15:17:22

       ((re-search-forward "^RCS file:[^/]*/?\\(.*\\),v" nil t)
	(setq file (match-string 1))
	(and (re-search-forward rcs-re nil t)
	     (setq v1 (match-string 1))
	     (re-search-forward rcs-re nil t)
	     (setq v2 (match-string 1)))
	(setq msg (format "patch: %s --> %s %s" v1 v2 file)))
       (buffer-file-name

	;;  Regular file, see if this one has RCS version information

	(if (setq ver (ti::vc-rcs-buffer-version))
	    (setq ver (concat " " ver " ")))

	(setq msg (concat
		   (or ver "")
		   (file-name-nondirectory buffer-file-name))))

       ((setq type (ti::buffer-diff-type-p))

	;; *** /users/jaalto/T.orig  Sun Mar 23 16:37:43 1997
	;; --- /users/jaalto/T	     Sat Mar 22 14:44:34 1997

	(save-excursion
	  (ti::pmin)
	  (if (or (re-search-forward "^--- \\([^ \t\n]+\\)" nil t)
		  (re-search-forward "^\\*\\*\\* \\([^ \t\n]+\\)" nil t)
		  (re-search-forward "^\\+\\+\\+ \\([^ \t\n]+\\)" nil t))
	      (setq file (match-string 1))))
	(setq msg (format "%s diff %s"
			  (prin1-to-string (car type))
			  (if file
			      (file-name-nondirectory file )
			    ""))))))
    msg))


;;; ----------------------------------------------------------------------
;;; Sep 9 1995 <jari.aalto@poboxes.com> (Finland) Public domain.
;;;
(defun tinymy-mail-buffer ()
  "Mail current buffer.
The subject line is constructed by looking at the buffer content:
eg if buffer contains rcsdiff of diff,
The subject line will tell the versions."
  (interactive)
  (let* ((data-buffer   (current-buffer))
	 subj)
    (setq subj (tinymy-mail-subject-get))

    (compose-mail)

    ;;  This package gives nice alias expansion
    (ti::package-require-mail-abbrevs)

    (ti::mail-text-start 'move)

    (insert "\n\n\n\n")
    (save-excursion (insert-buffer data-buffer))

    ;; Make sure the outlline/folding is opened first

    (ti::buffer-xtra-open-outline)

    (if subj
	(ti::mail-kill-field "Subject:" subj))

    (ti::pmin)
    (end-of-line)			;"TO:" field
    (run-hooks 'tinymy-:mail-buffer-hook)))


;;}}}

;;{{{ Programming: function bounds, debug


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-function-bounds (&optional forward)
  "Find function area. Return (beg . end).
The search is first done backward, unless FORWARD is given,
to find function beginning.

Notes:
 All function start lines must be left flushed, ie. no empty spaces before
 function name declaration. Functions must start/end with left flushed
 \"{\" and \"}\".

Supported modes:
  C/C++
  perl
  awk
  lisp"
  (let* ((mode                (or (ti::id-info) (symbol-name major-mode)))
         (max-lines           1500)        ;rows, function cannot be bigger
         (skip-lines          1300)         ;maximum skip lines backward
         (start               (point))
         beg end
         range point
         fwd-flag)

    (cond
     ((string-match "lisp" mode)
      ;;  Only lisp has decent find functions
      (save-excursion
        (if forward
            (ignore-errors
             (end-of-defun)
             (forward-line 2)
             (setq fwd-flag t)))
        (ignore-errors
         (beginning-of-defun)
         (setq beg (point)))
        (if (or (and beg (null forward))
                (and beg forward fwd-flag))
            (ignore-errors
             (end-of-defun)
             (setq end (point))))))

     ((string-match "perl\\|awk" mode)
      (save-excursion
        (cond
         (forward
          (if (re-search-forward "^sub\\|^function" nil t)
              (setq fwd-flag t))))
        (cond
         ((and (or (null forward)
                   (and forward fwd-flag))
               (re-search-backward "^sub\\|^function" nil t))
          (beginning-of-line)
          (setq beg (point))
          (if (re-search-forward "^}" nil t)
              (setq end (point)))))))

     ((string-match "code-c\\|c-\\|cc-\\|c[+]" mode)

      ;;  The opening block says where is function start, this is only
      ;;  possible for NEW styled programming, not K&R styled 'hanging'
      ;;
      ;;  C++: int funtion() {             ;; nope, too diffucult to detect.
      ;;  perl sub funtion   {             ;; allowed

      (save-excursion

        (cond
         (forward
          (if (re-search-forward "^{" nil t)
              (setq fwd-flag t))))

        (cond
         ((and (or (null forward)
                   (and forward fwd-flag))
               (re-search-backward "^{" nil t)
               (re-search-backward "(" nil t)) ;find parameter list beginning

          ;;  There is a problem in writing the C++ funcs:
          ;;
          ;;  // Comment
          ;;  /* Comment
          ;;  */
          ;;  int
          ;;  functionName
          ;;  ( parameters
          ;;

          (beginning-of-line)
          (setq point (point))

          ;; We just search line by line backward until no comment,
          ;; or empty line

          (while (not (looking-at " *//+\\| *[*]+/\\| *[*]+ \\|^[ \t]*$"))
            (forward-line -1))

          (if (not (eq point (point)))  ;if the while loop moved.
              (forward-line 1))         ;go to func beginning.

          (setq beg (point))

          (if (re-search-forward "^}" nil t)      ;;  This is easy.
              (setq end (point))))))))

;;;    (ti::d! (count-lines beg start))

    (if (and beg end
             ;; must not be too far away from current point
             (< (count-lines beg start) skip-lines))
        (setq range (count-lines beg end)))

    ;;  The return value

    (if (and range (< range max-lines))
        (cons beg end)
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-beginning-of-defun (&optional end-of-fun)
  "See `tinymy-function-bounds'. END-OF-FUN must be nil or t."
  (interactive)
  (let* ((bounds (tinymy-function-bounds  end-of-fun))
         (beg    (car-safe bounds))
         (end    (cdr-safe bounds))
         (point  (if end-of-fun end beg)))
    (if (null bounds)
        (message "TinyMy: Sorry, can't find function.")
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-end-of-defun ()
  "See `tinymy-function-bounds'."
  (interactive)
  (tinymy-beginning-of-defun 'end))

;;}}}

;;{{{ rectangle

;;; ............................................................ &rect ...

;;; ----------------------------------------------------------------------
;;; From Ralf Fassel <ralf@prl.philips.nl>, 22 Jun 95.
;;;
(defun tinymy-add-rectangle (START END &optional insert)
  "Add or Multiply columns in rectangle in START END.
With optional arg INSERT, insert the sum and product to
the current point."

  (interactive "r\nP")
  (require 'rect)
  (let ((sum	    0)
	(rownum	    0)
	(prod	    1)
	(rowval	    0))
    (operate-on-rectangle
        (lambda (POS BEFORE AFTER)
          (setq rownum (1+ rownum))
          (setq rowval (string-to-number (buffer-substring POS (point))))
          (setq sum  (+ sum  rowval))
          (setq prod (* prod rowval)))
        START END 't)
    (if (interactive-p)
        (message "TinyMy: For %d rows, sum=%f, product=%f" rownum sum prod))
    (if insert
        (insert (format "%0.2f %0.2f" sum  prod)))))

;;}}}
;;{{{ scrolling

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-scroll-up ()
  "Call `tinymy-scroll'."
  (interactive)
  (tinymy-scroll 'up))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-scroll (&optional up)
  "Scrolls down, optionally UP. No errors generated.
This function behaves like DOS/windows scroll commands, where cursor jumps
to the end or beginning of window first and only next scrolls. It also
keeps the cursor in the bottom or top of window according to the direction
of scroll."
  (interactive "P")
  ;; Don't know which Emacs version introduced this function.
  ;; Use old trusted implementation if it exists if there is no function
  (if (not (fboundp 'move-to-window-line))
      (tinymy-scroll-old up)
    (cond
     ((and up (ti::window-pmin-visible-p))
      (ti::pmin))
     ((and up (eq (point) (window-start)))
      (scroll-down)
      (move-to-window-line 0))
     (up
      (move-to-window-line 0))
     ((ti::window-pmax-visible-p)
      (ti::pmax))
     ((eq (point) (ti::window-pmax-line-bol))
      (scroll-up)
      ;;  Keep cursor at bottom
      (move-to-window-line -1))
     ((move-to-window-line -1)
      (beginning-of-line)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-scroll-old (&optional up)
  "Scrolls down, optionally UP. No errors generated.
This function behaves like DOS/windows scroll commands, where cursor jumps
to the end or beginning of window first and only next scrolls. It also
keeps the cursor in the bottom or top of window according to the direction
of scroll.

Note:

  This function does not work properly if the lines in the window
  exceed the length of the window. If the current line is longer than
  window length, then normal Emacs scroll command will be called."

  ;;  - the 'error' call is most disturbing if you have
  ;;  - debug-on-error t
  ;;  - This is for *interactive* only! Lisp manual forbids using scroll
  ;;    command in normal lisp code.

  (interactive "P")
  (let* ((mode   tinymy-:scroll-mode)
         lines)

    (cond
     ((ti::line-wrap-p)
      ;; ............................................. wrapping line ...

      (cond
       (up
	(if (ti::window-pmin-visible-p)
	    (ti::pmin)
	  (scroll-down)))
       (t
	(if (ti::window-pmax-visible-p)
	    (ti::pmax)
	  (scroll-up)))))
     (t
      ;; ........................................... non wrapping line ...
      (cond
       (up
	(if (bobp)
	    (message "TinyMy: beg of buffer.")
	  (if (ti::window-pmin-visible-p)	;if top is visible
	      (goto-char (point-min))
	    (if (or (null mode)
		    (and mode
			 (ti::window-pmin-line-p)))
		(scroll-down))
	    (goto-char (window-start))
	    (beginning-of-line))))
       (t
	(if (eobp)
	    (message "TinyMy: end of buffer.")
	  (if (ti::window-pmax-visible-p)
	      (goto-char (point-max))

	    (if (or (null mode)
		    (and mode
			 (ti::window-pmax-line-p)))
		(progn

		  ;;   - if outline/folding mode is on, we can't determine
		  ;;     line count with count-lines function
		  ;;   - The count gives 1 extra line, check with M-x =
		  ;;     around the window region

		  (setq lines
			(1-
			 (count-char-in-region
			  (window-start) (window-end) ?\n)))

		  ;; the scroll command does not update window points
		  ;; in 19.28! That means that the function window-end
		  ;; can't be trusted. only when this function ends,
		  ;; the window is updated.

		  (scroll-up)

		  ;;  We must manually go to the end line
		  ;;  - The cursor is always left in the line 2, after
		  ;;    scrolling in window. We have to go N lines downward
		  ;;    to put cursor at window end line
		  ;;  - next-line is used, because it hanbdles folding/outline.
		  ;;    forward-line can't be used.
		  ;;
		  ;;  Note:
		  ;;  - If the lines are longer than window-width; then this
		  ;;    whole next-line call may end anywhere...can't help
		  ;;    that
		  ;;  - The safe do is here in case this calls error,
		  ;;    which it does if the buffer size has changes, like
		  ;;    in live *Messages* buffer

		  (ignore-errors (next-line (- lines 1))))
	      (goto-char (ti::window-pmax-line-bol)))))))))))


;;}}}
;;{{{ shell -- shar, tar, uu

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-shar (single-or-list)
  "Generate SHAR file using SINGLE-OR-LIST.
List of  files can include shell regexps. The result is put into
`tinymy-:register'."
  (interactive
   (let* (arg1)
     (setq arg1
	   (ti::file-complete-filename-minibuffer
	     (read-from-minibuffer
	      (format "[%s] Shar files: " default-directory)
	      nil
	      map)))
     (list arg1)))
  (let* ((cmd         (concat tinymy-:shar-command " "))
         (register    tinymy-:register)
         (verb        (interactive-p))
         out)
    (if (ti::nil-p single-or-list)
        (error "Missing args")
      (setq out
            (shell-command-to-string
             (format "cd %s; %s %s"
                     default-directory
                     cmd
                     (ti::list-to-string (ti::list-make single-or-list)))))
      (set-register register out)
      (if verb
          (message (format "TinyMy: Register %s has shar"
                           (char-to-string register)))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-tar (tar-file file-list)
  "Generate TAR-FILE using FILE-LIST.
Return:
 t or nil       ,if tar created."
  (interactive
   (let* ((default-directory    default-directory)
          (default-tar-name     "pkg.tar")
          (default-tar          (concat default-directory default-tar-name))
          arg1 arg2
          tar-dir)

     (setq arg1
	   (ti::file-complete-filename-minibuffer
	     (read-from-minibuffer
	      (format "[%s] Tar name: " default-tar)
	      nil map)))

     (cond
      ((ti::nil-p arg1)
       (setq arg1 default-tar))
      ((file-directory-p arg1)
       (setq arg1 (concat arg1 default-tar))))

     (setq tar-dir (or (file-name-directory arg1)
                       default-directory))

     (setq default-directory tar-dir)

     (setq
      arg2
      (ti::file-complete-filename-minibuffer
        (read-from-minibuffer
         (format "[%s] Files: " arg1)
         nil
	 map)))
     (list arg1 arg2)))

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...  main . .

  (if (or (ti::nil-p tar-file)
          (ti::nil-p file-list))
      (error "Missing args"))

  (let* ((tar-cmd       (concat tinymy-:tar-command " "))
         (edir          (file-name-directory
                         (expand-file-name tar-file)))
         (cmd           (concat "cd " edir "; "
                                tar-cmd
                                (file-name-nondirectory tar-file)
                                " " file-list))
         ret)

    ;; ................................................... then case ...

    (if (and (file-exists-p edir)                  ;; must exist
             (or (not  (file-exists-p tar-file))   ;; good if not exist
                 (and  (file-exists-p tar-file)    ;; we have to remove it
                       (y-or-n-p "Tar exists, remove ? ")
                       (progn
			 (delete-file tar-file) t))))
               (progn
                 (setq cmd (read-from-minibuffer "cmd: " cmd))
                 (shell-command cmd)
                 (setq ret (file-exists-p tar-file)))
        (message "TinyMy: Aborted"))
    ret))

;;}}}
;;{{{ compilation

;;; ..................................................... &compilation ...

;;; ----------------------------------------------------------------------
;;;  Some special compile commands for C/C++, which usually
;;;  have .mak files
;;;
(defun tinymy-compile-command-search (type)
  "Search match car of `tinymy-:compile-table' against TYPE and return cdr."
  (dolist (elt tinymy-:compile-table)
    (when (string-match (car elt) type)
      (return (cdr elt)))))

;;; ----------------------------------------------------------------------
;;;  Some special compile commands for C/C++, which usually
;;;  have .mak files
;;;
(defun tinymy-compile-cc-command ()
  "Construct C/C++ compile command"
  (let* (;;  Check if there are any .mak files in directory ?
	 (file       (file-name-nondirectory (buffer-file-name)))
	 (make-files (and file
			  (ti::directory-files
			   (file-name-directory (buffer-file-name))
			   "\\.make?$\\|makefile$\\|Makefile$")))
	 (cc-cmd     (or (getenv "CC") "gcc"))
	 (flags      (or (getenv "CFLAGS") "-g")))
    (if make-files
	"make"
      (format "%s %s %s -o %s "
	      cc-cmd
	      file
	      flags
	      ;;  Drop extension
	      (ti::string-match "^[^.]+" 0 file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-compile-run-command ()
  "Run current buffer through compile buffer.
This function remembers what command you have used for each buffer
and offers it next time you call it.

If mode 'text' or 'fundamental'

    Do not try to identify buffer, but ask compile command directly

If mode is not 'text' or 'fundamental'

    Try to find suitable compile command by identifying the buffer
    and looking at the command table.

    For C/C++ code the default command suggested if 'mak', but if there
    is no makefile in the directory, then a normal compile command
    is proposed.

References:

  `tinymy-:compile-table'
  `tinymy-:compile-command-c-code'"
  (interactive)

  (let* (;;  We change this so that compile goes to right dir
	 (fid	    "tinymy-compile-run-command")
         (file      (buffer-file-name))
	 (mname     (symbol-name major-mode))

         (type      (cond
		     ((string-match "fundamental" mname)
		      mname)
		     ((ti::id-info))
		     (t
		      mname)))

	 (buffer    (current-buffer))
         elt
         run-it
         filename                       ;without directory part
         cmd)

    (tinymy-debug fid  file "type" type)

    (if (null file)
        (message "TinyMy: Can't compile, no file in this buffer.")

      ;;  Try to find last typed commad first, only if there is
      ;;  no previous command, make one.

      (unless (setq cmd (get 'tinymy-:compile-table buffer))

	(setq filename (file-name-nondirectory file)
	      elt      (tinymy-compile-command-search type))

	(tinymy-debug "No prev cmd" filename elt)

        ;; .............................................. make command ...

	;;  Only if the compile command is constant string: save it
	;;  Dynamically evaled compile commands cannot be saved.

	(setq cmd
	      (cond
	       ((stringp elt)
		(format elt file))
	       ((functionp elt)
		(funcall elt))
	       ((setq elt (eval elt))
		(format elt file))))) ;; unless

      ;; ............................................... ask from user ...

      (setq run-it
	    (ti::file-complete-filename-minibuffer
	      (read-from-minibuffer
	       "Compile: " (or cmd "make")
	       map
	       nil
	       'compile-history)))

      ;; ......................................... per buffer cmd save ...
      ;;  Save command per buffer basis

      (tinymy-debug "CMD" cmd)

      (cond
       ((not (setq cmd (get 'tinymy-:compile-table buffer)))
	(put 'tinymy-:compile-table buffer run-it))
       ((not (string= cmd run-it))
	;;  User gave different command. Update
	(put 'tinymy-:compile-table buffer run-it)))

      ;; ................................................ call compile ...

      (unless (ti::nil-p run-it)  ;Command given ?
	(compile-internal run-it "No more errors.")
	(pop-to-buffer "*compilation*")))))

;;}}}

;;{{{ timers, saving buffer

;;; ........................................................... &timer ...


;;; ----------------------------------------------------------------------
;;; 1996-03-14 gnu.emacs.help Idea by Kevin Rodgers <kevinr@ihs.com>
;;; Adjusted by [jari]
;;;
(defun tinymy-save-buffers-periodically ()
  "*Periodically save selected buffers.
References:
  `tinymy-:save-buffer-modes'
  `tinymy-:save-buffer-regexp'"
  (interactive "P")
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(when (and (stringp buffer-file-name)
		   (buffer-modified-p)
		   (stringp tinymy-:save-buffer-regexp)

		   (or (memq major-mode tinymy-:save-buffer-modes)
		       (string-match (buffer-file-name)
				     tinymy-:save-buffer-regexp)))
	  (message "TinyMy: Periodic save for buffer %s" (buffer-name))
	  (save-buffer))))))

;;}}}
;;{{{ word movement

;;; --------------------------------------------------- &word-movement ---
;;; #todo: Uhm; rewrite sometime.
;;;
(defun tinymy-word-move-1 (&optional back)
  "Low level word movement control. Optionally move BACK."
  (let* ((up-case (memq major-mode tinymy-:move-word-case-modes))
	 (regexp "[a-z0-9]*[A-Z]+[a-z0-9]+[A-Z]+")

         (case-fold-search (not up-case))

         (charset
          (cond
           ((and up-case
                 (or (if back
			 (save-excursion
			   (cond
			    ((not (eq 0 (skip-chars-backward "a-z")))
			     (let (case-fold-search)
			       (string-match
				"[A-Z]"
				(char-to-string (preceding-char)))))
			    ((not (eq 0 (skip-chars-backward " \t")))
			     (bolp))))
		       (looking-at regexp))
		     ;; Cursor is at the end of word
		     (and
		      (member (char-to-string (char-syntax (preceding-char)))
			      '("w" "."))
		      (not
		       (member (char-to-string (char-syntax (following-char)))
			       '("w" "."))))))

            ;;  This Upcase charset is only used if the cursor is
            ;;  within AnUpCaseWord.

            tinymy-:move-word-case-set)
           (t
            tinymy-:move-word-set))))
    (cond
      ;;  Skip to the end of word if at EOL
      ;;  (this-he-is-word sse-it-now?)
      ;;			  *	cursor here
      ;;			    *	after
      ;;  otherwise it would skip to next line's word begin.
     ((and (null back)
           (or (looking-at "\\([A-Z]+\\)[^a-zA-Z \t]*$")
	       (looking-at "\\([a-z]+\\)[^a-zA-Z \t]*$")
	       (looking-at "\\([A-Z][a-z]+\\)[^a-zA-Z \t]*$")))
      (goto-char (match-end 1)))
     (t
      (ti::buffer-word-move charset back)))))

(defun tinymy-backward-word ()
  "Word backward See `tinymy-:move-word-case-set'."
  (interactive)
  (tinymy-word-move-1 'back))


(defun tinymy-forward-word ()
  "Word forward. See `tinymy-:move-word-case-set'."
  (interactive)
  (tinymy-word-move-1))

;;}}}
;;{{{ minor mode: display

;;; .................................................... &mode-display ...

;;;###autoload (autoload 'tinymy-display-mode	       "tinymy" "" t)
;;;###autoload (autoload 'turn-on-tinymy-display-mode  "tinymy" "" t)
;;;###autoload (autoload 'turn-off-tinymy-display-mode "tinymy" "" t)


(add-hook 'tinymy-display-:mode-define-keys-hook
	  'tinymy-display-mode-define-keys)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinymy-display-" " D" "\C-cD" "Tdisp" 'TinyDisplay "tinymy-display-:" ;1-6

  "Minor mode to switch between different diplsy tables.
The default display table hides dos file ^M characters and other
special characters.

Note:

  While the diplay table is active, your cursor does _not_ move as
  you think it would. Bear in mind that the character are still there
  in the buffer, but they are not displayed. When you press cursor
  key and the cursor doesn't move, it all due to underlaying non-printed
  character. Use only Mouse (if you can use one) or PgUp and PgDown to
  view the file.

Mode description:

\\{tinymy-display-:mode-prefix-map}"

  "TinyDisplay"

  (progn
    (tinymy-display-mode-save-original)
    (tinymy-display-mode-init)
    (if tinymy-display-mode
        	(tinymy-display-mode-sandard)
      (tinymy-display-mode-restore-original)))

  "Display minor mode"

 (list
  tinymy-display-:mode-easymenu-name
  ["Original display"		      tinymy-display-mode-restore-original  t]
  ["Standard display. Hide non-ascii" tinymy-display-mode-sandard	    t]
  ["MSDOS CR/LF hide display"	      tinymy-display-mode-M		    t]
  "----"
  ["Mode help"			      tinymy-display-mode-help		    t]
  ["Mode off"			      tinymy-display-mode		    t])

 (progn
   (define-key map "o"  'tinymy-display-mode-restore-original)
   (define-key map "s"  'tinymy-display-mode-sandard)
   (define-key map "m"  'tinymy-display-mode-M))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-init ()
  "Initialize mode: define display tables."
  (unless (get 'tinymy-display-mode 'table-std)
    (put 'tinymy-display-mode 'table-std (tinymy-display-mode-standard-table))))

;;; ----------------------------------------------------------------------
;;;  1997-10-07 Idea by Michael Welsh Duggan <md5i@cs.cmu.edu> in
;;;  ding mailing list
;;;
(defun tinymy-display-mode-standard-table  ()
  "Return standard display table for tinymy-display mode."
  (let* ((table (make-display-table)))
    (ti::dotimes counter 0 31 (aset table counter  []))
    (aset table 9   nil)		;preserve tab
    (aset table 10  nil)		;preserve newline

    (ti::dotimes counter 127 255 (aset table counter  []))

    ;;  Save some special marks.
    (aset table 133 [?. ?. ?.])	;\205 = ...
    (aset table 145 [?`])	;\221 = `
    (aset table 146 [?'])	;\222 = '
    (aset table 147 [?\"])	;\223 = "
    (aset table 148 [?\"])	;\224 = "
    (aset table 149 [?*])	;\225 = *
    (aset table 150 [?- ?-])	;\226 = --
    (aset table 151 [?- ?- ?-])	;\227 = ---
    (aset table 153 [?( ?T ?m ?)])	;\231 = (Tm)
    table))


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-activate (&optional table force-display)
  "Activate display TABLE and possibly FORCE-DISPLAY redraw."
  ;;  nil TABLE is same as default table.
  (let* ((sym 'buffer-display-table))
    (cond
     ((boundp sym)			;Emacs
      (set sym table))
     (t
      (if (not (xemacs-p "20.3"))
	  ;;  Bug in XEmacs. must use this one.
	  (ti::funcall 'add-spec-to-specifier
		    (symbol-value 'current-display-table)
		    table (current-buffer) nil)
	;;  New Xemacs 20.3+ gets this right
	(ti::funcall 'set-specifier (symbol-value 'current-display-table)
		  table (current-buffer)))))
    ;; Don't know why this was sometimes needed, but otherwise the table
    ;; change weren't seen.
    (when force-display
      (recenter))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-property-name ()
  "Return property name."
  (make-symbol (or buffer-file-name (buffer-name))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-save-original ()
  "Save `buffer-display-table'."
  (let* ((property (tinymy-display-mode-property-name))
	 (table    (if (boundp 'buffer-display-table)
		       (symbol-value 'buffer-display-table) ;Emacs
		     (symbol-value 'current-display-table))))
  (unless (get 'buffer-display-table property)
    (put 'buffer-display-table property table)
    ;;  Tell that we did something
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-restore-original ()
  "Restore `buffer-display-table'."
  (interactive)
  (let* ((table (get 'buffer-display-table
		     (tinymy-display-mode-property-name))))
    (tinymy-display-mode-activate table)
    (setq tinymy-display-:mode-name " Do")
    (if (interactive-p)
	(message "TinyMy: Display table restored to original."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-sandard ()
  "Hide all non-ascii characters."
  (interactive)
  (tinymy-display-mode-init)
  (tinymy-display-mode-activate (get 'tinymy-display-mode 'table-std))
  (setq tinymy-display-:mode-name " Ds")
  (if (interactive-p)
      (message "TinyMy: Display table standard, hide non-ascii, active.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-display-mode-M ()
  "Hide all non-ascii characters. Treat ^M as spaces.
What we would like to have is ^M as newlines, but that's not possible
with display table, so the second best is to display spaces."
  (interactive)
  (tinymy-display-mode-init)
  (let* ((table (get 'tinymy-display-mode 'table-std)))
    (aset table 13 [?\ ?\ ?\ ])
    (tinymy-display-mode-activate table)
    (setq tinymy-display-:mode-name " Dm")
    (if (interactive-p) (message "TinyMy: Display table MSDOS active."))))



;;}}}
;;{{{ minor mode: sort

;;; ....................................................... &mode-sort ...

;;;### (autoload 'turn-off-tinymy-sort-mode "tinymy" ""	t)
;;;### (autoload 'turn-on-tinymy-sort-mode  "tinymy" ""	t)
;;;### (autoload 'tinymy-sort-mode	    "tinymy" ""	t)


(add-hook 'tinymy-sort-:mode-define-keys-hook ;To be sure
	  'tinymy-sort-mode-define-keys)


(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinymy-sort-" " S" "\C-cS" "Tsort" 'TinySort "tinymy-sort-:" ;1-6

  "Minor mode for sorting lines (by columns) in the buffer easily.
Remember to select region to sort.

When you sort by columns, the line must have enough columns, e.g.
if you select following area and try to sort by clumn 3, that is
not possible. Also, There must be no empty lines inside sorted area.

    123 123 123
    123 123
    123 123 123

Mode description:
\\{tinymy-sort-:mode-map}"

  "TinySort"

  nil

  "Column sort minor mode"

 (list
  tinymy-sort-:mode-easymenu-name
  ["By column 1"                 tinymy-sort-column-1  t]
  ["By column 2"                 tinymy-sort-column-2  t]
  ["By column 3"                 tinymy-sort-column-3  t]
  ["By column 4"                 tinymy-sort-column-4  t]
  ["By column 5"                 tinymy-sort-column-5  t]
  ["By column 6"                 tinymy-sort-column-6  t]
  ["By column 7"                 tinymy-sort-column-7  t]
  ["By column 8"                 tinymy-sort-column-8  t]
  ["By column 9"                 tinymy-sort-column-9  t]
  "----"
  ["Mode help"			 tinymy-display-mode-help t]
  ["Mode off"			 tinymy-display-mode	t])

 (progn
    (define-key map "1"  'tinymy-sort-mode-column-1)
    (define-key map "2"  'tinymy-sort-mode-column-2)
    (define-key map "3"  'tinymy-sort-mode-column-3)
    (define-key map "4"  'tinymy-sort-mode-column-4)
    (define-key map "5"  'tinymy-sort-mode-column-5)
    (define-key map "6"  'tinymy-sort-mode-column-6)
    (define-key map "7"  'tinymy-sort-mode-column-7)
    (define-key map "8"  'tinymy-sort-mode-column-8)
    (define-key map "9"  'tinymy-sort-mode-column-9)
    (define-key map "?"  'tinymy-sort-mode-help))))



;; Create functions like this:
;;
;; (defun tinymy-sort-column-0 (beg end)
;;    (interactive "*r") (tinymy-sort-column beg end 0))

(mapcar
 (function
  (lambda (x)
    (let ((sym (intern (format "tinymy-sort-mode-column-%d" x)))
          def)
      (setq def
            (` (defun (, sym) (beg end)
                 (interactive "*r")
                 (tinymy-sort-column beg end (, x) ))))
      (eval def))))
 '(1 2 3 4 5 6 7 8 9))


;;; ----------------------------------------------------------------------
;;;
(defun tinymy-sort-column (beg end nbr)
  "Sort region BEG END according to column NBR."
  (interactive "r\np")
  (let* ((opoint (point)))
    (untabify beg end)
    (goto-char (min beg end))		;Sort breaks otherwise
    (sort-fields nbr beg end)
    (goto-char opoint)))


;;}}}
;;{{{ alias definitions and others

;;; ........................................................... &alias ...

(defun tinymy-alias ()
  "Install some aliases."
  ;;  Say always y-or-n-p; so that there is no need to type "yes" or "no"
  (defalias 'yes-or-no-p 'y-or-n-p))

;;; ----------------------------------------------------------------------
;;; Idea by 1997-11-05 Kevin Rodgers gnu-emacs.help
;;;
(defun tinymy-maybe-disable-auto-save ()
  "If the directory is read only, do not keep auto save files."
  (when (and (stringp buffer-file-name)
	     (not (file-writable-p
		   (file-name-directory buffer-file-name))))
    (auto-save-mode nil)
    (set (make-variable-buffer-local 'auto-save-interval) 0)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymy-find-file-hook ()
  "Activate DOS display table for dos files (in UNIX) ."
  (when (and (not (eq 'win32 (ti::xe-window-system))) ;NT port
	     (ti::dos-file-p))
    (tinymy-display-mode 1))
  (tinymy-maybe-disable-auto-save)
  ;; hook return value
  nil)

;;}}}



;;{{{ examples

;;; ........................................................ &examples ...
;; Here is 2 files that you propably want to use in addition to this
;; elisp package.
;;
;; mak      Bourne sh-script, run all .mak files in a directory
;; emc      Bourne sh-script, easy Emacs package compile command
;;
;; These files have been tarred and signed with PGP. The PGP is used
;; because it allows ASCII armor and ZIP compression. To unpack this,
;; call M-x tinymy-install-files
;;
;; file pkg.tar:
;; r--r--r-- 240/222    593 Feb 20 21:20 1997 mak.sh
;; r-xr-xr-x 240/222   1725 Feb 20 20:56 1997 emc
;;
;; -----BEGIN PGP MESSAGE-----
;; Version: 2.6.3ia
;; Comment: base64 signed file pkg.tar. Uncompresses to approx. 10K
;;
;; owHtVkFoHFUY3ioVdlT0YG8t/E62Jk0zszO72cSsxjZNY1MxSXG7WImBvp15m3nN
;; zLztvNlkF8Sr1oMIonjyKIh48KIgPfQmFC8ePAp6FBS8FAUV/N97s5tNTZCiQQrz
;; kezMvPf/7//+//3//96NQvPBo4XqI1+9devLn7+98MSJ2pEj39058uex4x+tffbi
;; 7z98/NTjb7//4a9ff3I7Xm5svDu3/NMfK5M/Tn7wyw3z0cfmLn1z+70T5265nx9/
;; 4847N7+f/e3mQ+yL5gPzbx77dOLh1lGR+iwuICKyZYugcKhwENPT0+pZnXH0szqj
;; ngjXrbgFZ6bqVN1KxXGrOO5WZqcrBedwaWl0RUoSNHmNkDDlB8vxyItIfPB85szw
;; eZ+gbhhjAHAp4ZsJiYD5xpgakDg7MXYKFsuLp0/DCtmiQMIQMF9om4VUgI2vwGLw
;; uklC4xR8luzVLF3066Dza2obXNsFd25utuxUyhUHX+sVpz5dBR13WOp1oDRiu3Se
;; ewz1n6ctqDhpoHSh4tZdF14gCYMFpVbaVVjkcUq8VCo9ew0lbLXw2Q5v8R4Vtsej
;; 54YWGv2YdwQT8rN4UpKE9YWXLjRXllYvb6jBywH62+LbFDzp9g7DnzaLfRWESeW7
;; DsPeCFAv5Ukf9QmKslTrqRUo8QLgMcUh9IZ3UsZjEsLAqo1WjcWV8/Ml1zCoF3Aw
;; pVfLa43LdbgacJHGJKJXTdwu655hhCLj/Bq8auwESBwSSnxp3fC5IS3t2pRbDFYb
;; SjiLYV05bxr7DqNmTA2ZPxSd5W0VECPf//tq/2nkHXqPkf2wVqsd1P+rldla1v/d
;; WfxT/b8q5Q+dWSHv//v1/6Kq36WVhcUGtEiKiYvV08GsmYJOsikTEdat3oaqAP1j
;; 2/ZATZU9ppWq+dqemn+6Xpupu7W/1XzxgGp3UP6uai/eU52v8pQqAy9TiHCnISBY
;; 0SSFkBL8ohHxBNqyq87pKWhRj3QFhZDv0CSbS6iUpNgoikpV0ITxroBWd1NWvkW3
;; sYYFlrQX2CB7Bg99VMY6F1j28XiKij6HVj+lOoRE1r3uAcT3qQ8dkgbCViSbuPq4
;; wBCzbZIiDyY6uz0FmADR7XS4QCWMRYsiAaX2eplKUfX+Cu9qH1HCC0i8iW/IypwQ
;; NL2OnhHfkgZhwuMxHuG2Kb1IA7m2l7COpMva0OfdZD/zPKI7AU0o0FBQ21C9H5qC
;; xZvYKwUPu8haLY/CbQywnB5lFHGftfvaXock6RQMjLVYjFuJQSO4esxTTUtQucKA
;; A07baI3KZMRS3L2pnARsaZikassstZ16Dk2qnu3xRC6hYy0brZxs6GBCo7lqrTW0
;; QQrNmG3TRLAUm7hxZQl5zZe3eVjGl3JPGTCWqs6eYT0ql1yI4CIk3TiWEVFe4l5H
;; fdjhyRYeMl7AsP8vX7KaV8B1zqABdH8dLB/KeB6UQ44HBWw8I3nERnasQcZhKDDC
;; RLrqTg8lB7z2io4Gpc0M49zF1fmSFs3iQKnchh0Kmxh3GQOrl51PsqztAU2z5Jow
;; D6bVMyXJAUsRsHaKT+0KXg8U4YEbxTHwuTwlsdh6DEvujKarWChJ/JbZNDKsyRVR
;; tYH71p8apo8sThXVzDS6Iz3S9EpjSM4Bi8OTigYuNRLLwYmqz+yICZm0U8ATTDZY
;; l8IbWHdU4DFum8aQEIZnbYsgBYyOZhEM0w/kv/xWudvX/omBmjxV8EbAdSZQGKl+
;; W8uM3LoU2VEJLExDzWeRx3kVex2yPfFXdx+pb+lODVlTGt+/5M2sW5i7M6dOjcuI
;; KXVLdior6/ZQmtQsVDz2sWVdB8uKuYXlQi11DgyNw93W4SDz8M/2Bxs9dtddM9//
;; f73///8NJEeOHDly5MiRI0eOHDly5MiRI0eOHDly5MjxX+Mv
;; =UTL4
;; -----END PGP MESSAGE-----

;;}}}

(provide   'tinymy)

(add-hook  'after-save-hook  'tinymy-maybe-make-file-executable)

(run-hooks 'tinymy-:load-hook)

;;; tinymy.el ends here
