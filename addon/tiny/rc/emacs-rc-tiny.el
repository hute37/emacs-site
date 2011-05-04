;; @(#) emacs-rc-tiny.el -- Emacs rc file for Tiny tool (tiny*el) packages setup
;; @(#) $Id: emacs-rc-tiny.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{  Documentation
;;; Documentation:
;;
;;  File id
;;
;;	.Copyright (C) 1996-2001 Jari Aalto
;;	.Author:       Jari Aalto <jari.aalto@poboxes.com>
;;	.Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;;	.Created:      1996-01
;;	.Keywords:     tools
;;
;;	This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that.. Just rename this file to whatever is shown
;;      in the first line.
;;
;;      The file layout is managed with tinytab.el, folding.el
;;
;;  Installation
;;
;;	Put following entry entry in your $HOME/.emacs and make sure this
;;      package is along your `load-path'. (I suggest ~/elisp/rc/ )
;;
;;          (require 'emacs-rc-tiny)
;;
;;  Description
;;
;;      Customized setup for all tiny*el Emacs pacakges. This file defines
;;      global keybindings that may conflict yours. The prefix key for all
;;      minor modes is defined in variable 'my-:minor-mode-prefix' which
;;      is by default
;;
;;	    C-c m f	TinyTf
;;	    C-c m d     TinyDiff
;;	    C-c m e	TinyLisp  (e for elisp)
;;	    ...
;;
;;  Autoload note
;;
;;	I try to keeps as many function in autoload state as possible, so
;;	that this Emacs rc file loads as fast as possible. Only when I
;;	happen to need the feature in emacs, Emacs loads it for me.
;;
;;  Code note
;;
;;      There are two ways to load packages when some mode runs. Let's
;;      investigate what happens when I'm starting to compose mail and
;;      `mail-mode' is about to be turned on.
;;
;;      Traditional method
;;
;;	    (autoload 'timi-mail        "tinymail" "" t)
;;	    (add-hook 'mail-setup-hook	'timi-mail)
;;
;;      But I use
;;
;;	    (autoload 'timi-mail        "tinymail" "" t)
;;	    (add-hook 'mail-setup-hook	'my-timi-mail)
;;	    (defun my-timi-mail ()
;;	      (unless (featurep 'tinymail) (require 'tinymail) (timi-mail) ))
;;
;;	That's because I have my tinymail.el compressed in one site and in
;;      one site it it not compressed. I enable tinyezip.el in the site
;;      that has quota limit. TinyEzip cannot use autoloaded functions, so
;;      the *Traditional* *method* would cause run time error. In the latter,
;;      I manually use command `require' which triggers TinyEzip and it can
;;      find the compresses package.
;;
;;      So don't wonder why I use complex looking load setups below. They
;;      are not complex, but arranged a bit differently.
;;
;;  Using eval-and-compile
;;
;;	You see lot of autoloads wrapped inside `eval-and-compile'. This
;;	is necessary so that byte compiler sees the function which is used
;;	a bit later.
;;
;;  Use of defconst
;;
;;	I have a habbit to define the basic variables with `defconst' because
;;	then the byte compiler doesn't whine about "not seen" variables, which
;;	it would if I had used `setq'.
;;
;; }}}

;;; Change Log:
;;; Code:

;;}}}

;;; ....................................................... &beginning ...


(eval-when-compile
  (require 'advice)
  (require 'cl)
  )

(eval-and-compile
;;  (autoload 'my-:minor-mode-keymap "emacs-rc-lib" "" nil 'keymap)
  )

;;{{{ libraries


(require 'tinylibm)

(eval-when-compile

  ;; XEmacs has console-type, but it also has
  ;; window-system, but it gives compiler warning.
  ;; (if (xemacs-p) (put 'window-system 'byte-obsolete-variable nil))
  )


;;; ........................................................... keymap ...

(defun my-minor-mode-map-define ()
  "Make minor mode (prefix keymap) where to put all minor mode enables.
To put key into keymap, use
  (define-key my-:minor-mode-keymap KEY MINOR-MODE-ENABLE-FUNCTION)"
  (defconst my-:minor-mode-prefix "\C-cm")
  (defvar   my-:minor-mode-keymap (make-keymap))
  (use-prefix-key global-map my-:minor-mode-prefix)
  (define-key global-map my-:minor-mode-prefix my-:minor-mode-keymap))

(my-minor-mode-map-define)

;;; ....................................................... &lib-setup ...
;;;  define more easily typed name M-x describe-symbols

(autoload 'ti::y-describe-symbols	"tinyliby"	"" t)

(unless (fboundp 'describe-symbols)   ;; easier function name to remember
  (defalias-maybe 'describe-symbols 'ti::y-describe-symbols))

;;}}}
;;{{{ tiny

(when-package 'tinypath nil

  (add-hook 'tinypath-:cache-duplicate-report-ignore-functions
	    'my-tinypath-duplicate-ignore)

  (defun my-tinypath-duplicate-ignore (file)
    "Ignore some files from report."
    ;;  don't care to show duplicated for Emacs cc-mode and CVS updated
    ;;  cc-mode
    (string-match "/jde/\\|gnus\\|cc-mode\\|load-path\\|_pkg" file))

  )


;;; ........................................................ &comments ...
;;;  Let this handle commmenting for me, substitute Emacs command.

(when-package 'tinycom nil
  (autoload 'tinycom-indent-for-comment  "tinycom" "" t)
  (global-set-key "\e;"  'tinycom-indent-for-comment)
  )

;;; ......................................................... &compile ...
;;;  Compile buffer addition, eg. kill uninteresting lines
;;;  The defun has require, because it forces loading even compressed file.

(when-package 'tinycompile nil
  (autoload 'tinycompile-mode		    "tinycompile" "" t)
  (autoload 'turn-on-tinycompile-mode	    "tinycompile" "" t)
  (add-hook 'compilation-mode-hook 'my-tinycompile-hook 'append)

  (defun my-tinycompile-hook ()
    (require 'tinycompile))

  )


;;; ........................................................ &procmail ...

(when-package 'tinyprocmail nil
  (define-key my-:minor-mode-keymap "P" 'tinyprocmail-mode)
  (autoload 'tinym-mode	"tinyprocmail" "" t))


;;; ............................................................. &Url ...

(when-package 'tinyurl nil

  (define-key my-:minor-mode-keymap "up" 'tinyurl-plugged-mode-toggle)
  (define-key my-:minor-mode-keymap "uu" 'tinyurl-mode)
  (define-key my-:minor-mode-keymap "u1" 'tinyurl-mode-1)

  (autoload 'tinyurl-mode	    "tinyurl" "" t)
  (autoload 'tinyurl-mode-1	    "tinyurl" "" t)
  (autoload 'turn-on-tinyurl-mode-1 "tinyurl" "" t)

  (add-hook 'Man-mode-hook	'turn-on-tinyurl-mode-1)
  )

;;; ................................................... &electric-file ...

(when-package 'tinyef nil
  (add-hook   ;;  Minibuffer Electric file minor mode
   'minibuffer-setup-hook
   '(lambda ()
      (require-p 'tinyef)
      (turn-on-tinyef-mode))))


(defconst tinyef-:mode-key "\C-cmr")

(defconst tinyef-:mode-key-table
  '(
    (?\[   . step-delete-back)		;KEY -- action symbol
    (?\]   . step-delete-fwd)
    (?\*   . chunk-delete)
    (?\;   . move-back)
    (?\'   . move-fwd)
    (?\~   . e-tilde)			;electric keys
    (?\/   . e-slash)
    (?\$   . e-dollar)
    ))

;;{{{ tinyeat

;;; ..................................................... &eating-text ...

(when-package 'tinyeat nil

  (eval-and-compile
    (autoload 'tinyeat-delete-whole-word	"tinyeat" "" t)
    (autoload 'tinyeat-forward-preserve		"tinyeat" "" t)
    (autoload 'tinyeat-backward-preserve	"tinyeat" "" t)
    (autoload 'tinyeat-delete-paragraph		"tinyeat" "" t)
    (autoload 'tinyeat-kill-line-back		"tinyeat" "" t)
    (autoload 'tinyeat-kill-buffer-lines	"tinyeat" "" t)
    (autoload 'tinyeat-kill-buffer-lines-min	"tinyeat" "" t)
    (autoload 'tinyeat-zap-line			"tinyeat" "" t)
    (autoload 'tinyeat-install-default-bindings "tinyeat")
    )

  (defun my-tinyeat-:load-hook  ()
    "TinyEat customizations."
    (interactive)

    (tinyeat-install-default-bindings)
    (global-set-key [(meta backspace)]	    'tinyeat-erase-buffer)
    (global-set-key [(meta delete)]	    'tinyeat-erase-buffer)
    (global-set-key [(control backspace)]   'tinyeat-forward-preserve)
    (global-set-key [(meta backspace)]	    'tinyeat-backward-preserve)
    (global-set-key [(alt backspace)]	    'tinyeat-backward-preserve)
    (global-set-key [(shift backspace)]	    'tinyeat-delete-whole-word)
    (global-set-key [(control meta backspace)] 'tinyeat-kill-buffer-lines)
    (global-set-key [(control meta shift backspace)]
		    'tinyeat-kill-buffer-lines-min)

    (global-set-key [(control meta ?k)] 'tinyeat-zap-line)
    (global-set-key [(control alt ?k)]  'tinyeat-zap-line)


    (cond
     ((ti::xe-window-system)
      ;; ... ... ... ... ... ... ... ... ... ... ... ...  other pc setting . .
      ;;  - notice the word "delete" for backspace
      ;;  - And there is no ALT-backspace key available :-(
      ;;    This means that the Meta M- modifier is useless.

      (global-set-key [(control shift delete)]	'tinyeat-delete-paragraph)
      (global-set-key [(control meta delete)]	'tinyeat-kill-buffer-lines)
      (global-set-key [(control meta shift delete)] 'tinyeat-kill-buffer-lines-min)
      (global-set-key [(alt meta delete)]	'tinyeat-kill-buffer-lines)
      (global-set-key [(alt meta shift delete)]	'tinyeat-kill-buffer-lines-min)

      ;;  My Alt key produces Meta....

      (global-set-key [(meta delete)]		'tinyeat-backward-preserve)
      (global-set-key [(meta shift delete)]	'tinyeat-kill-line-back)
      (global-set-key [(alt delete)]		'tinyeat-backward-preserve)
      (global-set-key [(alt shift delete)]	'tinyeat-kill-line-back)
      (global-set-key [(meta k)]		'tinyeat-kill-line)
      )))


  ;;  Use these keys to "autoload" the package

  ;; (global-set-key [(control mouse-2)] 'tinyeat-forward-preserve)
  (global-set-key [(control backspace)]  'tinyeat-forward-preserve)
  (global-set-key [(meta backspace)]	 'tinyeat-backward-preserve)

  (add-hook 'tinyeat-:load-hook 'my-tinyeat-:load-hook)

  ) ;; WHEN tinyeat

;;}}}


;;; ................................................. &text-formatting ...

(when-package 'tinypage nil  ;; ^L pages viewer and handle
  (autoload   'tinypage-mode "tinypage" "" t))


(when-package 'tinytf nil

  ;;  Writing 'Technical text', a minor-mode. I manage all my text
  ;;  files with this
  ;;
  ;;  See Also perl script http://poboxes.com/jari.aalto/t2html.html

  (autoload 'tinytf-mode                 "tinytf" "" t)
  (autoload 'turn-on-tinytf-mode-maybe   "tinytf")
  (add-hook 'find-file-hooks             'turn-on-tinytf-mode-maybe)

  (define-key my-:minor-mode-keymap "f" 'tinytf-mode)

  ;;  Make sure the default bindings are run first and my hook runs
  ;;  after that, That's why the 'append parameter.

  (add-hooks 'tinytf-:mode-define-keys-hook
	    '(tinytf-mode-define-keys
	      tinytf-mode-define-f-keys
	      ))

  (add-hook 'tinytf-:mode-define-keys-hook
	    'my-tinytf-mode-define-keys 'append)

  (defun my-turn-on-flyspell-mode ()
    (if tinytf-mode
	t ;; (flyspell-mode 1)
      (flyspell-mode -1)))

  (if (fboundp 'flyspell-mode)
      (add-hook 'tinytf-:mode-hook 'my-turn-on-flyspell-mode))

  )


(defun my-tinytf-mode-define-keys ()
  "Changing some bindings to fit current keyboard better."
  (defvar tinytf-:mode-prefix-map nil)
  (defvar tinytf-:mode-prefix-key nil)
  (let ((map tinytf-:mode-prefix-map)
	)
    ))


;;; ............................................................ &perl ...


(when-package 'tinyperl nil

  (dolist (dir '("~/tmp/pause"))
    (when (file-directory-p dir)
      (defconst tinyperl-:pause-directory dir)
      (return)))

  (define-key my-:minor-mode-keymap "p" 'tinyperl-mode)

  (eval-and-compile
    (autoload 'turn-off-tinyperl-mode   "tinyperl" "" t)
    (autoload 'turn-on-tinyperl-mode    "tinyperl" "" t)
    (autoload 'tinyperl-mode	        "tinyperl" "" t)
    )

  ;;  I do all my perl editing in text-mode + TinyTab
  ;;  Add TinyPerl minor mode if text-mode contains perl script:

  ;; (add-hook  'text-mode-hook 'my-tinyperl-mode-hook)

  (defun my-tinyperl-mode-hook ()
    "Turn on TinyPerl mode is perl code is found from buffer."
    (if (string= "code-perl" (ti::id-info))
	(turn-on-tinyperl-mode)))
  )

;;; .................................................. &tab-minor-mode ...

(when-package 'tinytab nil

  (define-key my-:minor-mode-keymap "+" 'tinytab-mode)

  (autoload 'tinytab-return-key-mode	    "tinytab" "" t)
  (autoload 'tinytab-mode		    "tinytab" "" t)
  (autoload 'turn-on-tinytab-mode	    "tinytab" "" t)
  (autoload 'turn-off-tinytab-mode	    "tinytab" "" t)
  (add-hook 'tinytab-:mode-hook		    'my-tinytab-mode-hook)

  (global-set-key [(control shift backtab)] 'tinytab-mode)
  (global-set-key [(control shift tab)]	    'tinytab-mode)
  (global-set-key [(control shift kp-tab)]  'tinytab-mode)
  (global-set-key "\C-c\C-m"		    'tinytab-return-key-mode)

  )

(when-package 'tinyindent nil

  (define-key my-:minor-mode-keymap "t" 'tinyindent-tt-mode)
  (define-key my-:minor-mode-keymap "T" 'tinyindent-mode)

  (autoload 'tinyindent-mode		"tinyindent"	"" t)
  (autoload 'turn-on-tinyindent-mode	"tinyindent"	"" t)
  (autoload 'turn-off-tinyindent-mode	"tinyindent"	"" t)
  (autoload 'tinyindent-tt-mode		"tinyindent"	"" t)

  (global-set-key [(shift backtab)]	'tinyindent-mode)
  (global-set-key [(shift tab)]		'tinyindent-mode)
  (global-set-key [(control tab)]	'tinyindent-tt-mode)
  )

;;; ----------------------------------------------------------------------
;;;
(defun my-tinytab-mode-hook ()
  "My TinyTab mode settings."

  (defvar   tinytab-:mode-map   nil)
  (defconst tinytab-:mode-table nil)		;No mode specific actions

  ;;  I have other reserved setting for esc-`

  (define-key tinytab-:mode-map "\e`" nil)
  (define-key tinytab-:mode-map [(control ?`)] 'tinytab-tab-backward)

  (define-key   tinytab-:mode-map
    [(meta shift kp-tab)] 'tinytab-indent-by-tab-width-back)

  (define-key   tinytab-:mode-map
    [(control meta tab)] 'tinytab-indent-by-tab-width-back)

  (when (win32-p)
    ;;  Top-leftmost key. just above tab key. In Finnish keyboard this the
    ;; "Law" character
    (define-key tinytab-:mode-map [(control ?\247)] 'tinytab-indent-by-tab-width)
    )

  (define-key tinytab-:mode-map [(meta shift backtab)]
    'tinytab-indent-by-tab-width-back)

  )


;;; ----------------------------------------------------------------------
;;;
(defun my-tinytab-special-tab-insert ()
  "Special tab handling.
Continues comment if needed. Integrates tinytab and tinyindent."
  (let* (fill
	 )
    (require 'tinyindent)
    (when (= 0 (current-column))
      (save-excursion
	(forward-line -1)		;peek previous line ?
	(setq fill (sfuncall 'tinyindent-special-handle)))

      (when (stringp fill)
    	(insert fill)

	t				;We handled it. Return indication
	))))


;;}}}
;;{{{ file, macro, searching, lock

;;; ..................................................... &search-word ...
;;;  Searching word backward and forward under point


(when-package 'tinyreplace nil

  (use-prefix-key global-map "\C-z")

  (global-set-key  "\C-z5"
		   (definteractive
		     (if buffer-read-only
			 (message "My: Cannot start replace, buffer is read-only.")
		       (ti::menu-menu 'tinyreplace-:menu))))

  (defconst tinyreplace-:menu
    '("replace: 5=fwd, w=word, r=reg, c=compile buffer files f=over files"
      (
       (?5  . ( (call-interactively 'tinyreplace-replace-forward)))
       (?w  . ( (call-interactively 'tinyreplace-word-replace)))
       (?r  . ( (call-interactively 'tinyreplace-replace-region)))
       (?c .( (call-interactively 'tinyreplace-replace-over-files-compile-buffer)))
       (?f  . ( (call-interactively 'tinyreplace-replace-over-files)))
     ))
    "")

  )

;;; ................................................... &making-macros ...
;;;  Making ad-hoc macros easily

(when-package 'tinymacro nil
  (autoload 'tinymacro-assign	                "tinymacro" "" t)
  (autoload 'tinymacro-end-kbd-macro-and-assign "tinymacro" "" t)

  (global-set-key [(f9)]	    'start-kbd-macro)
  (global-set-key [(control f9)]    'tinymacro-end-kbd-macro-and-assign)

  )

;;; ............................................................ &lock ...
;;;  Emacs locking utility

(when-package 'tinylock nil
  (autoload 'tinylock-user-activity     "tinylock")
  (autoload 'tinylock-lock	        "tinylock" "" t)

  (defconst tinylock-:auto-lock-interval1 45)	;in minutes
  ;; (add-hook 'tinylock-:before-lock-hook 'my-tinylock-before-lock-hook)
  ;; (add-hook 'tinylock-:after-lock-hook  'my-tinylock-after-lock-hook)
  )


;;}}}
;;{{{ hotlist, mbx

;;; ............................................................. &mbx ...

;;  browsing mailbox files easily

(when-package 'tinymailbox nil
  (define-key my-:minor-mode-keymap "m" 'tinymailbox-mode)
  (autoload 'tinymailbox-mode "tinymailbox" "" t)
  (assoc-entry-replace-maybe-add	    ;; Turn on the mode for these files
   'auto-mode-alist
   '(("\\.mbo?x\\'"  . tintmbx-mode)
     ("\\.spool\\'"  . tinymailbox-mode)
     ("Incoming"     . tinymailbox-mode) ;; Gnus mail input files
     )))

;;; ......................................................... &hotlist ...
;;; X-popup: hotlist of most used buffers; only usable in window systems.


(when (ti::xe-window-system)
  (when-package 'tinyhotlist nil

    (autoload	     'tinyhotlist-control	"tinyhotlist" "" t)
    (global-set-key  [(control shift mouse-3)]	'tinyhotlist-control)
    (add-hook        'tinyhotlist-:load-hook	'tinyhotlist-load-hotlist)


    (let* ((h (concat
	       (or (getenv "HOME") (error "no HOME env variable"))
	       "/"))
	   (txt   (concat h "txt/"))
	   (wtxt  (concat h "wtxt/"))         ;; Work text files
	   (elisp (concat h "elisp/"))
	   )

      (defconst tinyhotlist-:abbreviate-file-name-table
	(list

	 ;;   Remember: the substitution order must be _BIGGEST_
	 ;;   substitution first.
	 ;;
	 ;;  Shorten ange ftp references

	 (list
	  "/ssjaaa@kielo.uta.fi:/home3/ss4/ssjaaa/"
	  "~UTA")

	 (list
	  (or (and (getenv "HOME")
		   ;;   The path names are seen as lowercase in Emacs in Win32
		   (if (win32-p)
		       (downcase (getenv "HOME"))
		     (getenv "HOME")))
	      (error "TinyHotlist: no HOME env variable"))
	  "~")

	 (list txt    "t")
	 (list wtxt   "wt")
	 (list elisp  "")
	 (list h      "")		;Substitute HOME with nothing
	 (list "\\.el" "")		;Substitute nothing
	 )))

    (defconst tinyhotlist-:default-regexp
      (concat
       "^RMAIL$\\|scratc\\|diff\\|buffer menu\\|vc-log\\|Messages"

       ;; Procmail
       "\\|procmailrc\\|pm-.*\\(hdr\\|log\\|rc\\|txt\\)"

       ;; text
       "\\|elisp.txt\\|ssjaaa.txt"

       ;; perl
       "\\|\\.pl"

       "\\|.mak"

       ;; emacs project files
       "\\|emacrs\\|funcs.ja.el\\|tinylibm.el\\|tinylib.el"

       ;; GNUS
       "\\|article\\|newsgroup\\|Summary\\|MIME-out"
       ))

    ;; ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ window-system ^ ^
    ))

;;}}}
;;{{{ tinybm

;;; ....................................................... &bookmarks ...
;;; Those lines that you see here with '&bookmark'

(when-package 'tinybm nil


  (when (emacs-p)
    (global-set-key [(?\e) (control mouse-1)]	     'tinybookmark-mouse)
    (global-set-key [(?\e) (control shift mouse-1)]  'tinybookmark-mouse-parse)
    )

  ;;  Drawing "bookmarks" -- straight lines and moving between them easily

  (defalias 'bm	'tinybookmark-insert)
  (autoload 'bm "tinybm" "" t)

  (autoload 'tinybookmark-repeat	"tinybm" "" t)
  (autoload 'tinybookmark-insert	"tinybm" "" t)
  (autoload 'tinybookmark-forward	"tinybm" "" t)
  (autoload 'tinybookmark-backward	"tinybm" "" t)

  (global-set-key "\C-zib"	        'tinybookmark-insert)
  (global-set-key [(shift left)]	'tinybookmark-backward)
  (global-set-key [(shift right)]	'tinybookmark-forward)


  (cond
   ((ti::xe-window-system)
    (autoload 'tinybookmark-mouse-parse   "tinybm" "" t))
   (t
    (autoload 'tinybookmark-keyboard	     "tinybm" "" t)
    (autoload 'tinybookmark-keyboard-parse "tinybm" "" t)))

  )


;;}}}
;;{{{ dired

;;; ........................................................... &dired ...
;;; Loading files at background and other dired enchancements.

(when-package 'tinydired nil

  (autoload 'tinydired-kill-all-ange-buffers		 "tinydired" "" t)
  (autoload 'tinydired-switch-to-some-ange-ftp-buffer	 "tinydired" "" t)
  (autoload 'tinydired-kill-all-ange-and-dired-buffers   "tinydired" "" t)
  (autoload 'tinydired-hook-control			 "tinydired" "" t)

  (add-hook 'tinydired-:load-hook 'tinydired-hook-control)
  (add-hook 'dired-mode-hook '(lambda () (require 'tinydired) nil))


  (defadvice tinydired-kill-files (around my act)
    "Let ange-ftp files show everything."
    (let* ((tinydired-:unwanted-files-regexp tinydired-:unwanted-files-regexp))
      (if (string-match "@" (or dired-directory ""))
	  (setq tinydired-:unwanted-files-regexp nil))
      ad-do-it))

  )

(defconst tinydired-:force-add-keys	'override)
(defconst tinydired-:enable-dos-support 'yes)

(defconst tinydired-:mput-sites
  '(
    "/ftp@ftp.mcp.com:/pub/mcp/author_resources/"

    "/jaalto@tierra.ddns.org:/d/pub/incoming"
    "/ftp@tierra.ddns.org:/pub/"
    "/ftp@tierra.ddns.org:/pub/incoming"

    "/jaalto@sun.cs.tpu.fi:~/"
    "/jaalto@sun.cs.tpu.fi:~/public_html"

    "/ssjaaa@cs.uta.fi:~ftp/pub/ssjaaa/"
    "/ssjaaa@kielo.uta.fi:~/elisp"

    "/jari@ada.eu.org:~/elisp/"

    ;; ftp://pause.kbx.de/tmp/J/JA/JARIAALTO
    ;; http://www.cpan.org/modules/by-authors/id/J/JA/JARIAALTO/
    ;; https://pause.kbx.de/pub/PAUSE/authors/id/J/JA/JARIAALTO/

    "/ftp@pause.kbx.de:/incoming/"

    "/jaalto@shell.sourceforge.net:"
    "/jaalto@newton.tpu.fi:"
    "/jaalto@terra.cs.tpu.fi:"
    "/ftp@pause.kbx.de:/tmp/J/JA/JARIAALTO"

    ))

;;}}}
;;{{{ igrep and TinyLisp, VC, cache
;;; ............................................................ &lisp ...


(when-package 'tinylisp nil

  (define-key my-:minor-mode-keymap "e" 'tinylisp-mode)

  (autoload 'tinylisp-mode		"tinylisp" "" t)
  (autoload 'turn-on-tinylisp-mode	"tinylisp" "" t)
  (add-hook 'lisp-mode-hook		'my-turn-on-tinylisp-mode)
  (add-hook 'emacs-lisp-mode-hook	'my-turn-on-tinylisp-mode)

  ;;  Not good, because emacs starts with buffer *scratch*,
  ;;  --> loads tinylisp.el immediately, I don't want that.
  ;;
  ;;  (add-hook 'lisp-interaction-mode-hook	'turn-on-tinylisp-mode)

  (add-hook 'tinylisp-:mode-hook 'my-tinylisp-mode-hook)


  ;;  I have mapped  ?Бе to produce $ in `function-key-map' in Finnish keyboard.
  ;;  The ?Бе  is useless in Emacs sessions and i want to hit easily
  ;;  $ due to shell and perl programming.

  (add-hook 'tinylisp-:install-menu-hook 'my-tinylisp-install-menu-hook)



  (defun my-tinylisp-install-menu-hook ()
    "Make swedish ae =>  201 to be accepted as self insert command for $."
    ;; ?е 2277  ?д 2276 ?ц 2294 (format "%c" 2277)
    ;;             228     246
    ;;
    (defvar tinylisp-:menu-main nil)
    (let ((list (nth 1 tinylisp-:menu-main)))
      (pushnew (cons ?Б (list '(insert tinylisp-:mode-prefix-key)))
	       list :test 'equal)
      (setnth 1 tinylisp-:menu-main list))

    (let* ((fmt (nth 0 tinylisp-:menu-main))
	   (kbd (nth 1 tinylisp-:menu-main))
	   )
      (unless (char-assq ?ц kbd)
	(push (cons ?ц   ;; finnish oe
		    (list '(call-interactively 'tinylisp-backward-user-option)))
	      kbd))
      (unless (char-assq ?д kbd)
	(push (cons ?д    ;; finnish ae
		    (list '(call-interactively 'tinylisp-forward-user-option)))
	      kbd))
      (defconst tinylisp-:menu-main (list fmt kbd))
      )

    )

  (if (featurep 'tinylisp)
      (my-tinylisp-install-menu-hook))  ;; Make sure this has been run

  )


(defun my-turn-on-tinylisp-mode ()
  (require 'tinylisp)			;Package may be compressed
  (turn-on-tinylisp-mode))


(defun my-tinylisp-mode-hook ()
  "TinyLisp setup."
  ;;  Add more faster keybinding.
  (let* ((p  tinylisp-:mode-prefix-key)
	 )

    ;;   Add fast keybinding "$mouse-1"  to jump and update call chain.
;;;    (ti::menu-add 'tinylisp-:menu-main ?+ nil 'delete)
;;;    (ti::menu-add
;;;     'tinylisp-:menu-main ?+
;;;     '((tinylisp-jump-to-definition '(4))))

    ))

;;; .............................................................. &vc ...
;;;  This is bit tricky autoload setup, but it is the only way.
;;;  Otherwise you would have to say (require 'tinyvc),
;;;  which is not nice at all

(defadvice vc-print-log (after tinyvc act)
  "Run hook `tinyvc-:vc-print-log-hook'."
  (require 'tinyvc)
  (run-hooks 'tinyvc-:vc-print-log-hook))

;;  See tinyad.el `vc-register' which can draw symlink to main
;;  repository. Define where it is in my $HOME.
;;  Win32 does not have symlinks, so bypass this

(unless (win32-p)
  (defconst tiad-:vc-main-rcs-dir
    (cond
     ((file-exists-p "~/rcs/RCS") "~/rcs/RCS")
     ((file-exists-p "~/RCS")     "~/RCS"))))

;;; ........................................................... &cache ...

(add-hook 'compilation-mode-hook '(lambda () (require 'tinycache)))
(add-hook 'dired-mode-hook       '(lambda () (require 'tinycache)))

;;; ........................................................... &igrep ...
;;;

(when-package 'tinyigrep nil

  (autoload 'tinyigrep-db-push-elt "tinyigrep" nil nil)
  (autoload 'tinyigrep-menu         "tinyigrep" "" t)

  (use-prefix-key global-map "\C-z")	;Make sure we can use C-z
  (global-set-key            "\C-zg" 'tinyigrep-menu)

  (defconst tinyigrep-:load-hook 'my-tinyigrep-load-hook)

  (defun my-tinyigrep-load-hook ()
    (require 'emacs-rc-tinyigrep))

  (add-hooks 'tinyigrep-:load-hook
	     '(
	       tinyigrep-install-default-key-bindings
	       tinyigrep-install-default-databases
	       ))

  )



(eval-and-compile
  (when-package 'tinydiff nil

    (define-key my-:minor-mode-keymap "d" 'tinydiff-mode)

    (autoload	     'tinydiff-diff-show    "tinydiff" "" t)
    (autoload        'tinydiff-patch        "tinydiff" "" t)
    (autoload        'tinydiff-mode         "tinydiff" "" t)
    (autoload        'turn-on-tinydiff-mode "tinydiff" "" t)
    (global-set-key  "\C-z\C-d"	           'tinydiff-diff-show)
    (global-set-key  "\C-z\C-p"	           'tinydiff-patch)
    ))


;;; ............................................... &circulate-buffers ...

(when-package 'tinybuffer nil   ;;  Change buffer easily
  (autoload 'tinybuffer-iswitch-to-buffer	"tinybuffer.el" nil t)
  (autoload 'tinybuffer-previous-buffer         "tinybuffer.el" nil t)
  (autoload 'tinybuffer-next-buffer             "tinybuffer.el" nil t)
  (autoload 'tinybuffer-:sort-mode              "tinybuffer.el" nil t)

  (global-set-key [(control ,)]  'tinybuffer-previous-buffer)
  (global-set-key [(control <)]  'tinybuffer-iswitch-to-buffer)
  (global-set-key [(control ?.)] 'tinybuffer-next-buffer)
  (global-set-key [(alt ?.)]     'tinybuffer-:sort-mode)
  )

(defconst tinybuffer-:ignore-regexp
  (concat
   "^ "                 ;hidden buffers
   "\\|completion\\|summary"
   "\\|buffer list\\|help\\|ispell\\|abbrev"
   "\\|temp\\|tmp\\|vc\\|compile-log\\|occur"
   )
  "*Buffers to ignore when changing to another.")


;;}}}

;;{{{ TinyPgp TinyMail, TinyGnus

;;; ............................................................ &gnus ...

(when-package 'tinygnus nil

  (defconst tinygnus-:show-dormants t)
  (add-hook 'gnus-startup-hook 'my-tinygnus-startup)

  (defun my-tinygnus-startup ()
    (require 'emacs-rc-ding)   ;; my personal Ding setup
    (require 'tinygnus))
  )

;; ........................................................... &mail ...

(when-package 'tinymail nil
  (autoload 'tinymail-mail		"tinymail" "" t)
  (autoload 'sort-fields		"sort"     "" t)

  ;;	This package displays last arrived mail in the Frame!
  ;;	and the count of pending mails unread in mailbox. Beeps
  ;;	when mail arrives.
  ;;
  ;;	"foo@bix.com 6"

  (when (fboundp 'display-time)
    (let* (process-connection-type)	;Nicer process communication pipe/pty
      (display-time)))			;time.el

  (add-hook 'mail-setup-hook	'my-tinymail-load)
  (add-hook 'message-mode-hook	'my-tinymail-load)

  (defun my-tinymail-load ()

    ;;  If there is setup, try loading it ONLY ONCE.
    ;;  We laod the tinymail setup before, because it contains important
    ;;  defvar settings that must be seen before Tinymail tries to figure
    ;;  out the defaults. 8and generate an error if it can't set default)

    (when (and (not (featurep 'emacs-rc-tinymail))
	       (null (get 'my-tinymail-load 'emacs-rc-tinymail)))
      (when-package 'emacs-rc-tinymail nil
	(load "emacs-rc-tinymail"))
      (put 'my-tinymail-load 'emacs-rc-tinymail t) ;; We tried to load it.
      )

    (unless (featurep 'tinymail)
      (require 'tinymail) ))

  )

;;; ........................................................... &rmail ...

(when-package 'tinyrmail nil
  (autoload 'tinyrmail-install		      "tinyrmail" "" t)
  (autoload 'tinyrmail-rmail-summary-by-labels-and  "tinyrmail" "" t)

  (add-hook 'rmail-mode-hook 'my-tinyrmail-install)

  ;;  converts finnish 8-bit chars to 7bit character; this my
  ;;  private fucntion.

  (add-hook 'tinyrmail-get-new-mail-hook		'skandix)
  )

(defconst tinyrmail-:delete-regexp    "make.*money\\|this is your chance.*")
(defconst tinyrmail-:ube-message-file "~/txt/spam2.txt")

(defconst tinyrmail-:ube-ignore-site-regexp
  (concat
   "netforward\\|nokia\\|uta\\."
   "\\|204.57.67.27\\|131.228"
     ))

(defun my-tinyrmail-install ()
  (unless (featurep 'tinyrmail)
    (require 'tinyrmail)
    (tinyrmail-install)
    ))


(defconst tinyrmail-:label-table
  '((ti::mt-pgp-p   "pgp")
    (ti::mt-mime-p  "mime")
    (my-tinyrmail-label  "")
    ))

(defun my-tinyrmail-label  ()
  (let* (str
	 )
    (cond
     ((re-search-check
       (concat
	"def\\(var\\|const\\|fun\\|subst\\|macro\\)"
	"\\|setq\\|[alt z:]-[a-z:]-[a-z:]"
	"\\|emacs"
	))
      "ema")

     ((re-search-check "To: procmail Mailing List")
      (setq str "pm"))

     ((re-search-check "From:.*x-pgp@.*jena")
      (setq str "x-pgp"))

     ((re-search-check "tipgp-\\|tinypgp")
      (setq str "tinypgp"))
     )
    str
    ))


;; ............................................................ &pgp ...

(when nil ;; 1999-12 disabled; broken
(when-package 'tinypgp nil

  (define-key my-:minor-mode-keymap "-" 'tinypgp-mode)

  (eval-and-compile
    (autoload 'tinypgp-mode	    "tinypgp" "" t)
    (autoload 'turn-on-tinypgp-mode "tinypgp" "" t)
    (autoload 'tinypgp-install	    "tinypgp" "" t)
    )

  (add-hook 'message-mode-hook 'my-tinypgp-load)
  (add-hook 'mail-mode-hook    'my-tinypgp-load)
  (add-hook 'rmail-mode-hook   'my-tinypgp-load)
  (add-hook 'gnus-startup-hook 'my-tinypgp-install)

  (defun my-tinypgp-load ()
    (require 'tinypgp))


  (defun my-tinypgp-install ()
    (require 'tinypgp)
    (tinypgp-install))


  (add-hook 'tinypgp-:load-hook
	    '(lambda () (require 'rc-tinypgp "emacs-rc-tinypgp")))
  )
)

;;}}}
;;{{{ extra:


;;; ............................................................. &nbr ...

(when-package 'tinynbr nil
  (define-key my-:minor-mode-keymap "n" 'tinynbr-mode)
  (autoload 'tinybr-mode "tinynbr" "" t))


;;; .......................................................... &search ...

(when-package 'tinysearch nil
  (defconst tinysearch-:word-boundary-set "-:/_A-Za-z0-9")

  (autoload 'tinysearch-search-word-main	"tinysearch" "" t)
  (autoload 'tinysearch-search-word-forward	"tinysearch" "" t)
  (autoload 'tinysearch-search-word-backward	"tinysearch" "" t)


  (global-set-key [(alt control mouse-1)]
		  'tinysearch-search-word-forward)

  (global-set-key [(control meta mouse-1)]
		  'tinysearch-search-word-forward)

  (global-set-key [(alt control shift mouse-1)]
		  'tinysearch-search-word-backward)

  (global-set-key [(control meta shift mouse-1)]
		  'tinysearch-search-word-backward)

  (global-set-key [(f2)]
		  'tinysearch-search-word-forward)

  (global-set-key [(shift f2)]
		  'tinysearch-search-word-backward)

  )


;;; ......................................................... &replace ...

(when-package 'tinyreplace nil

  ;; 99% of the time I use only this relace tool and 0.8% of the time
  ;; I use Emacs M-%. The rest I do by hand.

  (autoload 'tinyreplace-replace-forward	"tinyreplace" "" t)
  (autoload 'tinyreplace-replace-region	        "tinyreplace" "" t)
  (autoload 'tinyreplace-word-replace		"tinyreplace" "" t)
  (autoload 'tinyreplace-word			"tinyreplace" "" t)
  (autoload 'tinyreplace-replace-over-files     "tinyreplace" "" t)
  (autoload 'tinyreplace-replace-over-files-compile-buffer "tinyreplace" "" t)
  (autoload 'tinyreplace-define-keys-compile-map "tinyreplace" "" t)

  (add-hook 'compilation-mode-hook 'tinyreplace-define-keys-compile-map)

  (defconst tinyreplace-:symmetry-rest 	'follow)
  (defconst tinyreplace-:arrow-initial-state	'hide)
  (defconst tinyreplace-:symmetry		nil)
  )


;;; .................................................. &c++-code-help ...

(when-package 'tinytag nil

  (global-set-key "\C-z."       'tinytag-main)

  (autoload 'tinytag-main		"tinytag" "" t)
  (autoload 'tinytag-main-mouse		"tinytag" "" t)
  (autoload 'tinytag-install		"tinytag" "" t)


  (defconst tinytag-try-hook
    '(
      tinytag-try-function-basic
      tinytag-try-function-db
      tinytag-try-function-man
      my-tinytag-try
      ))

  ;;  The macs-config-tinytag-jdk1.2.2 file must be located
  ;;  under tinytag-:database-dir


  (defconst my-tinytag-:db-map-java
    (list
     (list 'func       "emacs-config-tinytag-jdk1.2.2")
     )
    "Java database.")

  (defconst my-tinytag-:db-re-java
    '(("."        (func)))
    "Java database.")


  (defconst my-tinytag-:db-map-c++
    '(
      (func       "emacs-config-tinytag-c++-functions")
      (struct     "emacs-config-tinytag-c++-structs")
      (types      "emacs-config-tinytag-c++-types")
      )
    "C++ database.")

  (defconst my-tinytag-:db-re-c++
    '(
      ("_t"       (types structs))
      ("_s"       (struct types))
      ("."        (func))
      )
    "C++ database.")


  (defconst tinytag-:database-setup-table
    (list

     (list
      "code-java\\|java"
      '(my-tinytag-:db-map-java
	my-tinytag-:db-re-java
	))

     (list
      (concat
       "c-mode\\|cc-mode\\|c[+]+-mode"
       "\\|code-c\\|code-c[+]+"           ;See tinylibid.el
       )
      '(my-tinytag-:db-map-c++
	tmy-inytag-:db-re-c++
	))
     ))

  (defconst tinytag-:filter-word-table
    (list

     (list
      "code-java\\|java"
      '(or (< (length string) 4))
      ))

    (list
     (concat
      "c-mode\\|cc-mode\\|c[+]+-mode"
      "\\|code-c\\|code-c[+]+"           ;See tinylibid.el
      )
     '(or (< (length string) 4)     ;too short word ?
	  (string-match tinytag-:filter-default-c++-words string))
     ))

  (defun my-tinytag-require ()
    (require 'tinytag))

  (add-hook 'jde-mode-hook  'tinytag-install)
  (add-hook 'java-mode-hook 'tinytag-install)

  (add-hook 'c++-mode-hook 'tinytag-install)

  (when (boundp 'c++-mode-hook)
    (add-hook 'c++-mode-hook 'tinytag-install))

  (when (boundp 'cc-mode-hook)
    (add-hook 'cc-mode-hook 'tinytag-install))

  )

;;; (require 'tinychist)  ;; really, I don't use this nowadays.

;; ......................................................... &scroll ...

(when-package 'tinyscroll nil    ;;  Automatic scrolling

  (autoload 'tinyscroll-control                  "tinyscroll" "" t)
  (autoload 'tinyscroll-list                     "tinyscroll" "" t)
  (autoload 'tinyscroll-timer-process-control    "tinyscroll" "" t)

  (add-hook 'compilation-mode-hook '(lambda () (require  'tinyscroll) nil))
  )

;; ........................................................ &printer ...
;;  Different Printer and print style management

(when-package 'tinylpr nil
  (autoload 'tinylpr-set-command                     "tinylpr.el" nil )
  (autoload 'tinylpr-setting-status                  "tinylpr.el" nil t)
  (autoload 'tinylpr-queue                           "tinylpr.el" nil t)
  (autoload 'tinylpr-select-printer                  "tinylpr.el" nil t)
  (autoload 'tinylpr-print-style-select              "tinylpr.el" nil t)

  (add-hook 'tinylpr-:load-hook 'my-tinylpr-load-hook)
  )


(defun my-tinylpr-load-hook ()
  "My tinyLpr settings."

  (defvar tinylpr-:printer-list nil)  ;; Byte compiler silencer

  ;; (pushnew "01a2" tinylpr-:printer-list  :test 'string=)

  (cond
   ((win32-p)
    (defconst dos-ps-printer "PRN")
    (defconst tinylpr-:queue-cmd nil)
    (defconst tinylpr-:printer-list
      (list
       (getenv "PRINTER")
       ))
    (defconst tinylpr-:print-style-list
      (let* ((p   "c:/gs/gs386")
	     )
	(list
	 (list
	"ps"
	(concat
	 p
	 "-q -dNOPAUSE -sDEVICE=ljet4"
	 "-r600 -sPAPERSIZE=a4 -sOutputFile=LPT1"
	 "-Ic:/gs -"
	 )))
	)))
   ((eq 'x (ti::xe-window-system))

    (aput
     'tinylpr-:print-style-list
      "groff print"
      '(concat "hg.pls -g XXX | groff -ms | mpage -A -4 -P#"))

    (aput
     'tinylpr-:print-style-list
     "lisp code printing"
     "lispp.sh")

    (aput
     'tinylpr-:print-style-list
     "C/C++ code printing"
     "c-print.sh")

    (aput
     'tinylpr-:print-style-list
     "Pretty print."
     "lpp.pls")

    )))


;; ........................................................ &pairing ...

(when-package 'tinypair nil ;;  Pairing of character

  (eval-and-compile
    (autoload 'turn-on-tinypair-mode "tinypair"))

  (global-set-key              ;; Make pressing " to autoload the package
   "\"" (definteractive
	  (turn-on-tinypair-mode)
	  (insert (char-to-string ?\"))
	  (global-set-key "\"" 'self-insert-command)))

  )


;; ----------------------------------------------------------------------
;; - This is good idea, but what if I'm writing comment and I want
;;   pairing...this keeps me getting it permanently
;;
;; (setq tipi-:pair-allow-check-function 'my-pair-allow-check-function)
;;  my-buffer-pair-info

(defun my-pair-allow-check-function ()
  "Checks if pairing is allowed by looking buffer contents."
  (if (not (boundp 'my-buffer-pair-info))
      (progn
	(defvar my-buffer-pair-info nil)
	(make-local-variable 'my-buffer-pair-info)
	(setq my-buffer-pair-info
	      (not (memq (ti::id-info 'sym) '(perl-mode lisp-mode)))))
    my-buffer-pair-info
    ))

;; ................................................. x-register-jump ...

(when-package 'tinyxreg nil   ;; save point via X-popup
  (autoload 'tinyxreg-jump-to-register		"tinyxreg"	"" t)
  (autoload 'tinyxreg-jump-to-register-mouse	"tinyxreg"	"" t)
  (autoload 'tinyxreg-point-to-register		"tinyxreg"	"" t)
  (autoload 'tinyxreg-point-to-register-mouse	"tinyxreg"	"" t)
  (autoload 'tinyxreg-remove-reg		"tinyxreg"	"" t)
  )


;; ........................................................ &desktop ...

(when-package 'tinydesk nil ;;  constant Desktop session saver

  (add-hook 'tinydesk-:key-hook 'my-tinydesk-keys)
  (autoload 'tinydesk-mode			"tinydesk"	"" t)
  (autoload 'tinydesk-save-state		"tinydesk"	"" t)
  (autoload 'tinydesk-recover-state		"tinydesk"	"" t)
  (autoload 'tinydesk-edit-state-file		"tinydesk"	"" t)

  (autoload 'my-buffer-rename-all               "funcs.ja" "" t)
  (add-hook 'tinydesk-:recover-after-hook 'my-buffer-rename-all)

  (defconst  tinydesk-edit-hook	          'my-tinydesk-edit-hook)

  )



(defconst tinydesk-:face-table
  '(
    (file-pick .  highlight)
    (error     .  bold)
    )
  "*Faces used for marking text that activated under mouse.")


(defconst tinydesk-:save-exclude-regexp
  (concat
   "RMAIL\\|\\(ftp\\|anonymous\\)@.*/"

   ;;  Temporary files: /users/jaalto/T.xx

   "\\|/[tT][0-9]*$"
   "\\|/[tT][0-9]*\\.[tT0-9]$"
   "\\|state\\."

   ;;  No files from these directories

   "\\|^/tmp/\\|/junk/\\|/trash/\\|/[aA]utosaved?/"

   ;; Forget emacs Info pages and Emacs distribution files
   ;; I may have viewed.

   "\\|/info/"
   ))


(defun my-tinydesk-keys ()
  "my tinydesk keys"

  (defvar tinydesk-mode-map nil)  ;; Byte Compiler silencer

  (define-key tinydesk-mode-map  [(mouse-3)]
    'tinydesk-mouse-load-file)

  (define-key tinydesk-mode-map  [(shift mouse-3)]
    'tinydesk-clear-buffer-properties)

  (define-key tinydesk-mode-map  [(control mouse-3-down)]
    'ignore)

  (define-key tinydesk-mode-map  [(control mouse-3)]
    'tinydesk-mark-buffer-loadable)

  (define-key tinydesk-mode-map  [(control meta mouse-3)]
    'tinydesk-set-face-non-files-buffer)

  )


(defun my-tinydesk-edit-hook ()
  "When editing."
  ;; #todo:
  )

;; .................................................... &text-append ...


(when-package 'tinyappend nil   ;;  gathering snippets around emacs.
  (autoload 'tinyappend-beg		"tinyappend" "" t)
  (autoload 'tinyappend-end		"tinyappend" "" t)
  (autoload 'tinyappend-kill		"tinyappend" "" t)
  (autoload 'tinyappend-yank		"tinyappend" "" t)
  (global-set-key  "\C-c="		'tinyappend-end)
  (global-set-key  "\C-c-"		'tinyappend-beg)
  (global-set-key  "\C-c_"		'tinyappend-kill)
  (global-set-key  "\C-c|"		'tinyappend-yank)
  )

;; ........................................................... &diff ...

(when-package 'tinydiff nil   ;;  Diff and Patch commands.
  (autoload 'tinydiff-mode		"tinydiff" "" t)
  (autoload 'tinydiff-diff		"tinydiff" "" t)
  (autoload 'tinydiff-diff-show		"tinydiff" "" t)
  (autoload 'tinydiff-diff-show-noask	"tinydiff" "" t)
  (autoload 'tinydiff-patch		"tinydiff" "" t)
  ;;  (add-hook 'tinydiff-:diff-hook 'my-tinydiff-diff-hook)
  )

(defconst tinydiff-:extra-diff-program 'my-extra-diff-function)

(defconst tinydiff-:diff-command
  (let ((gdiff (executable-find "gdiff"))
	(diff  (executable-find "diff"))
	)
    (cond
     (gdiff
      gdiff)
     (diff
      diff)
     (t
      (message  "My: Can't find DIFF program for tinydiff-:diff-program")
      ))))


(defconst tinydiff-:patch-list            ;; Where to loop for files to patch
  '(( "[.]el$"		load-path)
    ( "\\.[hc]c?$"      '("~/work/wmpman"))

    ( "\\.pl"           '("~/bin/perl/my"
			  "~/elisp/tiny/bin"
			  ))

    ( "."		'("~/txt"
			  "~/work/pm/pm/doc"
			  "~/work/pm/pm/lib"
			  "~/bin"
			  "~/bin/perl"
			  "~/elisp"
			  "~/elisp/rc"
			  "~/txt/procmail"
			  "~/elisp"
			  ))
    ))


;;  #todo: 1999-09 I'm not sure if this works as I intended. Check some time.
;;
;;  This is special to my .cc and .h files, it deletes the
;;  Rlog section block out from the patch, so that the log it not
;;  patched. <you really aren't interested in this>

(defun my-tinydiff-goto-cc-comment-end ()
  (let* ((re-com "^[!+]? +\\* \\*")
	 )
    (while (cond
	    ((looking-at re-com))
	    (t
	     ;; Another ************ block, see what follows after
	     ;; it
	     ;;
	     (or (save-excursion
		   (forward-line 2)
		   (looking-at re-com))
		 (save-excursion
		   (forward-line 3)
		   (looking-at re-com))
		 ))
	    )
      (forward-line 1)
      (point)
      )
    ))


(defun my-tinydiff-diff-hook  ()
  "Do some cleanup"
  (interactive)
  (let* ((re-com "^[ \t]+\\*[ \t]+\\*")
	 buffer-read-only
	 point
	 )
    (defvar tinydiff-:diff-buffer nil)
    (cond
     ((and (re-search-check "RCS file: RCS/\\(wmp\\|pmg\\)")
	   (re-search-check re-com)
	   (y-or-n-p "Filter RCSLOG comment out from diff? ")
	   )
      ;;  Kill Prereq tag first, it prevent's applying the patch

      (ti::b-kill-line)

      (PMIN)

      ;;  Kill first diff block

      (re-search-forward (regexp-quote "***************"))
      (beginning-of-line)
      (setq point (point))
      (forward-line 3)
      (my-tinydiff-goto-cc-comment-end)
      (delete-region point (point))
      )
     )))


(defun my-tinydiff-options  ()
  "Return options based on file."
  (interactive)
  (let* ((ret	"-u")			;the default
	 (bf  (buffer-file-name))
	 )
    (when (and bf (vc-registered bf)) 	;It's RCS
      ;;  Ignore comments that are placed to the top of C++ files
      ;;  * *
      ;;  * *    Revision 3.5.1.3  1997/01/27  09:27:23  jaalto
      (setq ret
	    (concat
	     ;; "-kk "
;;; Too bad, rcsdiff doesn't know this GNU diff option
;;;		 "--ignore-matching-lines='^ +[*] [*] ' "
	     "-u"
	     )))
    ret
    ))

(defconst tinydiff-:diff-option	       '(progn (my-tinydiff-options)))


;; ............................................................. &my ...

(when-package 'tinymy nil		;;  Conatains many utilities

  (defconst tinymy-:load-hook
    '(tinymy-install
      tinymy-define-keys
      tinymy-alias
      my-tinymy-load-hook
      ))

  (if (ti::xe-window-system)
      (global-set-key "\C-\\"  'tinymy-backslash-fix-paragraph))

  (autoload 'tinymy-sort-mode "tinymy" "" t)

  (define-key my-:minor-mode-keymap "S"	'tinymy-sort-mode)

  (defconst tinymy-:define-key-force t)

  (defconst tinymy-:stamp-lcd-regexp
    (concat "[|,]" user-mail-address "[|]"
	    "\\|\\(jaalto\\|jari.aalto\\|ssjaaa\\)@"
	    ))

  (if (fboundp 'my-user-mail-address)
      (defconst tinymy-:stamp-user-mail-address  'my-user-mail-address))


  (autoload 'tinymy-read-only "tinymy")

  ;;  Make these keys to autolaod the package.

  (global-set-key "\C-x\C-q" 'tinymy-read-only)

  (global-set-key "%"
		  (definteractive
		    (global-unset-key "%") ;; ..that tinymy.el doesn't complain
		    (require 'tinymy)
		    (funcall (lookup-key global-map "%" ))))

  (unless (featurep 'tinymy)
    (global-set-key "\C-zcc"
		    (definteractive
		      (global-unset-key "\C-zcc")
		      (require 'tinymy)
		      (let ((func (lookup-key global-map "\C-zcc" )))
			(if func
			    (funcall func)
			  (load "tinymy")
			  (error "Auto key define didn't work. Try calling again."))
			))))

  )


(defun my-tinymy-load-hook ()
  "Custom Tinymy settings after load."
  (when (executable-find "emacs-compile.pl")
    (aput 'tinymy-:compile-table
	  "lisp"
	  "perl -S emacs-compile.pl -v %s"))

  (when (executable-find "t2h.pl")
    (aput 'tinymy-:compile-table
	  "lisp"
	  "perl -S t2h.pl %s"))
  )


;;}}}


(provide 'emacs-rc-tiny)

;;; emacs-rc-tiny.el ends here
