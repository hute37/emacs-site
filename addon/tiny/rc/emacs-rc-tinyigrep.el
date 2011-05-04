;; @(#) emacs-rc-tinyigrep.el -- Emacs tinyigrep.el customizations
;; @(#) $Id: emacs-rc-tinyigrep.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{ Documentation
;;; Documentation:
;;
;;  File id
;;
;;      .Copyright (C) 1996-2000 Jari Aalto
;;      .Author:       Jari Aalto <jari.aalto@poboxes.com>
;;      .Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;;      .Created:      1996
;;      .keywords:     tools
;;
;;      This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that.. Just rename this file to whatever is shown
;;      in the first line.
;;
;;  Installation
;;
;;      Put following statement in your .emacs
;;
;;          (add-hook 'tigr-load-hook         ;; See `tinyigrep.el'
;;		'(lambda () (require 'rc-tigr ".emacs.tigr")))
;;
;;  Description
;;
;;      ** MODIFY FILE BEFORE USING. THIS IS my personal SETUP **
;;
;;      This file is offered as an example how to define *TinyIgrep*
;;	search criterias.
;;
;;	There is package called *igrep.el* by Kevin Rodgers, and when I saw
;;	it, I made a small top level interface to *igrep.el*. The package
;;	was called *tinyigrep.el*. If you grep certain directories or files
;;	very often; you define "igrep database" and this rc file defines
;;	the completed dtabases that stear *igrep.el*.
;;
;;  Code management
;;
;;	o   folding.el is used to keep code in {{{ }}} sections
;;	o   tinybm.el is used to jump to "..... &tag ..." bookmarks
;;	o   tinytab.el minor mode used for writing documentation
;;

;;}}}

;;; Change Log:
;;; Code:

(require 'tinyigrep)

;;{{{ variables: defining Man paths and so on

(defconst agrep  (or (executable-find "agrep")
		     "egrep")
  "Approximate grep. If not found, uses standard egrep.")


;;}}}
;;{{{ preparsed

;;; .................................................. &preparse-paths ...

;; - Find dynamically where the gnus and Emacs installation
;;   directories are this time (I'm running several Emacs versions on
;;   variety operating systems)

;; - Actually these variables are not only path components; but hold also
;;   filename. The filename will be removed later.
;;
;; - Notice: Finding root dir works only in Emacs that keeps all the
;;   distribution files in one directory

(defvar my-:tinyigrep-emacs (locate-library "cl.el"))

(defconst igrep-expression-option "-e")

;;  In XEmacs, we have to find the root lisp tree differently from the
;;  Emacs case above.

(defconst my-:tinyigrep-xemacs
  (let* ((ver  (emacs-version-number-as-string)) ;eg "19.14"
	 match
	 ret
	 )
    (dolist (path load-path)

      ;;  When we find the version from the path, ve know the root
      ;;  directory
      ;;
      ;;  /opt/local/lib/xemacs-19.14/lisp/vms -->
      ;;  /opt/local/lib/xemacs-19.14/lisp/

      (when (and path
		 (setq match (strmatget (concat ".*" ver) 0 path)))
	(setq ret (concat match "/lisp"))
	(return)))
    ret
    )
  "The XEmacs lisp root tree.")


;;}}}

;;; ........................................................ &database ...

;;{{{ my lisp

(defun my-tinyigrep-init ()


  ;;	Define shorter names. The default database names are prefixed with
  ;;	lisp- These don't need recursice search.

  (dolist (package '("vm" "irchat" "semi-def" "mc" "tinylib" "bbdb"))
    (tinyigrep-db-push-elt-lisp-package package (concat package ".el")))

  ;;	Recursively seached

  (tinyigrep-db-push-elt-lisp-package "gnus" "gnus.el" "egrep" '(nil))

  (message "My: wait, initialising TinyIgrep database....") ;May take awhile

  ;;	Packages that are my handwriting

  (let* ((file1  (locate-library "tinylib.el"))
	 (dir1   (file-name-directory (or file1 "")))
	 (file2  (locate-library "funcs.ja.el"))
	 (dir2   (file-name-directory (or file2 "")))
	 (file3  (locate-library "emacs-rc-set.el"))
	 (dir3   (file-name-directory (or file3 "")))
	 )
    (when (or dir1 dir2)
      (tinyigrep-db-push-elt
       (list "Lisp-my"
	     (list "egrep"
		   (delq nil
			 (list (if dir1 (concat dir1 "*el"))
			       (if dir2 (concat dir2 "*el"))
			       (if dir3 (concat dir3 "*el"))
			       ))))))

    )

   ;;	Include packages I have downloaded but maybe not yet
   ;;	installed or which are under testing

  (when (file-directory-p "~/ftp/elisp")
    (tinyigrep-db-push-elt
     '("lisp-downloaded" ("egrep" ("~/ftp/elisp/*.el")))))

  (dolist (elt '(("txt-rfc"  "~/txt/rfc/" "*")
		 ("lisp-ftp" "~/ftp/elisp/" "*.el" )
		 ))
    (when (file-directory-p (nth 1 elt))
      (tinyigrep-db-push-elt
       (list
	(car elt)
	(list
	 "egrep"
	 (list (concat  (nth 1 elt) (nth 2 elt)))
	 )))))

;;}}}
;;{{{ misc

   ;; .......................................................... &gnus ...

   ;;  grep GNUS files.
   ;;  Find where gnus.el is and return list of two paths
   ;;  PATH/gn*el  and PATH/nn*el

  (tinyigrep-db-push-elt
   (tinyigrep-db-lisp-elt
    (if (file-directory-p "~/elisp/gnus/")
	"~/elisp/gnus/"			;privatedly installed newest copy
      "gnus.el")
    "Gnus"
    "egrep"
    '(list (concat dir "*el"))
    '(nil)
    ))



;;}}}
;;{{{ XEmacs

   ;; ........................................................ &XEmacs ...

   ;; - XEmacs lisp files may be in compressed format: zgrep
   ;; - The following returns something only if I'm currently
   ;;   running XEmacs; which defines load-path to find images.el

   (tinyigrep-db-push-elt
    (tinyigrep-db-lisp-elt
     "images.el"
     "xe-now"
     "zgrep"
     ;; The root directory is one directory up, delete one directory level
     '(list (concat (replace-regexps-in-string "/[^/]+/$" "" dir) "/*el"))
     ))

   ;;  But here I may be in Emacs; but I still want to grep XEmacs
   ;;  tree. We have to use absolute path.

   (when my-:tinyigrep-xemacs

     (tinyigrep-db-push-elt
      (tinyigrep-db-lisp-elt
       my-:tinyigrep-xemacs
       "xe-my-defined"
       "zgrep"
       '(list (concat dir "*el"))
       ))

     (tinyigrep-db-push-elt
      (tinyigrep-db-lisp-elt
       my-:tinyigrep-xemacs
       "xemacs-now"
       "zgrep"
       '(list (concat dir "*el"))
       ))

     )


   (when (and (emacs-p)
	      (win32-p))
     (let* ((ver    (emacs-install-root))
	    (xemacs (concat
		     (ti::f-dir-up (ti::f-dir-up ver))
		     "XEmacs/"))
	    )
       (when (file-directory-p xemacs)
	 (tinyigrep-db-push-elt
	  (list
	   "xemacs-all"
	   (list
	    "egrep"
	    (list
	     (concat xemacs "*el")
	     )
	    '(nil)
	    ))))))




;;}}}
;;{{{ WORK: C/C++ code and others

   ;; ........................................................... &C++ ...


   ;;  my current projects

   (tinyigrep-db-push-elt '("wmp"   ("egrep" ("~/work/wmpman/*[ch]"))))
   (tinyigrep-db-push-elt '("wmp9"  ("egrep" ("~/work/wmpman/t9/*[ch]"))))
   (tinyigrep-db-push-elt '("wmp10" ("egrep" ("~/work/wmpman/t10/*[ch]"))))
   (tinyigrep-db-push-elt '("pmg"   ("egrep" ("~/work/pmglib/*[ch]"))))


   (tinyigrep-db-push-elt
    '("c-proj" ("egrep" ("~/work/wmpman/*[ch]"
			 "~/work/pmglib/*[ch]"
			 ))))

   (tinyigrep-db-push-elt
    (list
     "c-proj-libs"
     (list
      "egrep"
      (let ((p (or (getenv "PROJECT") "")))
	(list
	 (concat p "/common/*")
	 ))
      '(nil)				;use -exec method
      )))


   (tinyigrep-db-push-elt
    (list
     "c-proj-include"
     (list
      "egrep"
      (let ((p (or (getenv "PROJECT") "")))
	(list
	 (concat p "/include/*")
	 ))
      '(nil)
      )))

   (tinyigrep-db-push-elt
    (list
     "c-proj-werlog"
     (list
      "egrep"
      (let ((p (or (getenv "PROJECT") "")))
	(list
	 (concat p "/include/wer*")
	 ))
      '(nil)
      )))


   (tinyigrep-db-push-elt
    (list
     "c-proj-prg"
     (list
      "egrep"
      (let ((p (or (getenv "PROJECT") "")))
	(list
	 (concat p "/image/*")
	 ))
      '(nil)				;use -exec method
      )))


   (tinyigrep-db-push-elt
    (list
     "work-lists"
     (list
      agrep
      (list
       "/users/root/mail/lists/*"
       "/users/distrmgr/vaxlists/*"
       )
      '(nil)				;use -exec method
      )))

;;}}}
;;{{{ C++ libs

   ;; ...................................................... &lib-proj ...
   ;; all the include directories are listed in my private
   ;; environment variable _INCDIRS

   (tinyigrep-db-push-elt
    (list
     "c++include"
     (list
      "egrep"
      (mapcar
       (function
	(lambda (x)
	  (concat
	   ;;  Make sure it has ending slash
	   (ti::f-make-path (replace-regexps-in-string "-I" "" x))
	   "*"
	   )))
       ;;  split my private environment varible that has all the
       ;;  possible include directoes in format: -I/dir1 -Idir2
       ;;
       (split-string2 (or (getenv "_INCDIRS") "") " *-I")
       )
      '(nil)
      )))


;;}}}
;;{{{ my text


   ;; .................................................... Server docs ...

   (tinyigrep-db-push-elt
    '("Xitami-root"
      ("egrep"
       (
	"d:/bin/wbin/server/xitami/*cfg"
	"d:/bin/wbin/server/xitami/*txt"
	"d:/bin/wbin/server/xitami/*aut"
	)
       )))

;;;   (tinyigrep-db-push-elt
;;;    '("Xitami-root" ("egrep" ("d:/bin/wbin/server/xitami/*") )))


   ;; .......................................................... &text ...
   ;; My mail spool

   (tinyigrep-db-push-elt '("Mail"	("egrep" ("~/Mail/*") (nil))))
   (tinyigrep-db-push-elt '("mail-spool" ("egrep" ("~/Mail/spool/*") (nil))))
   (tinyigrep-db-push-elt '("mail-tpu"	("egrep" ("~/Mail/tpu/*") (nil))))
   (tinyigrep-db-push-elt '("mail-junk"	("egrep" ("~/Mail/junk/*") (nil))))
   (tinyigrep-db-push-elt '("mail-uta"	("egrep" ("~/Mail/uta/*") (nil))))
   (tinyigrep-db-push-elt '("mail-list"	("egrep" ("~/Mail/list/*") (nil))))
   (tinyigrep-db-push-elt '("Incoming"	("egrep" ("~/Mail/Inc*"))))
   (tinyigrep-db-push-elt '("txt"	("zgrep" ("~/txt/*"))))
   (tinyigrep-db-push-elt '("wtxt"	("zgrep" ("~/wtxt/*"))))

   (tinyigrep-db-push-elt
    '("vise" ("egrep" ("~/wtxt/*vise*"
		       "~/wtxt/response/*"
		       "~/vax/*vise*"
		       "~/work/cn/*"
		       ))))

   (tinyigrep-db-push-elt
    '("txt-uta" ("egrep" ("~/work/uta/*.txt") '(nil) )))

   (tinyigrep-db-push-elt
    '("txt-tiny" ("egrep" ("~/elisp/tiny/doc/*")  )))


   (tinyigrep-db-push-elt '("txt-all" ("zgrep" ("~/txt/*" "~/wtxt/*" ))))

   ;;	Procmail files
   ;;	http://www.xray.mpe.mpg.de/mailing-lists/procmail/

   (tinyigrep-db-push-elt '("pm-all" ("zgrep" ("~/txt/pm/arc/*" "~/txt/pm-*"))))

   (tinyigrep-db-push-elt
    '("pm-my" ("egrep" ("~/.procm*" "~/work/pm/procmail/*") '(nil) )))

   ;;	My procmail modules

   (tinyigrep-db-push-elt '("pm-lib" ("egrep" ("~/work/pm/procmail/lib/*rc"))))

   ;; Procmail archive files

   (tinyigrep-db-push-elt '("pm-arc" ("egrep" ("~/txt/pm/arc/*"))))

   ;; .......................................................... &news ...
   ;;  Many times I remember that that particulr piece of information
   ;;  was mentioned by someone...voila! igrep helps me!

   (tinyigrep-db-push-elt '("news-all"   ("egrep" ("~/News/*"))))
   (tinyigrep-db-push-elt '("news-emacs" ("egrep" ("~/News/*ema*"))))

;;}}}

   (message "My: wait, initialising igrep database....done.")


   ;; ........................................................... perl ...

   (tinyigrep-db-push-elt '("my-perl"	("egrep" ("~/bin/perl/") (nil) )))
   (tinyigrep-db-push-elt '("my-bin"	("egrep" ("~/bin/") (nil) )))


   )

(my-tinyigrep-init)

(provide 'emacs-rc-tinyigrep)

;;; emacs-rc-tinyigrep.el ends here
