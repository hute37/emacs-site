;; @(#) .emacs.path.el -- Emacs rc file for path setup
;; @(#) $Id: emacs-rc-path.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{ Documentation
;;; Documentation:
;;
;;  File id
;;
;;	Copyright (C)	1995-2001 Jari Aalto
;;	Author:		Jari Aalto <jari.aalto@poboxes.com>
;;	Maintainer:	Jari Aalto <jari.aalto@poboxes.com>
;;	Created:	1995-02
;;	Keywords:	emacs rc initalize
;;	Url:		http://poboxes.com/jari.aalto/
;;
;;	This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;  Description
;;
;;      This file configures all Emacs paths and file related mode lists.
;;      This is the first file I load from $HOME/.emacs.
;;
;;          (require 'emacs-rc-path)
;;
;;      This file may be under some other name but do not mind that.
;;      Just rename this file to whatever is shown in the first line.
;;
;;      ** YOU MUST MODIFY THIS FILE BEFORE YOU CAN USE IT **
;;
;;  Quick overview
;;
;;	The heart of all is package *tinypath.el* that does the hard
;;	work of sniffing all the installed lisp direcories and adds
;;	then to `load-path'. I do not have to manually configure
;;	`load-path' at all.
;;
;;  More details
;;
;;	Coping with different environments is not that simple task at all,
;;	especially if you have several sites and they all have different
;;	Emacs and XEmacs version, plus if you run NTemacs in different
;;	PC machines. Oh well.
;;
;;      I keep exactly the same [X]Emacs installation, no mater in what
;;      operating system or Emacs version I'm running. When I notice that
;;      the installation does not work in some host, I used to adjust the
;;      setup accordingly.
;;
;;      This is the key file to set up the paths and it is the first file
;;      that I load from .emacs. When the paths are in order, loading other
;;      packages can start. Hard wired path names should be avoided by
;;      all means. The following is wrong:
;;
;;	    (require 'rc-vc "~/.rc-emacs/.emacs.vc") ;; load VC customizations
;;
;;	I environment variable $HOME is not set, it breaks, as it did
;;	when I started using NTemacs in different PCs (Yes, you can set
;;      HOME, but if you want to portable, you try to make things work
;;      even is broken environment, like someone else's PC). Do this
;;      instead. Note missing directory part in the file name.
;;
;;	    (require 'rc-vc ".emacs.vc")  ;; Searches load-path
;;
;;	It's equally fast, because the file will be "known" in a cache
;;      from where the lookup happens after *tinypath.el* has done its work.
;;
;;  Structure of paths
;;
;;	Emacs lisp packages are installed in our HP system under
;;	/opt/local/share/emacs/site-lisp
;;
;;	I have arranged My Emacs lisp structure as follows. Understanding this
;;	may help you to configure this file to your environment or if you
;;	have similar interests than I, you can arrange your path structure
;;	similarly and just plug-in this module with few changes. Three
;;	dots "..." refers to previously mentioned root path.
;;
;;	    ~/elisp	    downloaded 3rd party packages from the net
;;
;;	    MIME ROOT DIR:
;;
;;	    ~/elisp/mime    Privatedly installed mime tools
;;
;;	    BELOW mime root there is packages like TM ROOT
;;
;;	    ~/elisp/mime/
;;	    .../tm/tm/	    Symbolic link to latest tm dir
;;			    The latest kit is under .../tm/tm-7.106
;;
;;	    AND semi (successor to tm) ROOT
;;
;;	    ~/elisp/mime/
;;	    .../semi/semi   Symbolic link to latest semi
;;	    .../semi/apel   Symbolic link to latest apel
;;	    .../semi/mel    Symbolic link to latest mel
;;	    .../semi/flim
;;
;;	    GNUS ROOT
;;
;;	    ~/elisp/gnus/   Root directory for various gnus versions
;;	    .../gnus	    Symbolic link to latest gnus release
;;	    .../gnus-NN	    Latest Apha gnus.
;;	    .../tmp-gnus-NN unused Gnus release
;;
;;  Code note
;;
;;	This file requires `cl' package and library files from tiny tools
;;	distribution. See URL at the beginning of file.
;;
;;	variables
;;
;;	o   `my-:root', is the root directory where you keep
;;	    privatedly installed packages. Usually ~/elisp
;;
;;  Windows NT X/Emacs
;;
;;	If you have mounted Unix Disk, say H: which equals to NT $HOME variable,
;;	call function `tinypath-load-path-dump' from _Unix_ Emacs.
;;	This dumps absolute patch to separate file. This is needed because
;;	NT Emacs doesn't see those symlinked directories in your `load-path'.
;;      An explanation was given by Larry Smith.
;;
;;	Give prefix disk eg. "H:" to map your Unix "~" before dumping.
;;
;;	  [1998-10-21 NTEmacs-L Larry Smith <lsmith@cio2000.eds.com>]
;;  	  We are using Samba on the Unix side. If you're using NFS or something
;;  	  else perhaps something similar is happening.
;;
;;  	  NT 4 doesn't have the concept of a symbolic link. So Samba exports
;;  	  them to the Windows side as if they were separate files. Let's say
;;  	  Samba is exporting the directore foo as \\host\\foo. There's a
;;  	  setting in Samba that tells it what to do is a symbolic link under
;;  	  the foo directory points to something outside the tree. If you have
;;
;;	    /foo/bar/bug/ug -> /arf/meow/goo
;;
;;  	  then Samba will either make ug appear as an ordinary file (or
;;  	  directory) in the bug directory, or it will not appear at all.
;;
;;  The grand old way
;;
;;	If you just want very simple installation which tells where you're
;;	private lisp files are located, you can use following which adds
;;	two directories and only if they aren't in `load-path' already.
;;      If you choose to use code below, forget this file together.
;;
;;	    (require 'cl)
;;	    (dolist (dir '("~" "~/elisp"))
;;	      (pushnew (expand-file-name dir) load-path :test 'string=))
;;
;;	If your Emacs has `add-to-list' function (19.34+), then you can write
;;
;;	    (dolist (dir '("~" "~/elisp"))
;;	      (add-to-list 'load-path dir))
;;
;;  Code management
;;
;;      View this file with tinybm.el   -- &tag
;;      and folding.el                  -- {{{ }}}
;;}}}

;;; Change Log:
;;; Code:

;;{{{ init

;;; ............................................................ &path ...

(message "emacs-rc-path start")

(defconst debug-on-error t)

(eval-when-compile
  (require 'cl))



(eval-and-compile
  (when nil
    (autoload 'emacs-p   "tinyliba")
    (autoload 'xemacs-p  "tinyliba")
    (autoload 'win32-p   "tinyliba")
    (autoload 'emacs-version-number-as-string-major "tinyliba")
    (autoload 'assoc-entry-replace-maybe-add "tinylibm")
    (autoload 'ti::f-make-path "tinylibm")
    (autoload 'list-to-string  "tinylibm")

    (autoload 'jde-mode  "jde")
    ))


(unless (featurep 'emacs-rc-lib)
  (require 'emacs-rc-lib))

(defmacro my-maybe-setq (var value)
  (`
   (when (or (not (boundp (quote (, var))))
	     (null (, var)))
     (defconst (, var) (, value)))))


;;; ....................................................... &load-path ...

(defconst my-:root
  (find-if
   (function
    (lambda (dir)
      (setq dir (my-expand-file-name dir))
      (and (stringp dir)
	   (file-directory-p dir))))
   (list

    ;; Search these first. this may be temporary Drop-in Account or PC
    ;; where the private Emacs setup copy is unzipped under this dir.

    "~/temp/elisp"
    "~/temp/lisp"
    "~/temp/emacs/elisp"
    "~/temp/emacs/lisp"
    "~/temp/emacs/my/elisp"
    "~/temp/emacs/my/lisp"

    "~/tmp/elisp"
    "~/tmp/lisp"
    "~/tmp/emacs/elisp"
    "~/tmp/emacs/elisp"
    "~/tmp/emacs/my/lisp"
    "~/tmp/emacs/my/lisp"

    "~/elisp"
    "~/lisp"
    (getenv "HOME")  ;; The root directory
    )))

(let ((home (getenv "HOME")))
  (when (and home
	     (not (file-directory-p home)))
    (error "My: HOME environment varaible is not correctly set %s"
	   home)))

(unless my-:root
  (error "My: Can't set `my-:root'"))



;; (my-directory-location 'news)
;;
(defun my-directory-location (type &optional add-path no-error)
  "Return directory where TYPE is located. Append ADD-PATH to it.
With NO-ERROR, do not signal error if directory does not exits.

TYPE values:

  'home     home directory
  'mail     Gnus Mail hierarchy root
  'mail-archive     The ARCHIVE directory.
  'spool    The incoming multiple file directory. Eg for procmail.
  'news     Gnus News hierarchy root
  'glimpse  Glipmse search index hierarchy root
  'cygwin   Gygwin installation root

  'emacs-config
            Location of Emacs package configuration files."
  (let* ((mail      (getenv "MY_HOME_MAIL_ROOT"))
	 (home      (or (getenv "HOME")
			(error "HOME not set.")))
	 (spool     (getenv "MY_HOME_MAIL_SPOOL"))
	 (news      (or (getenv "MY_HOME_NEWS_ROOT")
			;;  Require that this is set in Win32
			;;  --> Utilise separate partition for NEWS download
			(if (win32-p)
			    (error "Define MY_HOME_NEWS_ROOT"))))
	 (glimpse   (getenv "MY_HOME_GLIMPSE_ROOT"))
	 (config    (getenv "MY_HOME_EMACS_CFG_ROOT"))
	 (cygwin    (getenv "BASH_UNIX_ROOT_START"))
	 ret
	 )

    (if add-path
	(setq add-path (file-name-as-directory add-path))
      (setq add-path ""))

    (flet ((search-dir (list)
		       (dolist (choice list)
			 (when (stringp choice)
			   (setq choice
				 (concat
				  (file-name-as-directory
				   (my-expand-file-name choice))
				  add-path))
			   (when (file-exists-p choice)
			     (return choice)))))
	   )
      (cond
       ((eq type 'home)
	(setq ret (search-dir (list home "~" "c:/" ))))

       ((eq type 'mail-archive)
	(let ((mail- (my-directory-location 'mail)))
	  (setq ret (search-dir (list
				 (if mail-
				     (concat
				      mail-
				      "/nnfolder/archive" ))
				 "~/Mail/nnfolder/archive")))))

       ((eq type 'mail)
	(setq ret (search-dir (list mail "~/Mail" "c:/"))))

       ((eq type 'spool)
	(setq ret (search-dir (list spool "~/Mail/spool"))))

       ((eq type 'news)
	(setq ret (search-dir (list news "~/News"))))

       ((eq type 'cygwin)
	(setq ret (search-dir (list cygwin))))

       ((eq type 'glimpse)
	  (setq ret (search-dir (list glimpse "~/glimpse"))))

       ((eq type 'emacs-config)
	(setq ret (search-dir (list config "~/elisp/config" "~"))))
       )

      (when (and (null no-error)
		 (null ret))
	(error "My: No directory found or invalid input: %s [%s]"
	       (prin1-to-string type)
	       (if (stringp ret)
		   ret
		 "nil")))

      ret
      )))




(defun my-config-path (&optional file-name)
  "Emacs list configuration file path location with optional FILE-NAME."
  (concat (my-directory-location 'emacs-config)
	  (or file-name "")))


;; (my-emacs-root-by-load-path)
;;
(defun my-emacs-root-by-load-path ()
  "Get the ROOT of emacs installation directory by reading `load-path'."
  (let* ((ver (car (my-emacs-versions)))
	  )

    (dolist (path load-path)
      (when (and
	     ;;  the PATH name must contain version for this emacs
	     (string-match (regexp-quote ver) path)
	     (string-match
	      (concat
	       "^.*emacs[0-9.-]+"
	       "\\|^.*emacs[/\\][0-9.-]+"
	       )
	      path))
	(return
	 (list
	  (match-string 0 path)
	  path
	  ))))))


;; (my-win32-emacs-install-root)
;;
(defun my-win32-emacs-install-root ()
  "Return current Emacs's installation directory."
  (dolist (dir (list

		;; Win32 Emacs defines emacs_dir at runtime

		(when (string-match "^\\(.*/emacs\\)/"
				    (or (getenv "emacs_dir") ""))
		  (match-string 1 (getenv "emacs_dir")))

		(getenv "EMACS_INSTALL_ROOT")

		(getenv "BASH_UNIX_ROOT_DRIVE_START")

		;;  Peek load-path where emacs could be installed
		;;  /usr/local/share/emacs/20.4/site-lisp

		"/usr/local/share/emacs"

		))
    (when (and dir (file-directory-p dir))
      (return
       (file-name-as-directory dir)))))


(defun my-emacs-versions ()
  "Return possible verion numbers for current Emacs."
  (interactive)
  (let* ((str            (emacs-version))
	 (patch          (progn
			   (if (string-match "patch \\([0-9]+\\)" str)
			       (match-string 1 str))))

	 (major-version-x-x  (progn
			       (string-match "[0-9]+\\.[.0-9]" str)
			       (match-string 0 str)))
	 (major-version  (progn
			   (string-match "[0-9]+\\.[.0-9]+" str)
			   (match-string 0 str)))

	 (version        (concat major-version   ;; 20.6.1
				 (if patch
				     (concat "." patch)
				   "")))

	 ret
         )
    (dolist (ver (list  version  major-version major-version-x-x))
      (when ver
	(pushnew ver ret :test 'string=)))
    (or ret
	(error "My: Can't get any Emacs versions."))
    ))

;; (my-emacs-install-root)

(defun my-emacs-install-root (&optional add-path noerr)
  "ADD-PATH is appended to the Emacs root. Do not check with NOERR."
  (let* ((win32               (file-directory-p "c:/"))
	 (win32-install-root  (my-win32-emacs-install-root))
	 (install-root        (car-safe (my-emacs-root-by-load-path)))

	 (root-base      (concat (or win32-install-root)
				 (if (boundp 'xemacs-logo)
				     "XEmacs/xemacs-"
				   "gnu-emacs/emacs-")))
	 (roots          (list install-root
			       ;; win32-install-root
			       ))
	 dir
	 )

    (dolist (ver (my-emacs-versions))
      (push (file-name-as-directory
	     (concat root-base ver))
	    roots))

    (dolist (path roots)
      (when (stringp path)
	(setq path (file-name-as-directory path))
	;;  In Unix, the "emacs" binary is usually installed in separate
	;;  Place, like /opt/bin/emacs in SunOS, whereas the libraries
	;;  in standard emacs are installed in /usr/local/share/emacs/NN.N/
	;;
	;;  In Win32, the zip packet is isnatlled in the same directory
	;;  location, where is subdirectory "bin". Use that to verify
	;;  that we found the correct emacs ROOT  installation directory.
	(when (or (and win32
		       (dolist (dir '("bin"
				      "i586-pc-win32"  ;; Xemacs binaries
				      ))
			 (if (file-directory-p (concat path dir))
			     (return dir))))
		  (file-directory-p path))
	  (setq dir (concat path (or add-path "")))
	  (return))))

    (if noerr
	dir
      (if (or (not (stringp dir))
	      (not (file-exists-p dir)))
	  (error "My: Can't find Emacs install root: %s" dir)
	dir))

    ))


(defun my-tinypath-load-path-emacs-version-root-install (site-lisp-root)
  "Return Emacs version dependant root under  `site-lisp-root'."
  (if (boundp 'xemacs-logo)
      (list
       site-lisp-root

       (let ((win-root (my-win32-emacs-install-root) ))
	 (when win-root
	   (concat win-root "XEmacs/xemacs-packages/lisp/")))

       (my-emacs-install-root "lisp")

       ;;  These are too old. i have more reacent in site-lisp dir
	;; "E:/usr/share/XEmacs/site-packages"
       )
    (list site-lisp-root)))


(defun my-remove-load-path-elements ()
  "Removed few standard Emacs installation roots, like Gnus from the path."
  (let ((root (my-emacs-install-root))
	(re   (concat "\\("  root "\\|xemacs-packages\\).*gnus"))
	list
	)
    (dolist (path load-path)
      (unless (string-match re (regexp-quote path))
	(push path list)))
    (reverse list)))


;; (my-site-lisp-root)

(defun my-site-lisp-root ()
  "Find system wide site-lisp ROOT start directory."
  (interactive)
  (let* ((env (getenv "SITE_LISP_ROOT_START"))
	 ret
	 )
    (cond
     ((not (stringp env))
      (message "My: Warning env. var. SITE_LISP_ROOT_START not set"))
     ((not (file-directory-p env))
      (message "My: ERROR env. var. SITE_LISP_ROOT_START not dir %s"
	       env)
      (setq env nil)
      ))

    ;;  Guess where the site lisp might be
    ;;  It is very unfortunate that most sites
    ;;  Keep SPARATE site-lisp directories for
    ;;  emacs and xemacs. That doesn't make sense.
    ;;
    ;;  E.g.  /usr/local/share/emacs/site-lisp
    ;;
    ;;  We don't search those instalations. It's
    ;;  better to handle like
    ;;
    ;;  /usr/local/share/site-lisp/common  Common packages
    ;;  /usr/local/share/site-lisp/emacs   Emacs specific
    ;;  /usr/local/share/site-lisp/xemacs  XEmacs --''--
    ;;
    (dolist (try (list
		  env
		  "/usr/share/site-lisp"
		  "/opt/share/site-lisp"
		  "/usr/local/share/site-lisp"
		  "/usr/local/share/emacs/site-lisp"
		  ))
      (when (and (stringp try)
		 (file-directory-p try))
	(setq ret (file-name-as-directory try))))

    (if ret
	ret
      ;;  don't die on error if this is \\server\path\
      ;;  and not a local disk.
      (if (and (stringp env)
	       (string-match "^\\\\"))
	  (message "My: server UNC path SITE_LISP_ROOT_START not available.") 
	(error (concat
		"My: Can't find site-lisp. Set environment "
		"variable SITE_LISP_ROOT_START"))))
    ))


(defun my-tinypath-load-path-setup ()
  "Define Emacs root directories to `tinypath-:load-path-root'."
  (interactive)
  (let* ((site-lisp  (file-name-as-directory (my-site-lisp-root)))
	 (common1    (and site-lisp
			  (concat site-lisp "common")))

	 (common     (or (getenv "SITE_LISP_ROOT_COMMON")
			 (and (file-directory-p common1)
			      common1)))

	 (architecture   ;; /usr/share/site-lisp/[x]emacs
	  (when common
	    (concat (file-name-directory common)
		    (if (boundp 'xemacs-logo)
			"xemacs"
		      "emacs"))))

	 (dir (list
	       (my-expand-file-name my-:root)

	       common

	       (when site-lisp
		 (concat site-lisp "net"))

	       architecture       ;; some packages work only with [X]Emacs

	       (concat (or (getenv "BASH_UNIX_ROOT_START") "")
		       "/opt/share/site-lisp/common")

	       (if (fboundp 'xemacs-logo)
		   (concat (my-win32-emacs-install-root)
			   "XEmacs/xemacs-packages/lisp/"))

	       (my-emacs-install-root "lisp")

	       ))
	 )

    ;; make sure this is list, otherwise pushnew barfs

    (when (boundp 'tinypath-:load-path-root)
      (unless (listp tinypath-:load-path-root)
	(setq tinypath-:load-path-root
	      (if (stringp tinypath-:load-path-root)
		  (list tinypath-:load-path-root)
		nil))))

    ;; Define variable before pushnew

    (defvar tinypath-:load-path-root nil)

    ;;  It doesn't matter if there are non-existing directories,
    ;;  they get dropped anyway

    (dolist (elt dir)
      (when (stringp elt)
	(if (file-directory-p elt)
	    (pushnew elt tinypath-:load-path-root :test 'string=)
	  (message "My: Not a lisp ROOT %s" elt))))

    tinypath-:load-path-root
    ))


(my-tinypath-load-path-setup)

;; ("g:/BIN/EMACS/gnu-emacs/emacs-20.7.3/lisp" "g:/unix-root/u/usr/share/site-lisp/emacs" "g:/unix-root/u/usr/share/site-lisp/net" "G:/unix-root/u/usr/share/site-lisp/common" "e:/home/jaalto/elisp" "~/elisp")

(defun my-dir-list (dir)
  "Return directories under DIR."
  (let* (list)
    (when (file-directory-p dir)
      (dolist (elt (directory-files dir 'full))
	(if (file-directory-p elt)
	    (push elt list))))
    list))


(when (or t  ;; Use always non-compressed cache
	  (boundp 'xemacs-logo)
	  (memq window-system '(w32 mswindows win32)))


  ;; Do not load compiled site-lisp files in XEmacs. I share all my Common
  ;; libraries with XEmacs and Emacs, but I only use compiled versions in
  ;; Emacs
  ;; (defconst tinypath-:ignore-file-regexp "/site-lisp/common.+\\.elc")

  (defconst tinypath-:ignore-file-regexp "\\.elc")

  ;; For some reason Win32 XEmacs corrupts .gz files. Use only .el,
  ;; not .el.gz suffix

  (my-maybe-setq tinypath-:cache-file-postfix ".el")

  (let ((root (my-emacs-install-root))
	up
	pkg
	)
    (when root
      (setq up
	    (file-name-directory
	     ;; Delete trailing slash
	     (substring root 0 (1- (length root)))))

      (setq pkg (concat up
			(file-name-as-directory
			 "xemacs-packages\\lisp")))

      (when (file-directory-p pkg)
	(pushnew pkg load-path :test 'string=)
	(dolist (dir (my-dir-list pkg))
	  (pushnew dir load-path :test 'string=)))
      ))

  )




(defconst tinylib-:package-config-file-directory
  (if (file-directory-p "~/elisp/config")
      "~/elisp/config"
    "~"))


;;  I want to see all messages from tinypath.el

(when (boundp 'message-log-max)
  (defconst message-log-max 20000)) ;; Emacs variable

;;  Must load package with absolute path, because this is very first file
;;  we load and the load-path is not yet defined.


;;  put all Emacs Lisp package configuration files here:

(my-maybe-setq tinypath-:cache-file-prefix
	       (concat (file-name-as-directory
			tinylib-:package-config-file-directory)
		       "emacs-config-tinypath-cache"))



(defun my-load-file-tinypath (&optional force)
  "Load tinypath.el."

  (or (featurep 'tinypath)
      (load "tinypath" 'noerr)		;if sysadm has TinyTools installed
      
      ;;  This is the only time we have to use ABSOLUTE path to load a
      ;;  lisp package.

      (load (my-expand-file-name "~/elisp/tiny/lisp/tinypath.el") 'noerr)
      (error "My: Cannot load tinypath.el")
      ))


(defun my-tinypath-:load-path-ignore-regexp-hook ()
  ;;  Do this only once
  (unless (get 'my-tinypath-:ignore-file-regexp 'set)
    (put 'my-tinypath-:ignore-file-regexp 'set t) ;; mark as done.
    (setq tinypath-:load-path-ignore-regexp
	  (concat
	   tinypath-:load-path-ignore-regexp
	   "\\|[/\\]x?emacs[-/\\0-9.]+[\\/]lisp[\\/]gnus"))))



(add-hook 'tinypath-:load-path-ignore-regexp-hook
	  'my-tinypath-:load-path-ignore-regexp-hook)

(my-load-file-tinypath)

;;; ............................................................ &libs ...

;; The path  is now known. Next we load some libraries that are
;; used everywhere. Library `tinyliba' contains autoload
;; forward declarations to all other functions.

(unless (featurep 'tinyliba)
  (load "tinyliba"))


;;}}}

;; ........................................................... efs ...

;; Efs distribution contains modified dired: it must be loaded instead of
;; Emacs dired.
;;
;; #todo: This is in fact irrelevent, because EFS is not Emacs compatible.
;; #todo: This supposes I have private EFS installed for Emacs

(if (and nil  ;; disabled for now
	 (xemacs-p)
	 (not (featurep 'dired)))
    (load "efs" 'noerr))


;;{{{ info dir

;;; .................................................. &info-directory ...


(defun my-info-setup (&optional list)
  "Add LIST of path to `Info-default-directory-list'.
Loop through LIST and remove non-existing paths. Also make sure
that each directory has central file `dir' otherwise the directory
is broken.

LIST contains additional info directories to be added to
`Info-default-directory-list'"
  (let* ((fid  "tinypath-info-setup:")
	 (info (append list Info-default-directory-list))
	 list				;Remove paths that does not exist
	 not-exist
	 )
    (dolist (path info)
      (when (and (stringp path) (file-exists-p path))
	(add-to-list 'list path)
	;;  See if we can find "dir", otherwise the Info is broken and
	;;  can't be used.
	(unless (file-exists-p (ti::f-make-path path "dir"))
	  (push path not-exist))))

    (when not-exist
      (message
       "%s: Couldn't find central 'dir' file(s). Broken Info dirs: %s "
       fid
       (list-to-string not-exist))
      (sit-for 2))

    (setq Info-default-directory-list list)

    (if (boundp 'Info-directory-list)
	(set 'Info-directory-list list))

    ;; Info-additional-directory-list

    ))


;;  Add my private info to the path list. If dir doesn't exist in my
;;  current account, it will be automatically filtered out.
(let ((root (getenv "BASH_UNIX_ROOT_START"))  ;; Cygwin installation root
      )
  (my-info-setup
   (list
    (concat my-:root "/info/")

    ;;  Take into account win32 Emacs dirs too. E: is separate partition
    ;;  that mimics Unix disk layout.

    (my-emacs-install-root "info" 'noerr)

    (when root (concat root "/usr/info"))
    (when root (concat root "/usr/local/info"))
    (when root (concat root "/usr/share/site-lisp/common/info"))

    "E:/usr/share/XEmacs/site-packages/info"
    "E:/usr/share/XEmacs/xemacs-packages/info"

    (concat "E:/usr/share/"
	    (if (emacs-p)
		"emacs"
	      "XEmacs")
	    (emacs-version-number-as-string-major)
	    "/site-packages/info")
    )))



(defun my-info-setup-other ()
  "Other things for Info. Support compression."
  (when (boundp 'Info-suffix-list)
    (aput 'Info-suffix-list ".info.bz2"  "bzip2 -dc %s")
    (aput 'Info-suffix-list ".bz2"       "bzip2 -dc %s")
    ))


(add-hook 'Info-mode-hook 'my-info-setup-other)

;;}}}
;;{{{ cdpath

;;  See `files.el'. NT Emacs 20.3 dies if `cd-path' is not set
;;  and you do programmatically `cd' in .emacs*
;;  #todo: ivestigate, this may be a too fast one minute hack.

(defvar cd-path nil)

(when (and (null (getenv "CDPATH"))
	   (null cd-path)
	   (emacs-p)
	   (win32-p))
  (dolist (path (list
		 "C:/"
		 "C:/work/unix/root/users/"
		 (concat "C:/work/unix/root/users/"
			 (getenv "USER"))
		 "H:/"
		 ))
    (if (file-directory-p path)
	(add-to-list 'cd-path path))))

;;}}}

(when (or (emacs-p "20")
	  (xemacs-p "20"))
  ;;  Make sure load compiled versons. The Byte compiled files are not
  ;;  compatible between Emacs versions.
  (load "custom.elc")
  (load "widget.elc"))

(message "emacs-rc-path end")
(provide 'emacs-rc-path)

;;; emacs-rc-path.el ends here
