;;; @(#) tinypath.el --- Manage Emacs startup dynamically.
;;; @(#) $Id: tinypath.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1999-2002 Jari Aalto
;; Author:          Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:      Jari Aalto <jari.aalto@poboxes.com>
;; Created:         1999-02
;; Keywords:        extensions
;;
;; To get information on this program use ident(1) or do M-x tinypath-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinypath|Jari Aalto|jari.aalto@poboxes.com|
;; Manage your Emacs load-path dynamically. Easy inst. of new packages.|
;; 2002-08-01|$Revision: 1.1 $|~/misc/|

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

;; ....................................................... &t-install ...
;;
;;  The very fast start
;;
;;	If you really insist doing the reading later, here is what to
;;      do. No guarantees that this will work. Then you better read docs.
;;
;;	o   Include perl script *emacs-util.pl* in your `PATH'. If you
;;	    do not have perl, get it at http://www.perl.com/ or
;;	    win32 http://www.activestate.com/
;;	o   If you use XEmacs, see `tinypath-:core-emacs-load-path-list'
;;	o   If you use private Gnus (not gnus from Emacs distribution)
;;	    YOU MUST SET `tinypath-:load-path-ignore-regexp-extra'.
;;      o   Move all your private lisp under $HOME/elisp
;;      o   Create directory `$HOME/elisp/config' for cache location.
;;	o   Load *tinypath.el* like this, please copy verbatim:
;;
;;          ;; $HOME/.emacs
;;          (require 'cl)
;;          ;; location of tinypath.el
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'string=)
;;          (load "tinypath.el")
;;
;;      o   After Emacs is up, call `M-x' `tinypath-cache-problem-report'
;;
;;  First user note
;;
;;	You may see message "TinyPath: EXT Process running ...
;;	[may take a while]" and Emacs hangs for a while when you use this
;;	package for the first time. Please wait and read the documentation
;;	about "Faster Emacs configuration" later in this file.
;;
;;	At any time you can read the manual with `M-x' `tinypath-version'
;;
;;      Private Gnus users, read section from manual below.
;;
;;      ********************************************************************
;;      It is important that you use the EXT method, because the old lisp
;;      method is present only for backward compatibility. Old lisp
;;      method also has a drawback, that it does not support rearranging
;;      paths to order: 1) $HOME 2) site-lisp-files 3) core-emacs-lisp-files
;;      ********************************************************************
;;
;;      This new method guarantees, that anything you put into your
;;	private ~/elisp will override and precede any other package
;;	found elswhere in `load-path' hierarchy.
;;
;;  Emacs Lisp install
;;
;;	Put this file on your Emacs-Lisp load path, add following into your
;;	$HOME/.emacs startup file.
;;
;;          ;; NO SYMLINK DIRS HERE, THEY ARE IGNORED!
;;	    (setq tinypath-:load-path-root '("~/elisp" .. ))
;;          ;;
;;	    ;; If you use Latest XEmacs, which ships the lisp separately:
;;          ;;
;;          (setq  tinypath-:core-emacs-load-path-list
;;          	   '("/usr/local/share/xemacs/xemacs-packages"
;;          	     "/usr/local/share/xemacs/site-packages"
;;          	     "/usr/local/share/xemacs/mule-packages"
;;          	     ..))
;;          ;; PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;; THAT ACTIVATE If YOU ADD THE PATH
;;          (require 'cl)
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'stringp=)
;;          (load "tinypath.el")
;;
;;  Cache file location
;;
;;	Create directory where the cache information is saved by default or
;;	change `tinypath-:cache-file-prefix' which should be pathname +
;;	file-prefix. The cache size depends on your installed files,
;;	with 600 directories and 8000 lisp files, the cache size is around
;;      500k and if you use compression, it takes somewhere 200k.
;;
;;	    % mkdir -p ~/elisp/config
;;
;;  Transparent compression
;;
;;	This package supports transparent compression, where you can keep
;;	your files in compressed format without touching the code in your
;;	emacs startup files. Calls like below are interpreted as if
;;	there were a `.el.gz' extension attached to the file.
;;
;;	    (load "some-file")
;;	    (require 'somefile)
;;
;;      IMPORTANT: If you compress or decompress a lisp file, always
;;	update cache immediately with `M-x' `tinypath-cache-regenerate'.
;;
;;  Contact and support
;;
;;	You should pump up the `tinypath-:verbose' level to 10 if you
;;	think there is something odd going on. All the messages will
;;	appear in Emacs *Message* buffer. If you have any questions,
;;	use function `M-x' `tinypath-submit-bug-report'. Send
;;	*Message* buffer content.
;;
;;	********************************************************************
;;
;;	IT IS HIGHLY RECOMMENDED THAT YOU VALIDATE YOUR SETUP
;;      AFTER YOU HAVE LOADED THIS PACKAGE
;;
;;	Start Emacs and call report function to investigate any problems,
;;	like duplicate packages that shadow each other. See documentation
;;	below for more. The general rule is that you should delete
;;	any offending packages (use `C-d' to delete file in the buffer
;;      that displays the problem report)
;;
;;	    C-u M-x tinypath-cache-problem-report   (or without C-u argument)
;;
;;	*******************************************************************
;;
;;	HTML Documentation of this file can be generated with two
;;      perl scripts available at
;;	http://cpan.perl.org/modules/by-authors/id/J/JA/JARIAALTO/
;;
;;	    % ripdoc.pl tinypath.el | t2html.pl > tinypath.html

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Feb 1999
;;
;;  How it all begun
;;
;;      When you have set up your Emacs installation to your liking, a day
;;      comes when you decide that it's time to seriously reconsider the
;;      directory structure of your installed lisp packages. At start, it
;;      is customary to use simple file hierarchy where all private
;;      packages are installed under:
;;
;;          ~/elisp   ;; some people also prefer ~/lisp
;;
;;      Complete kits are usually installed directly under the root:
;;
;;          ~/elisp/packages/bbdb-2.00.06/
;;          ~/elisp/packages/psgml-1.0.3/
;;          ~/elisp/packages/pcl-cvs-2.9.2/
;;
;;      A more sophisticated way is to use symlinks to the latest
;;      versions, so that you don't have to change `load-path' every
;;      time you install a new version. It is only matter of updating
;;      the symlink:
;;
;;          ~/elisp/packages/pcl-cvs/  --> ~/elisp/packages/pcl-cvs-2.9.2/
;;          |
;;          This path is in the `load-path'
;;
;;      In windows world you may have your _H:_ disk mounted to Unix _$HOME_:
;;
;;          H:  --> Unix $HOME  \\SERVER\DIRECTORY\YOUR-LOGIN-DIR
;;
;;      Now, there is a catch when you use Unix symlinks in `$HOME/elisp'
;;      and try to access the directories from Windows. Having set PC's
;;      HOME environment variable to point to H:, Emacs can start reading
;;      Unix `$HOME/.emacs' startup file, but you soon encounter simple
;;      error messages like "Can't load library xxx", which soon follow by
;;      bigger concerns: "autoloading xxx failed". The problem is the
;;      mounted H: disk. You see, PC's network mount can't distinguish
;;      symlinked directories from real directories, so all symlinked Unix
;;      directories in `load-path' are dead. And that's why most of the
;;      files cannot be found any more.
;;
;;     The conclusions
;;
;;      For cross platform solution it is best not to rely on symlinks,
;;      because they don't work over a Windows mount. Secondly,
;;      updating `load-path' should not be needed by hand after a new
;;      package installation, after a directory name change, after
;;      directory structure change, etc. A dream package would solve this
;;      all and do the hard work: "There, that is the root(s) of all Emacs
;;      lisp, go and sniff all the directories and update `load-path'"
;;      That was what this package originally was all about. Nowadays it
;;      does a little more than that. Your `load-path' is updated
;;      automatically without any manual work. You only have to give the
;;      start ROOT path(s) of your installed lisp hierarchies in the file
;;      system. This package is highly effective: scanning thousands of
;;      files in a matter of seconds and once the cache has been created,
;;      it takes only a snap to load it in next sessions. All `require' and
;;      `load' commands execute also faster than previously, because the
;;      information about existing files is immediately available. The
;;      speedup is helped through advised functions.
;;
;;  Overview of features
;;
;;     Automatic load-path configuration
;;
;;      o   Define list of `root' directories of your Emacs lisp and this
;;          package will recursively add directories which contain .el or
;;          .elc files to `load-path'
;;      o   A cache is utilized to remember previous scan and
;;          expired periodically. Using cache speeds up loading files
;;          considerably if you have many directories. The number of lisp
;;          directories doesn't affect the load performance any more.
;;          This is accomplished by using extra advice code in functions:
;;          `load', `load-library', `require', `locate-library' and
;;          `autoload'.
;;      o   When Emacs becomes idle (some 15 minutes of idle time) the
;;          cache and `load-path' is validated for erroneous entries and
;;          rebuilt as needed. This feature should auto-detect changes in
;;          directory structure automatically and help semi auto-installing
;;          new directories (packages) for you.
;;      o   The `load-path' is optimized so, that users' files automatically
;;          take precedence first (~/elisp), next any other files found,
;;          and last the core Emacs files in the distribution.
;;          For example speedbar.el that comes with Emacs is too outdated
;;          if used with JDE package and that is the reason why core Emacs
;;          files come last.
;;
;;     Automatic Info-default-directory-list configuration
;;
;;      o   If you download packages that include Emacs info files,
;;          the `Info-default-directory-list' is updated at the same time
;;          as the `load-path', when root directories are examined.
;;      o   No more manual updating of info files. The missing
;;          `dir' entry is created or updated as needed.
;;      o   You can update all _new_ info files in your system by calling
;;          M-x `tinypath-info-scan-Info-default-directory-list'
;;
;;      If you have added new info files by hand, just call function
;;	`tinypath-info-handler' to update your Emacs and update the
;;	`dir' entry.
;;
;;     Automatic manpath configuration
;;
;;      o   In Unix systems the MANPATH contains directories where to
;;          find manual pages, but in Win32, there is no default
;;          MANPATH and `M-x' `man' does not work.
;;      o   If package *woman.el* http://centaur.maths.qmw.ac.uk/Emacs/
;;          is along `load-path', it is automatically configured to
;;          support to read manual pages. It replaces the `M-x' `man'
;;          command.
;;
;;     Win32 Cygwin environment support
;;
;;      o   If *cygwin1.dll* (<http://www.cygwin.com/>) is in your path,
;;          automatic detection tries to find the Cygwin root and scan
;;          manual and info pages for use with *woman.el*
;;      o   The other file Unix-like hierarchies, like reed-kotler
;;          etc., are also dealt with variable `tinypath-:extra-path-root'.
;;
;;     Compressed lisp file support
;;
;;      o   Overloads commands LOAD, LOAD-LIBRARY, LOAD-FILE, REQUIRE
;;          and AUTOLOAD to accept `jka-compr' compressed lisp .el files.
;;          This may include .Z .gz .z .bz2 files.
;;      o   Primarily meant to be used in low quota accounts.
;;      o   You don't have to change a thing in your Emacs startup file, all
;;          will work as usual.
;;      o   Correctly handles aliased commands that turn out to be
;;          in `autoload' state.
;;
;;  How to set up your load path
;;
;;      The `tinypath-:load-hook' contains function `tinypath-setup' which
;;      starts examining all directories under `load-path' and
;;      `tinypath-:load-path-root' which is set to reasonable defaults of
;;      site wide and personal installations. If you keep all your lisp
;;      files under *$HOME/elisp*, then you do not need to configure anything
;;      for this package to work. Your `load-path' has been updated after
;;      this statement at the beginning of your *$HOME/.emacs*
;;
;;          (load "~/elisp/tiny/tinypath")   ;; Or where you have it installed
;;
;;      If you have _many_ separate Emacs lisp root directories, like one
;;      for *site-lisp* and one for *site-packages* and one for your
;;      *personal* *lisp* files, then you have add those to variable
;;      `tinypath-:load-path-root'. Below there is an example for PC users,
;;      where the E: partition replicates identical Unix tree structure.
;;      The following actually works for Unix too, because non-existing
;;      directories will be ignored:
;;
;;          (setq tinypath-:load-path-root
;;            '("~/elisp"  "E:/usr/share/emacs/site-lisp/common"))
;;          (load "~/elisp/tiny/tinypath")
;;
;;  XEmacs and Emacs specific directories
;;
;;      In spite of great effort from developers to make packages
;;      compatible for both Emacs platforms, there is always some packages
;;      that only works with Emacs or XEmacs. It is assumed that the site
;;      admin has created directories like these to keep the *site-lisp*
;;      installation clean:
;;
;;          ;;   This might be also under /opt/share/site-lisp
;;          ;;   Refer to file hierarchy standard at
;;          ;;   http://www.pathname.com/fhs/
;;
;;          /usr/share/site-lisp/common/   .. For XEmacs and Emacs
;;          /usr/share/site-lisp/emacs/    .. Packages only for Emacs
;;          /usr/share/site-lisp/xemacs/   .. Packages only for XEmacs
;;
;;      To take care of the Emacs specific `load-path' setting, use code
;;      similar to this snippet. If you load the setup multiple times, the
;;      `pushnew' ensures that the directories are not added multiple
;;      times.
;;
;;          (require 'cl)
;;          (dolist (path ("~/elisp"
;;                         "/usr/share/emacs/site-lisp/common"
;;                         ;;  Select Emacs or XEmacs specific installations
;;                         (if (boundp 'xemacs-logo)
;;                             "/usr/share/site-lisp/xemacs"
;;                           "/usr/share/site-lisp/emacs")))
;;            (when (stringp path)
;;              (pushnew path tinypath-:load-path-root :test 'string=)))
;;
;;          ;; PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;; THAT ACTIVATE If YOU ADD THE PATH
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'stringp=)
;;          (load "tinypath.el")
;;
;;      In fact this package will check current emacs version and make sure
;;      that only correct directories are included to the `load-path'. If
;;      you simply instructed to search the whole site-lisp root
;;      `/usr/share/site-lisp', and current emacs binary is "emacs", then all
;;      directories that contain path portion `/xemacs' are automatically
;,      ignored.
;;
;;     Building part of site-lisp using Net
;;
;;      If we continue talking a bit more about site-lisp, you will find in
;;      the Tiny Tools kit an utility *bin/mywebget.pl* and configuration
;;      file *mywebget-emacs.conf* which contains current knowledge where
;;      the various lisp developers' home pages are and how to download all
;;      known lisp tools that does not come with Emacs. If you have lot of
;;      disk space and you're interested in getting more lisp tools to go
;;      with your Emacs, follow the instruction laid out in the
;;      configuration file. This usually means changing the configuration
;;      file's _ROOT_ directory where the download will happen. On disk,
;;      you are inclined to add few new file hierarchy levels:
;;
;;          /usr/share/site-lisp/net/users
;;          /usr/share/site-lisp/net/packages
;;
;;	If you further are interested in Emacs packages, you should consider
;;	adding yet another hierarchy and investigate some of your time to
;;	download CVS version control software from http://www.cvshome.com/
;;	and would be ready to keep track of Gnus, BBDB and other package
;;	developments very effectively (minimizing network traffic) and timely
;;	fashion. It is suggested that you put all the CVS downloads under:
;;
;;	     /usr/share/site-lisp/net/cvs-packages
;;
;;      The overall structure of whole site list may look something like this.
;;      The CVS packages are mostly available at http://www.sourceforge.net/
;;
;;	    /usr/share/
;;    		     +--site-lisp/
;;    			+--emacs/
;;    			|  +--packages/
;;    			|  |  +--pcl-cvs-2.9.9/
;;                      |  |  +-... and so on
;;    			|  +--win32/
;;    			|     +--gnuserv/
;;                      |     +-... and so on
;;    			+--net/
;;    			|  +--users/
;;                      |     +-LispDeveloperA
;;                      |     +-LispDeveloperB
;;                      |     +-... and so on
;;    			|  +--cvs-packages/
;;    			|     +--liece/
;;    			|     +--lookup/
;;    			|     +--ILISP/
;;    			|     +--jess-mode/
;;    			|     +--devel/
;;    			|     +--emacro/
;;    			|     +--tnt/
;;    			|     +--cc-mode/
;;    			|     +--mailcrypt/
;;    			|     +--bbdb/
;;    			|     +--gnus/
;;                      |     +-... and so on
;;    			+--common/
;;		        |     ...COMMON dir to Emacs and XEmacs
;;			|     =======================================
;;			|     ...Packages that you find posted to the
;;			|     ...gnu.emacs.sources and whose author's
;;                      |     ...do not have a homepage
;;    			+--xemacs/
;;    			   +--cvs-packages/
;;    			      +--xemacs-packages/
;;
;;     XEmacs 21.2+ core packages
;;
;;      The recent XEmacs versions ship with only the very basic
;;      installation. Lisp packages are distributed in separate archive
;;      *xemacs-packages* (nick named SUMO due to its huge size). There is
;;      also *mule-packages* and *site-packages* archives. A built-in
;;      heuristics tries to guess the location of these by looking under and
;;      near your XEmacs installation:
;;
;;          .../XEmacs/XEmacs-NN.N/xemacs-packages
;;          .../XEmacs/xemacs-packages
;;
;;      If the archives have been installed elsewhere, you have to tell the
;;      location by defining following variable prior loading TinyPath. You
;;      can't put these to `tinypath-:load-path-root' because this is
;;      special information that needs to present during the very initial
;;      boot-up to find crucial packages like *jka-compr.el*.
;;
;;          (setq tinypath-:core-emacs-load-path-list
;;                '("/usr/share/site-lisp/xemacs/xemacs-packages"
;;                   "/usr/share/site-lisp/xemacs/mule-packages"
;;                   "/usr/share/site-lisp/xemacs/site-packages"))
;;
;;  Finding load-path directories
;;
;;      If you only used default $HOME/elisp directory for your files, the
;;      `tinypath-:load-path-function' starts recursively searching all
;;      the directories under the root(s) `tinypath-:load-path-root'. Not all
;;      directories are counted in when the search descends below the root(s).
;;      Variable `tinypath-:load-path-ignore-regexp' decides if the directory
;;      should be ignored. By default:
;;
;;      o   Package's additional subdirectories like texinfo, tex, doc, etc,
;;          misc, RCS, CVS, zip are ignored.
;;      o   Any temporary directories named .../t/ .../T/ .../tmp* .../temp*
;;          are ignored.
;;      o   Directories that do not contain any files ending to .el or .elc are
;;          ignored.
;;
;;  Gnus and other 3rd party packages
;;
;;	    _NOTE:_ in the latest version *Gnus* is treated specially. All
;;	    Gnus versions are detected along load-path and the very latest
;;	    Gnus version is installed to your `load-path'. This is based on
;;	    the knowledge in the `gnus-version' variable and the heuristics
;;	    will pick the newest for you. You actually do not have to do
;;	    anything else, but to drop latest Gnus somewhere, to be able to
;;	    use it immediately.
;;
;;      It is important to understand how this package works: It caches
;;      every possible lisp directory it can find. Now, if you have
;;      installed private copy of Gnus, say in `~/elisp/cvs-packages/gnus',
;;      there is a problem, because Emacs distribution also includes Gnus.
;;      There is NO WAY TO TELL OR CHANGE path order when the cache is in
;;      use. This is a design decision and cannot be changed. The old trick,
;;      where a new directory was added in front of `load-path', will not
;;      work because everything goes through cache. What you need to do
;;      instead, is to tell that the "other" Gnus should be ignored during
;;      cache creation, so that it is completely unknown.
;;
;;     Solution: ignoring directories
;;
;;	There is very simple way. Put your regular expression to
;;	`tinypath-:ignore-file-regexp-extra' and it will tell which
;;	directories to ignore.  Naturally you must put the lisp code
;;	_before_ you load package.
;;
;;          (setq tinypath-:load-path-ignore-regexp-extra
;;                "\\|[/\\]x?emacs[/\\0-9.]+[/\\]lisp[/\\]gnus")
;;          ;; PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;; THAT ACTIVATE If YOU ADD THE PATH
;;          (require 'cl)
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'stringp=)
;;          (load "tinypath.el")
;;
;;      [For advanced Lisp programmers] You can add ignored gnus directory
;;      to `tinypath-:load-path-ignore-regexp' via
;;      `tinypath-:load-path-ignore-regexp-hook'. When the hook is run, the
;;      default value for `tinypath-:load-path-ignore-regexp' is already
;;      available. In hook, append regular expression that excludes the
;;      Gnus directory. Here is an example; make sure that you don't add
;;      the regexp multiple times. The multiple invocations is protected by
;;      setting a plist property and checking it. The ugly [\\/] makes the
;;      regexp compatible with both Unix and win32 paths. System
;;      directories in Unix are typically /emacs/NN.NN/ and in win32
;;      /emacs-NN.NN/, that's why added "-".
;;
;;          (add-hook 'tinypath-:load-path-ignore-regexp-hook
;;                    'my-tinypath-:load-path-ignore-regexp-hook)
;;
;;          (defun my-tinypath-:load-path-ignore-regexp-hook ()
;;            ;;  Do this only once
;;            (unless (get 'my-tinypath-:load-path-ignore-regexp-hook 'set)
;;              ;; mark as done.
;;              (put 'my-tinypath-:load-path-ignore-regexp-hook 'set t)
;;              (setq tinypath-:load-path-ignore-regexp
;;                    (concat
;;                     tinypath-:load-path-ignore-regexp
;;                     "[/\\]x?emacs[/\\0-9.]+[/\\]lisp[/\\]gnus"))))
;;
;;      #todo: What about XEmacs public/private Gnus installations ?
;;
;;  Updating new lisp packages
;;
;;      Suppose you have installed a new version of a package:
;;
;;          ~/elisp/gnus/pgnus-0.74/
;;          ~/elisp/gnus/pgnus-0.95/    ;; NEW
;;
;;      Both these directories end up being added to the `load-path', but
;;      that is not preferable. It is the latest version that should be in
;;      the `load-path'. The solution is to move the old versions under
;;      some directory name that will be ignored by default. It is
;;      recommended that a backup of previous packages are renamed to start
;;      with a word "tmp-". All directories that start with prefix *tmp*
;;      are ignored, the problem has been solved. However if you update
;;      package in a site-lisp directory, there may be a distant problem
;;      that somebody needs older version of the package. If you made the
;;      backup like above, that user cannot load the old package any more,
;;      because it doesn't show up in `load-path'
;;
;;          % mv ~/elisp/gnus/pgnus-0.74/ ~/elisp/gnus/tmp-pgnus-0.74/
;;
;;        There is no easy answer to keep old packages. Admin could
;;        announce that: "new version has been installed in DIR, the old
;;        one is in TMP-OLD-DIR" and have users manually arrange their
;;        `load-path' if needed. Following simple lisp command would solve
;;        their setup. The statement below adds the old directory to the
;;        *beginning* of `load-path' and thus load commands would find the
;;        old version of the package first.
;;
;;          (load "~/elisp/tiny/tinypath")
;;          (pushnew TMP-OLD-OLD-DIR load-path :test 'string=)
;;          (tinypath-cache-regenerate)
;;
;;      Remember to mention to users that they need to update cache with
;;      `tinypath-cache-regenerate' (called with prefix argument) to see
;;      the changes.
;;
;;  Duplicate files in path
;;
;;      If you have accustomed to putting your path to specific order, you
;;      have to rethink your strategy if you are going to use this package.
;;      The philosophy behind creating this utility was: YOU SHOULD NOT
;;      NEED TO DO MANUAL WORK TO UPDATE PATHS. This means that the order of
;;      the paths *must* not be significant. Now, You may face a situation
;;      where you install a library or package that contains a file, which
;;      has already been installed to your hierarchy. Take for example,
;;      *smtpmail.el*:
;;
;;	    /usr/local/bin/emacs-20.4/lisp/mail/smtpmail.el
;;	    /usr/share/site-lisp/common/packages/semi/flim-1.12.1/smtpmail.el
;;
;;      We have a problem here if FLIM's *smtpmail.el* is not compatible with
;;      the one in Emacs. If it is, then there is no problem. Either one can be
;;      loaded, and the `load-path' order does not matter. But you don't
;;      know that before you get error "function smtpmail-xxxx not defined"
;;      and you start investigating with (locate-library "smtpmail") which
;;      package is actually active.
;;
;;      After installing tinypath.el package, please investigate your path
;;      with [C-u] `M-x' `tinypath-cache-problem-report' and see if you
;;      find many duplicate entries. Investigate each one and possibly move
;;      the file to another name or remove older ones. E.g. in the above
;;      situation, the cure might be moving FLIM's *smtpmail.el* under name
;;      *flim-smtpmail.el* so that it doesn't get loaded with (require
;;      'smtpmail). The best is to contact the maintainer(s) and tell them
;;      about conflicts. Here is a sample of one generated problem report:
;;
;;          imenu.el
;;            323 34073 1998-05-07 16:28:08 e:/usr/share/site-lisp/common/other/
;;            910 37169 1999-12-04 02:47:58 e:/usr/share/site-lisp/common/programming/java/jde/jde-2.1.6beta13/lisp/
;;            1350 38663 1999-11-28 01:14:38 e:/usr/local/bin/emacs/gnu-emacs/emacs-20.4.1/lisp/
;;          base64.el
;;            515  9943 1999-12-11 19:15:20 e:/usr/share/site-lisp/common/packages/gnus-5.8.2/lisp/
;;            807  9892 1999-11-15 00:00:12 e:/usr/share/site-lisp/common/packages/w3-4.0pre.46/lisp/
;;
;;      _Explanation:_ User had used *imenu* as a separate package since
;;      early Emacs versions in "other" directory. The latest Emacs already
;;      ships with one, so it is best to delete the offending
;;      `other/imenu.el.' Keep on eye on the numbers here: The lower, the
;;      more close it is to the beginning of cache when the directories
;;      were searched. The Package with lowest score will get loaded. For
;;      base64.el there seems to be no problem. Gnus path has lowest score, it
;;      is the current latest development version, so it will get loaded
;;      before w3's base64.el . A simple major mode `tinypath-report-mode' is
;;      turned on for report buffer. Unnecessary files can be deleted with
;;      `Control-shift-mouse-1' or `C-c' `C-d'.
;;
;;  Symlinked directories are ignored
;;
;;      It is traditional to use symlinks a lot in Unix to arrange easy
;;      access to versioned packages. Here is the traditional way how you
;;      can always refer to ~/elisp/gnus/ no matter what version is
;;      currently installed.
;;
;;          ln -s ~/elisp/packages/gnus-N.NN  ~/elisp/packages/gnus
;;
;;      This package however *skips* those symlinks and records the
;;      absolute path name to the `load-path'. There are couple of points:
;;      a) it is more instructive to peek the `load-path' to actually see
;;      what versions have been installed to the Emacs b) The symlinks are
;;      error prone since there may be several symlinks that lead to same
;;      directory. In general, it is not a problem that the symlink
;;      directory is not included in the `load-path'. There may be one
;;      situation  where symlink causes trouble for this package:
;;
;;        If you draw a symlink to the the current directory from *SEPARATE*
;;        directory, then that directory will never be seen:
;;
;;          ln -s ~/some-disk/elisp/artist-1.1/ ~/elisp/packages/artist-1.1
;;
;;      You shouldn't do this, instead either _a)_ move the package
;;      physically under the ~/elisp/ from the ~/some-disk/elisp/ so
;;      that the recursive search will record it or _b)_ add the
;;      separate directory ~/some-disk/elisp to the
;;      `tinypath-:load-path-root'.
;;
;;  Using cache
;;
;;      Now when you're freed from update burden of the directories in your
;;      disk, you can concentrate organizing the files under sensible
;;      directories. Here is an example how the organizing could go:
;;
;;          ~/elisp/users/kevinr/       Kevin Rodger's files
;;          ~/elisp/users/ilya/         Ilya Zakharevich's files
;;          ..
;;          ~/elisp/packages/bbdb-2.00.06/  Version-ed packages
;;          ~/elisp/packages/psgml-1.0.3/
;;          ~/elisp/packages/pcl-cvs-2.9.2/
;;          ~/elisp/packages/tiny-19990215/
;;	    ...
;;          ~/elisp/mime/               All MIME relates packages under this
;;          ~/elisp/mime/semi/          SEMI packages
;;          ~/elisp/mime/tm/            TM packages
;;	    ...
;;          ~/elisp/lcd/                LCD (lisp code) hierarchy
;;          ~/elisp/lcd/misc/           http://www.cs.indiana.edu/LCD/
;;          ~/elisp/lcd/functions/
;;          ...
;;          ~/elisp/other/              All single add-on packages
;;
;;      All these paths in `load-path' and you can imagine how slow a
;;      standard Emacs would become: it takes even more time to find some
;;      package xxx, when Emacs sees a call (require 'xxx), because Emacs
;;      must start looking into every single directory under `load-path'
;;      until it can determine if it can or cannot load the asked package.
;;      This utility will store all lisp files in cache, and it is
;;      activated by default. The variable `tinypath-:cache-expiry-days'
;;      controls the interval when it is concluded that a new tree
;;      recursion is needed. If you install new packages during those
;;      non-expiry days, it is best to call `C-u' `M-x'
;;      `tinypath-cache-regenerate' to build up to date image of your files
;;      and `load-path' directories.
;;
;;        If you want one short advice: always call `tinypath-cache-regenerate'
;;        after any lisp file or directory update.
;;
;;  Cache file and different Emacs versions
;;
;;      It is important that each Emacs loads correct cache file. The cache
;;      file's name is derived from the emacs version. The emacs type is
;;      "xemacs" "win32-xemacs" "emacs" or "win32-emacs".
;;
;;            tinypath-:cache-file-prefix
;;          + EMACS-TYPE
;;          + HOST
;;          + EMACS-VERSION
;;          + tinypath-:cache-file-postfix
;;
;;          ~/elisp/config/emacs-config-tinypath-cache-win32-HOST-emacs-20.4.1.el.gz
;;          ==========================================                        ======
;;          prefix                                                           postfix
;;
;;     Unix hosts and NFS mounts
;;
;;	In Unix environment, it is also common that several hosts are NFS
;;	mounted so that your home disk is available from every server. The
;;	available programs are not usually NFS mounted, but programs are
;;	stored locally on each server's own disk. Now, there would be a
;;	problem if you logged to host *A* and started tinypath.el which had
;;	made cache in on host *B*, because *A* does not have the same
;;	directories as *B* did (site-lisp). This has been taken care of by
;;	including _hostname_ part in the cache file name. For each host, a
;;	separate cache file is created. Now, suppose all the Unix hosts are
;;	same brand, say Sun OS, Linux, or HP-UX and a good administrator has
;;	separated the programs and the data in their own directory
;;	structures. Furthermore, these structures are NFS mounted and thus
;;	visible to the remote machines. In this scenario, it would not
;;	really matter to which host you log into, because you would always
;;	see the same programs and site-lisp directories and there would not
;;	be need for host specific cache files. In that case, disable the
;;	*HOST* word by writing a simple function which will return an empty
;;	string "":
;;
;;	    (setq tinypath-:cache-file-hostname-function '(lambda () ""))
;;
;;  Info file support
;;
;;      In addition to updating the `load-path', the recursive function
;;      has a chance to search for installed info files as well. When you
;;      keep all your site lisp under one directory, it is not uncommon
;;      that the bigger packages include documentation files in info format
;;      as well. Like:
;;
;;          /usr/share/site-lisp/emacs/pcl-cvs-2.9.9/
;;          /usr/share/site-lisp/common/packages/psgml-1.2.1/
;;
;;      One possibility is that after you download and uncompressed
;;      package, you would copy the info file to some central directory
;;      where you keep all you info files. This is lot of manual work.
;;      (Never mind that in Unix you might use Makefile to install
;;      everything, in Win32 it's all manual work). This package do the
;;      same job by looking for directories that either have info files or
;;      a central info repository called `dir'. `dir'. If the repository
;;      has all the info files up to date, nothing is done. In other cases:
;;
;;      o   If the central `dir' in the directory does not exits,
;;          it is created.
;;      o   If `dir' does not contain entry for info file, it is added.
;;          The entry name is derived from the filename.
;;
;;      The `Info-default-directory-list' is updated to include any new
;;      directory locations and they are saved to same cache file. When you
;;      call `C-h' `i' you will see the new info entries. Easy and
;;      maintenance friendly. No need to worry about supplied info files any
;;      more, they are automatically integrated to your Emacs. If you have
;;      installed any new packages to your system, Emacs packages or Unix
;;      packages that installed something with "install -c", it is best to
;;      update your info files with `M-x'
;;      `tinypath-info-scan-Info-default-directory-list'. This is also
;;      called if you call: `C-u' `M-x' `tinypath-cache-regenerate'
;;
;;  Cygwin support (Win32 and woman.el)
;;
;;      It is common that Emacs in Win32 environment is coupled with
;;      http://www.cygwin.com toolkit which contains all the manual pages
;;      for the unix commands and possibly new info pages. This package
;;      will locate `cygwin1.dll' file along PATH and recurse whole cygwin
;;      installation root to find new entries that can be used inside
;;      Emacs. In theory this all should happen automatically and the only
;;      thing you have to do is to ensure that you have proper PATH
;;      settings at your OS level before this package is started. If Cygwin
;;      /bin directory in in PATH, `tinypath-:extra-path-root' will get set
;;      to a correct value at boot time.
;;
;;      If you have more places where you keep Unix tools which contain
;;      more manual or info pages, like Reed Kotler
;;      http://www.reedkotler.com/ you _must_ manually set variable
;;      `tinypath-:extra-path-root' to the list of search root directories.
;;      If you set this yourself, you _must_ also include the cygwin
;;      installation root directory
;;
;;          (setq tinypath-:extra-path-root
;;                '("e:/unix-root/cygwin"
;;                  "e:/unix-root/reed-kotler"
;;                  ...))
;;
;;      Package *woman.el* will be configured automatically if it is along
;;      `load-path' to handle manual page viewing with command `M-x'
;;      `man'. Please make sure that you do not destroy the pre-defined
;;      `woman-manpath' in your Emacs startup files with lisp commands or
;;      the efforts to find out new manual pages are thrown off the window.
;;      Search you startup files for anything that looks like `setq',
;;      `defvar', `defconst': (setq woman-manpath ... and change the code
;;      to _add_ to the variable instead:
;;
;;          (require 'cl)
;;          (dolist (path '("one" "two" "three"))
;;            (pushnew (expand-file-name path) woman-manpath :test 'string))
;;
;;  Faster Emacs configuration (Perl emacs-util.pl)
;;
;;      Indication of this feature at startup is following message, where
;;      EXT refers to externally launched process which must be waited
;;      until further processing is done.
;;
;;          TinyPath: EXT Process running ... [may take a while]
;;
;;      As this package evolved and more support was added to various
;;      environments, like Cygwin, which requires traversing hundred of
;;      directories to find out if they contained info or manual pages,
;;      it came evident that Emacs Lisp method is too slow. An alternative
;;      method was developed using Perl language and script *emacs-util.pl*
;;      which can traverse directory hierarchies to find relevant
;;      directories for the setup. This interface is automatically used
;;      if two conditions are met in current environment:
;;
;;      o   Binary *perl* must be along PATH. (according  `executable-find')
;;      o   perl script *emacs-util.pl* must be along PATH. Either copy
;;          the file to suitable place or include tiny-tool kit's /bin
;;          directory to your PATH (This is not the whole story, see
;;          "Internal optimizations").
;;
;;      If all goes well, a `call-process' to the utility script will
;;      return the file hierarchies much faster than the Emacs Lisp ever
;;      could. The difference is that you don't see the traversing progress
;;      as you would if Emacs Lisp did the same thing. The command line
;;      arguments passed to the utility scripts can be found from the
;;      *Message* buffer and you can run the program yourself if you think
;;      that it returns incorrect listing. Print the script help with
;;      following command:
;;
;;          % perl emacs-util.pl --help
;;
;;      Here are some performance statistics of the perl script in action.
;;      (Use --verbose argument to see the statistics)
;;
;;      o   Intel 400MHz, IBM GXP 80G IDE disk, whole Cygwin installation
;;          scan: 3 min 46 sec, dirs: 2373, files: 35 271
;;      o   Same PC, but this time site-lisp directory, subset of Cygwin
;;          hierarchy at /usr/share/site-lisp took:
;;          0 min 13 sec, dirs: 648, files: 8750
;;
;;      Let's consider one scenario that you may encounter if you intend to
;;      use Cygwin similarly as the big brother Linux. Let's suppose that
;;      you have dedicated a disk portion where you intend to duplicate
;;      whole Linux-like directory hierarchy. You have ROOT under which you
;;      keep all the files, including anything that is Cygwin-related.
;;
;;          E:/usr/share/site-lisp      Emacs lisp as outlined earlier
;;          E:/usr/share/site-perl      Perl packages and scripts
;;          E:/usr/share/site-php       PHP code
;;          E:/usr/share/site-cvs       Various other external CVS C-packages
;;
;;      The default heuristics `win32-cygwin-p' should find *cygwin1.dll*
;;      installed and report that Cygwin root is *E:/* This means that
;;      `tinypath-:extra-path-root' will get set for you when package
;;      loads. Suppose further that you have set variable
;;      `tinypath-:load-path-root' to point out suitable locations in
;;      *E:/usr/share/site-lisp*. It would seem that this combination means
;;      that the hierarchies would be traversed multiple times, since the
;;      Cygwin root already includes all the rest:
;;
;;          E:/                             Cygwin root
;;          E:/usr/share/site-lisp/emacs    For this emacs...
;;          E:/usr/share/site-lisp/common   Emacs and XEmacs compatible tree
;;
;;      Don't worry. The Perl utility is smart enough to reduce this to
;;      search only *E:/* and discard other roots as redundant. Hm, what if
;;      other lisp files are found _outside_ of the
;;      *E:/usr/share/site-lisp/*, because it searches every dir starting
;;      from *E:/* Say:
;;
;;          E:/tmp/try/some-file.el
;;
;;      Will the directory *E:/tmp/try/* reported as lisp `load-path'
;;      candidate and added to search list? Yes and no. Yes, it will be
;;      reported, but No, it will not be added to the `load-path' because it
;;      doesn't match the initial user's idea where to look for lisp files. If
;;      you pump up the `tinypath-:verbose' to level 5, you can see PATH-NOK
;;      messages labeled "candidate" to indicate those rejections. Only files
;;      that reside under `tinypath-:load-path-root' directories are counted
;;      in.
;;
;;  Updating running Emacs
;;
;;      Suppose you have downloaded the latest versions of packages X, Y and Z
;;      and you want your current emacs's paths updated. You can do this:
;;
;;          M-x tinypath-cache-regenerate
;;
;;      A bit of skepticism: It is a fortunate event if it all worked that
;;      easily. You see, you already have several packages loaded in your
;;      Emacs and they are using the "old" code. Now you wiped the old
;;      directories away and told Emacs to look for only "new" directories.
;;      After a while you may run into bizarre dependency problems. I
;;      recommend that after any major package update, which contains lot
;;      of files (like Gnus), you:
;;
;;      o    Install package and regenerate cache in current Emacs with
;;           `M-x' `tinypach-cache-regenerate'.
;;      o    Save your  current session (see *desktop.el*, *tinydesk.el*)
;;      o    Quit, and restart Emacs
;;
;;  Compressed lisp file support
;;
;;      IN ORDER TO USE THE FULL COMPRESSION SUPPORT FOR AUTOLOAD FUNCTIONS
;;      AS WELL, SET VARIABLE `tinypath-:compression-support' to symbol
;;      `all'. THE DEFAULT SUPPORT ONLY HANDLES `require' and `load' commands.
;;      You must set this variable before package loads.
;;
;;     Jka-compr and this package
;;
;;      jka-compr has native support to un/compress any file that have
;;      specific extensions. The handling is done via
;;      `file-name-handler-alist' and commands like these will load
;;      properly including any autoloads.
;;
;;          (load "my-el.gz")
;;
;;      The obvious problem is that you have to manually go and change all
;;      you load commands so that they end in .gz so that jka-compr takes
;;      care of loading. What if you later uncompress the file? You have to
;;      go and update all the load commands in you Emacs startup files.
;;      This isn't very nice, since you should be able to un/compress elisp
;;      files whenever you wish and still have permanent statement like one
;;      below. Basically this is what the compression support here is all
;;      about; you don't have to worry if the file is compressed or not
;;      when these advised functions are in effect. The following statement
;;      will always work:
;;
;;          (load "my-el")
;;
;;     How the compressed loading works
;;
;;      o   When user request `load' FILE, try to find some compressed file
;;          that JKA knows about by adding extensions ".gz" and ".Z" and
;;          whatever user has configured JKA to handle. _LIMITATION:_
;;          only .gz .Z .bz2 and the like that compress one file at a time
;;          is currently supported. Don't try using .zip or similar.
;;      o   If the FILE is absolute path, then look from that
;;          directory only.
;;      o   If no directory is given, find the file along the path.
;;      o   If there was somewhere a compressed file, just load it (because JKA
;;          will transparently uncompress it), eval it, and kill the buffer.
;;      o   If NO COMPRESSED file was found, just follow normal
;;          emacs rules.
;;
;;     Note: Why you should not prefer compressed .elc files
;;
;;      The purpose of compression support is to make it possible to
;;      have more useful lisp files in an account that has a limited
;;      disk space (quota).  Keeping lisp files in compressed format
;;      saves quite a much disk space.
;;
;;      o   Plain text, lisp `.el', files may compress better.
;;      o   The documentation in comments is important, e.g all the
;;          instruction to use the file are there. Byte compiling
;;          strips away documentation.
;;      o   In order to debug or send bug reports you have to use .el files,
;;          the .elc files are useless.
;;      o   The performance ratio that the .elc files offer may not
;;          be a crucial factor (many times you couldn't tell).
;;
;;     Note: advised emacs commands
;;
;;      The adviced functions can be further adviced, but
;;      if the redefined function uses `interactive-p' test, it will
;;      not indicate user call (like M-x load-library). The reason why
;;      the advised functions detect it, is that advice.el's
;;      `ad-do-it' macro cannot pass the interactive flag information
;;      to the original functions.
;;
;;  Trouble shooting
;;
;;      There is no denying it, this package is dangerous. When something
;;      goes wrong, it really goes wrong and your Emacs is messed up
;;      completely. So, here are some trouble shooting tips, that you
;;      might want to try to rescue the situation or understand what is
;;      going on. The most usual blame is the *cache* content which does not
;;      contain the correct or up to date information.
;;
;;     Package is not found or loaded?
;;
;;      Please confirm that the file location is known and is in right
;;      directory by calling `M-x' `locate-library'. If the result is
;;      not correct, please check `tinypath-:load-path-root' and
;;      `tinypath-:extra-path-root'. Try to remedy the situation,
;;      regenerate cache with `M-x' `tinypath-cache-regenerate'.
;;
;;     You don't know what particular package is causing troubles
;;
;;      Go to the *Message* buffer and clear it (`C-x' `h' followed by
;;      `C-w'). Run the path generation engine with debug `M-x'
;;      `tinypath-debug-external-helper' and study the output. It may
;;      be ignoring some files that you think should be included. Please
;;      check content of `tinypath-:load-path-ignore-regexp' and
;;      `tinypath-:load-path-ignore-regexp-extra'.
;;
;;     You need to see the internals
;;
;;      Call function `tinypath-cache-file-find-file' to display the current
;;      Cache and use `C-s' and `C-r' to search entries in the file. Remember
;;      that you must not modify this file, because any changes you do, will
;;      get overwritten next time the cache is created. The problem is
;;      somewhere else if you can see incorrect settings in cache file.
;;
;;  Code note: General
;;
;;      Because this package is among the first that is loaded from Emacs
;;      startup file, It contains copies of some functions from TinyLib
;;      libraries, to make the package independent until the point where
;;      the `load-path' has been set up and other libraries are available.
;;      In the code you may find marks "#copy:" which indicate code that
;;      has been copied/simplified to be used here. Autoloads statements in
;;      this package defer loading functions until the end is reached and
;;      `load-path' is determined and the rest of the functions can be
;;      loaded from the libraries.
;;
;;  Code note: Where is that emacs package
;;
;;      If you ever need to know the location of a package that Emacs
;;      would load or has loaded, while this utility is in effect,
;;      use this call:
;;
;;          (insert (tinypath-cache-p "gnus.el"))
;;
;;      In fact the regula call yields same result, because
;;      `locate-library' is adviced:
;;
;;          (insert (locate-library ""gnus.el"))
;;
;;      More easily, if you have tinylisp.el, which takes advantage of
;;      tinypath.el cache, you can load any emacs package for editing
;;      with command:
;;
;;          M-x load-library RET tinylisp RET
;;          M-x tinylisp-library-find-file
;;          (tinypath cache)Lisp Library: gnus.el RET
;;
;;      Alternatively there is mode hot-keys $ l f  and $ l p :
;;
;;          M-x load-library RET tinylisp RET
;;          M-x tinylisp-mode  (in *scratch* buffer, see "E" in modeline)
;;          $ l f
;;          (tinypath cache)Lisp Library: gnus.el RET
;;
;;  Code note: Internal optimizations
;;
;;      In the installation section it is instructed that the location of the
;;      package is pushed into the `load-path' before the package is loaded:
;;
;;          (require 'cl)
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'stringp=)
;;          (load "tinypath.el")
;;
;;      Please follow this instruction. The reason is that program tries to
;;      use most efficient code to boot everything up and the first thing
;;      it can do is to check the location where it has been saved. This
;;      package will use this information assume that the Perl program
;;      emacs-util.pl is available in *~/some/path/bin/emacs-util.pl*. If
;;      that fails, the Perl program is searched along `exec-path'. This is
;;      usually desirable, situation because every new installation include
;;      newer version of emacs-util.pl and the one at `exec-path' may not
;;      be up to date. The perl code will speed up booting compared to pure
;;      Emacs Lisp implementation. In addition the Perl code section in
;;      this file (often referred as "external") has extra features
;;      included.
;;
;;  Code note: *Messages*
;;
;;      This package will print loads of messages to Emacs "*Message*" or
;;      XEmacs " *Message-Log*" buffer. This is a design decisions so that
;;      execution can be easily traced during Emacs load time. It also help
;;      reporting errors. The default `tinypath-:verbose' 3 will log the most
;;      important messages.  Even if you set the level to 0 or nil, still
;;      some messages are displayed. Have a look at Message buffer if you have
;;      not much used it before. You may find interesting information to
;;      debug some of your own mis-configurations, like stale directories
;;      in `exec-path'.
;;
;;  Code note: Custom
;;
;;      If you have Emacs that does not contain *custom.elc* (Yes, it must be
;;      in compiled format, be sure to check), you can download Noah
;;      Friedman's excellent custom emulation package *cust-stub.el* at
;;      http://www.splode.com/~friedman/software/emacs-lisp/ You have to load
;;      it from absolute location before loading this packages like this:
;;
;;          (load "~/elisp/noah/cust-stub")
;;          (load "~/elisp/tiny/lisp/tinypath")
;;
;;  Code note: Insinuating packages
;;
;;      Some packages can be auto-configured when the perl script reads the
;;      contents of the directories. Like package *woman.el* which
;;      needs to know the location of man path directories. For other
;;      packages there are different "installations". Gnus is one
;;      interesting example: Every Emacs and XEmacs release comes with
;;      Gnus version, which is usually outdated and many install Gnus
;;      privately. The problem is multiple Gnus versions in the load
;;      paths and the wished situation is that there would be only the
;;      latest. there is experimental code to find out which of the
;;      Gnus packages along `load-path' is the latest and after making
;;      hopefully right decision (according to gnus-version-*
;;      variable) the other Gnus locations are hidden by modifying
;;      `load-path' and `tinypath-:load-path-ignore-regexp'. This is a
;;      complimentary method
;;	to that suggested in the manual section "3rd party packages".
;;
;;  Code note: Elp profiling results 2001-03-01
;;
;;      The profiling results were run using method below. It must be note,
;;      that the `tinypath-external-*' is the time when the external perl
;;	program examines all the directories, so EXT time is not significant
;;	because it varies from system to system. The
;;      `tinypath-external-setup-parse-data' is the actual time spent in
;;      parsing the returned data. The functions that are called most of the
;;      time are the ones that must be kept on eye on and they seem to
;;      perform very well. Immediate below are the most important functions
;;      that perform the Parsing after the perl has returned results (these
;;      are not from the total listing, but after tweaking)
;;
;;          tinypath-external-output-parse                   1    4.89  4.89
;;            tinypath-external-output-parse-1               5    1.09  0.21
;;            tinypath-external-output-parse-1-cache         1    3.79  3.79
;;
;;
;;          tinypath-external-setup-parse-data               1    5.77  5.77
;;            tinypath-external-setup-1-load-path            249  0.70  0.002
;;            tinypath-external-setup-1-man-path             44   0.0   0.0
;;            tinypath-exec-path-append                      73   0.92  0.012
;;            tinypath-info-handler                          31   8.46  0.27
;;            tinypath-external-setup-cache                  1    0.0   0.0
;;
;;     The timing information was tested and generated with:
;;
;;	o   C-x C-f Load file to buffer.
;;      o   `M-x' `load-library' tinylisp.el
;;      o   `M-x' `turn-on-tinylisp-mode'
;;	o   $ -    to eval current buffer
;;	o   $ e I  to instrument everything
;;	o   `M-x' `tinypath-cache-regenerate'
;;	o   $ e s  to show results
;;
;;
;;          Function Name                                    Count  Elap Ave
;;          ===============================================  ====  ===== =====
;;          tinypath-cache-regenerate                        1     78.11 78.11
;;          tinypath-cache-setup                             1     71.63 71.63
;;          tinypath-cache-setup-scan                        1     66.35 66.35
;;          tinypath-external-setup                          1     65.52 65.52
;;          tinypath-external-helper                         1     56.08 56.08
;;          tinypath-external-helper-call                    1     52.29 52.29
;;          tinypath-info-handler                            31    12.47 0.402
;;          tinypath-info-files-in-directory                 62    10.81 0.174
;;          tinypath-external-setup-parse-data               1     8.23  8.23
;;          tinypath-info-scan-Info-default-directory-list   1     6.48  6.48
;;          tinypath-info-directory-contents-update          31    4.07  0.131
;;          tinypath-external-output-parse                   1     3.79  3.79
;;          tinypath-external-output-parse-1-cache           1     3.63  3.63
;;          tinypath-load-path-clean                         3     3.25  1.083
;;          tinypath-directory-list-clean                    12    2.85  0.237
;;          tinypath-expand-file-name                        1493  2.59  0.001
;;          tinypath-file-extension-compressed               2584  2.47  0.000
;;          tinypath-ti::write-file-variable-state             1     1.38  1.38
;;          tinypath-cache-file-write                        1     1.38  1.38
;;          tinypath-external-setup-1                        1     1.21  1.21
;;          tinypath-path-ok-p                               995   1.10  0.001
;;          tinypath-cache-setup-clear                       1     0.77  0.77
;;          tinypath-external-setup-1-load-path              249   0.70  0.002
;;          tinypath-path-ok-this-emacs-p                    1604  0.38  0.000
;;          tinypath-exec-path-check                         1     0.33  0.33
;;          tinypath-exec-path-check-verbose                 1     0.33  0.33
;;          tinypath-external-output-parse-1                 5     0.16  0.032
;;          tinypath-exec-path-clean                         1     0.16  0.16
;;          tinypath-external-bin-location                   1     0.06  0.06
;;          tinypath-exec-path-append                        73    0.06  0.000
;;          tinypath-executable-find                         1     0.06  0.06
;;          tinypath-Info-default-directory-list-clean       2     0.05  0.025
;;          tinypath-external-setup-1-man-path               44    0.05  0.001
;;          tinypath-emacs-versions                          1     0.0   0.0
;;          tinypath-info-file-DIR                           31    0.0   0.0
;;          tinypath-cache-status-string                     1     0.0   0.0
;;          tinypath-cache-file-name                         1     0.0   0.0
;;          tinypath-emacs-lisp-file-list-cache-clear        1     0.0   0.0
;;          tinypath-cache-status-message                    1     0.0   0.0
;;          tinypath-time-string                             1     0.0   0.0
;;          tinypath-use-compression-maybe                   1     0.0   0.0
;;          tinypath-directory-up                            1     0.0   0.0
;;          tinypath-self-location                           1     0.0   0.0
;;          tinypath-external-setup-cache                    1     0.0   0.0
;;          tinypath-info-handler-DIR                        31    0.0   0.0
;;          tinypath-exec-path-from-path                     1     0.0   0.0
;;
;;  Todo
;;
;;      o   In theory it is possible to load remote files with ange-ftp/EFS in
;;          manner of load-library RET /user@host:/path/to/file but this
;;          has never been tested using this package.
;;      o   It may be possible to add /user@host:/path/to/dir/
;;          to `load-path', but that has never been tested.
;;      o   *xemacs-packages* can be downloaded using CVS. Hm, where would
;;          this cvs download be? Any chance to make auto-boot find that too?

;;}}}

;;; Change Log:

;;; Code:

;;{{{ Require (a)

;;; ......................................................... &require ...

;;  While loading this package XEmacs garbage collects like mad.
;;  Ease it up for a while. These values will be restored at the end.

(unless (get 'gc-cons-threshold 'tinypath-initial)
  (put 'gc-cons-threshold 'tinypath-initial gc-cons-threshold))

(put 'gc-cons-threshold 'tinypath gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024))

;;  Why the provide is at the start of file?
;;  Because XEmacs does not record load-history entry unless it sees
;;  provide statement. In the code there is check of SELF LOCATION
;;  by looking at the `load-history' and it must be immediately available.

(provide   'tinypath)

(eval-and-compile

  (require 'cl)

  ;;  #todo: Mysterious byte compile bug:
  ;;  Remove all cache files, compile tinypath, launch emacs.
  ;;  => Dies with a message of: "function member* not found".

  (unless (fboundp 'member*)
    (autoload 'member* "cl-seq"))


  (or (fboundp 'xemacs-p)   ;; #copy from tinyliba.el (simplistic)
      (defsubst xemacs-p (&optional version-string)
	"If running XEmacs."
	;; Variable `version-string' is not supported.
	(if version-string
	    ;;  Load original definition
	    (error
	     (substitute-command-keys
	      "Please run \\[load-library] tinyliba"))
	  (or (boundp 'xemacs-logo)
	      (featurep 'xemacs)))))

  ;;  [Mostly for Win32 environment]
  ;;
  ;;  If installed site wide, the sysadm can set this variable to
  ;;  t, so that the buffer does not "jump to face".
  ;;  Messages are still logged to the *Message* buffer.
  ;;
  ;;  For regular user, who installs this file, it makes
  ;;  sense to display the message immediately.

  (defvar tinypath-:startup-no-messages nil
    "*If non-nil, do not display error message buffer at startup.")

  (defvar font-lock-mode)   ;; Byte compiler silencers
  (defvar lazy-lock-mode)

  (autoload 'ti::macrof-version-bug-report "tinylib" "" nil 'macro)

  ;; Quiet byte compiler. These are checked with `boundp' in the code

  (defvar Info-default-directory-list)
  (defvar Info-dir-file-attributes)
  (defvar woman-manpath)
  ;; See find-file.el
  (defvar ff-search-directories))

;;}}}
;;{{{ Environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	Basic Environment check and definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-message (msg)
  "Print messages to user."
  (pop-to-buffer (get-buffer-create "*tinypath.el ERROR*"))
  (goto-char (point-min))
  (insert msg);; Insert message first
  ;; Then print to *Messages* buffer as well.
  (message msg))


;;; ----------------------------------------------------------------------
;;;
(defun tinypath-win32-p ();;#copy from tinyliba.el
  "Check if running under Win32 system."
  (cond
   ((memq system-type '(ms-dos windows-nt)));; Emacs
   ((fboundp 'console-type)
    ;; Quiet Emacs byte compiler
    (memq (funcall (symbol-function 'console-type))
	  '(win32 w32 mswindows)))
   ((boundp 'window-system)
    (memq (symbol-value 'window-system) '(win32 w32 mswindows)))
   ((error "Internal alert, contact maintainer of TinyLib."))))


;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment-home ()
  "Check environment: HOME."
  (when (or (not (getenv "HOME"))
	    (not (file-directory-p (getenv "HOME"))))
    (tinypath-tmp-message
     (concat
      "\
** TinyPath.el: [ERROR] HOME variable error set.

   The variable is either a) not set or b) it points to invalid directory.

   An environment variable named HOME must be set so that Emacs knows where to
   read initialization file like $HOME/.emacs. The HOME variable is crucial
   to Emacs functioning and lot of packages depend on its existence.

"
      (cond
       ((tinypath-win32-p)
	"")
       (t
	"\
   Hm. This error should not happen under Unix/Linux system.
   Please recheck your environment and contact your sysadm
   to determine cause of this.")
       (t
	"\
   In Windows Win95/98/NT: Add this statement to your c:\\AUTOEXEC.BAT file
   and reboot the computer.

      set HOME=C:\yourname

   The `yourname' is a directory which you must create and it should not
   contain spaces in the directory name.

   In Windows ME/2000/etc You have to use Start=> Control-Panel=> System
   icon, select `advanced' tab and button `environment' to alter the
   values. Click `apply' and `ok' to make new settings effective.\n\n")))))
  ;;  Return value from function
  (getenv "HOME"))


;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment-user ()
  "Check environment: USER, USERNAME, LOGNAME."
  (let* ((user  (getenv "USER"))
	 (uname (getenv "USERNAME"));; W2k variable
	 (log   (getenv "LOGNAME"))
	 unix-fix
	 win32-fix)

    ;;  In Unix, require that both LOGNAME and USER is correct
    ;;  Different shells snd Unix/Linux systems do not define always
    ;;  both.

    (cond
     ((and user
	   (null log))
      ;; After this, all is ok.
      (setq unix-fix "LOGNAME")
      (setenv "LOGNAME" user))
     ((and log
	   (null user))
      (setq unix-fix "USER")
      (setenv "USER" user)))


    (when (and uname
	       (null user))
      (setq win32-fix "USER")
      (setenv "USER" user))


    ;;  Read variables again, because the above may have updated something

    (setq user  (getenv "USER")
	  uname (getenv "USERNAME")
	  log   (getenv "LOGNAME"))

    (when (and unix-fix
	       (not (tinypath-win32-p)))
      (tinypath-tmp-message
       (format
	(concat
	 "\
** TinyPath.el: [INFO] environment variable %s was `%s'

   Hm. This error should not normally happen in Unix environment, but this
   may be a bash(1) problem, which does not define USER by default.
   Please check you environment by logging in from a fresh terminal. You
   can correct it in your shell's startup file or inform System
   Administrator of your site. Here is an example:

       $HOME/.bashrc:   export USER=$LOGNAME    # If you have $LOGNAME
       $HOME/.tcshrc:   setenv USER foo")
	unix-fix (getenv unix-fix))))

    (when win32-fix
      (tinypath-tmp-message
       (format
	(concat
	 "\
** TinyPath.el: [INFO] environment variable %s set to `%s'

   In this Windows ME/NT/2000 there was variable USERNAME which was copied
   to USER. Note however, that this only sets Emacs environment, and does
   not affect outside environment, so you're adviced to define these
   variables permanetly through Start=> Control-Panel=>
   SystemIcon/Environment tab/

   If you want to set this locally to your Emacs, add following code
   to your startup file at $HOME/.emacs

      ;; \"username\" must contain no spaces. Max 8 characters
      (setenv \"USER\"  \"username\")

   In Windows Win95/98/NT: Add this statement to your c:\\AUTOEXEC.BAT file
   and reboot the computer.

      set USER=johndoe
      set LOGNAME=johndoe


   The `johndoe' is a short, usually maximum of 8 characters, which must
   not contain spaces. The value usually is the same as the HOME path's
   last directory name.

   In Windows ME/2000/etc use Start => Control-Panel => System-Icon and
   select `advanced' tab and `environment' button to alter the values.
   Click `apply' and `ok' to make new environment effective.\n\n")
	win32-fix (getenv win32-fix))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment ()
  "Check environment variables."
  (tinypath-install-environment-home)
  (tinypath-install-environment-user))

;;}}}

;;{{{ Load time functions and macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;	This section must be before variable definitions.
;;	The functions must be available during the variable
;;      initializations, that's why `eval-and-compile' wrapping.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++--  eval-and-compile --

(eval-and-compile

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-byte-compile-running-p ()
  "Return t if byte compiling file."
  (string= (buffer-name) " *Compiler Input*"))

;;; ----------------------------------------------------------------------
;;; Only some values are recorded as messages to the *Messages* buffer
;;; Showing the values possibly makes user think if he needs
;;; to change the defaults.
;;;
(put 'tinypath-set-default-value-macro 'lisp-indent-function 1)
(put 'tinypath-set-default-value-macro 'edebug-form-spec '(body))
(defmacro tinypath-set-default-value-macro (var &rest body)
  "Print verbose messages when activating VAR and run BODY."
  (`
   (let* (val)
     ;;  This may call several functions.
     (setq val (,@ body))
     (unless (tinypath-byte-compile-running-p)
       (message "TinyPath: Default value for `%s' ... %s"
		(, var)
		(prin1-to-string val)))
     val)))


;;; ----------------------------------------------------------------------
;;; #todo: not working right
(defcustom tinypath-:verbose-timing
  (tinypath-set-default-value-macro
      "tinypath-:verbose-timing"
    nil)
  "*If non-nil, dispaly laod time of each `load' `load-library' `require' call.")

;;; ----------------------------------------------------------------------
;;;
(defcustom tinypath-:verbose
  (tinypath-set-default-value-macro
      "tinypath-:verbose"
    3)
  "*If number, bigger than zero, let user know what's happening.
In error situations you can look old messages from *Messages* buffer.
If you want all messages, set value to 10.

If you want killer-logging, select 20. All this will also save
everything to `tinypath-:log-file'."
  :type  '(integer :tag "Verbose level 0..10")
  :group 'TinyPath)

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-verbose-macro 'lisp-indent-function 1)
(defmacro tinypath-verbose-macro (level &rest body)
  "When LEVEL is =< `tinypath-:verbose' run BODY."
  (`
   (when (and (numberp tinypath-:verbose)
	      (or (= (, level) tinypath-:verbose )
		  (< (, level) tinypath-:verbose )))
     (,@ body)
     (when (> tinypath-:verbose 19)
       (tinypath-log-write)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-directory-sep-char-macro 'lisp-indent-function 0)
(defmacro tinypath-directory-sep-char-macro (&rest body)
  "Emacs and XEmacs compatibility.
In let, set `directory-sep-char' to / and run BODY."
  (`
   (let ((directory-sep-char ?/))
     (if (null directory-sep-char)  ;; Byte compiler silencer
	 (setq directory-sep-char nil))
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-expand-file-name (path)
  "Expand filenames always using forward slashes."
 (tinypath-directory-sep-char-macro
  (setq path (expand-file-name path))
  (if (win32-p)
      (setq path (downcase path)))
  path))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-expand-file-name-variable-macro  'lisp-indent-function 0)
(defmacro tinypath-expand-file-name-variable-macro (var)
  "Expand all paths in VAR."
  (`
    (let (list)
      (dolist (path (, var))
	(push (tinypath-expand-file-name path) list))
      (setq (, var) (reverse list)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-message-bug (bug &optional die)
  "Tell how to report BUG (string) and optionally DIE."
  (let* ((msg
	  (substitute-command-keys
	   (concat
	    (format
	     "TinyPath: [ERROR] report bug with name [%s]"
	     bug)
	    " \\[tinypath-submit-bug-report]"))))
    (if die
	(error msg)
      (message msg)
      (sit-for 5))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-up (dir)
  "Return precious DIR."
  (setq dir (file-name-as-directory dir))  ;; Ensure trailing slash
  (when (stringp dir)
    (file-name-directory
     ;; Delete trailing slash
     (substring dir
		0
		(1- (length dir))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-subdirs (dir)
  "Return directories under DIR."
  (let* (list)
    (when (file-directory-p dir)
      (dolist (elt (directory-files dir 'full))
	(if (file-directory-p elt)
	    (push elt list))))
    list))

;;; ----------------------------------------------------------------------
;;; #copy: tinyliba.el
;;;
(or (fboundp 'win32-cygwin-p)
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
      ret)))


;;; ----------------------------------------------------------------------
;;; Earlier XEmacs and Emacs `executable-find' functions are buggy
;;; and do not find binaries correctly, so we use our own implemantation.
;;;
(defun tinypath-executable-find (file)
  "Find FILE along path. FILE must be absolute name with possible .exe
Emacs `executable-find' tries various suffixes in Win32, but this
function just looks if FILE exists along load path."
  (let* (ret name)
    (dolist (path exec-path)
      (setq name (concat (file-name-as-directory path) file))
      (when (and (not (file-directory-p name))
		 (file-exists-p name))
	(setq ret (tinypath-expand-file-name name))
	(return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-executable-find-binary (file)
  "Try finding binary: FILE or FILE.exe in win32."
  (if (win32-p)
      (tinypath-executable-find (concat file ".exe"))
    (tinypath-executable-find file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-versions (&optional noerr)
  "Return possible version numbers for current Emacs. NOERR."
  (interactive)
  (let* ((str (emacs-version))

	 ;;   XEmacs beta has spaces in this variable. Just take
	 ;;   the first word from it. There must be no spaces
	 ;;   in filename returned from this function
	 ;;
	 ;;   emacs-version: "21.2  (beta19) \"Shinjuku\" XEmacs Lucid"

	 (patch          (progn
			   (cond
			    ((string-match "patch \\([0-9]+\\)" str)
			     (match-string 1 str))
			    ;;  XEmacs 21.1  (beta23)
			    ((string-match "(beta\\([0-9]+\\))" str)
			     (match-string 1 str)))))

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

	 ret)

    (dolist (ver (list  version  major-version major-version-x-x))
      (when ver
	(pushnew ver ret :test 'string=)))

    (or ret
	(and (null noerr)
	     (tinypath-message-bug "Can't parse `emacs-version'.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-root-by-other-methods ()
  "Return ROOT of emacs installation directory."
  (let* ((sym  'invocation-directory)
	 ;;  Use `symbol-value' to compile cleanly in all
	 ;;  Emacs and XEmacs versions. It just hides the variable form
	 ;;  Byte compiler
	 (val  (if (and (boundp sym)
			(stringp (symbol-value sym)))
		   (symbol-value sym)))
	 (dir  (and val
		    (file-directory-p val)
		    (file-name-as-directory val))))
    (when dir
      (tinypath-directory-up dir))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-core-path-p (path &optional version)
  "Test if PATH is core Emacs path. VERSION number can be found from path."
  (and
   ;;  PATH name must contain version for this emacs and a
   ;;  subdirectory "lisp"
   (or (and version
	    (string-match (regexp-quote version) path))
       t)
   ;; /usr/local/share/emacs/20.7/site-lisp
   (string-match "[/\\]lisp" path)
   (string-match (concat
		  ;;  Win32 installs emacs-20.4
		  "^.*emacs-[0-9]+\\.+[0-9.-]+"
		  ;;  Unix installs emacs/20.4
		  "\\|^.*emacs[/\\][0-9]+\\.+[0-9.-]+")
		 path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-root-by-load-path ()
  "Return ROOT of emacs installation directory by reading `load-path'.
Return:

   '(matched-part original-path)."
  (let* ((ver (car-safe (tinypath-emacs-versions 'noerr)))
	 ret)
    (if (null ver)
	(tinypath-message-bug "root-by-load-path")
      (dolist (path load-path)
	(when (and (stringp path)
		   (tinypath-emacs-core-path-p path ver))
	  (return
	   (setq ret (list
		      (match-string 0 path)
		      path))))))

    (unless ret
      ;; User has wiped the load-path information by accident,
      ;; Try doing something about it.
      ;;
      ;; #todo: Should we restore part of the path from $EMACSLOADPATH ?
      ;; --> I'm afraid not many set the variable at all
      (let ((path (tinypath-emacs-root-by-other-methods)))
	(if path
	    (setq ret (list path path)))))

    (tinypath-verbose-macro 7
      (message "TinyPath: EMACS ROOT %s" (or (car-safe ret) "<nil>")))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-emacs-root-directory ()
  "Return Emacs installation root directory."
  (car-safe (tinypath-emacs-root-by-load-path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-add-subdirs (root)
  "Add all subdirectories of ROOT to `load-path'."
  (dolist (subdir (tinypath-directory-subdirs root))
    ;;  the function `tinypath-expand-file-name' converts forward slashes
    ;;  to backward slashes.
    (setq subdir
	  (tinypath-expand-file-name subdir))
    (unless (string-match "CVS\\|RCS\\|info\\|texi" subdir)
      (tinypath-verbose-macro 9
	(message "TinyPath: add subdir %s" subdir))
      (pushnew subdir load-path :test 'string=))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-default-load-path-root-user ()
  "Return user's Emacs Lisp path by guessing various directories."
  (flet ((msg (m)
	      (message m)
	      (unless tinypath-:startup-no-messages
		(sit-for 3))
	      nil))
    (if (null (getenv "HOME"))
	(msg "TinyPath: Environment variable HOME not set.")
      (let* (ret)
	(dolist (dir (list
		      "~/elisp"
		      "~/lisp"
		      "~/.elisp"
		      "~/.lisp"
		      (if (xemacs-p)
			  "~/.xemacs")))
	  (when (and (stringp dir)
		     (file-directory-p dir))
	    (setq ret dir)))
	(unless ret
	  ;;  Try to scan all of home for lisp. Hm, Ugh.
	  ;;  Wow, if this is first time user with HOME set to c:/
	  (msg "TinyPath: [ERROR]  Please create $HOME/elisp"))
	ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-default-load-path-root-dirs ()
  "Find default directories for `tinypath-:load-path-root'."
  (let (list)
    (dolist (dir
	     (list
	      (tinypath-default-load-path-root-user)

	      ;;  site wide configuration
	      ;;  #todo: where is XEmacs installed by default?
	      (if (not (xemacs-p))
		  (concat
		   "/usr/local/share/emacs/"
		   (if (string-match "[0-9]+\\.[0-9]+" emacs-version)
		       (match-string 0 emacs-version)
		     "")
		   "/lisp"))

	      "/usr/local/share/site-lisp"
	      "/opt/share/site-lisp"))
      (when (stringp dir)
	(message "TinyPath: default tinypath-:load-path-root => %s %s"
		 dir
		 (if (file-directory-p dir)
		     "OK"
		   "NOT EXIST"))
	(if (file-directory-p dir)
	    (push dir list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-directory-search (dir list &optional verb bug)
  "Search DIR in the hierarchy of directories upward.

Input:

  DIR	    Directory to search. This can be nil.

  LIST	    List of possible search directories.
            -- A simple string means absolute location/DIR
            -- Directory enclosed in (dir count) means that the directory is
	       also searched `count' levels upward.
            -- Directory enclosed in (dir 'abs) means absolute location
               without using parameter DIR.

	    '(/dir1 (/some/more/of/dir2 2) (/this/location abs)  /dir3 ..)

	    Choices searched are:

	    /dir1/DIR
	    /some/more/of/dir2/DIR
	    /some/more/of/DIR
            /this/location
	    /dir3/DIR

  VERB     Verbose messages.
  BUG      If set, and DIR not found, call `tinypath-message-bug'."
  (let* (found)
    (flet ((check-dir
	    (try dir)
	    (setq try (tinypath-expand-file-name
		       (concat (file-name-as-directory try)
			       dir)))
	    (if verb
		(message "TinyPath: directory search ... %s" try))
	    (when (file-directory-p try)
	      (if verb
		  (message "TinyPath: directory search ... found %s" try))
	      try)))

      (or dir
	  (setq dir ""))

      (dolist (try list)
	(cond
	 ((stringp try)
	  (if (setq found (check-dir try dir))
	      (return)))
	 ((listp try)
	  (multiple-value-bind (path count) try
	    (cond
	     ((and (stringp path)
		   (eq count 'abs))
	      (if (setq found (check-dir path dir))
		  (return)))
	     ((and (stringp path)
		   (integerp count))
	      (while (and (stringp path)
			  (not (zerop count))
			  (> count 0))
		(if (setq found (check-dir path dir))
		    (return))
		(decf count)
		(setq path
		      (tinypath-directory-up path)))))))))

      (cond
       (found       ;;#todo: anything to do here?
	t)
       (t
	;;  Hope people that have it in non-standard locations
	;;  will tell it to maintainer.
	(when (and verb bug)
	  (message "TinyPath: [WARNING] %s not found." dir)
	  (tinypath-message-bug
	   (format "Directory lookup fail %s" dir)))))

      found)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-initial-value (&optional dir-list)
  "Add Emacs installation lisp directories to `load-path'.
This is solely used for booting up tinypath.el package, so that
`require' commands can be satisfied. Without the core packages available
in `load-path' it is not possible to use Emacs.

The DIR-LIST is location of additional directories to consider
Emacs core-lisp installation directories."
  (let* ((root-base (tinypath-emacs-root-directory))
	 (dir-p     (and root-base
			 (file-directory-p root-base)))
	 root)

    (message "TinyPath: load-path auto-boot (Emacs install dir)... %s"
	     (if root-base
		 root-base
	       "[can't find Emacs install root]")
	     (if dir-p
		 "(dir nok)"
	       "(dir ok)"))

    (when (and root-base
	       dir-p)

      (message "TinyPath: load-path auto-boot [running]")

      (setq root-base (file-name-as-directory root-base))

      ;;  Make ROOT/lisp directory. This is the same for all
      ;;  Emacs versions. Win32 conversion to lowercase

      (setq root (tinypath-expand-file-name (concat root-base "lisp")))

      ;; This is just ultimate safeguard. We did find the
      ;; root, but that doesn't mean it is included in the `load-path'
      ;; E.g. there may be directories /ROOT/lisp/something
      ;;
      ;; It is still possible that member fails, because
      ;;
      ;; - Win32 can have mixed case paths, C:/ and c:/ are
      ;;   different to pushnew
      ;; - Win32 slashes c:\ c:/ confuse pushnew.
      ;;
      ;; These will be handled in the final install phase,
      ;; see function `tinypath-load-path-clean'

      (unless (or (member root load-path)
		  (member (file-name-as-directory root) load-path))
	(pushnew root load-path :test 'string=)
	(message "TinyPath: load-path auto-boot [%s added]." root))

      ;;  We might have included this line inside the above `unless',
      ;;  after `pushnew' but we do not do that. It's not a guarantee
      ;;  that subdirectories are there if ROOT was there.
      ;;  => run this to be absolutely sure. Will take only fraction
      ;;  of seconds.

      (tinypath-load-path-add-subdirs root)

      (unless (xemacs-p)
	(message "TinyPath: bbooting emacs standard lisp paths.")
	(tinypath-load-path-add-subdirs root))

      ;;  But that's not quite all. XEmacs does not include all of its
      ;;  packages in the standard installation any more, but in
      ;;  a huge archive called "SUMO", which contains subdirectory
      ;;  xemacs-packages. We have no way of knowing where that
      ;;  directory has been unpacked, but try few guesses anyway.
      ;;
      ;;  Look around XEmacs installation directory.

      (when (and (xemacs-p)
		 (boundp 'emacs-major-version)
		 (> (symbol-value 'emacs-major-version) 20))
	(message "TinyPath: load-path auto-boot [XEmacs] ...")
	(let* (found
	       xemacs-packages)

	  ;;  Search under standard location
	  ;;  XEmacs/xemacs-packages  or XEmacs/XEmacs-21.2/xemacs-packages

	  (dolist (lisp '("xemacs-packages"
			  "mule-packages"
			  "site-packages"))
	    (setq lisp (concat lisp "/lisp"))
	    (when (setq found
			(tinypath-directory-search
			 lisp
			 (list (list root-base 3))
			 'verb
			 'bug))
	      (if (string= lisp "xemacs-packages/lisp")
		  (setq xemacs-packages 'found))
	      (tinypath-load-path-add-subdirs found)))

	  ;; Still not found, try few more alternatives. This time
	  ;; we only try to find the core xemacs-packages-

	  (unless xemacs-packages
	    (when (setq found
			(tinypath-directory-search
			 "xemacs-packages/lisp"
			 (list
			  ;;  The first is historical location
			  ;;  In a vanilla-configured XEmacs
			  '("/usr/local/lib/xemacs/" abs)
			  '("~/.xemacs-packages/lisp" abs)
			  '("~/xemacs-packages/lisp" abs)
			  '("~/site-lisp/xemacs-packages/lisp" abs)
			  '("~/lisp/xemacs-packages/lisp" abs)
			  '("~/elisp/xemacs-packages/lisp" abs))
			 'verb
			 'bug))
	      (tinypath-load-path-add-subdirs found)))
	  (message "TinyPath: load-path auto-boot [XEmacs]... done.")))
      (if dir-list
	  (tinypath-load-path-add-subdirs dir-list))
      (message "TinyPath: load-path auto-boot... done."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-find-root-home ()
  "Return suitable root user HOME directory. /home/root etc."
  (let (ret)
    (dolist (path '("/home/root"
		    "/users/root"
		    "/"))
      (when (file-directory-p path)
	(message "TinyPath: tinypath-tmp-find-root-home [%s]" path)
	(setq ret path)
	(return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-find-writable-dir (&optional file)
  "Find writable directory and append FILE to it. Only used at startup.
This function sets initial values for variable
`tinypath-:cache-file-prefix'.

User should `setq' this variable before calling tinypath.el

References:

  `tinypath-:cache-file-prefix'
  `tinypath-:load-path-dump-file'"
  (let ((root-home   (tinypath-tmp-find-root-home))
	(root-user-p (and (not (win32-p))
			  (string= (expand-file-name "~") "/")))
	(user        (or (getenv "USER")
			 (getenv "LOGNAME")
			 (let ((home (expand-file-name "~")))
			   (if (string-match "\\([^/\\]+\\)$" home)
			       (match-string 1 home)))
			 ""))
	ret)

    (if (not (file-directory-p "~/tmp"))
	(message "TinyPath:   **   PLEASE CREATE $HOME/tmp directory."))

    (dolist (dir '("~/elisp/config/"
		   "~/elisp/conf/"
		   "~/lisp/config/"
		   "~/lisp/conf/"
		   "~/tmp/"
		   "~"
		   "/tmp/"
		   "/var/tmp/"
		   "c:/temp/"
		   "c:/tmp/"
		   "c:/"))

      ;; The ROOT user is special case. (expand-file-name "~")
      ;; will return plain "/".
      ;; check if SysAdm is up to date his tasks and has created
      ;; /home/root or /users/root directory.

      (cond
       ((and root-user-p
	     (string-match "~" dir))
	(setq dir
	      (if (string= root-home "/")
		  ;; ~  =>  ""
		  (replace-match "" nil nil dir)
		;; ~/tmp =>  /home/root/tmp
		(replace-match root-home nil nil dir))))
       (t
	(setq dir (file-name-as-directory
		   (expand-file-name dir)))

	(when (and (file-directory-p dir)
		   (file-writable-p
		    (concat dir
			    (or file "###tinypath.el-test###"))))

	  ;; In multi-user environment, we must say /tmp-USER-file

	  (when (string= dir "/tmp/")
	    (setq dir (concat dir "-" user "-" )))

	  (setq ret (concat dir (or file "")))
	  (return)))))

    ;;  Last thing to do. If User has set his HOME to point to
    ;;  C:/, that is not a good idea. Move cache file under C:/TEMP

    (when (and (string-match "^[Cc]:[/\\]?$" ret)
	       (file-directory-p "C:/temp"))
      (message
       "TinyPath: [WARNING] find-writable-dir Using c:/temp instead of c:/")
      (setq ret "c:/temp"))

    (if ret
	ret
      (error "TinyPath: Can't find writable directory for %s" file))))

;;; ----------------------------------------------------------------------
;;;
(or (fboundp 'win32-p) ;; #copy: from tinyliba.el
    (defsubst win32-p ()
      (cond
       ((fboundp 'console-type)
	(memq (let ((f 'console-type))
		(funcall f))
	      '(win32 w32 mswindows)))
       ((boundp 'window-system)
	(memq (symbol-value 'window-system)
	      '(win32 w32 mswindows)))
       ((error "Internal alert, contact maintainer of TinyLib.")))))

) ;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- eval-and-compile +--

;;}}}
;;{{{ variables

;;; ......................................................... &v-hooks ...

(defcustom tinypath-:load-hook '(tinypath-install)
  "*Hook run when package is loaded.
Please make sure that this hook contains function `tinypath-install'
or nothing will be set up to Emacs when you load tinypath.el."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath-:load-path-function 'tinypath-load-path-setup
  "*Function define all additional paths to the `load-path'."
  :type  'function
  :group 'TinyPath)

(defcustom tinypath-:report-mode-define-keys-hook
  '(tinypath-report-mode-default-bindings)
  "*List of functions to run which define keys to `tinydesk-mode-map'."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath-:report-mode-hook nil
  "*Hook run after the `tinypath-report-mode' is turned on."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath-:cache-duplicate-report-hook nil
  "*Hook run after the `tinypath-cache-duplicate-report' function.
The point is at the beginning of `tinypath-:report-buffer' when
the hook is run."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath-:load-path-ignore-regexp-hook  nil
  "*Hook run after the `tinypath-:load-path-ignore-regexp' is defined.
You can use this to add more ignore regexps to the default value.
See Manual for the details M-x tinypath-version and \"Gnus\"."
  :type  'hook
  :group 'TinyPath)

;;; ........................................................ &v-public ...
;;; User configurable


(defcustom tinypath-:load-path-accept-criteria t
  "*Control which incarnation of the installed package is respected.
When Emacs is installed, it contains many packages that are still
updated and you can find more up to date version from developer's site.

Example: cperl-mode.el

  Take for example cperl-mode.el which is avalable at
  http://cpan.perl.org/modules/by-authors/Ilya_Zakharevich/cperl-mode/

  The package is installed in Emacs kit at location:

      <root>/emacs-20.7/lisp/progmodes/cperl-mode.el

  For ystem wide installation, more up to date package could
  be found at:

      /usr/share/site-lisp/net/users/zakharevich-ilya/cperl-mode.el

  and private user may keep the package in

     ~/elisp/cperl-mode.el

Which package loads?

  nil		First one that is in `load-path', when the cache was built.
		See `tinypah-cache-problem-report'.

  t             Choose package under $HOME, or one at site wide or
                last the Emacs installation.

  function      If this is a callable function, pass LIST of paths
		to it to choose the correct package. Function must
		return string: path  or nil.")


(defcustom tinypath-:compression-support 'default
  "*Type of compression support: 'default, 'all or 'none.

'default

    Files ending to .gz and .bz2 files are counted in when
    a load command is issued.

'all

    In addition to 'default, also autoloaded functions can be found from
    compressed files. This means that statements like these will work:

    (autoload 'jka-compr \"jka-compr\")

    The recommendation is that you set this value to 'all if you keep your lisp
    files in compressed format to save space.

'none

    Do not use cache at all. Use this is the cache is broken. In Total
    emergency, call M-x -1 `tinypath-cache-mode' to disable all advises.

This value must be set once, before package is loaded. Changing it afterwards
has no effect."
  :type '(choice (const default)
		 (const all)
		 (const none))
  :group 'TinyPath)

(when (and (boundp 'command-line-args)
	   (member "-debug-init" (symbol-value 'command-line-args)))
  (put 'tinypath-:verbose 'debug-init tinypath-:verbose)
  (message "tinypath: VERBOSE 10 -debug-init")
  (setq tinypath-:verbose 10))

(defcustom tinypath-:cache-expiry-days
  (tinypath-set-default-value-macro
      "tinypath-:cache-expiry-days"
    14)
  "*How many days until expiring `load-path' cache and rescan paths.
If set to nil; do not use cache feature, but scan directories at startup."
  :type 'integer
  :group 'TinyPath)

(defcustom tinypath-:report-mode-name "TinyPathReport"
  "*The name of the `tinypath-report-mode'."
  :type  'string
  :group 'TinyPath)






(eval-and-compile

  (defun tinypath-cygwin-p ()
    "Return Cygwin installation root if Cygwin is along PATH."
    (let ((cygwin-p
	   (cond
	    ((locate-library "executable-find")
	     (autoload 'executable-find "executable-find")
	     ;;  Should be in /bin/cygrunsrv.exe
	     ;;  The funcall just hides this from idiot byte compiler
	     ;;  Which doesn't see autoload definition.
	     (funcall (symbol-function 'executable-find) "cygrunsrv"))
	    ((let (file)
	       (dolist (dir exec-path)
		 (setq file
		       (concat (file-name-as-directory dir)
			       "cygrunsrv.exe"))
		 (if (file-exists-p file)
		     (return file))))))))
      (when cygwin-p
	;;  X:/SOME/PREFIX/bin/cygrunsrv.exe => X:/SOME/PREFIX/
	(when (string-match "^\\(.*\\)/[^/]+/" cygwin-p)
	  (match-string 1 cygwin-p)))))


  (defun tinypath-info-default-path-list ()
    "Return default Info path candidate list."
    (let ((cygwin-p (tinypath-cygwin-p))
	  (list
	   '("/usr/info"
	     "/usr/local/info"
	     "/usr/info/"
	     "/doc/info"
	     "/usr/share/info"
	     "/opt/info"
	     "/opt/share/info"))
	  ret)
      ;;  Add more default info paths to search
      (when cygwin-p
	(dolist (elt '("usr/info"  "usr/local/info"))
	  (push (concat (file-name-as-directory cygwin-p)  elt) list)))
      ;;  Drop non-existing directories
      (dolist (elt list)
	(when (file-directory-p elt)
	  (push elt ret)))
      ret))

(defcustom tinypath-:Info-default-directory-list
  (tinypath-info-default-path-list)
  "*Additional INFO directories to check for inclusion.
Any new entries in these directories are checked and
fixed and added to `Info-default-directory-list'."
  :type '(list directory)
  :group 'TinyPath))  ;; eval-and-compile end

(message "TinyPath: [VAR] tinypath-:Info-default-directory-list %s"
	 (prin1-to-string tinypath-:Info-default-directory-list))






;;  We can't use `ti::package-config-file-prefix' NOW, because the tinylibm.el
;;  is not yet loaded - `load-path' is not yet know for sure.
;;
;;  #todo: this is hard coded location. If Emacs ever defines similar function
;;  #todo: then we can start using it to put config files to common place.

(defcustom tinypath-:compressed-file-extensions
  (delq
   nil
   (list
    ;;  The symbol-function is only a byte compiler silencer.
    (if (tinypath-executable-find-binary "bzip2")    ".bz2")
    (if (tinypath-executable-find-binary "gzip")     ".gz")
    (if (tinypath-executable-find-binary "compress") ".Z")))
    ;; '(".gz" ".bz2" ".Z"))
  "*List of supported compressed file extensions.
The default list is built dynamically by checking the binary in `exec-path'.
The default list is:

\(setq tinypath-:compressed-file-extensions '( \".gz\" \".bz2\" \".Z\"))"
  :type  '(list  string)
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath-:compressed-file-extensions %s"
	 (prin1-to-string tinypath-:compressed-file-extensions))

(defcustom tinypath-:cache-file-prefix
  ;;
  ;; Can't use `ti::package-config-file-prefix', because the library
  ;; is not loaded yet. USER MUST SETQ THIS VARIABLE
  ;;
  (tinypath-set-default-value-macro
      "tinypath-:cache-file-prefix"
    (tinypath-tmp-find-writable-dir "emacs-config-tinypath-cache"))
  "*File where to store `tinypath-:cache'. See `tinypath-:cache-file-postfix'.
This is only a prefix for filename. The whole filename is returned by
function `tinypath-cache-file-name' which appends emacs version id after
this prefix string.

An example:  /home/some/elisp/config/tinypah-cache-"
  :type  'string
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath-:cache-file-prefix %s"
	 (prin1-to-string tinypath-:cache-file-prefix))

(defcustom tinypath-:cache-file-hostname-function nil
  "*Function to return HOST for the cache file name.
You're interested on this variable only if you're running several networked
machines and 1) you always have same, ONE mounted $HOME directory 2) and
each machine has its own run-files, like site-lisp.

Use nil or this code to prevent using hostname in cache file name.
(= shared, common, site-lisp)

  (setq tinypath-:cache-file-hostname-function '(lambda ()  \"\"))

To activate the hostname portion in cache name, set variable to like this:
(= each machine has its oen site-lisp)

  (setq tinypath-:cache-file-hostname-function 'tinypath-cache-hostname)

See manual M-x tinypath-version for more information."
  :type  'function
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath-:cache-file-hostname-function %s"
	 (prin1-to-string tinypath-:cache-file-hostname-function))

;;  We select the compressed file to save space if we can detect gzip
;;  in this environment.

(defcustom tinypath-:cache-file-postfix
  (if t
      ".el"
    ;; 2000-01 Disabled for now
    ;; While saving space may be good idea, don't make this default, but
    ;; let user turn on the compressions by setting this variable.
    ;; The reason I left this out is speed and jka: jka gets loaded
    ;; if we set compression on and that may slow down Emacs if all
    ;; user needs is Emacs for a while without extra features.
    (if (tinypath-executable-find-binary "gzip")
	".el.gz"
      ".el"))
  "*Extension for `tinypath-:cache'. See also `tinypath-:cache-file-prefix'.
Normally \".el\"  but to save space you could set this to \".el.gz\"."
  :type  'string
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath-:cache-file-postfix %s"
	 (prin1-to-string tinypath-:cache-file-postfix))

(defcustom tinypath-:load-path-dump-file
  ;;
  ;; Can't use `ti::package-config-file-prefix', because the library
  ;; is not loaded yet. USER MUST SETQ THIS VARIABLE
  ;;
  (tinypath-tmp-find-writable-dir "emacs-config-tinypath-dump.el")
  "*Where to store dumped load path. See `tinypath-load-path-dump'."
  :type  'file
  :group 'TinyPath)

(defcustom tinypath-:cache-duplicate-report-ignore-functions
  '(tinypath-cache-duplicate-report-ignore-function)
  "*Functions called with FILE. Return t to ignore FILE in duplicate report.
Called from function `tinypath-cache-duplicate-report'."
  :type  'function
  :group 'TinyPath)

(message
 "TinyPath: [VAR] tinypath-:cache-duplicate-report-ignore-functions %s"
 (prin1-to-string
  tinypath-:cache-duplicate-report-ignore-functions))

(defcustom tinypath-:ignore-file-regexp nil
  "*Prohibit loading lisp file if regexp matches absolute path.
If \"\\\\.elc\" ignore all compiled files and load only source files.

This regexp is matched against absolute filename being loaded and
if it matches, the file is ignore. An error is signaled
if there is no single choice available after exclude.

There may be reasons why you would always load only the non-compiled
version and ignore compiled versions:

--  You are developing packages or debugging packages and you
    want your Emacs to load only non-compiled versions. The *Backtrace*
    buffer output is more sensible with non-compiled functions.

    ==> Setting value to \".\" will ignore all compiled files.

--  You have share some site-lisp files with Emacs and XEmacs, but
    you primarily use GNU Emacs and the compiled files are for it.
    XEmacs must not load the compiled versions.

    ==> Set this regexp in your $HOME/.emacs when XEmacs is loaded, to
    match the directory part of file which is located in shared lisp
    directory for Emacs and Xemacs."
  :type 'regexp
  :group 'TinyPath)

(defcustom tinypath-:manpath-ignore-regexp
  "terminfo"
  "*Regexp to exclude directories for MANPATH additions.
It really isn't very serious if MANPATH contains few faulty directories,
do don't worry. You can see the final results in `tinypath-:extra-manpath'."
  :type  'regexp
  :group 'TinyPath)

(defcustom tinypath-:exec-path-ignore-regexp nil
  "*Regexp to exclude directories for `exec-path' additions.
The automatic Perl utility will find every directory under
`tinypath-:extra-path-root' which contain executable files and them to
`exec-path. Set this variable to ignore certain directories."
  :type  'regexp
  :group 'TinyPath)

(defcustom tinypath-:load-path-ignore-regexp
  (concat
   "[/\\]"     ;; windows or unix dir separator start
   "\\("       ;; START grouping

   ;;	Skip Distributed help files

   "tex\\(i\\|info\\)$"
   "\\|doc"

   ;;	Skip Other directories

   "\\|RCS\\|CVS\\|zip"

   ;; Skip temporary directories /T/ /t/ /tmp* /temp*

   "\\|/[Tt][/\\]\\|te?mp"

   (if (xemacs-p) 		;EFS doesn't work in Emacs
       ""
     "\\|efs")

   ;;  20.x has custom lib, so we don't want to install private
   ;;  custom.el copy that we used for 19.x Emacs
   ;; (if (> emacs-major-version 19) "\\|custom" "")

   ;;  Do not use TM in latest Emacs. Gnus and VM has MIME handling.
   ;;  SEMI might be ok.

   ;; (if (> emacs-major-version 19) "\\|tm/\\|tm-[^/]+" "")

   "\\)")
  "*Regexp to match directories which to ignore. Case sensitive.
If `tinypath-:load-path-ignore-regexp-extra' is string, it is appended ONCE
to this default regexp."
  :type  '(string :tag "Regexp")
  :group 'TinyPath)

(defvar tinypath-:load-path-ignore-regexp-extra nil
  "*String to add to `tinypath-:load-path-ignore-regexp'.
Remember to start the regexp with OR-statement \\\\| because the regexp
is added to existing value.

Value of this regexp is added every time the file is loaded.
See Manual for explanation: M-x tinypath-version and \"Gnus\".")

;; Appending to default value is easiest this way.

(when (and (stringp tinypath-:load-path-ignore-regexp)
	   (stringp tinypath-:load-path-ignore-regexp-extra))
  (setq tinypath-:load-path-ignore-regexp
	(concat tinypath-:load-path-ignore-regexp
		tinypath-:load-path-ignore-regexp-extra)))

;;  Experienced users have a chance to add more regexps to the variable

(run-hooks 'tinypath-:load-path-ignore-regexp-hook)

(message "TinyPath: [VAR] tinypath-:ignore-file-regexp %s"
	 (prin1-to-string tinypath-:ignore-file-regexp))

(eval-and-compile  ;;  Needed at boot-time.
(defcustom tinypath-:core-emacs-load-path-list nil
  "*Location of core Emacs lisp directories.

Setting this variable is mandatory if the initial `load-path'
in Emacs startup does not contain core lisp packages.

Emacs:

    In Emacs, this would be directory where core lisp files
    reside, typically /usr/share/emacs/NN.N/lisp.

XEmacs:

    In XEmacs, you would add the location of
    xemacs-packages, mule-packages and site-packages or in older versions
    /usr/lib/xemacs-NN.N/lisp/

   you do not need to set this variable for XEmacs, because the automatic boot
   up will find the core packages provided that packages have been
   installed at the same level as the XEmacs itself:

       XEmacs/xemacs-NN.N/
       XEmacs/site-packages/
       XEmacs/mule-packages/
       ..."
  :type 'directory
  :group  'TinyPath))

(message "TinyPath: [VAR] tinypath-:core-emacs-load-path-list %s"
	 (prin1-to-string tinypath-:core-emacs-load-path-list))

(defcustom tinypath-:load-path-root
  (tinypath-set-default-value-macro
      "tinypath-:load-path-root"
    (tinypath-default-load-path-root-dirs))
  "*List of root directories of Emacs lisp packages.
Put list all lisp package installation roots here, like

 (setq tinypath-:load-path-root
   (list
    (if (not (xemacs-p))
       ;; This is for Emacs only
        \"/usr/local/share/emacs/site-lisp\")
     \"/usr/local/share/site-lisp\"
     \"/opt/share/site-lisp\"
     ;; or ~/lisp
     \"~/elisp\")

Non-existing directories do no harm, because every
element that is not a string and a valid directory is ignored."
  :type  '(list directory)
  :group 'TinyPath)

(defcustom tinypath-:extra-path-root
  (tinypath-set-default-value-macro
      "tinypath-:extra-path-root"
    (let* ((path (win32-cygwin-p)))
      (if path
	  (list path))))
  "*Win32 Cygwin installation root or other search directories.
This is list of directories.

In many times people working with Emacs also install http://www.cygwin.com/
Unix environment, which contains manual pages and info files for the
utilities.

Set this variable to LIST of additional search root directories
for manual pages and info files."
  :type  '(list directory)
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath-:extra-path-root %s"
	 (prin1-to-string tinypath-:extra-path-root))

;;; ....................................................... &v-private ...

(defvar tinypath-:original-load-path load-path
  "Original load-path of emacs before loading this package.
It is used later in \\[tinypath-cache-regenerate]. DO NOT TOUCH.")

(defvar tinypath-:log-file
  (tinypath-tmp-find-writable-dir "emacs-tinypath.el.log")
  "With `tinypath-:verbose' set to 20, the message buffer
is constantly written to disk. Prepare, everything will take oodles
of time...")

(defvar tinypath-:external-data-structure nil
  "Whole data structure from external tool. See `tinypath-external-setup'.
Do not touch. This is highly important for debugging purposes.")

(defvar tinypath-:extra-manpath nil
  "Additional paths found. See `tinypath-:extra-path-root'.")

(defvar tinypath-:extra-ff-search-directories nil
  "Additional C/C++ include paths found. See `tinypath-:extra-path-root'")

(defvar tinypath-report-mode-map nil
  "Keymap for buffer generated by `tinypath-cache-duplicate-report'.")

(defvar tinypath-:cache nil
  "List of all lisp files along `load-path'.
\((\"file\" (POS . PATH)) .. ).")

(defvar tinypath-:time-data nil
  "When each package is loaded, its load time is recoded here.
See `tinypath-time-display'. The data structure is ((package . time-sec)).")

(defvar tinypath-:time-buffer "*tinypath-time-results*"
  "Buffer to put results of `tinypath-time-display'.")

(defvar tinypath-:cache-level-two nil
  "Cache of tinypath-:cache. It keeps the files already resolved by
consulting the cache. Its aim is to speed up the search.")

(defvar tinypath-dumped-load-path nil
  "Load path with Disk Drive letters. See `tinypath-load-path-dump'.")

(defvar tinypath-:cache-mode nil
  "State of `tinypath-cache-mode'. DO NOT CHANGE THIS VARIABLE DIRECTLY.
There is more than just changing this variable's state.
Use function `tinypath-cache-mode' which modifies everything needed.")

(defvar tinypath-:report-buffer "*tinypath-report*"
  "*Buffer where to report e.g. after `tinypath-cache-duplicate-report'.")

(defvar tinypath-:timer-elt nil
  "Timer process.")

(defvar tinydesk-mode-map nil
  "Local keymap for STATE files loaded by edit.")

(defconst tinypath-:font-lock-keywords
  (list

   ;; filenames

   (list
    (concat
     "[0-9][0-9]:[0-9][0-9]:[0-9][0-9][ \t]+"
     "\\(.*\\)")
    1 'font-lock-reference-face)

   (list
    (concat
     "^[ \t]+[0-9]+[ \t]+"
     "\\([0-9]+\\)")
    1 'font-lock-variable-name-face)

   (list
    "ERROR:"
    0 'font-lock-constant-face)

   ;; filename heading at the start of the line

   (list
    "^[^ \t\r\n]+"
    0 'font-lock-string-face)

   (list                   ;; mark deleted files
    "^[*].*"
    0 'font-lock-comment-face t))
  "*Font lock keywords for the `tinypath-:report-buffer' buffer.")

(defvar tinypath-:external-util-bin "emacs-util.pl"
  "*External utility to help finding Emacs boot up information.
DO NOR TOUCH THIS VARIABLE unless you rename the utility.
See M-x tinypath-version (manual) for more information.")

;;}}}

;;{{{ Require (b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The require statements are unconventionally put here and not to the
;;  beginning of file, because sometimes Win32
;;  XEmacs development betas do not have correct `load-path' and
;;  require `advice' and `jka-compr' would fail.
;;
;;  At this point the load-path has been partially fixed (that is: booted)
;;  and we can run the `require' commands.
;;
;;  The files can be in compressed format as well.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  ;;  We MUST run this at compile time too, because in XEmacs
  ;;  it will make loading custom.elc possible. Without it, the
  ;;  defcustomed variables give errors
  (when (or (not (tinypath-byte-compile-running-p))
	    (and (tinypath-byte-compile-running-p)
		 (boundp 'xemacs-logo)))
    (tinypath-load-path-initial-value tinypath-:core-emacs-load-path-list)))

(require 'pp)
(require 'advice)
(require 'info)
(require 'executable)
(require 'assoc)

;;}}}

;;{{{ Macros

;;; ########################################################## &macros ###

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-Info-default-directory-list ()
  "Emacs and XEmacs compatibility."
  (if (boundp 'Info-directory-list)         ;; XEmacs
      (intern "Info-directory-list")
    (intern "Info-default-directory-list")))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-Info-default-directory-list-sym ()
  "Emacs and XEmacs compatibility."
  (`
   (if (boundp 'Info-directory-list)         ;; XEmacs
       (intern "Info-directory-list")
     (intern "Info-default-directory-list"))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-message-log-max-sym ()
  "Emacs and XEmacs compatibility."
  (`
   (cond
    ((boundp 'log-message-max-size)  ;; XEmacs
     (intern "log-message-max-size"))
    ((boundp 'message-log-max)
     (intern "message-log-max"))
    (t
     (error "tinypath-message-log-max-sym")))))


;;; ----------------------------------------------------------------------
;;; #copy: from tinyliba.el
(defmacro tinypath-ti::bool-toggle (var &optional arg)
  "Toggle VAR according to ARG like mode would do.
Useful for for functions that use arg 0/-1 = off, 1 = on, nil = toggle.
Minor modes behave this way.

VAR is set to following values when ARG is:

  arg 0/-1  VAR -> nil
  arg nbr   VAR -> t
  arg nil   VAR -> not(var)     toggles variable"
      (` (setq (, var)
	       (cond
		((and (integerp (, arg))
		      (< (, arg) 1))	;Any negative value or 0
		 nil)
		((null (, arg))
		 (not (, var)))
		(t
		 t)))))



;;; ########################################################### &Funcs ###

;;}}}
;;{{{ Duplicated functions

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defsubst tinypath-ti::date-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g., if you want to calculate days; you'd do

\(/ (tinypath-ti::date-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (let ((hi (- (car a) (car b)))
        (lo (- (car (cdr a)) (car (cdr b)))))
    (+ (lsh hi 16) lo)))

;;; ----------------------------------------------------------------------
;;; #copy: tinylibm.el
(defmacro tinypath-ti::funcall (func-sym &rest args)
  "Call FUNC-SYM with ARGS.
Like funcall, (s)ecretly calls function if it exists.

The full story:

  Byte Compiler isn't very smart when it comes to knowing if
  symbols exist or not. If you have following statement in your function,
  it still complaints that the function \"is not known\"

  (if (fboundp 'some-non-existing-func)
      (some-non-existing-func arg1 arg2 ...))

  instead use:

  (if (fboundp 'some-non-existing-func)
      (tinypath-ti::funcall 'some-non-existing-func arg1 arg2 ...)

  to get rid of the unnecessary warning.

Warning:

  You _cannot_ use ti::funcall if the function is in autoload state, because
  `symbol-function' doesn't return a function to call. Rearrange
  code so that you do (require 'package) or (ti::autoload-p func) test before
  using ti::funcall."
  (`
   (let* ((func (, func-sym)))
     (when (fboundp (, func-sym))
       ;; Old
       ;;   (apply (symbol-function (, func-sym)) (,@ args) nil)
       (apply func (,@ args) nil)))))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylib.el
(defun tinypath-days-old (file)
  "How old FILE is in days. An approximation."
  (let* ((a  (current-time))
	 (b  (nth 5 (file-attributes file)))
	 (hi (- (car a) (car b)))
	 (lo (- (car (cdr a)) (car (cdr b)))))
    (/ (+ (lsh hi 16) lo) 86400)))

;;; ----------------------------------------------------------------------
;;; #copy from tinylibm.el
(defun tinypath-ti::replace-match (level &optional replace string)
  "Kill match from buffer at sub-match LEVEL or replace with REPLACE.
Point sits after the replaced or killed area.

Optionally you can give STRING. If level didn't match, do nothing.

Call:

  (level &optional replace string)

Return:

  t	Action taken
  nil	If match at LEVEL doesn't exist.
  str   If string was given. Use (setq str (ti::replace-match 1 replace str))"
  (if (null string)
      (cond
       ((match-end level)
	(delete-region (match-beginning level) (match-end level))

	;;  I think emacs has bug, because cursor does not sit at
	;;  match-beginning if I delete that region, instead it is off +1
	;;  --> force it to right place

	(and replace
	     (goto-char (match-beginning level))
	     (insert replace))))
    (when (match-end level)		;Handle string case
      (concat
       (substring string 0 (match-beginning level))
       (if replace replace "")
       (substring string (match-end level))))))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylibb.el
(defun tinypath-replace-regexps-in-string
  (regexp rep string &optional fixedcase literal subexp start)
  (let* ((i  0))
    (or subexp
	(setq subexp 0))

    (while (string-match regexp string)
      (if (> (incf i) 5000)
	  (error "Substituted string causes circular match. Loop never ends.")
	(setq string (inline (tinypath-ti::replace-match subexp rep string)))))
    string))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylibm.el
(defun tinypath-ti::pp-variable-list (list &optional buffer def-token)
  "Print LIST of variables to BUFFER. DEF-TOKEN defaults to `defconst'."
  (let* (val)
    (or buffer
	(setq buffer (current-buffer)))

    (or def-token
	(setq def-token "defconst"))

    (dolist (sym list)

      (unless (symbolp sym)
	(error "List member is not symbol %s" sym))

      (setq val (symbol-value sym))
      (insert (format "\n\n(%s %s\n" def-token (symbol-name sym)))

      (cond
       ((numberp val)
	(insert val))

       ((stringp val)
	(insert (format "\"%s\"" val)))

       ((memq val '(t nil))
	(insert (symbol-name val)))

       ((and (symbolp val)
	     (fboundp val))
	(insert "(function " (symbol-name val) ")"))

       ((symbolp val)
	(insert "'" (symbol-name val)))

       ((listp
	 (insert "'" (pp val))))

       (t
	(error "unknown content of stream" sym val)))

      (insert ")"))))

;;; ----------------------------------------------------------------------
;;; #copy from tinylibm.el
(defun tinypath-ti::write-file-variable-state
  (file desc list &optional fast-save bup)
  "Save package state to FILE.

Input:

  FILE	    filename
  DESC	    One line description string for the file.
  LIST	    List of variable symbols whose content to save to FILE.

  FAST-SAVE The default `pp' function used to stream out the contents
	    of the listp variables is extremely slow if your variables
	    contain lot of data. This flag instructs to use alternative,
	    much faster, but not pretty on output, method.

  BUP       If non-nil, allow making backup. The default is no backup."
  (with-temp-buffer
    (let ((backup-inhibited (if bup nil t))
	  ;;  prohibit Crypt++ from asking confirmation
	  (crypt-auto-write-buffer  t))
      (unless crypt-auto-write-buffer	;Bytecomp silencer
	(setq crypt-auto-write-buffer nil))
      (insert ";; @(#) " file " -- " desc "\n"
	      ";; Date: "
	      (tinypath-time-string)
	      "\n\n")
      (if (not fast-save)
	  (tinypath-ti::pp-variable-list list)
	(dolist (var list)
	  (insert (format "\n\n(defconst %s\n" (symbol-name var)))
	  ;;  While `pp' would have nicely formatted the value, It's
	  ;;  unbearable SLOW for 3000 file cache list.
	    ;;  `prin1-to-string' is 10 times faster.
	  (insert "'" (prin1-to-string (symbol-value var)) ")\n")))
      (insert (format "\n\n;; end of %s\n" file))
      (write-region (point-min) (point-max) file))))

;;; ----------------------------------------------------------------------
;;; #copy from tinylib.el
(defun tinypath-ti::advice-control
  (list regexp &optional disable verb msg)
  "Enables/disable SINGLE-OR-LIST of advised functions that match REGEXP.
Signals no errors, even if function in LIST is not advised.
All advice classes ['any] are ena/disabled for REGEXP.

Input:

  LIST                  list of functions.
  REGEXP                advice name regexp. Should normally have ^ anchor
  DISABLE               flag, of non-nil then disable
  VERB                  enable verbose messages
  MSG                   display this message + on/off indication"
  (dolist (func list)
    (ignore-errors
      (if disable
	  (ad-disable-advice  func 'any regexp)
	(ad-enable-advice     func 'any regexp))
      (ad-activate func)))                              ;;change state
  (if verb
      (message
       (concat
	(or msg "advice(s): ")
	(if disable
	    "off"
	  "on")))))

;;; ----------------------------------------------------------------------
;;; #copy
(defun tinypath-ti::string-remove-whitespace (string)
  "Squeezes empty spaces around beginning and end of STRING.
If STRING is not stringp, then returns STRING as is."
  (when (stringp string)
    (if (string-match "^[ \t]+\\(.*\\)" string)
	(setq string (match-string 1 string)))

    (if (string-match "[ \t]+\\'" string)
	(setq string
	      (substring string 0  (match-beginning 0)))))
  string)

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-message-get-buffer ()
  "Return *Message* buffer pointer."
  (or (get-buffer "*Messages*")
      (get-buffer " *Message-Log*"))) ;; XEmacs

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-log-write ()
  "*Write log to `tinypath-:log-file'."
  (let* ((buffer (tinypath-message-get-buffer))
	 (file   tinypath-:log-file))
    (ignore-errors
      (with-current-buffer buffer
	(write-region (point-min) (point-max) file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-ti::xe-timer-elt  (function)
  "Search FUNCTION and return timer elt.
You can use this function to check if some function is currently
in timer list. (i.e. active)

The timer lists are searched in following order:

  `itimer-list'
  `timer-list'
  'timer-idle-list'

Return:

  '(timer-elt timer-variable)"
  (let* (pos
         list
	 item
	 ret)

    (flet ((get-elt (elt place)
		    (if (vectorp elt)
			(aref elt place)
		      (nth place elt))))
      (dolist (timer '(
			;; (("Mon Dec  9 10:01:47 1996-0" 10
		        ;;     process nil))
			(timer-idle-list . 5)
			(timer-alist . 2)
			(timer-list  . 2) ;; 19.34+
			(itimer-list . 3)))
	(when (boundp (car timer))

	  (setq list (symbol-value (car timer))
		pos  (cdr timer))

	  ;;  NOTE: this is different in Xemacs. It is not a vector
	  ;; timer-[idle-]list Emacs 19.34
	  ;;  NOTE: this is different in Xemacs. It is not a vector

	  ;; ([nil 12971 57604 0 60 display-time-event-handler nil nil])
	  ;; [nil 13971 14627 646194 60
	  ;;      (lambda (f) (run-at-time ...))
	  ;;      (irchat-Command-keepalive) nil]

	  (if (and (not (xemacs-p))
		   (vectorp (car list)))
	      (setq pos 5))

	  (dolist (elt list)
	    (setq item (get-elt elt pos))
;;;	(d!! (functionp item) (get-elt elt (1+ pos)))
	    (when (or (and (symbolp item)
			   (eq item function))
		      ;;  It may be lambda expression
		      (and (functionp item)
			   (string-match (regexp-quote (symbol-name function))
					 (prin1-to-string
					  (get-elt elt (1+ pos))))))
	      (setq ret (list elt (car timer)))
	      (return))))))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-ti::xe-timer-cancel  (key &optional cancel-function)
  "Delete timer KEY entry, where KEY is full element in (i)`timer-alist'."
  (let (var)
    (when key
      (when (and (null var)
		 (boundp 'timer-alist))             ;Emacs
	(setq var 'timer-alist)
	(tinypath-ti::funcall 'cancel-timer key)
	(set var (delete key (symbol-value 'timer-alist))))

      (when (and (null var)
		 (boundp 'timer-list))              ;Emacs 19.34
	(setq var 'timer-list)
	;;  Must use this command
	(tinypath-ti::funcall 'cancel-timer key))

      (when (and (null var)
		 (boundp 'timer-idle-list))              ;Emacs 19.34
	(setq var 'timer-idle-list)
	;;  Must use this command
	(tinypath-ti::funcall 'cancel-timer key))

      (when (and (null var)
		 (boundp 'itimer-list))             ;XEmacs
	(setq var 'itimer-list)
	(tinypath-ti::funcall 'cancel-itimer key)
	(set var (delete key (symbol-value 'itimer-list))))

      var)))

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defun tinypath-ti::xe-timer-cancel-function (function)
  "Delete all timer entries for FUNCTION."
  (let (key)
    (while (setq key (car-safe (tinypath-ti::xe-timer-elt function)))
      (tinypath-ti::xe-timer-cancel key))
    key))

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defun tinypath-ti::directory-recursive-do (root function)
  "Start at ROOT and call FUNCTION recursively from each ascended directory."
  (let* ((list (tinypath-subdirectory-list root)))
    (if (null list)
	(funcall function root)
      (dolist (path list)
	(tinypath-ti::directory-recursive-do path function)))))

;;}}}
;;{{{ Modes

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-default-bindings ()
  "Define default key bindings to `tinypath-report-mode-map'."

  (cond
   ((xemacs-p)
    (define-key tinypath-report-mode-map [(control shift button1)]
      'tinypath-report-mode-delete-file))
   (t
    (define-key tinypath-report-mode-map [C-S-mouse-1]
      'tinypath-report-mode-delete-file)))

  ;; ............................................. users with no mouse ...

  (define-key tinypath-report-mode-map "\C-d"
    'tinypath-report-mode-delete-file)

  (define-key tinypath-report-mode-map "\C-c\C-d"
    'tinypath-report-mode-delete-file-noconfirm)

  (define-key tinypath-report-mode-map "\C-p"
    'tinypath-report-mode-previous)

  (define-key tinypath-report-mode-map [(home)]
    'tinypath-report-mode-previous)

  (define-key tinypath-report-mode-map "\C-n"
    'tinypath-report-mode-next)

  (define-key tinypath-report-mode-map [(end)]
    'tinypath-report-mode-next)

  (define-key tinypath-report-mode-map "\C-cr"
    'tinypath-cache-duplicate-report)

  (define-key tinypath-report-mode-map "\C-cg"
    'tinypath-cache-regenerate)

  (define-key tinypath-report-mode-map "\C-cf"
    'tinypath-report-mode-find-file))

(unless tinypath-report-mode-map
  (setq tinypath-report-mode-map (make-sparse-keymap))
  (run-hooks 'tinypath-:report-mode-define-keys-hook))

;;}}}
;;{{{ Misc

;;; ----------------------------------------------------------------------
;;; (tinypath-eval-after-load "woman" 'tinypath-insinuate-woman)
;;;
(defun tinypath-eval-after-load (file function)
  "Simulate `eval-after-load'. load FILE and run FUNCTION."
  (cond
   ((not (fboundp 'eval-after-load));; Older Emacs versions do not have this.
    (and (load file 'noerr)
	 (funcall function)))
   (t
    ;; See after-load-alist
    ;; ... If FILE is already loaded, evaluate FORM right now.
    (eval-after-load file
      (` (progn (funcall (quote (, function)))))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-debug-wrapper-macro 'lisp-indent-function 0)
(put 'tinypath-debug-wrapper-macro 'edebug-form-spec '(body))
(defmacro tinypath-debug-wrapper-macro (&rest body)
  "Increase `tinypath-:verbose' and `message-log-size'."
  (`
   (let* ((tinypath-:verbose 12))
     (set (tinypath-message-log-max-sym) 900000)
     (with-current-buffer (tinypath-message-get-buffer)
       (,@ body)
       (pop-to-buffer (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-debug-test-run (&optional clear)
  "Test everything with full debug and CLEAR buffer."
  (interactive "P")
  (tinypath-debug-wrapper-macro
    (if clear
	(erase-buffer))
    (tinypath-cache-regenerate)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-debug-external-helper ()
  "Test external helper."
  (interactive)
  (tinypath-debug-wrapper-macro
    (tinypath-external-helper-call
     (current-buffer)
     (tinypath-external-setup-1)
     'debug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-string (&optional time)
  "Return TIME in ISO 8601 format YYYY-MM-DD HH:MM:SS"
  (format-time-string "%Y-%m-%d %H:%M:%S" (or time (current-time))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-list-display (msg list &optional insert)
  "Display MSG and LIST to *Messages* or INSERT.
The MSG should contain %s format string to write each element."
  (let* ((i 0))
    (dolist (elt list)
      (incf i)
      (setq elt (if (stringp elt)
		    elt
		  (prin1-to-string elt)))
      (setq elt (format (concat "%3d " msg) i elt))
      (if insert
	  (insert elt "\n")
	(message elt))))
  (unless insert
    (let* ((buffer (tinypath-message-get-buffer)))
      (when buffer
	(display-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-suffixes (file)
  "Return list of try suffixes for FILE. '(\".el\" \".elc\")."
  (cond
   ((string-match "\\.elc?$" file)
    '(""))
   (t
    '(".el" ".elc"))))

;;; ----------------------------------------------------------------------
;;; We need this because we use advised `locate-library'
;;;
(defun tinypath-locate-library (file)
  "Like `locate-library' FILE, but return list of paths."
  (let (path-list
	(suffix (tinypath-suffixes file))
	path)
    (dolist (dir load-path)
      (setq dir (file-name-as-directory dir))
      (dolist (postfix suffix)
	(setq path (concat dir file postfix))
	(when (file-exists-p path)
	  (pushnew path path-list :test 'string=))))
    path-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-report-ignore-function (file)
  "Ignore from output in XEmacs _pkg.el and the like."
  ;; In XEmacs there are lot of these files. Ignore
  (string-match "auto-autoloads\\|_pkg\\.el" file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-maybe-warn-message-log-max ()
  "Print message if Message-Log size is too small.
Too small value would prevent debugging tinypath.el."
  (let* ((size 20000)
	 now)
    (setq now
	  (symbol-value (tinypath-message-log-max-sym)))
    (when (and (> tinypath-:verbose 9)
	       ;;  Value `t' is for unlimited size.
	       (or (not (eq t now))
		   (and (integerp now)
			(and (< now size)))))
      (message
       (concat "TinyPath: Possibly can't display all logs. Increase"
	       (symbol-name
		(tinypath-message-log-max-sym))))
      (sit-for 2))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-file-compressed-p (file)
  "Check if FILE looks compressed."
  (string-match "\\.\\(gz\\|[Zz]\\|bz2\\)$" file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-use-compression ()
  "Load jka-compr.el safely."
  (or (featurep 'jka-compr)
      (let ((file (or (tinypath-cache-p "jka-compr")
		      (locate-library "jka-compr")
		      (error "\
TinyPath: [PANIC] Can't find Emacs core library jka-cmpr.el."))))
	(if (fboundp 'ad-Orig-load)
	    (tinypath-ti::funcall 'ad-Orig-load file)
	  (load file))
	;; New X/Emacs releases need this
	(cond
	 ((fboundp 'auto-compression-mode)   ;; New Emacs: jka-compr.el
	  ;; symbol-function suppresses Byte compiler messages
	  (funcall (symbol-function 'auto-compression-mode) 1))
	 ((fboundp 'jka-compr-install)
	  (tinypath-ti::funcall 'jka-compr-install))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-use-compression-maybe (file)
  "Use compression if FILE name looks like compressed. If it concatain certain extension."
  (or (featurep 'jka-compr)
      (when (tinypath-file-compressed-p file)
	(tinypath-use-compression))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-warn-if-not-exist (file)
  "Print message if FILE does not exist."
  (when (stringp file)
    (tinypath-use-compression-maybe
     (file-name-nondirectory file)))
  (when (null (let (ret)
		(dolist (ext '("" ".el" ".elc"))
		  (when (file-exists-p (concat file ext))
		    (setq ret t)
		    (return)))
		ret))
    (message
     (substitute-command-keys
      (format
       "TinyPath: CACHE invalid. The cached file does not exist %s \
Please run \\[tinypath-cache-regenerate]"
	 file)))
    (sleep-for 1)
    t))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-compressed (&optional list)
  "Append `tinypath-:compressed-file-extensions' to each element in LIST."
  (let* (ret)
  (dolist (elt (or list '("")))
    (when (stringp elt)
      (dolist (ext tinypath-:compressed-file-extensions)
	(when (stringp ext)
	  (push (concat elt ext) ret)))))
  ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-list (package)
  "Return possible extensions to search for PACKAGE.
This function is used only once in the package to return the search
list to the cache function. This list is reused internally and you
cannot change the compress extensions during the Emacs lifetime."
   ;; tinypath-suffixes
  (cond
   ((string-match "\\.elc$" package)
    (append '(".elc")
	    (tinypath-file-extension-compressed '(".elc"))))
   ((string-match "\\.el$" package)
    (append '(".el")
	    (tinypath-file-extension-compressed '(".el"))))
   ((string-match "\\(z\\|bz2\\)$" package)
    nil)
   (t
    (let* (ret)
      ;;  The correct order is ELCs first then EL.
      ;;  The list is built in reverse order here.
      (setq ret (tinypath-file-extension-compressed '(".el")))
      (push ".el" ret)
      (dolist (elt (tinypath-file-extension-compressed '(".elc")))
	(push elt ret))
      (push ".elc" ret)
      ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-list-choices ()
  "Return list of choices to search.
 '((el . (list)) (elc . (list)) (nil . (list)))."
  (let* (
	 ;; As a fall back, should we search .el choices if .elc
	 ;; choices fail
	 (elc (append (tinypath-file-extension-list "package.elc")
		      (tinypath-file-extension-list "package.el")))
	 (el  (tinypath-file-extension-list "package.el"))
	 (all (tinypath-file-extension-list "package")))
    (list
     elc
     el
     (cons nil all))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-remove-trailing-slash (path)
  "Remove trailing slashes, unless it is a Win32 root dir f:/"
  (unless (string-match "^[a-z]:[\\/]$" path)
    (if (string-match "^\\(.*\\)[\\/]$" path)
	(setq path (match-string 1 path))))
  path)



;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-lisp-file-list (&optional from-cache)
  "Return only lisp file alist (file . path) from `tinypath-:cache'.
With optional parameter FROM-CACHE, use the latest cached value.
Be warned, this may not be the absolute latest."
  (let* ((id "tinypath-emacs-lisp-file-list")
	 list
	 save)
    (when from-cache
      (setq list (get 'tinypath-emacs-lisp-file-list 'cache)))

    (unless tinypath-:cache
      (message "%s: [ERROR] `tinypath-:cache' is nil." id))

    (unless list
      (setq save t)
      (dolist (elt tinypath-:cache)
	(when (string-match "\\.el.?$" (car elt))
	  (push (cons (car elt) (cdr (nth 1 elt)))
		list))))

    (if save
	(put 'tinypath-emacs-lisp-file-list 'cache list))

    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-lisp-file-list-cache-clear ()
  "Clear cache kept by `tinypath-emacs-lisp-file-list'."
  (put 'tinypath-emacs-lisp-file-list 'cache nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-list-clean (list &optional list-name)
  "Clean LIST for anything suspicious: non-directories, non-strings.
If you have moved directories from one place to another or some program has
added entries to it, it is possible that LIST is \"fragmented\".

- Remove non-strings, possibly (nil t) values.
- Expand all directories. In Win32, `downcase' every path.
- Convert to use only forward slashes.
- Remove trailing slashes.
- Remove duplicate paths.

Input:

  LIST         List of directories
  LIST-NAME    The name of variable for debug."
  (let* (new-path)

    (or list-name
	(setq list-name ""))

    (dolist (path list)
      (cond
       ((not (stringp path))
	(tinypath-verbose-macro 5
	  (message "TinyPath: %s Cleaning, NON-STRING ENTRY %s"
		   list-name
		   (prin1-to-string path))))

       ((not (file-directory-p path))
	(tinypath-verbose-macro 5
	  (message "TinyPath: %s Cleaning, directory does not exist %s"
		   list-name path)))
       (t

	;;  This will convert all paths to forward slashes
	;;  and downcase them in win32

	(setq path (tinypath-expand-file-name path))

	;;  Remove trailing slashes, unless it is a Win32 root dir f:/

	(setq path (tinypath-file-remove-trailing-slash path))

	(pushnew path new-path :test 'string=))))
    (reverse new-path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-find-dir  (file dir-list)
  "Search DIR-LIST and return directory when FILE is found.
If FILE is nil, then return first existing directory in DIR-LIST.

Note: directory list passed can contain non-string entries. They are ignored."
  (let* (ret)
    (dolist (dir dir-list)
      (when (stringp dir)
	(when (string-match "[/\\]$" dir) ;Remove trailing slash
	  (setq dir (substring dir 0 (1- (length dir))  )))
	(when (file-exists-p
	       (concat (file-name-as-directory dir)
		       (or file "")))
	  (setq ret (tinypath-expand-file-name dir))
	  (return))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-path-ok-this-emacs-p (path)
  "Check that /emacs path is for Emacs and /xemacs path is for XEmacs.
Return t if path is ok for current Emacs."
  (let* ((no-emacs-regexp (if (inline (xemacs-p))
			      ".*[/\\]emacs"
			    ".*[/\\]xemacs"))
	 (this-emacs-regexp (if (inline (xemacs-p))
				".*[/\\]xemacs"
			      ".*[/\\]emacs"))
	 (correct-emacs   t)
	 len1
	 len2)
    (when (string-match no-emacs-regexp path)
      (setq len1 (length (match-string 0 path)))
      ;;  If path contains both the word Emacs and XEmacs, the it
      ;;  is hard to know if this is invalid or not
      ;;
      ;;   /usr/share/bin/emacs/xemacs/xemacs-21.2
      ;;   /usr/share/bin/emacs/emacs/emacs-20.3
      ;;
      (when (string-match this-emacs-regexp path)
	(setq len2 (length (match-string 0 path)))
	(tinypath-verbose-macro 7
	  (message "TinyPath: PATH-NOK both emacs versions?? %s" path)))
      (when (or (null len2)
		(< len2 len1))   ;; the correct Emacs name must be LAST
	(setq correct-emacs nil)
	(tinypath-verbose-macro 7
	  (message "TinyPath: PATH-NOK WRONG EMACS %s" path))))
  correct-emacs))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-path-ok-p (path)
  "Check if path is accepted with `tinypath-:load-path-ignore-regexp'."
  (when (and (stringp path)
	     (tinypath-path-ok-this-emacs-p path))
    (cond
     ;; .................................................... directory ...
     ((not (file-directory-p path))
     (tinypath-verbose-macro 5
       (message "TinyPath: PATH-NOK dir does not exist: %s"
		path))
      nil)
     ;; ................................................ ignore regexp ...
     ((let* (case-fold-search)
	(string-match tinypath-:load-path-ignore-regexp path))
      (tinypath-verbose-macro 3
	(message
	 (concat "TinyPath: PATH-NOK tinypath-:load-path-ignore-regexp "
		 "matches [%s] (ignored) %s")
	 (match-string 0 path) path))
      nil)
     ;; ...................................................... symlink ...
     ((file-symlink-p path)
      (tinypath-verbose-macro 5
	(message "TinyPath: PATH-NOK symlink (ignored) %s" path))
      nil)
     ;; ........................................................... ok ...
     (t
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-lisp-p (path)
  "Check if directory has any files matching regexp `\\.elc?'."
  (cond
   ((not (stringp path))
    (tinypath-verbose-macro 5
      (message "TinyPath: [error] directory entry %s" (prin1-to-string path))))
   ((not (file-directory-p path))
    (tinypath-verbose-macro 5
      (message "TinyPath: [error] directory not found %s" path)))
   (t
    (dolist (elt (directory-files path))
      (when (string-match "\\.elc?" elt)
	(return t))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-subdirectory-list (path)
  "Return all subdirectories under PATH."
  (let* (list)
    (dolist (elt (directory-files path 'absolute) )
      (when (and (not (string-match "\\.\\.?$" elt))  ;; skip . and ..
		 (file-directory-p elt))	;; take only directories
	(push elt list)))
    list))

;;}}}
;;{{{ autoload and other system help functions

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-self-location-load-history ()
  "Return `load-history' entry"
  (let* (file)
    (dolist (elt load-history)
      (setq file (car elt))
      (when (and (stringp file)
		 (setq file (tinypath-expand-file-name file))
		 (string-match "^\\(.+\\)[\\/]tinypath\\." file))
	(return (match-string 1 file))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-self-location ()
  "If package was loaded with absolute path, return path.
Uses `load-history' and `load-path' information."
  (let* ((ret (tinypath-self-location-load-history)))
    (unless ret  ;; No luck with load-history, try load-path
      (dolist (path load-path)
	(setq path (file-name-as-directory (expand-file-name path)))
	(when (or (and (file-exists-p (concat path "tinypath.el"))
		       path)
		  (and (file-exists-p (concat path "tinypath.elc"))
		       path)
		  (and (file-exists-p (concat path "tinypath.el.gz"))
		       path))
	  (return (setq ret path)))))

    (unless ret
      (message
       (concat
	"TinyPath: SELF WARNING user did not load tinypath.el"
	"\tusing absolute path, as instructed in installation."
	"\t(load \"~/some/absolute/path/tinypath.el\")"
	"\tThis defeats internal optimizations."))
      (message "TinyPath: SELF %s" (or ret "<no load-history>" )))

    ;;  tinypath-* function is XEmacs and Emacs compatible version
    ;;  and ensures that forward slashes are used.

    (and ret
	 (setq ret (tinypath-expand-file-name ret)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-autoload-file-name (function)
  "Load package if FUNCTION is in autoload state."
  (let* ((str (prin1-to-string (symbol-function function))))
    (when (string-match "^(autoload[ \t]+\"\\([^\"]+\\)" str)
      (setq str (match-string 1 str))
      ;;  there is one problem. prin1-to-string doubles every backslash
      ;;  c:\\\\dir\\\\ ... (XEmacs problem)
      (if (string-match "/" str)
	  str
	(let* ((final ""))
	  ;; It's easier and faster to do this in buffer, than
	  ;; parsing STRING
	  (with-temp-buffer
	    (insert str)
	    (goto-char (point-min))
	    (while (re-search-forward "\\([^\\]+\\)" nil t)
	      (setq final (concat
			   final
			   (match-string 1)
			   "/"))))
	  ;; remove trailing "/"
	  (substring final 0 (1- (length final))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-autoload-require (function &optional noerr nomsg)
  "Load package if FUNCTION is in autoload state.
NOERR NOMSG are parameters to `load'."
  (let* ((file (tinypath-autoload-file-name function)))
    (when file
      (load file noerr nomsg))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-admin-remove-matching (path)
  "Remove PATH from `load-path' and add to `tinypath-:load-path-ignore-regexp'."
  (let ((fid  "tinypath-admin-remove-matching"))
    ;; Initially the idea was that the entries were purged fom cache too, but
    ;; looping and reconstructing it takes too much time.
    ;;
    ;; It's more efficient to disable packages by using regexps in
    ;; tinypath-:load-path-ignore-regexp, although this is not as transparent.
    ;;
    ;; --> #todo: Add better functionality to perl code.

    ;;  Kill second level cache which "remembers" paths.
    (setq tinypath-:cache-level-two nil)

    (setq path (regexp-quote (tinypath-expand-file-name path)))
    (tinypath-load-path-remove path)
    (tinypath-load-path-remove-cache path)

    (message "TinyPath: %s adding to tinypath-:load-path-ignore-regexp [%s]"
	     fid path)

    (cond
     ((not (stringp tinypath-:load-path-ignore-regexp))
      (setq tinypath-:load-path-ignore-regexp path))
     ((not (string-match path tinypath-:load-path-ignore-regexp))
      (setq tinypath-:load-path-ignore-regexp
	    (concat tinypath-:load-path-ignore-regexp
		    "\\|" path))))))

;;}}}
;;{{{ External: emacs-util.pl

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse-1-cache ()
  "Parse files in format `tinypath-:cache'."
  (let* ((i 0)
	 (personal-count 0)      ;; User files 0 .. 2000
	 (other-count 2000)
	 (emacs-count 5000)
	 (font-lock-mode nil)
	 (lazy-lock-mode nil)
	 (regexp (concat "^LISP-FILE[ \t]+"
			 "\\("
			 "\\([^ \t\r\n]+[\\/]\\)"
			 "\\([^ \t\r\n]+\\)"
			 "\\)"))
	 path
	 dir
	 file
	 emacs
	 other
	 personal
	 elt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)

      (setq path (match-string 1)
            dir  (match-string 2)
	    file (match-string 3))

      ;; was: (tinypath-path-ok-p dir) , but now perl does
      ;; the checking

      (when t

	;; (set-text-properties 0 (length dir) nil dir)
	;; (set-text-properties 0 (length file) nil file)

	(incf i)

	(when (zerop (% i 10))
	  (tinypath-verbose-macro 2
	    (message "TinyPath: EXT Caching files... %d %s" i path)))

	;; data structure is ("file.el" (1 . "/home/foo/elisp/"))
	;;
	;;  The reason why we put paths to separate lists is that
	;;  OTHER directories must override the Core Emacs paths,
	;;  so that newest files are found. Usually you can download
	;;  newer versions than what Emacs has.

	(cond
	 ((tinypath-load-path-emacs-distribution-p path)
	  (incf emacs-count)
	  (setq elt (list file (cons emacs-count dir)))
	  (push elt emacs))
	 ((tinypath-load-path-personal-p path)
	  (incf personal-count)
	  (setq elt (list file (cons personal-count dir)))
	  (push elt personal))
	 (t
	  (incf other-count)
	  (setq elt (list file (cons other-count dir)))
	  (push elt other)))))

    (append (nreverse personal) (append other emacs))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse-1 (id)
  "Parse ID from current buffer. See `tinypath-external-helper'."
  (let* ((case-fold-search t)
	 (regexp (concat "^" id "[ \t]+\\([^ \t\r\n]+\\)"))
	 string
	 list)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq string (match-string 1))
      ;; (set-text-properties 0 (length string) nil string)
      (push string list))
    (unless list
      (tinypath-verbose-macro 1
	(message  "TinyPath: EXT PARSE FATAL\n%s\n" (buffer-string))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse ()
  "Parse current buffer. See'`tinypath-external-helper'."
  (let* (list
	 data
	 name)

    (if (fboundp 'font-lock-mode)
	(turn-off-font-lock))

    (if (fboundp 'lazy-lock-mode)
	(lazy-lock-mode -1))

    ;; Clear text properties so that the data structures are clean
    ;; and possibly faster to use

    (set-text-properties (point-min) (point-max) nil)

    ;;  This list of symbols is same ast the prefix string from
    ;;  the perl script:
    ;;
    ;;  LISP-FILE filename-here
    ;;  LISP-DIR filename-here
    ;;  ...

    (dolist (id '(info
		  bin
		  man
		  lisp-dir
		  c-src-dir))
      (setq name (symbol-name id)
	    data  (tinypath-external-output-parse-1 name))
      (if (null data)
	  (tinypath-verbose-macro 3
	    (message "TinyPath: EXT PARSE ERROR [%s]" name))
	(push (cons id data) list)))

    ;;  'cache (lisp-files) handling is different. Do it now
    (push (cons 'cache (tinypath-external-output-parse-1-cache))
	  list)

    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-bin-location (file)
  "Return location of BINARY. Look from the installation dir.
Look up `exec-path' and the kit installation directory /bin. See
Manual \\[tinypath-version] for more."
  (let* ((self  (tinypath-self-location))
	 (path  (tinypath-executable-find file))
	 (ret   path))
    (when self
      ;;  ~/elisp/..
      (setq self (tinypath-expand-file-name self))
      (setq self
	    (concat
	     (file-name-as-directory
	      (file-name-directory self))
	     "bin/"
	     file))
      (if (file-exists-p self)
	  (setq ret self)))

    (tinypath-verbose-macro 3
      (message "TinyPath: EXT bin location %s" ret))

    (when (and ret
	       (not (file-exists-p ret)))
      (message "TinyPath: EXT FATAL, bin location is wrong %s" ret)
      (setq ret nil))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-helper-call (buffer path-list &optional debug)
   "Use external helper Perl script if available.
First, Environment must contain perl executable and second
`tinypath-:external-util-bin' must be along path.

Input:

  BUFFER     Where to output.
  PATH-LIST  list of root directories to search.
  DEBUG      Request debug.

Return:

  t	     If external utility was found and called."
   (let* ((file  tinypath-:external-util-bin)
	  (perl  (tinypath-executable-find-binary "perl"))
	  (bin   (tinypath-external-bin-location
		  tinypath-:external-util-bin))
	  (opt   (or path-list
		     (error "TinyPath: path-list is empty.")))
	  (ignore tinypath-:load-path-ignore-regexp))

     (tinypath-verbose-macro 3
       (message "TinyPath: EXT perl location %s" (or perl "<not found>")))

     (tinypath-verbose-macro 5
       (message "TinyPath: EXT exec-path %s %s" file (or bin "<not found>")))

     (when debug
       (push "3" opt)
       (push "--debug" opt))

     (setq ignore
	   (concat
	    (or ignore "")
	    (if (stringp ignore)
		"\\|" "")
	    (if (xemacs-p)
		"[/\\]emacs"
	      "[/\\]xemacs")))

     (dolist (switch (list
		      "--Info"
		      "--Man"
		      "--Bin"
		      "--Lang-lisp-file"
		      "--Lang-lisp-dir"
		      "--Lang-c-src-dir"
		      ignore
		      "--ignore-emacs-regexp"))
       ;;  These will go to the beginning, which is ok.
       (push switch opt))

     (push bin opt)

     (if (null (and perl bin))
	 (tinypath-verbose-macro 5
	   "TinyPath: EXT ERROR Can't call external utility")
       (message "TinyPath: EXT Process running... [may take a while] %s"
		(mapconcat 'identity opt " "))
       (with-current-buffer buffer
	 (apply 'call-process
		perl
		nil
		(current-buffer)
		nil
		opt)
	 (tinypath-verbose-macro 9
	   (message
	    (concat "\nTinyPath: EXT OUTPUT END\n")))
	 (message "TinyPath: EXT done %s" bin)
	 t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-helper (path-list)
  "Call external helper with PATH-LIST and parse output.

Return:

  '((info . (path path ..))
    (man  . (path path ..))
    (bin  . (path path ..))
    (lisp . (path path ..))
    (cache . <FORMAT-EQUALS-TINYPATH-:CACHE>))."
  (with-temp-buffer
    (when (tinypath-external-helper-call (current-buffer) path-list)
      (tinypath-external-output-parse))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-1 ()
  "Return paths to pass to external program."
  (let* (list)
    (dolist (elt (list
		  ;;
		  ;; load-path must not be there, because it may be already
		  ;; populated from the cache file: the one that we are
		  ;; trying to build from fresh.
		  ;;
		  ;; -> do not add `load-path' to returned list
		  ;;
		  ;; But we can add the original load paths which were
		  ;; saved on startup.
		  ;;
		  tinypath-:original-load-path
		  tinypath-:extra-path-root
		  tinypath-:load-path-root
		  (tinypath-Info-default-directory-list)))
      (dolist (path elt)
	(when (file-directory-p path)
	  (push (tinypath-expand-file-name path) list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-cache (data)
  "Set `tinypath-:cache from DATA '((cache (DATA) ..)."
  (let* ((list (assq 'cache data)))
    (when list
      (setq list (cdr list))
      (setq tinypath-:cache list))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-external-setup-1-load-path (path regexp)
  "Add PATH to `load-path'. Use REGEXP to check validity."
  ;; The perl program recursed ALL directories, but we only
  ;; want to find out lisp dirs that USER requested in
  ;; `load-path' and `tinypath-:load-path-root'
  ;;
  ;; lisp-roots is a lookup string "PATH\\|PATH\\|PATH .."
  ;; which we can use to check if path is accepted
  ;;
  (cond
   ((not (string-match regexp path))
    (tinypath-verbose-macro 5
      (message "TinyPath: PATH-NOK not candidate %s" path)))
   ((tinypath-path-ok-p path)
    (pushnew path load-path :test 'string=))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-external-setup-1-man-path (path)
  "Add PATH to `tinypath-:extra-manpath'."
  (when (or (not (stringp
		  tinypath-:manpath-ignore-regexp))
	    (not (string-match
		  tinypath-:manpath-ignore-regexp
		  path)))
    (pushnew path tinypath-:extra-manpath :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-load-path-lookup-regexp ()
  "Return candidate `load-path' lookup regexp.
This is combination of `load-path' and `tinypath-:load-path-root'."
  (let* ((lisp-roots (append load-path
			     tinypath-:load-path-root)))
    ;; Make lookup regexp
    (mapconcat
     (function
      (lambda (x)
	(regexp-quote
	 (tinypath-expand-file-name x))))
     lisp-roots
     "\\|")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-parse-data (data)
  "Parse external tool's DATA structure."
  (let* ((lisp-lookup (tinypath-external-load-path-lookup-regexp))
	 correct-emacs
	 type)

    (when data
      (dolist (elt data)
	(setq type (car elt))
	(dolist (path (cdr elt))

	  ;; 'cache element is not a string.

	  (when (stringp path)
	    (setq correct-emacs
		  (tinypath-path-ok-this-emacs-p path)))

	  (cond
	   ((equal type 'cache)
	    (return))               ;; Not handled in this loop

	   ((and (equal type 'lisp-dir)
		 correct-emacs)
	    (tinypath-external-setup-1-load-path path lisp-lookup))

	   ((equal type 'man)
	    (tinypath-external-setup-1-man-path path))

	   ((equal type 'c-src-dir)
	    (pushnew path
		     tinypath-:extra-ff-search-directories
		     :test
		     'string=))

	   ((and (equal type 'bin)
		 correct-emacs)
	    (tinypath-exec-path-append path))

	   ((and (equal type 'info)
		 correct-emacs)
	    (tinypath-info-handler path)
	    (pushnew path
		     (tinypath-Info-default-directory-list)
		     :test
		     'string=)))))
      (tinypath-external-setup-cache data)) ;; When

    (tinypath-verbose-macro 3
      (message "TinyPath: EXT END tinypath-external-setup %s"
	       (if data "[DATA OK]" "[DATA NOK]")))

    data))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup ()
  "Use external tool to help setup emacs. See `tinypath-external-helper'."
  (setq tinypath-:external-data-structure
	(tinypath-external-helper
	 (tinypath-external-setup-1)))
  (tinypath-external-setup-parse-data tinypath-:external-data-structure))

;;}}}

;;{{{ Cache

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-initialize ()
  "Set internal extension cache."
  (put 'tinypath-cache-p-1
       'extension-cache
       (tinypath-file-extension-list-choices)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-extensions (package)
  "Return lisp of extensions for PACKAGE."
  (unless (get 'tinypath-cache-p-1 'extension-cache)
    (tinypath-cache-p-1-initialize))
  (if (string-match "\\.elc?$" package)
      (assoc (match-string 0 package)
	     (get 'tinypath-cache-p-1
		  'extension-cache))
    (cdr (assq nil
	       (get 'tinypath-cache-p-1
		    'extension-cache)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-new (package &optional no-special)
  "Check if PACKAGE is in tinypath-:cache. Return PATH or nil.
If package contains absolute directory part, return PACKAGE.

The search order for unidentified package is:
'(\".elc\" \".elc.bz2\" \".elc.gz\" \".el\" \".el.bz2\" \".el.gz\")

Input:

  PACKAGE       file to find from cache.
  NO-SPECIAL    There is special handling for jka-compr which is never
                checked for compressed file. Non-nil bypasses special
                case handling."

  (if (file-name-directory package)  ;; Do not search absolute paths
      package
    (when tinypath-:cache
      (let* ((fid      "TinyPath: tinypath-cache-p-1-new ")
	     (regexp1  tinypath-:ignore-file-regexp)
	     (regexp2  tinypath-:load-path-ignore-regexp)
	     ;; (dir      (file-name-directory package))
	     (choices  (tinypath-cache-p-1-extensions package))
	     elt
	     ret)
	(setq
	 ret
	 (catch 'done
	  (flet ((path-name (ELT)        ;; ELT = '("FILE.EL" (POS . "PATH"))
			    (concat (cdr (nth 1 ELT)) (car ELT)  ))
		 ;; Second function
		 (throw-ignore
		  (elt)
		  (if (or (and (stringp regexp1)
			       (string-match regexp1 (car elt)))
			  (and (stringp regexp2)
			       (string-match regexp2 (cdr (nth 1 elt)))))
		      (tinypath-verbose-macro 10
			(message "%s`ignore-file-regexp' %s"
				 fid
				 (car elt)))
		    (throw 'done (path-name elt)))))

	    (tinypath-verbose-macro 10
	      (message (concat fid "ENTRY %s %s")
		       package
		       (prin1-to-string choices)))

	    (when (setq elt (assoc package tinypath-:cache))
	      (tinypath-verbose-macro 10
		(message (concat fid "DIRECT HIT %s") package))
	      (throw-ignore elt))

	    ;; .................................................. search ...
	    (cond
	     ((and (null no-special)
		   (string-match "jka-compr" package))

	     ;; XEmacs 20.4  installs files under
	     ;; /usr/lib/xemacs-20.4/lisp and all the lisp file sources
	     ;; are in compressed format. This means, that we cannot load
	     ;; jka-compr.el.gz initially.
	     ;;
	     ;; This situation is evident if user has disabled the .elc
	     ;;  loading with tinypath-:ignore-file-regexp

	     (setq regexp1 nil)

	     (tinypath-verbose-macro 10
	       (message (concat fid "SPECIAL CASE %s") package))

	     (setq elt
		   (or (and (not (string-match "\\.el$" package))
			    (assoc "jka-compr.elc" tinypath-:cache))
		       (assoc "jka-compr.el" tinypath-:cache)))
	     (unless elt
	       (error "TinyPath: (cache-p-1) FATAL, can't find %s"
		      package))
	     (throw 'done (path-name elt)))

           ;; .......................................... regular files ...

	   ((not (string-match "\\.\\(g?z\\|bz2\\)$" package))
	    (let* ((file package)
		   try)
	      ;; Remove extension
	      (when (string-match "^\\(.*\\)\\(\\.elc?\\)$" package)
		(setq file (match-string 1 package))
		(tinypath-verbose-macro 10
		  (message (concat fid "REMOVE EXTENSION %s") package)))

	      (dolist (elt choices)
		(tinypath-verbose-macro 10
		  (message (concat fid "trying.. %s")  (concat file elt)))
		(setq try (concat file elt))
		(when (and (or (null regexp1)
			       (not (string-match regexp1 try)))
			   (setq elt (assoc try tinypath-:cache)))
		  (tinypath-verbose-macro 10
		    (message (concat fid "ASSOC %s") (prin1-to-string elt)))
		  (throw-ignore elt)))))))))

	(tinypath-verbose-macro 9
	  (message "TinyPath: cache hit: %s [%s]" package (or ret "")))

	ret))))

;;; ----------------------------------------------------------------------
;;; This is old and slow implementation, preserved only for backup
;;; purposes. The code is not used.
;;;
(defun tinypath-cache-p-1-old (package &optional no-special)
  "Check if PACKAGE is in tinypath-:cache. Return PATH or nil.
If package contains absolute directory part, return PACKAGE.

The search order for unidentified package is:
'(\".elc\" \".elc.bz2\" \".elc.gz\" \".el\" \".el.bz2\" \".el.gz\")

Input:

  PACKAGE       file to find from cache.
  NO-SPECIAL    There is special handling for jka-compr which is never
                checked for compressed file. Non-nil bypasses special
                case handling."

  (unless (get 'tinypath-cache-p-1 'extension-cache)
    (tinypath-cache-p-1-initialize))

  (if (file-name-directory package)
      package
    (when tinypath-:cache
      (let* ((fid      "TinyPath: tinypath-cache-p-1-old ")
	     (position 100000)
	     (regexp   tinypath-:ignore-file-regexp)
	     (dir      (file-name-directory package))
	     (choices  (if (string-match "\\.elc?$" package)
			   (assoc (match-string 0 package)
				  (get 'tinypath-cache-p-1
				       'extension-cache))
			 (cdr (assq nil
				    (get 'tinypath-cache-p-1
					 'extension-cache)))))
	     found-list-original
	     found-list
	     dir1
	     dir2
	     elt
	     ret)
	(flet ((path-name (ELT)        ;; ELT = '("FILE.EL" (POS . "PATH"))
			  (concat (cdr (nth 1 ELT)) (car ELT)  )))

	  (tinypath-verbose-macro 10
	    (message (concat fid "ENTRY %s %s")
		     package
		     (prin1-to-string choices)))

	  (when (setq elt (assoc package tinypath-:cache))
	    (tinypath-verbose-macro 10
	      (message (concat fid "DIRECT HIT %s") package))
	    (push elt found-list))

          ;; .................................................. search ...

	  (cond

           ;; .......................................... special cases ...
	   ((and (null no-special)
		 (string-match "jka-compr" package))

	    ;; XEmacs 20.4  installs files under
	    ;; /usr/lib/xemacs-20.4/lisp and all the lisp file sources
	    ;; are in compressed format. This means, that we cannot load
	    ;; jka-compr.el.gz initially.
	    ;;
	    ;; This situation is evident if user has disabled the .elc
	    ;;  loading with tinypath-:ignore-file-regexp

	    (setq regexp nil)

	    (tinypath-verbose-macro 10
	      (message (concat fid "SPECIAL CASE %s") package))

	    (setq elt
		  (or (and (not (string-match "\\.el$" package))
			   (assoc "jka-compr.elc" tinypath-:cache))
		      (assoc "jka-compr.el" tinypath-:cache)))

	    (unless elt
	      (error "TinyPath: (cache-p-1) FATAL, can't find %s"
		     package))

	    (push elt found-list))

           ;; .......................................... absolute path ...
	   ((and dir (file-directory-p dir))	;Absolute path. Do no search
	    (tinypath-verbose-macro 10
	      (message (concat fid "ABSOLUTE dir %s %s")
		       (or dir "<nil>")
		       package))
	    (setq ret dir))

           ;; .......................................... regular files ...
	   ((and (not (string-match "\\.\\(g?z\\|bz2\\)$" package))
		 ;; Remove extension
		 (string-match "\\(.*\\)\\(\\.elc?\\)?$" package))

	    (tinypath-verbose-macro 10
	      (message (concat fid "REMOVE EXTENSION %s") package))

	    (setq package (match-string 1 package))

	    (dolist (elt choices)

	      (tinypath-verbose-macro 10
		(message (concat fid "trying.. %s")  (concat package elt)))

	      (when (setq elt (assoc (concat package elt) tinypath-:cache))
		(tinypath-verbose-macro 10
		  (message (concat fid "ASSOC %s") (prin1-to-string elt)))
		;;  Set default return value to FILE that has lowest score.
		;;  => file that is at the beginning of load path
		(if (> position (car (nth 1 elt)))
		    (setq position (car (nth 1 elt))
			  ret      (path-name elt)))
		(tinypath-verbose-macro 10
		  (message (concat fid "REMOVE EXTENSION found %s")
			   (prin1-to-string elt)))
		(push elt found-list)))))

          ;; ................................................. results ...

	  (when found-list

	    (tinypath-verbose-macro 9
	      (message "TinyPath: found-list %s" (prin1-to-string found-list)))

	    ;; Should we ignore compiled files? Drop compiled files here.
	    (when (stringp regexp)
	      (let (list)
		(dolist (elt found-list)
		  (if (string-match regexp (path-name elt))
		      (tinypath-verbose-macro 9
			(message "TinyPath: REGEXP ignore %s" (path-name elt)))
		    (push elt list)))
		(setq found-list list)))

	    ;;  Drop non-existing files

	    (let (list path)
	      (dolist (elt found-list)
		(setq path (path-name elt))
		(if (file-exists-p path)
		    (push elt list)
		  (tinypath-verbose-macro 9
		    (message
		     (substitute-command-keys
		      (format
		       (concat "TinyPath: [cache-p-1] file does not exist %s."
			       " Run \\[tinypath-cache-regenerate]")
		       path)))
		    (sleep-for 1))))

	      (setq found-list (reverse list)))

	    ;;  Suppose there is situation:
	    ;;
	    ;;    '(xx.el  (1 . DIR-A))
	    ;;    '(xx.elc (5 . DIR-B))
	    ;;
	    ;;  Because the .elc is searched first; the assoc returns position
	    ;;  5. But there is xx.el which is before this path; when .el
	    ;;  extension is searched. We compare the last position and
	    ;;  1 goes before the 5. so the .el is correct answer.
	    ;;
	    ;;  But If they are in the SAME directory; the numbering could
	    ;;  be wrong:
	    ;;
	    ;;    '(xx.el  (1 . DIR-A))
	    ;;    '(xx.elc (5 . DIR-A))
	    ;;
	    ;;  We must not take the non-compiled .el version

	    (cond
	     ((and (null found-list)
		   found-list-original)
	      (error
	       (concat "TinyPat: `tinypath-:ignore-file-regexp' "
		       "[%s] excluded all found files [%s]. No file to load.")
	       tinypath-:ignore-file-regexp
	       (prin1-to-string found-list-original)))

	     ((and found-list
		   (not (eq (length found-list) 2)))
	      (tinypath-verbose-macro 10
		(message "TinyPath: not FOUND 2 %s" (prin1-to-string
						     (car found-list))))
	      (setq ret (path-name (car found-list))))

	     (found-list
	      (setq elt  (nth 0 found-list)
		    dir1 (cdr (nth 1 elt))
		    elt  (nth 1 found-list)
		    dir2 (cdr (nth 1 elt)))
	      (tinypath-verbose-macro 10
		(message "TinyPath: OTHER 0 = %s 1 = %s"
			 (prin1-to-string (nth 0 found-list))
			 (prin1-to-string (nth 1 found-list))))
	      (if (string= dir1 dir2)
		  (setq ret (concat dir1 package ".elc"))))))

          ;; .......................................... found-list end ...

	  (when ret
	    (tinypath-verbose-macro 9
	      (message "TinyPath: cache hit: %s"
		       (prin1-to-string found-list))))

	  ret)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-cache-p-1 (package)
  "Call correct cache implementation."
  (tinypath-cache-p-1-new package))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p (package)
  "Check if PACKAGE is in tinypath-:cache. Return PATH or nil.
If package contains absolute directory part, return PACKAGE."
  (let  ((list (if (file-name-directory package)
		   ;;  look up "package" first, because it is most
		   ;;  likely known to cache, only then "dir/package"
		   (list
		    (file-name-nondirectory package)
		    package)
		 (list package)))
	 elt
	 ret)
    (dolist (file list)
      ;; If level two cache exists, then check that the entry has not
      ;; been resolved before.
      (cond
       ((and tinypath-:cache-level-two
	     (setq ret
		   (cadr
		    (setq elt (assoc package tinypath-:cache-level-two))))
	     (or (file-exists-p ret)
		 ;;  Invalid cache-2 entry. Remove. File has been moved/deleted
		 (progn
		   (setq tinypath-:cache-level-two
			 (delete elt tinypath-:cache-level-two)))))
	(return))
       (t
	;;  Any absolute load paths are ignored by CACHE and returned
	;;  as is, so ignore references like ~/.emacs
	(when (or (and (not (string-match "^[~/\\]" file))
		       (setq ret (tinypath-cache-p-1 file)))
		  ;;  some emacs load commands are in format:
		  ;;  (load "term/vt100")
		  (and (string-match "[/\\]" file)
		       (setq ret (tinypath-cache-p-1
				  (file-name-nondirectory file)))))
	  (cond
	   ((file-exists-p ret)
	    (push (list package ret) tinypath-:cache-level-two))
	   (t
	    ;;  Invalid cache entry, file does not exist any more.
	    ;;  Do full scan.
	    (setq ret (tinypath-load-path-locate-library package))

	    (when (and ret (file-exists-p ret)) ;; Double guard
	      (push (list package ret) tinypath-:cache-level-two))
	    ;;  Don't add this )))) to the end of above lisp ...
	    nil))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-hostname ()
  "Return `system-name'."
  (or (getenv "HOST")
      "unknownhost"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-name ()
  "Return Emacs version specific cache file.

References:

  `tinypath-:cache-file-prefix'.
  `tinypath-:cache-file-postfix'"
  (let* (host
	 (type (if (xemacs-p)
		   "xemacs"
		 "emacs"))
	 (list (tinypath-emacs-versions))
	 (ver  (or (nth 1 list)
		   (nth 0 list)))
	 (win32  (if (win32-p)
		     "win32-"
		   ""))
	 (host-func tinypath-:cache-file-hostname-function)
	 ret)

    (when (functionp host-func)
      (let (ret)
	(setq ret (funcall host-func))
	(tinypath-verbose-macro 3
	  (message "TinyPath: CACHE file host function returned %s"
		   (or ret "nil")))
	(if (stringp ret)
	    (setq host ret))))

    (setq ret
	  (downcase
	   (concat tinypath-:cache-file-prefix
		   "-"
		   win32
		   (if (stringp host)
		       (concat host "-")
		     "")
		   type
		   "-"
		  ver
		  tinypath-:cache-file-postfix)))
    ret))


;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-delete ()
  "Delete existing cache file from disk, if it exists."
  (let ((file (tinypath-cache-file-name)))
    (when (file-exists-p file)
      (delete-file file)
      (tinypath-verbose-macro 5
	(message "TinyPath: Cache deleted: %s" file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-old-p (file)
  "Check cache. It must exist and it must not be too old."
  (when (and (file-exists-p file)
	     (integerp tinypath-:cache-expiry-days))
    (let* ((days  (tinypath-days-old file)))
      (when (> days tinypath-:cache-expiry-days)
	(tinypath-verbose-macro 2
	  (message "TinyPath: Cache is too old: %s days" days))
	t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-write (file)
   "Write TinyPath state information to FILE."
;;;   (interactive "FFile to save cache: ")

   (tinypath-verbose-macro 2
     (message "TinyPath: Saving cache to %s" file))

   (tinypath-ti::write-file-variable-state
    file
    (concat "Emacs load-path settings.\n"
	    ";; This file is automatically generated. Do not touch.\n"
	    ";; See tinypath.el and M-x tinypath-cache-regenerate.\n")
    (list
     'load-path
     'exec-path
     'tinypath-:extra-manpath
     'tinypath-:extra-path-root
     'tinypath-:extra-ff-search-directories
     (if (boundp 'Info-directory-list) ;; XEmacs
	 'Info-directory-list
       'Info-default-directory-list)
     'tinypath-:cache)
    'no-pp-print 'no-backup))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-load ()
  "Load cache. If it needs synching, return non-nil."
  (let* ((file (tinypath-cache-file-name))
	 ret)
    (tinypath-verbose-macro 2
      (message "TinyPath: Loading `load-path' from cache %s" file))
  (load file)
  (unless load-path
    (setq ret 'cache-file-content-error)
    (message "TinyPath: empty load-path in %s " file))
  (unless tinypath-:cache
    (setq ret 'cache-file-content-error)
    (message "TinyPath: empty tinypath-:cache in %s" file))
  ;;  Make sure that read cache is in synch with
  ;;  the `load-path'. If not, force rescanning.
  (when (and load-path
	     (tinypath-load-path-not-in-synch-p))
    (setq ret 'need-sync))
  ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-find-file ()
  "Display cache by calling `find-file'."
  (interactive)
  (let* ((file (tinypath-cache-file-name)))
    (tinypath-verbose-macro 2
      (message "TinyPath: find-file cache %s" file))
  (find-file file)))

;;}}}
;;{{{ Info files

(defconst tinypath-:info-file-basic-contents
  (concat
   "This is the file .../info/dir, which contains the\n"
   "topmost node of the Info hierarchy, called (dir)Top.\n"
   "The first time you invoke Info you start off looking at this node.\n"
   "\n"
   "File: dir,	Node: Top,	This is the top of the INFO tree\n"
   "\n"
   "  This (the Directory node) gives a menu of major topics.\n"
   "  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,\n"
   "  \"h\" gives a primer for first-timers,\n"
   "  \"mEmacs<Return>\" visits the Emacs manual, etc.\n"
   "\n"
   "  In Emacs, you can click mouse button 2 on a menu item or cross reference\n"
   "  to select it.\n"
   "\n"
   "* Menu:\n\n")
  "*This variable includes a basic `dir' File for Emacs info C-h i.
Do not touch.")

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-Info-default-directory-list-clean ()
  "Clean `Info-default-directory-list'.
Remove any suspicious elements: non-directories, non-strings."
  (set (tinypath-Info-default-directory-list-sym)
       (tinypath-directory-list-clean
	(tinypath-Info-default-directory-list)
	"Info-directory-list")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-write-region (beg end file)
  "Write region BEG END to FILE and ignore errors, but print message."
  (condition-case err
      (write-region (point-min) (point-max) file)
    (error
     (tinypath-verbose-macro 3
       (message "TinyPath: [INFO] No permission to write %s %s"
		(or file "<nil>")  (prin1-to-string err))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-files-in-directory (dir)
  "Return all info files in DIR.
The list is composed of capitalized names of the found files:

    tar.info       --> Tar
    fileutils.info --> Fileutils

Returned list in the above case is '(\"Tar\" \"Fileutils\")."
  ;;  Cache this value only once and reuse as needed.
  (unless (get 'tinypath-info-files-in-directory
	       'compress-extensions)
    (put 'tinypath-info-files-in-directory
	 'compress-extensions
	 (tinypath-file-extension-compressed)))
  (let* ((files      (directory-files dir))
	 (extensions (cons "" (get 'tinypath-info-files-in-directory
				   'compress-extensions)))
	 ret)
    (dolist (file files)
      (when (catch 'exit
	      (dolist (ext extensions)
		;;  NOTE:  Can't use \\| in here, because posix match engine
		;;  tries all possibilities and we want to stop after first
		;;  matched regexp.
		;;
		;;  File Examples:
		;;
		;;    cc-mode-1
		;;    eshell.info
		;;
		(dolist (re '("^\\(.*\\)\\.info-1"
			      "^\\(.*\\)\\.info"
			      "^\\(.*\\)-1"))
		  (setq re (concat re ext "$"))
		  (when (string-match re file)
		    (throw 'exit file)))))
	(pushnew (capitalize (downcase (match-string 1 file)))
		 ret
		 :test 'string=)))
    (sort ret 'string-lessp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-directory-entry-p (entry)
  "Search for info ENTRY."
  (let* ((point (point))   ;; Faster than using save-excursion.
	 ret)

    (goto-char (point-min))

    ;;  This check relies on using the same ENTRY for filename
    ;;
    ;;      * Oview: (Overview).
    ;;
    ;;  But what if user manually edit's the file and makes it read:
    ;:
    ;;      * Exim Oview: (Overview).
    ;;
    ;;  Ok, handle that too, but require thet "Oview" is still there.


    (when (and (goto-char (point-min))
	       (re-search-forward
		(format "^[*]\\([ \t]+[^ \t\r\n]+\\)?[ \t]+%s:[ \t]+"
			entry)
		nil t)
	       (setq ret (point))))

    (goto-char point)  ;; Restore point

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-directory-contents-update
  (file &optional verb interactive info-files)
  "Update the central `dir' with all new info files in directory.
Give the absolute path to the `dir' and the directory is scanned
for new entries which are updated to file `dir'.

Input:

  FILE         The `dir' file location
  VERB         Allow printing verbose messages
  INTERACTIVE  Leave the buffer in Emacs for editing.
  INFO-FILES   Info files in directory, like \"Eieio\"

Return:

  t   if any changes made."

  ;;  (interactive "FInfo file named `dir', location: ")

  (when (file-directory-p file)
    (error "You must give a filename"))

  (let ((buffer (find-file-noselect file))
	done
	buffer-file)
    (with-current-buffer buffer

      ;;  If we read /usr/local/info and we're not root, then
      ;;  this buffer will be read only. Make it writable. The
      ;;  save error is handled elsewhere.

      (setq buffer-read-only nil)

      (tinypath-verbose-macro 1
	(message "TinyPath: [INFO] found %s" file))

      (let* ((entries (or info-files
			  (tinypath-info-files-in-directory
			   (file-name-directory file)))))
	(dolist (entry entries)
	  (unless (tinypath-info-directory-entry-p entry)
	    (goto-char (point-max))
	    (unless (looking-at "^[\n\t ]*$")
	      (insert "\n"))
	    (insert (format "* %s: (%s).\n" entry entry))
	    (tinypath-verbose-macro 2
	      (message "TinyPath: [INFO] added entry `%s' to file %s"
		       entry file))
	    (setq done t)
	    (set-buffer-modified-p nil);; do not ask user  when killing buffer
	    (setq buffer-file (buffer-file-name))))) ;; let*

      (if (interactive-p)
	  (when done
	    (message "TinyPath: [INFO] Edit and verify changes at %s" file))
	(when (and done buffer-file)
	  (tinypath-write-region (point-min) (point-max) buffer-file))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer)))) ;; With-current

    done))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-file-DIR (path)
  "Make `dir' file name using PATH."
  (concat (file-name-as-directory path) "dir"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-handler-DIR (dir)
  "Handle creating/updating central info file DIR `dir' to current directory."
  (let* ((dir-file (tinypath-info-file-DIR dir)))
    (unless (file-exists-p dir-file)	;No central dir, generate one...
      (tinypath-verbose-macro 3
	(message "TinyPath: [INFO] missing central `dir' generating %s"
		 dir-file))
      (with-temp-buffer
	(insert tinypath-:info-file-basic-contents)
	(insert "file `dir' in directory "
		(tinypath-expand-file-name (file-name-as-directory dir))
		"\n")
	;;  maybe we don't have permission to write to this directory?
	(tinypath-write-region (point-min) (point-max) dir-file)
	t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-handler (dir)
  "Check if DIR contains info files and a special `dir' file.
This function will create `dir' file if it does not exist,
update `Info-default-directory-list' and add any new INFO entries in
DIR to central `dir' file in that directory.

Please suggest to the lisp package maintainer that he
should ship with default `dir' in next release so that it
could be automatically used.

Return

  t   if any changes made."
  (interactive "fGive directory with info files: ")
  ;;  If user calls us, make sure new files are also noticed.
  ;;
  (if (interactive-p)
      (tinypath-info-initialize))
  (let* ((list     (tinypath-info-files-in-directory dir))
	 (dir-file (concat (file-name-as-directory dir) "dir"))
	 done)
    (when (and (null list)
	       (interactive-p))
      (message "Tinypath: No info file candidates in %s" dir))
    (when list				;info files in this directory?
      (setq done (tinypath-info-handler-DIR dir))
      (tinypath-info-directory-contents-update
       dir-file
       (interactive-p)
       (interactive-p)
       list)
      (tinypath-verbose-macro 2
	(message "TinyPath: [INFO] Info-default-directory-list push => %s" dir))

      ;;  Always add found directories to the list.
      ;;  Notice, that directory may contain trailing slash, that's why
      ;;  two `member' tests
      ;;
      ;;   ../info
      ;;   ../info/
      ;;

      (let* ((dir1 (file-name-as-directory dir))      ;; with slash
	     (dir2 (substring dir 0 (1- (length dir1))))) ;; without
	(unless (or (member dir1 (tinypath-Info-default-directory-list))
		    (member dir2 (tinypath-Info-default-directory-list)))
	  (push dir2 (tinypath-Info-default-directory-list))))

      ;;  Kill all previous info files from Emacs, so that next info
      ;;  C-h i will force Emacs to regenerate found new entries.

      (if (interactive-p)
	  (tinypath-info-initialize)))
    done))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-initialize ()
  "Kill *info* buffer and set the info in pristine state.
After this function, the central `dir' creates all its parts from scratch
and not from cached directories."
  (interactive)
  (tinypath-Info-default-directory-list-clean)
  ;;  - This must be set to nil, because otherwise Info would not
  ;;    rescan new entries.
  (setq Info-dir-file-attributes nil)
  (when (buffer-live-p (get-buffer "*info*"))
    (kill-buffer "*info*")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-scan-Info-default-directory-list (&optional list)
  "Examine and possibly fix LIST of dirs to `Info-default-directory-list'.
Without any arguments, checks `Info-default-directory-list'
and `tinypath-:Info-default-directory-list'.

If there were any new entries or possibly new directory without
and root INFO file `dir', Emacs info cache cache is deleted and
existing *info* buffer if killed. You should run \\[info] to
regenerate all the info parts again.

Return

  t   if any changes made."
  (interactive)
  (let* (seen
	 done)
    (or list
	(setq list (append
		    (tinypath-Info-default-directory-list)
		    tinypath-:Info-default-directory-list)))
    (dolist (path list)
      (unless (member path seen)
	(push path seen)
	(when (file-directory-p path)
	  (when (tinypath-info-handler path)
	    (setq done t)))))
    (when (and done
	       (interactive-p))
      (tinypath-cache-file-write (tinypath-cache-file-name)))
    (when done
      (tinypath-info-initialize))
    done))

;;}}}
;;{{{ Timing support

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g., if you want to calculate days; you'd do

\(/ (tinypath-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (let ((hi  (- (car a) (car b)))
        (lo  (- (car (cdr a)) (car (cdr b))))
	(mic (- (car (cddr a)) (car (cddr b)))))
    (+
     (+ (lsh hi 16) lo)
     (/ mic 1000000))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-results (buffer)
  "Write load time results to BUFFER. Return buffer pointer."
  (let* (time
	 min
	 sec)
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (dolist (elt tinypath-:time-data)
	(setq time (cdr elt)
	      min  (/ time 60)
	      sec  (- time (* min 60)))
	(insert
	 (format "%-20s %d  %dmin %dsec\n"
		 (car elt)
		 time
		 min
		 sec)))
      (current-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-display ()
  "Display timing information of each package loaded."
  (interactive)
  (display-buffer (tinypath-time-results tinypath-:time-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-record (package start-time)
  "Record load time of PACKAGE, when START-TIME is known."
  (when  (stringp package)
    (let* ((stop-time (current-time))
	   (file (file-name-nondirectory package))
	   (name (if (string-match "^.*\\(.*\\)\\.elc$" file)
		     (match-string 1 file)
		   file))
	   (diff (tinypath-time-difference stop-time start-time)))
      (if tinypath-:verbose-timing
	  (message "TinyPath: load time %s %dsec" name diff)
	(tinypath-verbose-macro 9
	  (message "TinyPath: load time %s %dsec" name diff)))
      (aput 'tinypath-:time-data name diff))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-time-macro 'lisp-indent-function 1)
(put 'tinypath-time-macro 'edebug-form-spec '(body))
(defmacro tinypath-time-macro (package &rest body)
  "Record PACKAGE timing to `tinypath-:time-data' and run BODY."
  (`
   (let* ((start-time (current-time)))
     (prog1
	 (progn (,@ body))
       (tinypath-time-record (, package) start-time)))))

;;}}}
;;{{{ exec-path

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-from-path ()
  "Parse environment variable PATH."
  (let ((path   (getenv "PATH"))
	(regexp (concat "[^" path-separator "]+"))
	list)
    (when path
      (with-temp-buffer
	(insert path)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (push (match-string 0) list))))
    (reverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-append (path)
  "Add PATH to `exec-path'.
Add new PATH to the end, so that user's paths take precedence.
Ignore path if it matches `tinypath-:exec-path-ignore-regexp'."
  ;;  expand - Otherwise `member' would not do much good (duplicates)
  (setq path (tinypath-expand-file-name path))
  (unless (member path exec-path)
    (if (and (stringp tinypath-:exec-path-ignore-regexp)
	     (string-match
	      tinypath-:exec-path-ignore-regexp
	      path))
	(tinypath-verbose-macro 3
	  (message "\
TinyPath: PATH ignored by tinypath-:exec-path-ignore-regexp %s" path))
    (setq exec-path (append exec-path (list path))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-check ()
  "Check if `exec-path' lack any directory as in PATH.
Return missing paths."
  (let* ((exec  (tinypath-directory-list-clean exec-path "exec-path"))
	 (PATH  (tinypath-directory-list-clean
		 (tinypath-exec-path-from-path)
		 "PATH"))
	 missing)
    (dolist (path PATH)
      (unless (member path exec)
	(push path missing)))
    (reverse missing)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-check-verbose (&optional fix)
  "Print messages if `exec-path' lacks any directory found in PATH.
Optionally FIX by adding missing directories to the end."
  (interactive)
  (let ((missing (tinypath-exec-path-check)))
    (when missing
      (dolist (path missing)
	(message "TinyPath: PATH check. `exec-path' does not have %s%s"
		 path
		 (if fix
		     " [fixed]"
		   ""))
	(when fix
	  (tinypath-exec-path-append path))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-clean ()
  "Clean `exec-path' for anything suspicious: non-directories, non-strings."
  (setq exec-path (tinypath-directory-list-clean exec-path "exec-path")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-display (&optional insert)
  "Display `exec-path' by messaging' it. Optionally INSERT."
  (interactive "P")
  (tinypath-list-display "exec-path %s" exec-path insert))

;;}}}
;;{{{ load-path

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-emacs-distribution-p (path)
  "Return non-nil if PATH is from Emacs distribution."
  (string-match
   (concat
    "[/\\]x?emacs[/\\][0-9]+[0-9.]+[/\\]"  ;; Unix  emacs/20.7/
    "\\|[/\\]x?emacs-[0-9]+[0-9.]+[/\\]") ;; win32 emacs-20.7/
   path))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-personal-p (path)
  "Return non-nil if PATH is under $HOME"
  (string-match
   (regexp-quote (expand-file-name "~"))
   (expand-file-name path)))

;;; ----------------------------------------------------------------------
;;; (tinypath-load-path-search "gnus.el")
;;;
(defun tinypath-load-path-search (package &optional all include-all)
  "Search `load-path' for PACKAGE and optioanlly ALL occurrances.
This is the last resort if cache fails.

INCLUDE-ALL says that tinypath-:load-path-ignore-regexp'
is not used. Normally paths that match would be filtered out.


Return

  path          Absolute path location
  '(path ..)    If option ALL was set."
  (unless (get 'tinypath-cache-p-1 'extension-cache)
    (tinypath-cache-p-1-initialize))
  (let* (file
	 ret)
    (dolist (dir load-path)
      (when (and (stringp dir)
		 (file-directory-p dir)
		 (or include-all
		     (null tinypath-:load-path-ignore-regexp)
		     (not (string-match
			   tinypath-:load-path-ignore-regexp
			   dir))))
	(let* ((try     (file-name-sans-extension package))
	       (choices (tinypath-cache-p-1-extensions package))
	       (files   (directory-files
			 dir
			 nil
			 (concat "^"
				 (regexp-quote try)
				"\\("
				(mapconcat
				 ;;  "\\.el\\|\\.el\\.gz\\|..."  etc.
				 (function
				  (lambda (x)
				    (regexp-quote x)))
				 choices
				 "\\|")
				"\\)$"))))
	(cond
	 ((eq 1 (length files))
	  (setq file (concat
		      (file-name-as-directory
		       (expand-file-name dir))
		      (car files)))
	  (if all
	      (push file ret)
	    (return (setq ret file))))
	 (t
	  ;;  Multiple matches. Hm #todo.
	  nil)))))
    ;;  Retain order how files were encountered.
    (if (listp ret)
	(reverse ret)
      ret)))

;;; ----------------------------------------------------------------------
;;; (tinypath-load-path-locate-library "cperl-mode")
;;;
(defun tinypath-load-path-locate-library (package)
  "Locate PACKAGE alaong `load-path'.

References:

  `tinypath-:load-path-accept-criteria'."
  (let* ((criteria      tinypath-:load-path-accept-criteria)
	 (list          (tinypath-load-path-search
			 package criteria))

	 ;;  LIST can be '(path path ...) is ALL-MATCHES is activated.
	 ;;  otherwise the returned value is absolute path name.
	 (ret  (if (listp list)
		   (car-safe list)
		 list)))
    (cond
     ((or (null ret)			;Not found. Do nothing
	  (stringp list)		;Did not search all directories
	  (eq (length ret) 1)))		;Only one match, RET already set
     ((functionp criteria)
      (setq ret (funcall criteria list)))
     (criteria
      ;;  Third party package overrides Emacs installation
      (let* ((ver  (car-safe (tinypath-emacs-versions 'noerr)))
	     (home (ignore-errors (expand-file-name "~")))
	     home-list
	     emacs-list
	     other-list)
	(dolist (path list)
	  (cond
	   ((tinypath-emacs-core-path-p path ver)
	    (push path emacs-list))
	   ((and home
		 (string-match home path))
	    (push path home-list))
	   (t
	    (push path other-list))))
	;;  Now sort out the correct package
	;;  1) User comes first
	;;  2) non-Emacs installation
	;;  3) Emacs installation
	(setq ret (or (and home-list
			   (car (reverse home-list)))
		      (and other-list
			   (car (reverse other-list)))
		      (and emacs-list
			   (car (reverse emacs-list))))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-display (&optional insert)
  "Display `load-path' by messaging' it. Optionally INSERT."
  (interactive "P")
  (tinypath-list-display "load-path %s" load-path insert))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-not-in-synch-p ()
  "Check that all directories exist.
Return:
  List of directories that do not exist."
  (let (list)
    (dolist (path load-path)
      (when (and (stringp path)
		 (not (file-directory-p path)))
	(push path list)))
    (tinypath-verbose-macro 3
      (message "TinyPath: CACHE SYNC error [%s]"
	       (prin1-to-string list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-clean (&optional extensive-test)
  "Clean `load-path' for anything suspicious: non-directories, non-strings.
If EXTENSIVE-TEST is non-nil, remove any paths that do not contain lisp code.
The check will make function much slower."
  (let (list)
    (tinypath-verbose-macro 3
      (message "TinyPath: CLEAN load-path"))
    (setq load-path (tinypath-directory-list-clean load-path "load-path"))
    (dolist (path load-path)
      (when (and (tinypath-path-ok-p path)
		 (or (null extensive-test)
		     (and (file-directory-p path)
			  (tinypath-directory-lisp-p path))))
	(push path list)))
    (setq load-path (reverse list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-reorder ()
  "Move Emacs paths to predefined order.
- User paths at the beginning (HOME dir paths)
- Next anything in any order (site-lisp)
- Last core Emacs paths."
  (let* (personal
	 emacs
	 other)
    (dolist (path load-path)
      (cond
       ((tinypath-load-path-emacs-distribution-p path)
	(push path emacs))
       ((tinypath-load-path-personal-p path)
	(push path personal))
       (t
	(push path other))))
    (setq load-path
	  (append
	   (nreverse personal)
	   (append
	    (nreverse other)
	    (nreverse emacs))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-add-directory-one (path)
  "Add one PATH to the `load-path'. Old entry is removed."
  ;;  remove previous entry
  (if (null (tinypath-directory-lisp-p path))
      (tinypath-verbose-macro 3
	(message "TinyPath: Add ignored. No LISP files in %s" path))
    (if (member path load-path)
	(setq load-path (delete path load-path)))
    (pushnew
     (if (win32-p)
	 (downcase path)
       path)
     load-path
     :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-add-directory-many (list)
  "Add to `load-path' each directory in LIST.
LIST can contains single elements or lists:
 '(single single (elt elt) single (elt elt)))"
  (dolist (elt list)
    (when elt
      (if (not (listp elt))
	  (setq elt (list elt)))
      (dolist (path elt)
	(tinypath-add-directory-one path)))))

;;; ----------------------------------------------------------------------
;;; This function is recursive
;;;
(defun tinypath-add-directory-many-below-root-dir (root)
  "Add all directories below ROOT to `load-path'."
  (if (not (stringp root))
      (tinypath-verbose-macro 5
	(message "TinPath: Cannot add below root. Not a string: %s"
		 (prin1-to-string root)))
    (if (not (and (file-exists-p root)
		  (file-directory-p root)
		  (not (file-symlink-p root))))
	(tinypath-verbose-macro 3
	  (message "TinyPath: root does NOT exist: %s" root))
      (setq root (tinypath-expand-file-name root))

      (tinypath-verbose-macro 3
	(message "TinyPath: root %s" root))

      (let* ((list (tinypath-subdirectory-list root)))
	(when (tinypath-path-ok-p root)
	  (tinypath-verbose-macro 5
	    (message "TinyPath: adding        %s" root))
	  (tinypath-info-handler root)
	  (tinypath-add-directory-one root))
	(dolist (path list)
	  (tinypath-verbose-macro 3
	    (message "TinyPath: recursing => %s" path))
	  (tinypath-add-directory-many-below-root-dir path)))  )))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove-old (regexp)
  "Remove all paths matching REGEXP from `load-path'"
  (setq load-path
	(remove-if
	 (function
	  (lambda (x)
	    (string-match regexp x)))
	 load-path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove (regexp)
  "Remove any matching REGEXP from `load-path'.
Return t if removed something."
  (let* ((spare load-path)
	 list
	 status)
    (dolist (elt load-path)
      (if (string-match regexp elt)
	  (setq status t)
	(push elt list)))
    (cond
     ((null list)
      (setq load-path spare)
      (tinypath-verbose-macro 3
	(message "TinyPath: FATAL regexp %s cleaned whole load-path."
		 regexp)))
     (t
      (setq load-path list)))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove-cache (regexp)
  "Remove any matching REGEXP from `tinypath-:cache'.
Return t if removed something."
  (let* ((spare tinypath-:cache)
	 status)
    (dolist (elt tinypath-:cache)
      (when (string-match regexp
			;;  ELT = '("file.el" (POS . "path"))
			(cdr (nth 1 elt)))
	  (setq status t)
	  (setq tinypath-:cache (delete elt tinypath-:cache))))
    (unless tinypath-:cache
      (setq tinypath-:cache spare)
      (tinypath-verbose-macro 3
	(message "TinyPath: FATAL regexp %s cleaned whole tinypath-:cache."
		 regexp)))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-setup ()
  "This is default function to add paths to `load-path'.
Add all paths below `tinypath-:load-path-root'. See this variable.

References:

  `tinypath-:load-path-function'"
  (let ((list tinypath-:load-path-root))

    (if (stringp list)   ;; make one string into LIST
	(setq list (list list)))

    ;;  This message is a little premature, but it cleaner here,
    ;;  than after the dolist loop
    (message
     "TinyPath: SETUP `tinypath-:load-path-root' was checked and cleaned.")

    (dolist (elt list)
      (if (not (stringp elt))
	  (message "TinyPath: `tinypath-:load-path-root' ELT `%s' \
is not a string. `tinypath-:load-path-root': %s "
		   (prin1-to-string elt)
		   (prin1-to-string tinypath-:load-path-root)))
      (tinypath-verbose-macro 2
	(message "TinyPath: => load path root %s " elt))
      (tinypath-add-directory-many-below-root-dir elt))))

;;;}}}

;;;{{{ Cache

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-directory-files (path-list)
  "Return all files along PATH-LIST."
  (let ((count        0)
	(dont-regexp  "[#~]") ;; don't cache backups
	list)
    (dolist (dir path-list)
      (when (and (stringp dir)
		 (file-directory-p dir))

	;;   make sure directory has a slash at the end

	(setq dir (file-name-as-directory dir))

	;;  TRAD means "traditional Emacs Lisp way", because
	;;  there is new method EXT for "External tool" to do similar
	;;  caching. In fact if you see these messages, something
	;;  went wrong with the EXT method.

	(tinypath-verbose-macro 1
	  (message "TinyPath: TRAD Caching files... %d %s"
		   (length list)
		   dir))

	(dolist (file (directory-files dir))
	  (when (and (not (file-directory-p (concat dir file)))
		     (not (string-match dont-regexp file)))
	    (incf count)
	    (when (or t ) ;; (string-match "other" dir))
	      (tinypath-verbose-macro 9
		(message "TinyPath: TRAD Cached %s"
			 (concat dir file))))
	    (push (list file (cons count
				   (tinypath-expand-file-name dir)))
		  list)))))
    ;; Preserve find order.
    (reverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-merge (list)
  "Merge LIST to `load-path'."
  ;;  Merge original path to loaded path
  (dolist (path list)
    (pushnew path load-path :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-clear ()
  "Clear variables. Call `tinypath-cache-setup-scan' after this."
  (setq tinypath-:cache nil)
  (setq tinypath-:cache-level-two nil)
  (tinypath-load-path-clean))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-scan  (&optional traditional)
  "Build the cache either by using external program or Emacs Lisp."
  (let* ((external (not traditional)))
    (or (and external
	     (tinypath-external-setup))
	(progn
	  (tinypath-verbose-macro 3
	    (message
	     (concat
	      "TinyPath: "
	      "TRAD lisp method used for scanning.")))
	  (tinypath-maybe-warn-message-log-max)
	  (tinypath-info-scan-Info-default-directory-list)
	  (funcall tinypath-:load-path-function)
	  (setq tinypath-:cache (tinypath-load-path-directory-files
				 load-path))))
    ;;  There are many push and pushnew called. Clean Emacs on exit.
    (when (fboundp 'garbage-collect)
      (message "TinyPath: cache-setup-scan requested `garbage-collect'")
      (garbage-collect))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-status-string ()
  "Return cache statistics."
  (format "TinyPath: total %d, load-path %d, exec-path %d, Info %d"
	  (length tinypath-:cache)
	  (length load-path)
	  (length exec-path)
	  (length (tinypath-Info-default-directory-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-status-message ()
  "Print cache statistics."
  (interactive)
  (message (tinypath-cache-status-string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup (&optional force traditional)
  "Set `load-path' and possibly using cache.
If `tinypath-:cache-file' is recent enough load it, otherwise
Rescan directories if cache file is older than
`tinypath-:cache-expiry-days'. After Rescan save cache.

Input:

  FORCE       Rescan and save cache.
  TRADITIONAL Use traditional Emacs lisp cache scan."
  (interactive "P")
  (let* ((file       (tinypath-cache-file-name))
	 (read-cache (and (null force)
			  (stringp file)
			  (file-exists-p file)
			  (null (tinypath-cache-file-old-p file))))
	 no-save)

    ;; .............................................. compressed cache ...

    (tinypath-use-compression-maybe file)

    ;; .................................................... load cache ...

    (let ((orig load-path))
      (when (and (null force)
		 read-cache)
	(setq force (tinypath-cache-file-load))
	(tinypath-load-path-merge orig)))

    ;; .......................................................... scan ...

    ;;  We clean everything before scan. This has two purposes
    ;;
    ;;  - Remove invalid entries
    ;;  - Expand all paths to use absolute names and forward slashes.
    ;;    Expand is needed because all tests are done using absolute paths:
    ;;    `member', `pushnew' etc. Emacs and XEmacs Win32 differences are
    ;;    also solved with expand.

    (tinypath-load-path-clean)

    (tinypath-Info-default-directory-list)
    (tinypath-Info-default-directory-list-clean)

    (tinypath-directory-list-clean
     tinypath-:extra-path-root
     "tinypath-:extra-path-root")


    ;; .............................................. regenerate cache ...

    (when (or force
	      (null (file-exists-p file))
	      (null tinypath-:cache))
      (setq force t)  ;; Write cache too

      ;; Remove invalid entries so that they are not saved
      (tinypath-cache-setup-clear)
      (tinypath-cache-setup-scan traditional)

      ;; Clean invalid entries

      (tinypath-directory-list-clean
       tinypath-:extra-path-root
       "tinypath-:extra-path-root")

      (tinypath-directory-list-clean
       tinypath-:extra-manpath
       "tinypath-:extra-manpath")

      (tinypath-load-path-clean)
      (tinypath-Info-default-directory-list-clean))

    (tinypath-exec-path-clean)
    (tinypath-exec-path-check-verbose 'fix) ;; Missing items? (from PATH)

    ;; ................................................... write cache ...

    (unless load-path
      (tinypath-message-bug "FATAL SCAN load-path nil")
      ;;  Try rescue as best as we can, so that User's Emacs is still usable
      (message "TinyPath: FATAL trying to boot to restore load-path.")
      (tinypath-load-path-initial-value)
      (unless load-path
	(tinypath-message-bug
	 "FATAL SCAN2 load-path still nil, disable tinypath.el"))
      (setq no-save t))

    (tinypath-load-path-reorder)    ;; Emacs dist paths are put last

    (when (and (null no-save)
	       (or force
		   (and tinypath-:cache-expiry-days ;cache allowed
			(null read-cache))))	;but now expired
      (tinypath-cache-file-write file))

    (tinypath-cache-status-message)

    ;; Make sure that this list is cleared. It must be
    ;; regenerated as well.

    (tinypath-emacs-lisp-file-list-cache-clear)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-maybe ()
  "If `load-path' or `tinypath-:cache' is out of date, rebuild cache."
  (interactive)
  (when (or (tinypath-cache-non-existing-directory-list)
	    (tinypath-cache-non-existing-file-list))
    (tinypath-verbose-macro 2
      (message "TinyPath: Cache validate: inconsistent state, rebuilding..."))
    (tinypath-cache-setup 'force)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-font-lock (&optional buffer)
  "Call `font-lock' with `tinypath-:font-lock-keywords' in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (turn-on-font-lock)
    (make-local-variable 'font-lock-keywords)
    (set 'font-lock-keywords tinypath-:font-lock-keywords)
    (font-lock-fontify-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-different-size-p (elt)
  "Called by `tinypath-cache-duplicate-report'.
Check if ELT contains different files by size."
  (let (path
	file
	stat
	size
	size-old
	ret)
    (setq file (car elt)
	  elt  (cdr elt))
    (dolist (item elt)
      (setq path  (concat (cdr item) file)
	    stat  (file-attributes path)
	    size  (nth 7 stat))
      (when (and size-old
		 (not (eq size-old size)))
	(setq ret t)
	(return))
      (setq size-old size))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-report (&optional size-rank)
  "Report all identical lisp files in `tinypath-:cache' and rank by SIZE.

Input:

  SIZE-RANK

        if given, report duplicate file only if the size is
        different. If you just have copy of the same file in the
        `load-path' that is not critical, but if the file size differs
        then you have different versions of the file and you should
        remove the old one(s) from path.

Output:

  alist.el
	    35  2971 1999-02-27 12:51:12 /usr/share/site-lisp/common/mime/apel-9.13/
	  1166  2971 1999-11-25 00:37:18 /home/foo/elisp/tiny/lisp/other/
             |  |    |                   |
             |  |    |                   location
             |  |    ISO 8601 modification time
             |  size
             the order number in cache

References:

  `tinypath-:cache-duplicate-report-hook'."
  (interactive "P")
  (let* ((ignore-functions
	  tinypath-:cache-duplicate-report-ignore-functions)
	 accept
	 list
	 stat
	 size
	 date
	 list-tmp
	 list-dup
	 file
	 path
	 ptr
	 seen)

    ;; .................................................... build list ...

    ;;  result: ( (FILE . (PATH PATH PATH ..)) (FILE . (PATH ..)) )

    (dolist (elt tinypath-:cache)
      (setq file  (car elt)
	    path  (nth 1 elt))
      (when (string-match "\\.el" file)
	(when (win32-p)
	  (setq file (downcase file)))
	(setq accept
	      (or (and
		   ignore-functions
		   (null
		    (let (ret)
		      (dolist (func ignore-functions)
			(when (funcall func (concat (cdr path) file))
			  (setq ret t)
			  (return)))
		      ret)))
		  (null ignore-functions)))

	(when accept
	  (if (not (setq ptr (assoc file list)))
	      (push (cons file (list path)) list)
	    (setq list-tmp (cdr ptr))
	    (push path list-tmp)
	    (setcdr ptr list-tmp)))))

    ;; .............................................. check duplicates ...

    (dolist (elt list)
      (when (> (length (cdr elt)) 1)
	(push elt list-dup)))

    ;; ................................................. print results ...

    (if (null list-dup)
	(message "TinyMy: No duplicates in `tinypath-:cache'")
      (let ((sorted (sort
		     list-dup
		     (function
		      (lambda (a b)
			(setq a (car a)
			      b (car b))
			(string< a b))))))
	(setq list-dup sorted))

      (display-buffer (get-buffer-create tinypath-:report-buffer))

      (with-current-buffer tinypath-:report-buffer
	(erase-buffer)
	(tinypath-report-mode 'verbose)
	(dolist (elt list-dup)
	  (when (tinypath-cache-duplicate-different-size-p elt)

	    (setq file (car elt))
	    (insert file "\n")

	    (dolist (elt (reverse (cdr elt)))
	      (setq path  (concat (cdr elt) file))
	      (unless (member path seen)
		(push path seen)
		(if (not (file-exists-p path))
		    (insert "\t  ERROR: file does not exist " path "\n" )
		  (setq stat  (file-attributes path)
			size  (nth 7 stat)
			date  (nth 5 stat))
		;; ISO 8601 date
		(setq date (tinypath-time-string date))
		(insert (format "\t %5d %5d %s %s\n"
				(car elt)
				size
				date
				path))))))))) ;; dolist-dolist

    (with-current-buffer tinypath-:report-buffer
      (goto-char (point-min))
      (run-hooks 'tinypath-:cache-duplicate-report-hook))

    list-dup))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-timing-summary ()
  "Gather timing summary from *Message* buffer if `tinypath-:verbose-timing'."
  (interactive)
  (let* ((buffer (tinypath-message-get-buffer))
	 string)
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (while (re-search-forward "^TinyPath: load time.*" nil t)
      (setq string (concat (or string "") "=> " (match-string 0) "\n")))
    (message "Tinypath: [TIMING SUMMARY FROM ABOVE]" string)
    (goto-char (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-report-mode-map-activate ()
  "Use local `tinypath-report-mode-map' in current buffer.
\\{tinypath-report-mode-map}"
  (use-local-map tinypath-report-mode-map))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-previous ()
  "Go to previous file."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward "^[ \t]+[0-9].*/\\(.\\)" nil t)
      (goto-char (match-beginning 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-next ()
  "Go to next file."
  (interactive)
  (re-search-forward "^[ \t]+[0-9].*/" nil t))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-delete-file-noconfirm ()
  "Delete file in the current line without confirmation."
  (interactive)
  (tinypath-report-mode-delete-file 'force))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-find-file ()
  "Load file in current line to Emacs."
  (interactive)
  (let* ((file (tinypath-report-mode-file-name)))
    (cond
     ((null file)
      (message "TinyPath: No file in this line.")
      nil)
     (t
      (display-buffer (find-file-noselect file))))))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-report-mode-file-name ()
  "Read filename under point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward
	   " ..:..:..[ \t]+\\(.*\\)"
	   (save-excursion (end-of-line) (point))
	   t)
      (tinypath-ti::string-remove-whitespace (match-string 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-delete-file (&optional force)
  "Delete file in the current line. FORCE deleting.
See also `tinypath-report-mode-delete-file-noconfirm'."
  (interactive "P")
  (let* ((file (tinypath-report-mode-file-name)))
    (cond
     ((null file)
      (message "TinyPath: No file in this line."))
     ((not (file-exists-p file))
	(message "TinyPath: file not found %s" file))
     ((or force
	  (y-or-n-p (format "Really delete %s " file)))
      (delete-file file)
      (message "TinyPath: deleted %s" file)
      (overwrite-mode 1)
      (beginning-of-line)
      (insert "*")
      (overwrite-mode -1)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypath-report-mode (&optional verb)
  "Major mode to help working with `tinypath-cache-duplicate-report'. VERB.

\\{tinypath-report-mode-map}"
  (interactive "P")
  (tinypath-report-mode-map-activate)	;turn on the map
  (setq  mode-name   tinypath-:report-mode-name)
  (setq  major-mode 'tinypath-report-mode);; for C-h m
  (when verb
    (message
     (substitute-command-keys
      (concat
       "delete file \\[tinydesk-report-mode-delete-file]")))
    (sleep-for 1))
  (tinypath-report-mode-font-lock)
  (run-hooks 'tinypath-:report-mode-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-existing-file-list ()
  "Return list of non existing files in cache."
  (let (list
	path)
    (dolist (elt tinypath-:cache)
      ;; ( ("file" (POS . PATH)) .. )
      (setq path (concat (cdr (nth 1 elt))
			 (car elt) ))
      (unless (file-exists-p path)
	(push path list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-existing-directory-list ()
  "Return list of non existing directories in cache or `load-path'."
  (let (list
	path)
    (dolist (dir tinypath-:cache)
      ;; ( ("file" (POS . PATH)) .. )
      (setq dir (cdr (nth 1 dir)))
      (unless (file-exists-p dir)
	(pushnew path list :test 'string=)))
    (dolist (dir load-path)
      (unless (file-exists-p dir)
	(pushnew path list :test 'string=)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-exist-report ()
  "Report non-existing files in cache."
  (interactive)
  (let ((list (tinypath-cache-non-existing-file-list)))

    (if (null list)
	(message "TinyMy: No non-existing files in `tinypath-:cache'")

      (display-buffer (get-buffer-create tinypath-:report-buffer))

      (with-current-buffer tinypath-:report-buffer
	(goto-char (point-max))
	(tinypath-report-mode-font-lock)
	(insert "\nNon Existing files:\n")
	(dolist (elt list)
	  (insert "  %s\n" elt))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-problem-report (&optional size-rank)
  "Generate problem report: non-existing files and duplicates.
See SIZE-RANK in `tinypath-cache-duplicate-report'."
  (interactive)
  (tinypath-cache-non-exist-report)
  (tinypath-cache-duplicate-report))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-regenerate (&optional delete-cache)
  "Regenerate cache. `tinypath-cache-setup' is called with arg t.
The DELETE-CACHE removes any previous stored cache from disk.
Use it for completely clean any previous cache conflicts."
  (interactive "P")
  (when delete-cache
    (tinypath-cache-file-delete))
  (tinypath-info-scan-Info-default-directory-list)
  (tinypath-cache-setup 'regenerate))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-mode (mode)
  "Toggle fast package loading MODE by enabling or disabling advises.

Input:

    If MODE is positive integer, enable defadvice code to to utilize
    package (possibly compressed) lookup from `tinypath-:cache'.

Description:

    If you have many directories in your `load-path', turning this mode on
    makes packages load instantly without time consuming path lookup.

Warning:

  Regenerate cache with \\[tinypath-cache-regenerate] if you have installed new
  packages or if you have added new lisp files to your system. Keep also
  `tinypath-:cache-expiry-days' relatively small if you update often."
  (let* ((list '(locate-library
		 load
		 require)))

    ;; In Emacs (at least on 20.7), load-library is a wrapper for load. So,
    ;; it makes no sense advising it, because the cache is searched twice.
    ;; #todo: check this code .. and xemacs load-library

    (when t  ;;  (xemacs-p)
      (push 'load-library list))

    ;;  Activate only if user requested 'all
    (when (eq tinypath-:compression-support 'all)
      (push 'autoload list))

    (tinypath-ti::bool-toggle tinypath-:cache-mode mode)

    (cond
     (tinypath-:cache-mode
      (tinypath-ti::advice-control list "tinypath")
      (message "TinyPath: cache advice code ACTIVATED."))
     (t
      (tinypath-ti::advice-control list "tinypath" 'disable)
      (message "TinyPath: cache advice code DEACTIVATED.")))))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinypath-cache-mode ()
  "See `tinypath-cache-mode'."
  (interactive)
  (tinypath-cache-mode 1))

;;; ----------------------------------------------------------------------
;;;
(defun turn-off-tinypath-cache-mode ()
  "See `tinypath-cache-mode'."
  (interactive)
  (tinypath-cache-mode -1))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinypath-cache-mode-maybe ()
  "See `tinypath-cache-mode'.
Turn mode on only if `tinypath-:cache-expiry-days' is non-nil,
otherwise turn mode off."
  (interactive)
  (if (integerp tinypath-:cache-expiry-days)
      (turn-on-tinypath-cache-mode)
    (turn-off-tinypath-cache-mode)))

;;;}}}
;;;{{{ Advice code

(defun tinypath-advice-instantiate ()         ;; ######## BEGIN FUNCTION
  "Intantiate all advices."
  ;;  These are put into function to make them delayed and
  ;;  so that they can be called at apropriate time.

(require 'advice)

;;  I don't know what EFS does, but it certainly must be loaded before we
;;  try to advice `require' or `load' functions. It somehow overwrites the
;;  the original definitions.
;;
;;  efs.el
;;
;;  (efs-overwrite-fn "efs" 'load)
;;  (efs-overwrite-fn "efs" 'require)
;;
;;  See also efs-ovwrt.el

(when (xemacs-p)
  (require 'efs))

;;; ----------------------------------------------------------------------
;;;
(defadvice autoload (around tinypath dis)
  "Use `tinypath-:cache' for fast lookup of files."
  (let* ((file  (ad-get-arg 1))
	 (path  (tinypath-cache-p file)))
    (when path
      (tinypath-verbose-macro 5
	(message "TinyPath: (advice autoload) Cache hit %s" file))
      (tinypath-cache-warn-if-not-exist path)
      (ad-set-arg 1 path))
    ad-do-it))

;;; ----------------------------------------------------------------------
;;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
;;;
(defadvice load (around tinypath dis)
  "Use `tinypath-:cache' for fast lookup of files."
  (let* ((file        (ad-get-arg 0))
	 (nosuffix    (ad-get-arg 3))
	 (must-suffix (ad-get-arg 4)))
    (when (and (null nosuffix)
	       (null must-suffix))
      ;; #todo: this needs better handling, perhaps? Now we just
      ;; ignore cache if suffix parameters are set.
      ;;
      ;; If optional fourth arg NOSUFFIX is non-nil, don't try adding
      ;; suffixes `.elc' or `.el' to the specified name FILE. If optional
      ;; fifth arg MUST-SUFFIX is non-nil, insist on the suffix `.elc' or
      ;; `.el'; don't accept just FILE unless it ends in one of those
      ;; suffixes or includes a directory name.
      (let ((path  (tinypath-cache-p file)))
	(when path
	  (tinypath-verbose-macro 5
	    (message "TinyPath: (advice load) Cache hit %s" file))
	  (tinypath-cache-warn-if-not-exist path)
	  (ad-set-arg 0 path))))
    (tinypath-time-macro file
      ad-do-it)))

;;; ----------------------------------------------------------------------
;;;
(defadvice load-library (around tinypath dis)
  "Use `tinypath-:cache' for fast lookup of files."
  (let* ((file  (ad-get-arg 0))
	 (path  (tinypath-cache-p file)))
    (when path
      (tinypath-verbose-macro 5
	(message "TinyPath: (advice load-library) Cache hit %s" file))
      (tinypath-cache-warn-if-not-exist path)
      (ad-set-arg 0 path))
    ad-do-it))

;;; ----------------------------------------------------------------------
;;; In Win32 XEmacs 21.2 beta; the this function calls `locate-file' which
;;; for some reason breaks if given a absolute file name. The XEmacs
;;; docs also say that `locate-file' uses hash table to speed up processing.
;;; Hm.
;;;
;;; There is problem with functions that use (interactive-p) test, because
;;; advice can't pass the information to the underlying function, so any
;;; such test inside there won't work.
;;;
(defadvice locate-library (around tinypath act)
  "Use `tinypath-:cache' for fast lookup of files."
  (interactive
   (let ((cache (tinypath-emacs-lisp-file-list 'from-cache)))
     (list
      (completing-read
       (format "%slocate library: "
	       (if cache
		   "(TinyPath cache)"
		 ""))
       cache
       nil
       nil
       nil))))  ;;; Default word
  (let* ((file  (ad-get-arg 0))
	 (path  (tinypath-cache-p file))
	 (error (and path (tinypath-cache-warn-if-not-exist path))))
    (cond
     ((and path
	   (null error))
      (tinypath-verbose-macro 5
	(message "TinyPath: (advice locate-library) Cache hit %s => %s"
		 file path))
      (setq ad-return-value path))
     (t ;;  (fboundp 'locate-file)  ;; Do not continue in XEmacs
      (when (setq path  (car-safe (tinypath-locate-library file)))
	(setq ad-return-value path))))
    ;; We must simulate in the advice, this interactive behavior, because
    ;; underlying function does not know it any more, due to advice.
    (when (interactive-p)
      (if path
	  (message path)
	(message "locate-library: %s not found."
		 (or file "<no filename info>"))))))

;;; ----------------------------------------------------------------------
;;;
(defadvice require (around tinypath dis)
  "Use `tinypath-:cache' for fast lookup of files.
Property (get 'require 'tinypath-load-list) contains list
of required packages: '((feature . path)."
  (let* ((feature  (ad-get-arg 0))
         (opt      (ad-get-arg 1))       ;the optional "file" parameter
	 (alist    (get 'require 'tinypath-load-list))
	 lib
	 path)
    (when (and (not (featurep feature))
	       ;;  Avoid recursive calls.
	       (not (assq feature alist)))
      (setq lib   (cond
		   ((stringp opt)
		    (if (string-match "/" opt)
			(tinypath-expand-file-name opt)  opt))
		   (t
		    (symbol-name feature))))
      (when (setq path  (tinypath-cache-p lib))
	(tinypath-verbose-macro 5
	  (message "TinyPath: (advice require) Cache hit %s" lib))
	(tinypath-cache-warn-if-not-exist path)
	(push (cons feature path) alist)
	(put 'require 'tinypath-load-list alist)
	(ad-set-arg 1 path)))
    (tinypath-time-macro (symbol-name feature)
      ad-do-it)))

) ;; ############################   END FUNCTION -- end advice instantiate

;;;}}}
;;;{{{ win32: Unix $HOME directory mounted to PC, like to H: disk

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-dump (mount-point &optional file)
  "Dump load path directories to disk.

If you have Mounted Unix disk (say H: ) which sees your Unix $HOME directory,
then keep in mind that NT Emacs does not see symlinked directories.

Call this function from _Unix_ Emacs and it converts symbolic links to
real directory names and writes output to FILE.

You can then load that file in your NT emacs and make it see all
the same directories as your Unix Emacs does.

Repeat this every time you make symbolic path links in Unix.

References:

  `tinypath-:load-path-dump-file'"
  (interactive "sUnix $HOME is equivalent to: \nf")
  (let* ((home      (file-truename (tinypath-expand-file-name "~")))
	 (load-path load-path))
    (setq tinypath-dumped-load-path nil)

    (or file
	(setq file tinypath-:load-path-dump-file))

    (dolist (path load-path)
      (if (not (string-match "[a-z]" mount-point))
	  (setq path (file-truename path))
	(setq path (tinypath-replace-regexps-in-string
		    (regexp-quote home)
		    mount-point
		    (file-truename path))))
      (push path tinypath-dumped-load-path))

    (tinypath-ti::write-file-variable-state
     file "Absolute path dump for NTEmacs to access Unix Home disk"
     '(tinypath-dumped-load-path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-setup-win32 ()
  "Load `tinypath-:load-path-dump-file' in win32."
  (let* ((file tinypath-:load-path-dump-file))
    (when (win32-p)
      (when (load file 'noerr)
	;; Merge these unix paths with the NT Emacs paths.
	;; If these paths do not exist; they are not added
	(tinypath-verbose-macro 2
	  (message "TinyPath: load-path merge from %s" file))
	(tinypath-add-directory-many
	 (symbol-value 'tinypath-dumped-load-path))))))

;;}}}
;;{{{ Win32 support (cygwin)

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-manpage-handler (path)
  "If PATH has manual pages, add to `tinypath-:extra-manpath'."
  (let* (ret)
    (unless (member path tinypath-:extra-manpath)
      (dolist (file (directory-files path))
	(when (string-match "\\.[0-9]$" file)
	  (tinypath-verbose-macro 9
	    (message "TinyPath: MAN %s [found %s] " path file))
	  (pushnew path tinypath-:extra-manpath :test 'string=)
	  (setq ret path)
	  (return))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-extra-path-handler (path)
  "Check PATH for info files and manual pages."
    (tinypath-info-handler path)
    (tinypath-manpage-handler path))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-woman-setup ()
  "Install woman.el (if available) to read manual pages in Win32."
  (when (win32-p)
    (if (not (and (not (featurep 'woman))
		  (locate-library "woman.el")))
	(message "\
  ** tinypath.el: Hm, no woman.el found to read manual pages.
                  http://www.maths.qmw.ac.uk/~fjw/")

      (autoload 'woman		        "woman" "" t)
      (autoload 'woman-find-file	"woman" "" t)
      (autoload 'woman-dired-find-file  "woman" "" t)

      (unless (getenv "MANPATH") ;; woman-path
	(message "TinyPath: MANPATH does not exist, set `woman-manpath'."))

      (defalias 'man 'woman)
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-extra-path-setup (list)
  "Look for new info and manual pages under LIST of root directories."
  (dolist (path list)
    (if (or (not (stringp path))
	    (not (file-directory-p path)))
	(tinypath-verbose-macro 5
	  (message
	   "TinyPath: invalid search ROOT %s"
	   (prin1-to-string path)))
      (tinypath-ti::directory-recursive-do
       path 'tinypath-extra-path-handler))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cygwin-setup ()
  "If Cygwin is present add it to `tinypath-:extra-path-root'."
  (let* ((cygwin-path (win32-cygwin-p)))  ;; has trailing slash

    (if (null cygwin-path)
	(tinypath-verbose-macro 2
	  (message "TinyPath: [Cygwin] not found from PATH."))

      (pushnew cygwin-path
	       tinypath-:extra-path-root
	       :test 'string=)

      ;;  Be absolutely sure that the path is not added multiple
      ;;  times "f:/unix/cygwin" or "f:/unix/cygwin/" because
      ;;  this would make scanning the same directory twice

      (tinypath-directory-list-clean  ;; No trailing slashes after this
       tinypath-:extra-path-root
       "CYGWIN tinypath-:extra-path-root")

      (tinypath-verbose-macro 2
	(message "TinyPath: [Cygwin] found from PATH: %s" cygwin-path))

      ;; (tinypath-extra-path-setup list)
      tinypath-:extra-path-root)))

;;}}}
;;{{{ Install functions

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-timer (&optional uninstall)
  "Install or UNINSTALL timer to keep cache structure in synch with disk.
Reference:
  `tinypath-cache-setup-maybe'  15min, idle timer calls this periodically."
  (interactive "P")
  (let* (timer
	 status)
    (when (fboundp 'run-with-idle-timer)

      ;;  I don't think this ever fails, but be bullet proof anyway
      ;;  We ,ust run `require' because `run-with-idle-timer'
      ;;  must not be in autoload state.
      ;;
      ;;  timers are different in Emacs implementations. Load correct
      ;;  package.
      ;;  XEmacs keeps this in xemacs-packages/lisp/fsf-compat/timer.el

      (setq status
	    (cond
	     ((xemacs-p)
	      (or (require 'itimer)
		  (require 'timer)))
	     (t
	      (require 'timer))))

      (if (null status)
	  (tinypath-verbose-macro 1
	    (message "TinyPath: TIMER ERROR Can't install timers to emacs."))
	(cond
	 (uninstall
	  (tinypath-ti::xe-timer-cancel-function
	   'tinypath-cache-setup-maybe)
	  (message
	   "TinyPath: `load-path' synchronization watchdog UNINSTALLED."))
	 (t
	  (tinypath-ti::xe-timer-cancel-function
	   'tinypath-cache-setup-maybe)
	  ;;  At this point, we have wiped out the autoload definitions
	  ;;  with explicit `require', because `symbol-function'
	  ;;  won't work on autoloaded definitions.
	  (tinypath-autoload-require 'run-with-idle-timer)
	  (setq timer
		(funcall
		 (symbol-function 'run-with-idle-timer)
		 (* 60 15)
		 'repeat
		 'tinypath-cache-setup-maybe))
	  (message
	   "TinyPath: `load-path' synchronization watchdog INSTALLED.")))))
    (setq tinypath-:timer-elt timer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-woman ()
  "Add items in `tinypath-:extra-manpath' to `woman-manpath'."
  (when (boundp 'woman-manpath)
    (dolist (path tinypath-:extra-manpath)
      (when (stringp path)
	(tinypath-verbose-macro 7
	  (message "TinyPath: Adding to `woman-manpath' %s" path))
	(pushnew path woman-manpath :test 'string=)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-find-file ()
  "Add items in `tinypath-:extra-manpath' to `woman-manpath'."
  (when (boundp 'ff-search-directories)
    (dolist (path tinypath-:extra-ff-search-directories)
      (when (stringp path)
	(tinypath-verbose-macro 7
	  (message "TinyPath: Adding to `ff-search-directories' %s" path))
	(pushnew path ff-search-directories :test 'string=)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-load-path-list ()
  "Return Gnus locations in `load-path' by searching regexp gnus/?$"
  (let* (list)
    (dolist (path load-path)
      ;; "../gnus/"    or "../gnus"
      (if (string-match "\\(.*[/\\]gnus\\)\\([/\\]\\|$\\)" path)
	  (pushnew
	   (match-string 1 path)
	   list
	   :test 'string=)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-versions (&optional path-list)
  "Find out gnus version numbers along `load-path' or PATH-LIST.
The PATH-LIST must conatins the root directoryies of Gnus installations.
Return ((VER . PATH) ..)."
  (let* (file
	 list)
    ;; There is no way we can say which Gnus version is the latest without
    ;; loading the gnus.el and looking inside the file
    (with-temp-buffer
      (dolist (path (or path-list
			(tinypath-gnus-load-path-list)))
	;;  XEmacs installation drop all gnus lisp files directly under:
	;;
	;;      xemacs-packages/lisp/gnus/
	;;
	;;  But the Gnus CVS tree contains directory structure
	;;
	;;      cvs-packages/gnus/lisp/
	;;      cvs-packages/gnus/contrib
	;;      cvs-packages/gnus/etc
	;;
	(dolist (try '("gnus.el" "lisp/gnus.el"))
	  (setq file (concat
		      (tinypath-expand-file-name
		       (file-name-as-directory path))
		      try))
	  (when (file-exists-p file)
	    (erase-buffer)
	    ;;  About within 10%  of the file size the defconst can be found
	    (insert-file-contents file nil 1 10000)
	    (goto-char (point-min))
	    (when (re-search-forward
		   "defconst.*gnus-version.*\"\\([0-9.]+\\)"
		   nil t)
	      (push (cons (match-string 1) file)
		    list)))))
      (tinypath-verbose-macro 7
	(message "TinyPath: found Gnus versions %s" (prin1-to-string list)))
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-latest-version (path-list)
  "Return latest gnus version from PATH-LIST.
Return structure is ordered so, that the latest version is first:
'((VERSION-STRING . PATH) ..)."
  (let* ((ver    (tinypath-gnus-versions path-list))
	 (sorted (and ver
		      (sort
		       ver
		       (function
			(lambda (a b)
			  (setq a (car a)
				b (car b))
			  (string< a b))))))
	 (version-first (and sorted
			     (caar sorted))))
    (if (and (stringp version-first)
	     (string-match "^0" version-first))
	sorted   ;;  that's it, development gnus is the latest 0.01 ..
      ;; 5.8.8 or something is latest
      (reverse sorted))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-gnus ()
  "Examine `load-path' and leave the latest Gnus version."
  (let* ((list (tinypath-gnus-load-path-list)))
    (cond
     ((null list)
      (tinypath-verbose-macro 7
	(message "TinyPath: No newer Gnus found along `load-path'.")))
     ((eq 1 (length list))
      ;;  Make sure no old gnus is used.
      (setq tinypath-:cache-level-two nil)
      (tinypath-verbose-macro 1
	(message "TinyPath: One Gnus found along `load-path' %s"
		 (car list)))
      list)
     (t
      ;; Latest gnus version is first in the returned list, drop it out
      ;; and remove all other paths.
      ;;
      (dolist (path (cdr (tinypath-gnus-latest-version list)))
	(setq path
	      (tinypath-file-remove-trailing-slash
	       (file-name-directory (cdr path))))
	;;  some/dir/gnus/lisp/  -->  some/dir/gnus/
	(tinypath-verbose-macro 1
	  (message "TinyPath: Removing older Gnus from `load-path' %s"
		   path))
	(tinypath-admin-remove-matching path)
	path)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-setup (&optional no-cache force)
  "Add additional directories to `load-path'.
If `tinypath-:cache-expiry-days' is defined, use cached `load-path'
If cache is too old, read directories under `tinypath-:load-path-root'.

Input:

  NO-CACHE   If non-nil, do not use cache but read directories under
             `tinypath-:load-path-root'.
  FORCE      Regenerate cache.

References:

  `tinypath-:load-path-function'"
  (interactive "P")
  (if (or no-cache
	  (null tinypath-:cache-expiry-days)) ;Cache is not allowed
      (funcall tinypath-:load-path-function)
    (tinypath-cache-setup force)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-root-changed-p ()
  "Check if `tinypath-:load-path-root' has changed since last run.
The property value (get 'tinypath-:load-path-root 'tinypath-last-value)
holds the last stored value."
    (let ((last (get 'tinypath-:load-path-root 'tinypath-last-value)))
      (and last
	   (not (equal last tinypath-:load-path-root)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install ()
  "Install package. There is no uninstall."
  (interactive)
  (let* ((fid     "tinypath-install")
	 (time-a  (current-time))
	 time-b
	 diff)

    (message "TinyPath: %s BEGIN %s" fid (tinypath-time-string))

    ;;  Must be before the cygwin check, where cygwin1.dll is
    ;;  searched along `exec-path'

    (tinypath-exec-path-clean)
    (tinypath-exec-path-check-verbose 'fix) ;; Missing items? (from PATH)

    ;;  This is already set in default value for `tinypath-:extra-path-root'
    ;;  (when (win32-p) (tinypath-cygwin-setup))

    ;; ................................................ examine system ...

    ;;  Make sure all are absolute: use forward slash in all path names

    (tinypath-expand-file-name-variable-macro
      tinypath-:load-path-root)

    ;;  Suppose user has changed the value since the last time
    ;;  and does M-x load-library RET tinypath.el RET
    ;;  => check if we should regenerate cache or read from disk

    (if (not (tinypath-load-path-root-changed-p))
	(tinypath-setup)
      (message
       "TinyPath: INSTALL tinypath-:load-path-root changed, doing reboot.")
      ;; (tinypath-cache-regenerate)
      nil)

    ;; ........................................ cleanup and activation ...

    ;; Delay defining advises until this point

    (unless (eq tinypath-:compression-support 'none)
      (tinypath-advice-instantiate))

    ;;  The autoload statements must be here, because only now `autoload' is
    ;;  an advised function. The `fboundp' is just an extra measure,
    ;;  so that it does not even call the advised-autoload function if
    ;;  this file is loaded multiple times

    (unless (fboundp 'ti::macrof-version-bug-report)
      (autoload	'ti::macrof-version-bug-report "tinylib" "" nil 'macro))

    (unless (fboundp 'font-lock-mode)
      (autoload 'font-lock-mode "font-lock"  "" t))

    (unless (eq tinypath-:compression-support 'none)
      (turn-on-tinypath-cache-mode-maybe))

    (tinypath-install-timer)       ;; Install watchdog to check load-path

    ;;  woman.el, man page viewer for Win32
    ;;  We do not load this, but define autoloads and then add the found
    ;;  paths after woman is active.

    (when (win32-p)
      (if (tinypath-woman-setup)
	  (tinypath-eval-after-load "woman" 'tinypath-insinuate-woman)
	(when tinypath-:extra-manpath
	  (message "\
TinyPath: ** Hm, manual pages found, but you do not have woman.el
             Visit http://centaur.maths.qmw.ac.uk/Emacs/
             and you will be able to use `M-x man' in Win32 system."))))

    (tinypath-eval-after-load "find-file" 'tinypath-insinuate-find-file)

    (tinypath-insinuate-gnus)

    (setq time-b (current-time))
    (setq diff   (tinypath-ti::date-time-difference time-b time-a))


    (put 'tinypath-:load-path-root
	 'tinypath-last-value
	 tinypath-:load-path-root)


    (message "TinyPath: %s END %s" fid (tinypath-time-string))

    (message (concat (tinypath-cache-status-string)
		     (format " time %d sec" diff)))))

;;}}}
;;{{{ Install load time

;;; ----------------------------------------------------------------------
;;;
;;;  This double-definition is here only to hide the function
;;;  `ti::macrof-version-bug-report' from the byte compiler
;;;  in the first pass and let the tinylib.el be in compressed
;;;  format.
;;;
;;;  The real library function `ti::macrof-version-bug-report'
;;;  will take care of the details how to contruct a bug report
;;;  message, it's waste of code to duplicate the code here, because
;;;  user may not even call it.

;;;#### (autoload 'tinypath-version "tinypath" "" t)
(defun tinypath-version ()
  "STUB: This function will define a new function and call it."
  (interactive)
  (tinypath-submit-bug-report 'tinypath-version))

;;;#### (autoload 'tinypath-submit-bug-report "tinypath" "" t)
(defun tinypath-submit-bug-report (&optional function)
  "STUB: This function will define a new function and call it."
  (interactive)
  (defvar tinypath-:version-id)
  ;; We must put this definition to the end of file, because at this
  ;; point the load-path has been defined and
  ;; `ti::macrof-version-bug-report' can be found from the libraries
  (ti::macrof-version-bug-report
   "tinypath.el"
   "tinypath"
   tinypath-:version-id
   "$Id: tinypath.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
   '(tinypath-:version-id
     tinypath-:load-path-root
     tinypath-:load-hook)
   '(ti::buffer-pointer-of-messages))
  ;; Now call the real function
  (or (and function
	   (funcall function))
      (tinypath-submit-bug-report)))

;;; ----------------------------------------------------------------------
;;;
;;; INSTALL:

(defun tinypath-main ()
  "Main loader. This function is called when package is loaded."

  (eval-and-compile
    (unless (tinypath-byte-compile-running-p)
      (tinypath-install-environment)
      (run-hooks 'tinypath-:load-hook)))

  ;; RESTORE DEFAULTS:
  ;; Restore value that was saved at the beginning of file

  (setq gc-cons-threshold
	(get 'gc-cons-threshold 'tinypath))

  ;;  Restore original value for rest of the Emacs session

  (let ((val (get 'tinypath-:verbose 'debug-init)))
    (when (integerp val)
      (setq tinypath-:verbose val)))

  ;;  This last message is here solely so that with log level 20
  ;;  the message is also saved the log file

  (tinypath-verbose-macro 3
    (tinypath-cache-status-message)))

(tinypath-main)


;;}}}

;;; tinypath.el ends here
