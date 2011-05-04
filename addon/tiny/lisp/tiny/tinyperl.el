;;; @(#) tinyperl.el --- Grab-bag of Perl related utilities. Pod documentation
;;; @(#) $Id: tinyperl.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1998-2002 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;; Created:      1998-05
;; Keywords:     extensions
;;
;; To get information on this program use ident(1) or do M-x tinyperl-version
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

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file. Rip code with with tinylib.el/ti::package-rip-magic
;;
;;      (require 'tinyperl)
;;
;;  Autoload, prefer this one, your emacs starts quicker. The additional
;;  features are turned on only when `perl-mode' runs.
;;
;;   	(autoload 'turn-on-tinyperl-mode  "tinyperl" "" t)
;;   	(add-hook 'perl-mode-hook  'turn-on-tinyperl-mode)
;;   	(add-hook 'cperl-mode-hook 'turn-on-tinyperl-mode)
;;
;;  This package will keep the configuration information in a cache and
;;  if for some reason the cache becomes invalid, force rebuilding everything
;;  with command:
;;
;;      C-u M-x tinyperl-install
;;
;;  To completely uninstall package, call:
;;
;;      C-u M-x tinyperl-install-main
;;
;;  If you have any questions, suggestions, use this function
;;
;;      M-x tinyperl-submit-bug-report


;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, march 1998
;;
;;      Perl was quite new in 1994 and perl programs imported
;;      libraries using `require' command. Some time passed and the
;;      new Perl 5 was a complete rewrite. It introduced new Object
;;      and reference technologies to language but lot of perl coders
;;      couldn't grasp the new ideas immediately. Many made the
;;      decision to move to perl 5 only after it was mature
;;      enough. The perl 5 coding was so much cleaner and simpler
;;      compared to perl 4.
;;
;;      As a result some additional Emacs functions were needed the Perl
;;      work going and this module more or less concentrates on helping to
;;      document perl programs with POD or getting perl man pages via
;;      `perldoc' interface. The other companion that you would already
;;      know is the `cperl-mode' which is the best mode for coding the
;;      perl language.
;;
;;  Overview of features
;;
;;      In Windows, both Activestate Perl and native Cygwin perl are
;;      supported. But you cannot use both. If you have accustomed to
;;      Activestate Perl, you may consider moving to Cygwin Perl, because
;;      it is more close to the Unix Perl where you can even use
;;      "perl -eCPAN -e shell" to install modules directly from CPAN.
;;
;;      Sorry to tell you, but multiple perl installations are not
;;      supported. The one that is in your path first is used. Perl
;;      advances each time so much that you're much safer if you always
;;      have the latest version. This is true with Java and Python
;;      as well.
;;
;;      `tinyperl-mode' minor mode:
;;
;;      o   Instant function help: See documentation of `shift', `pop'...
;;      o   Show Perl manual pages in *pod* buffer
;;      o   Load library source code into Emacs, like Devel::DProf.pm
;;      o   Grep through all Perl manual pages (.pod)
;;      o   Follow POD manpage references to next pod page with TinyUrl
;;      o   Colored pod pages with `font-lock'
;;      o   Update perl `$VERSION' variable automatically on save.
;;
;;      Other minor modes:
;;
;;      o   Separate `tinyperl-pod-view-mode' for reading pod2text pages
;;      o   Separate `tinyperl-pod-write-mode' for writing POD documentation
;;
;;  Package startup
;;
;;      At package startup the perl binary's `tinyperl-:perl-bin'
;;      `@INC' content is cached. If you have modules somewhere else than
;;      the standard `@INC', then add additional `-I' switches to the
;;      `tinyperl-:inc-path-switches' so that these additional paths are
;;      cached too.
;;
;;      In addition the Perl POD manual pages and paths are cached at startup.
;;      This is derived from *Config.pm* module $Config{privlib}.
;;
;;      If you need to change any of the above settings in environment
;;      during the session, reload package or call `tinyperl-install' to
;;      update the changed values.
;;
;;  Saving TinyPerl state (cache)
;;
;;      When the package is used for the first time, the Perl `@INC' is read
;;      and all .pl and .pm files along the path are cached and written to
;;      file `tinyperl-:cache-file'. Next time this package is loaded, the
;;      cache file is read. This speeds up package initialization next time.
;;
;;      If you upgrade Perl or add new packages along @INC, you
;;      must rebuild the cached information and have it updated to
;;      `tinyperl-:cache-file'. You do this by calling `tinyperl-install'
;;      with a prefix argument (e.g. `C-u').
;;
;;      The cache information is expired periodically, so it should keep up
;;      with the environment changes quite well. The default cache period
;;      is 7 days, but this can be set via
;;      `tinyperl-:cache-file-days-old-max'.
;;
;;  Perl Minor Mode description
;;
;;      Turning on `tinyperl-mode' in any buffer gives you commands to
;;      retrieve Perl's POD (Plain Old Documentation) pages. This is
;;      most useful with the programming mode `perl-mode'. Function
;;      `turn-on-tinyperl-mode' is also added to hooks
;;      `perl-mode-hook' and `cperl-mode-hook' by default.
;;      The list of key below may be not completely up to date, so
;;      consult `C-h' `f' `tinyperl-mode'.
;;
;;          C-c ' f             tinyperl-pod-find-file
;;          C-c ' F             tinyperl-pod-find-file-this-buffer
;;          C-c ' P             tinyperl-pod-by-module
;;          C-c ' P             tinyperl-pod-by-manpage
;;          C-c ' k             tinyperl-pod-kill-buffers
;;
;;          C-c ' m             tinyperl-module-find-file
;;          C-c ' d             tinyperl-perldoc
;;          C-c ' g             tinyperl-pod-grep
;;
;;      o   `tinyperl-pod-find-file'
;;          run pod2text over file pointed by the function. After running this
;;          The internal POD documentation in the file is presented in man page
;;          format. You can use function `tinyperl-pod-find-file-this-buffer'
;;          to check the layout of the POD that you're writing to the current
;;          perl program.
;;      o   `tinyperl-pod-by-module'
;;          View module pages by completing the installed Perl modules
;;          and running pod2text. Like reading documentation of "Getopt::Long".
;;      o   `tinyperl-pod-by-manpage'
;;          View Perl manual pages, like "perlfunc.pod" and run pod2text
;;      o   `tinyperl-pod-kill-buffers'
;;          Kill all *pod* buffers from Emacs
;;      o   `tinyperl-module-find-file'
;;          Complete installed module in @INC and load source code into Emacs.
;;          Like if you want to see real code of "Getopt::Long"
;;      o   `tinyperl-perldoc' Use perldoc -f to display documentation of
;;          a perl function at point.
;;      o   `tinyperl-pod-grep'
;;          Grep regexp from all Perl POD manual pages. Answers to
;;          question "Is this mentioned in FAQ".
;;
;;  POD view mode description: navigating in pod page and following URLs
;;
;;      When pod is loaded to buffer, another package, *tinyurl.el*, is
;;      turned on. It can track several different kind of URLs, including
;;      perl pod manpages for references like:
;;
;;          See perlfunc manpage
;;              ^^^^^^^^^^^^^^^^
;;
;;          See [perltoc]
;;              ^^^^^^^^^
;;
;;          Devel::Dprof manpage
;;          ^^^^^^^^^^^^^^^^^^^^
;;
;;      You can use mouse-2 at the point to jump to the referenced POD
;;      page. Wait couple of seconds at the current line and any
;;      references or URLs found are marked. If you do not want to use
;;      TinyUrl package, add this setup:
;;
;;          (add-hook tinyperl-:load-hook 'my-tinyperl-:load-hook)
;;
;;          (defun my-tinyperl-:load-hook ()
;;            "My TinyPerl customisations."
;;            (remove-hook 'tinyperl-:pod2text-after-hook
;;                         'turn-on-tinyurl-mode-1))
;;
;;      In *pod* buffer where the pod documentation is displayed, an
;;      additional browsing mode, `tinyperl-pod-view-mode', is turned on to
;;      help moving around topics. If you find the PgUp keys non-customary,
;;      see variable `tinyperl-:key-pageup-control'.
;;
;;          ;;  moving down/up topics
;;
;;          PgDown              tinyperl-pod-view-heading-forward
;;          PgDown              tinyperl-pod-view-heading-backward
;;
;;          S-PgDown    tinyperl-pod-view-heading-forward2
;;          S-PgDown    tinyperl-pod-view-heading-backward2
;;
;;          ;; Moving down/up one pod page at a time
;;          ;; The pod pages are all gathered to single buffer *pod*
;;
;;          Meta-PgDown tinyperl-pod-view-forward
;;          Meta-PgUp   tinyperl-pod-view-backward
;;
;;          ;;  The normal PgUp/Down command is available here
;;
;;          Control-PgDown      scroll-up
;;          Control-PgUp        scroll-down
;;
;;	By default the POD documentation is kept in a single buffer where
;;	you can conveniently use C-s and C-r searches. If you would like to
;;	use separate POD buffers instead, a la M-x man, set variable
;;	`tinyperl-:pod-buffer-control' to 'many. The opposite is 'single.
;;
;;  POD Write mode description
;;
;;      There is additional minor mode to help you write POD in the current
;;      buffer The minor mode is in function `tinyperl-pod-write-mode' and
;;      you can switch to it any time you're adjusting the pod section.
;;      Don't keep on all the time, since it occupies some keys that are
;;      normally needed in programming.
;;
;;          PgDown      tinyperl-pod-write-heading-forward
;;          PgUp        tinyperl-pod-write-heading-backward
;;
;;      With shift
;;
;;          PgDown      tinyperl-pod-write-token-forward
;;          PgUp        tinyperl-pod-write-token-backward
;;
;;      Inserting default POD templates for program
;;
;;          C-c . m     tinyperl-pod-write-skeleton-script-manpage
;;          C-c . f     tinyperl-pod-write-skeleton-script-function
;;          C-c . i     tinyperl-pod-write-skeleton-item
;;
;;      Inserting default POD skeletons for Modules or Classes.
;;
;;          C-c . B     tinyperl-pod-write-skeleton-module-header
;;          C-c . E     tinyperl-pod-write-skeleton-module-footer
;;          C-c . F     tinyperl-pod-write-skeleton-module-function
;;
;;      POD skeleton for functions (C-c . F) is very different from the
;;      Module skeletons. This due to fact, that a Module offers documented
;;      function interface and the user callable functions should be
;;      described separately with POD in order to print the manual of the
;;      module.
;;
;;      The POD skeletons for Modules are based on following Module
;;      layout. This is my only a suggested layout, see
;;      Lingue::EN:Squeeze.pm for complete first hand example. The
;;      places below where you see "P O D" are the places where you
;;      add pod. For each, a different pod skeleton is inserted and
;;      when the whole file is printed, it gives nice and maintainable
;;      interface description.
;;
;;      There is another group of people that prefer writing the whole
;;      documentation after the __END__. It has drawback that then you
;;      separate the descriptions from the actual place where the code
;;      resides. The idea here has been that the documentation (function)
;;      is kept immediately above the code: if you change it (function),
;;      you can update the documentation at the same place.
;;
;;      In the other hand, by putting documentation after __END__, the
;;      load time of module is decreased, because POD text is never
;;      read by perl interpreter. Another point to keep in mind is,
;;      that the computing power and disk speed will increase, so the
;;      __END__ solution's benefit is neglible. The maintenance is
;;      easier when the documentation is not separated from the place
;;      where it would be the most natural (nearest to the code).
;;
;;          F I L E   B A N N E R
;;
;;          P O D  H E A D E R
;;          NAME
;;          REVISION
;;          SYNOPSIS
;;          DESCRIPTION
;;          EXPORTABLE VARIABLES
;;          EXAMPLES
;;
;;          #   module interface is written next
;;
;;          use strict;
;;
;;          BEGIN
;;          {
;;                .. EXPORT          # The export interface
;;                .. EXPORT_OK
;;          }
;;
;;          Define exported globals
;;
;;          Define private variables
;;
;;          P O D   I N T E R F A C E   S T A R T
;;
;;          P O D  P U B L I C for public functions or method
;;          sub ...
;;
;;          NORMAL banner of private function
;;          sub ...
;;
;;          P O D   F O O T E R
;;          KNOWN BUGS
;;          AVAILABILITY
;;          AUTHOR
;;
;;          1;
;;          __END__
;;
;;  Perl SelfStubber
;;
;;      If you're developing Perl modules, you can make it to use autoload
;;      interface. Module compiles much faster and it delays loading of
;;      functions until they are called. You can read about SelfStubber
;;      from the Module page *Devel::SelfStubber.pm* which links to
;;      *SelfLoader.pm*, which is (one file) to my opinion better
;;      autoload choice than *Autoloader.pm* (splits file to many files by
;;      function)
;;
;;      To use SelfStubber with this package, you need to arrange your
;;      module to read like below. Notice the "BEGIN:" and "END:"
;;      comment-tokens are for function `tinyperl-selfstubber-stubs',
;;      which will fill in the section with the right stubs.
;;
;;      If you don't have "BEGIN: Devel::SelfStubber" and "END:
;;      Devel::SelfStubber" sections in your file, calling
;;      `tinyperl-selfstubber-stubs' prints the found stubs in separate
;;      shell buffer.
;;
;;          package MyClass;
;;
;;          use Exporter;
;;          use SelfLoader;
;;          use vars qw( $VERSION @ISA @EXPORT @EXPORT_OK );
;;
;;          @ISA    = qw(Exporter);
;;
;;          @EXPORT = qw( .. );
;;
;;          $VERSION = ..
;;
;;          # BEGIN: Devel::SelfStubber
;;
;;          # END: Devel::SelfStubber
;;
;;          1;
;;          __DATA__
;;
;;          <implementation: functions and variables>
;;
;;          __END__
;;
;;  Updating the VERSION variable
;;
;;      If you plan to submit your perl module or program to the CPAN
;;      at http://cpan.perl.org/ the upload criteria is that your file
;;      must have a version number. The traditional method has long
;;      used some version control software's number (those of CVS or
;;      RCS etc.), but it really doesn't tell much to the *user*. It might
;;      tell something to the developer, but from user's point of view,
;;      he is much more interested in knowing when the file was
;;      last updated. The version number 2.77 may be two years old.
;;
;;      Where is that variable used? The *MakeMaker* perl module (that
;;      you use when making packages ready to CPAN upload) reads the
;;      first variable named VERSION and names your release according
;;      to it.
;;
;;      Consider to use two version numbers: one for the release and
;;      one for the kit name. In order to *MakeMaker* to pick up the
;;      version number for a kit (tar.gz release, that is, for the
;;      user), it must see a VERSION variable. You can store the
;;      (a) version control software's number at the beginning of file
;;      inside comments and the (b) release number to a perl variable.
;;
;;	    # @(#) $Id: tinyperl.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $
;;
;;          use vars qw ( $VERSION );
;;
;;    	    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
;;    	    #   So that it puts the tardist number in format YYYY.MMDD
;;    	    #   The REAL version number is defined later
;;          #
;;    	    #   The following variable is updated by Emacs setup whenever
;;    	    #   this file is saved
;;
;;	    $VERSION = '1234.1234';
;;
;;	If the VERSION variable uses number format NNNN.NNNN, then it
;;	is assumed to contain ISO 8601 date YYYY.MMDD and this package
;;	will update the `$VERSION' variable's date every time file is
;;	saved (see `write-file-hooks' and `tinyperl-version-stamp').
;;
;;  Submitting your perl script to CPAN
;;
;;      In addition to archiving your Perl *libraries* to CPAN, you can also
;;      submit perl *scripts* there. In order to get your submission right
;;      refer to page:
;;
;;          http://www.perl.com/CPAN-local//scripts/submitting.html
;;
;;      The most important point is that your script includes pod that
;;      describes your script. It must contain at minimum the headings
;;      README, SCRIPT CATEGORIES, COREQUISITES, OSNAMES which are already
;;      included in the default pod skeleton via command
;;
;;          `tinyperl-pod-write-skeleton-script-manpage'
;;
;;      Here is code that that can be used in Perl programs to print out
;;      the pod documentation when --help option is requested (Use
;;      Getop::Long.pm). The code works for both Win32 and Unix Perl
;;      implementations. The variable $LIB identifies the "group" where the
;;      function belongs, in this case it is program, while it could have
;;      been a Perl library module too. You set global $LIB variable at the
;;      beginning of file with:
;;
;;          use English
;;          use File::Basename;
;;
;;          use vars qw( $LIB );
;;          $LIB = basename $PROGRAM_NAME;
;;
;;      Here is the help function written with POD (perl 5.004 or higher)
;;
;;          <  Create this Help() function banner with mode key           >
;;          <  C-c . f   or `tinyperl-pod-write-skeleton-script-function' >
;;
;;          # ***************************************************************
;;          #
;;          #   DESCRIPTION
;;          #
;;          #       Print help and exit.
;;          #
;;          #   INPUT PARAMETERS
;;          #
;;          #       $msg        [optional] Reason why function was called.-
;;          #
;;          #   RETURN VALUES
;;          #
;;          #       none
;;          #
;;          # ***************************************************************
;;
;;          =pod
;;
;;	    < This part: appears after you have called		        >
;;          < C-c . m  or  `tinyperl-pod-write-skeleton-script-manpage' >
;;
;;          =cut
;;
;;          sub Help (;$)
;;          {
;;              my $id  = "$LIB.Help";
;;              my $msg = shift;  # optional arg, why are we here...
;;
;;              pod2text $PROGRAM_NAME;
;;
;;              print $msg if $msg;
;;
;;              exit 1;
;;          }
;;



;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(eval-and-compile

  (defvar compilation-error-regexp-alist)

  ;;  Follow pod URLs and other url links like cut(1)
  ;; Go to grep result.

  (autoload 'turn-on-tinyurl-mode-1      "tinyurl"  "" t)
  (autoload 'tinyurl-find-url-file       "tinyurl" "" t)

  ;;  Why do we autoload this? Because function turn-on-tinyperl-mode
  ;;  is not physically here, but automagically created by a macro call,
  ;;  --> byte compiler needs this hint so that it doesn't flag
  ;;      ** The following functions are not known to be defined:
  ;;      turn-off-tinyperl-mode, turn-on-tinyperl-mode

  ;; (autoload 'turn-on-tinyperl-mode     "tinyperl" "" t)
  ;; (autoload 'turn-off-tinyperl-mode    "tinyperl" "" t)

  (defvar font-lock-keywords))


(ti::package-defgroup-tiny TinyPerl tinyperl-: extensions
    "Additional function to perl programming.
  Overview of features

        o   Instant function help: See documentation of `shift', `pop'...
        o   Show Perl manual pages in *pod* buffer
        o   Load source code into Emacs, like Devel::DProf.pm
        o   Grep through all Perl manpages (.pod)
        o   Follow POD manpage references to next pod page with TinyUrl
        o   Coloured pod pages with `font-lock'
        o   Separate `tinyperl-pod-view-mode' for jumping topics and pages
            forward and backward in *pod* buffer.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinyperl-:load-hook '(tinyperl-install-main)
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyPerl)

(defcustom tinyperl-:pod2text-before-hook  nil
  "Hook run before calling pod2text pod buffer See `tinyperl-pod2text'."
  :type  'hook
  :group 'TinyPerl)

(defcustom tinyperl-:pod2text-after-hook  nil
  "Hook run after calling podchecker in that buffer.
See `tinyperl-podchecker'."
  :type  'hook
  :group 'TinyPerl)

(defcustom tinyperl-:podchecker-before-hook  nil
  "Hook run before calling pod2text pod buffer See `tinyperl-podchecker'."
  :type  'hook
  :group 'TinyPerl)

(defcustom tinyperl-:podchecker-after-hook  nil
  "Hook run after calling pod2text in that buffer. See `tinyperl-pod2text'."
  :type  'hook
  :group 'TinyPerl)

(defcustom tinyperl-:perldoc-hook nil
  "Hook run after calling `tinyperl-perldoc'."
  :type  'hook
  :group 'TinyPerl)

;;; ........................................................ &v-public ...

(defcustom tinyperl-:key-pageup-control 'heading
  "*How to use PgUp and PgDown keys. 'heading or 'normal."
  :type '(choice (const heading)
		 (const normal))
  :group  'TinyPerl)

(defcustom tinyperl-:pod-buffer-control 'one
  "*How to display POD documentation. 'single or 'many windows."
  :type '(choice (const one)
		 (const many))
  :group  'TinyPerl)

(defcustom tinyperl-:skeleton-script-ftp-url nil
  "*URL where your Perl code is available. Used by skeleton."
  :type 'string
  :group  'TinyPerl)

(defcustom tinyperl-:pause-directory nil
  "*Directory where to copy your PAUSE uploads.
A Perl script must have unique naming before it can be accepted
for PAUSE upload. If you do not know what PAUSE (The Perl Authors Upload
Server) is, learn more about becoming a Perl developer at
http://pause.kbx.de/pause/ => about pause.

This variable is used by `tinyperl-pause-copy-file' for default
location where the pause upload candidates are copied.

See also http://cpan.perl.org/authors/id/NEILB/ cpan-upload-1.9.tar.gz."
  :type 'directory
  :group 'TinyPerl)


(defcustom tinyperl-:copyright-function 'tinyperl-copyright
  "*Copyright notice for your Perl programs."
  :type  'function
  :group 'TinyPerl)


;;  This configuration file MUST BE in OS specific name. It is very
;;  common that Networked NT workstations access a SAMBA mounted
;;  Unix disk and then the HOME directory refer to
;;  the Unix disk.
;;
;;  However if you log into that Unix, You will run Unix Perl
;;  If you log into win32 workstation with SAMBA mount, you run win32 Perl.
;;  See that problem now and the need for OS specific filename?
;;
;;  The cache information for Win32 and Unix must be in different files,
;;  if your HOME points to the same location. E.g. the stored perl
;;  interpreter name and location is completely different in the cache.

(defcustom tinyperl-:cache-file-prefix
  (ti::package-config-file-prefix "tinyperl")
  "*File where to save cached perlmodule path and file info.
See function `tinyperl-save-state'."
  :type   'string
  :group  'TinyPerl)


(defcustom tinyperl-:cache-file-postfix ".el"
  "*Extension for `tinypath-:cache'. See also `tinypath-:cache-file-prefix'.
Normally \".el\"  but to save space you could set this to \".el.gz\"."
  :type  'string
  :group 'TinyPath)


(defcustom tinyperl-:cache-file-days-old-max 7
  "Maximum days before expiring `tinyperl-:cache-file'.
If your Perl environmnt lives a lot, new packages are installed in periodic
intervals, then keep this value withing 7 days. If your environment is on the
other hand very stable and packages don't change often, then you can set
this to very large value, say, 30 days.

You can always rebuild the cached Perl information with
\\[universal-argument] \\[tinyperl-install]"
  :type 'integer
  :group  'TinyPerl)


(defcustom tinyperl-:perl-bin
  (or (executable-find  "perl")
      (error "TinyPerl: Can't find binary: perl"))
  "*Perl interpreter used. Must be Perl 5.x."
  :type 'string
  :group  'TinyPerl)

(defcustom tinyperl-:perldoc-bin
  ;;  In Win32, this is perldoc.bat and old `executable-find' command
  ;;  does not search .bat files.
  (or (or (executable-find  "perldoc")
	  (and (win32-p)
	       (ti::file-get-load-path "perldoc.bat" exec-path))
	  ;;  Desperate search: this shuld be equal to exec-path but
	  ;;  the environment mey be messed up.
	  (and (win32-p)
	       (ti::file-get-load-path "perldoc.bat"
				    (split-string (getenv "PATH") ";" )))
	  ;;  Emacs executable-find cannot find pure Cygwin "perldoc".
	  ;;
	  (ti::file-get-load-path "perldoc" exec-path))
      (error "TinyPerl: Can't find binary perldoc or perldoc.bat"))
  "*Perldoc binary. Absolute path runs faster."
  :type 'string
  :group  'TinyPerl)


(defcustom tinyperl-:pod2text-bin
  (or (executable-find  "pod2text")
      (ti::file-get-load-path "pod2text" exec-path)   ;; Cygwin perl file
      (error "TinyPerl: Can't find binary: pod2text"))
  "*Perldoc binary. Absolute path runs faster."
  :type 'string
  :group  'TinyPerl)

(defcustom tinyperl-:inc-path-switches nil
  "*List of swithes you want to pass to perl to add mode @INC paths.
Example : '(\"-I\" \"/path/path\"."
  :type 'string
  :group 'TinyPerl)

(defcustom tinyperl-:pod-font-lock-keywords ;; &fonts
  (list

   ;; ....................................................... pod2text ...
   ;; Remeber that the order of the regular expressions is significant.
   ;; First come, first served

   ;; Like in File::Basename
   ;; NAME
   ;;	  fileparse - split a pathname into pieces
   ;;
   ;;	  basename - extract just the filename from a path
   ;;
   ;;	  dirname - extract just the directory from a path

    '("^    \\([^ \t\r\n]+\\)[ \t]+-[ \t]+"
 	 1 font-lock-reference-face)

    '("^    [^ \t\r\n]+[ \t]+-[ \t]+\\(.*\\)"
 	 1 font-lock-constant-face)   ;; font-lock-string-face

    ;; Headings and Sub headings
    ;; Method deescription in Class
    ;;
    ;;   $ua->from([$email_address])
    ;;   new()

    '("^ ? ? ? ?\\([\"$%@A-Za-z_]+\\)[ \t]*$"
 	 1 font-lock-type-face)

    ;;  =head2 Topic Name Here
    ;;  multipe words

    '("^  \\([A-Za-z]+[A-Za-z][ \t]+[A-Za-z].+[A-Za-z]\\)[ \t]*$"
      1 font-lock-type-face t)

    '("\\(perl[^ ]+\\)[ \t\n\r]+man\\(ual \\)?page"
 	 1 font-lock-type-face)


    ;; --this-option

    (list
     (concat
      "--[-a-zA-Z0-9]+\\>\\| -[-a-zA-Z0-9]\\>"  ;; option names
      "\\|\\(http\\|ftp\\|news\\|wais\\)://[^ \t\r\n]+"
      "\\|<?[^ \t\n\r]+@[^ \t\r\n]+>?"          ;; <foo@bar.com>
      "\\|`[^\"'`\n\r]+'"                       ;; `this'
      "\\|\\<[^( \t\n\r]+([$@%;*]*)"            ;; function($)
      "\\|[^ \t\r\n]+[\\/][^ \t\r\n]+")         ;; CPAN/modules/by-module
     0 'font-lock-reference-face)


   ;;  [Wall]

   (list
    (concat
     "\\[[a-zA-Z]+\\]+"
     "\\|\\<[-a-zA-Z0-9]+([0-9]+[A-Z]?)")  ;; chmod(1)
     0 'font-lock-constant-face)

    (list
     (concat

      ;;  "abc"
      ;;  `this'   US style
      ;;  'this'   European style

      "[\"][^\"\r\n]+[\"]"
      "\\|`[^'`\r\n]+'"
      "\\|'[^'\r\n]+'"

      ;;  Notice that BLOCK ... LOGIN-NAME

      "\\|\\<[%$@]*[A-Z_][-A-Z_]+\\>"	       ;; @VAR_HERE, BIG_LETTERS

      ;; it's *funny* that ...

      "\\|\\*[^ \r\n*]+\\*")
     0  'font-lock-keyword-face)

;;;    ;; like chdir() function ...

;;;    '("[a-z][^ \t\n\r(]+()" 0 font-lock-reference-face)



   ;; ...................................................... pod-write ...

   '("^=\\(head[0-9]\\|pod\\|begin\\|end\\|cut\\|item\\)"
     0 font-lock-function-name-face t)

   '("^=\\(head[0-9]\\|pod\\|begin\\|end\\|cut\\|item\\)[ \t]+\\(.*\\)"
     2 font-lock-reference-face t)

   '("^=item[ \t]+\\(.*\\)"
     1 font-lock-keyword-face t)

   '("^=.*"
     0 font-lock-type-face))
  "*Font lock keywords."
  :type   'sexp
  :group  'TinyPerl)



;;; ....................................................... &v-private ...

(defvar tinyperl-:inc-path nil
  "The content of @INC.
The path names are not in pure rwa @INC format, but they
have been processed to meet host Emacs's understanding of underlying
operating systems paths.

E.g Win32/Cygwin/perl returns paths in native Unix format which must
be translated to Emacs that is running. For GNU Emacs, this means
paths in DOS style.")

(defvar tinyperl-:inc-module-list nil
  "The content .pm files under @INC.")

(defvar tinyperl-:pod-path  nil
  "Path to perl distribution POD files.")

(defvar tinyperl-:pod-list  nil
  "List of pod files. '((file.pod . path) (file.pod . path) ..).")

(defvar tinyperl-:pod-buffer-name "*pod*"
  "Buffer where to print POD.")

(defvar tinyperl-:faq-buffer-name "*pod FAQ-grep*"
  "Buffer where to put context exerpts after grep search.
See `tinyperl-pod-grep-faq-answer'")


(defvar tinyperl-:perldoc-buffer "*perldoc*"
  "Buffer where to output perldoc.")

(defvar tinyperl-:podchecker-buffer "*podchecker*"
  "Buffer where to output Pod::Checker::podchecker().")

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinyperl-version "tinyperl" "Display commentary." t)

(eval-and-compile
(ti::macrof-version-bug-report
 "tinyperl.el"
 "tinyperl"
 tinyperl-:version-id
 "$Id: tinyperl.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tili-:version-id
   tinyperl-:load-hook
   tinyperl-:pod2text-before-hook
   tinyperl-:pod2text-after-hook
   tinyperl-:podchecker-before-hook
   tinyperl-:podchecker-after-hook
   tinyperl-:perldoc-hook
   tinyperl-:key-pageup-control
   tinyperl-:pod-buffer-control
   tinyperl-:skeleton-script-ftp-url
   tinyperl-:pause-directory
   tinyperl-:copyright-function
   tinyperl-:cache-file-prefix
   tinyperl-:cache-file-postfix
   tinyperl-:perl-bin
   tinyperl-:perldoc-bin
   tinyperl-:pod2text-bin
   tinyperl-:inc-path-switches
   tinyperl-:pod-font-lock-keywords
   tinyperl-:inc-path
   tinyperl-:inc-module-list
   tinyperl-:pod-path
   tinyperl-:pod-list
   tinyperl-:pod-buffer-name
   tinyperl-:perldoc-buffer
   tinyperl-:podchecker-buffer)))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ Macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinyperl-directory-files 'lisp-indent-function 3)
(defmacro tinyperl-directory-files (variable path &optional regexp)
  "Store to VARIABLE .pl and .pm files in PATH. Optionally match REGEXP."
  (` (setq (, variable)
           (directory-files
            (, path)
            nil
            (or (, regexp) "\\.pl\\|\\.pm")))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinyperl-executable-set 'lisp-indent-function 3)
(defmacro tinyperl-executable-set (sym bin &optional regexp)
  "Set variable SYM to executable BIN name searching REGEXP.
This is shorthand of saying, that locate the BIN in the `exec-path'
when it matches REGEXP and set variable SYM to that value, effectively:

    (setq tinyperl-:perldoc-bin
	  (tinyperl-executable-find-path
	   \"perldoc\" tinyperl-:perldoc-bin \"perldoc\"))

--> (tinyperl-executable-set 'tinyperl-:perldoc-bin \"perldoc\")"
  (`
   (set (, sym)
	(tinyperl-executable-find-path
	 (, bin)
	 (symbol-value (, sym))
	 (or (, regexp)
	     (, bin))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-perl-module-exists-p (module)
  "Return path if MODULE(.pm) is known to ´tinyperl-:inc-module-list'."
  (unless (string-match "\\.pm$" module)
    (setq module (concat module ".pm")))
  (let* ((elt (assoc module tinyperl-:inc-module-list))
	 (file (if (string-match ".*::\\(.*\\)" module)
		   (match-string 1 module)
		 module)))
    (when elt
      (concat (file-name-as-directory (cdr elt))
	      file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-grep-program ()
  "Return value of `grep-program' if available."
  ;;  Hide variable `grep-program' from byte compiler
  ;;  We do not need (require 'grep) only to get this variable
  ;;  defined.
  (let ((sym   'grep-program))
    (if (boundp sym)
	(symbol-value sym)
      "grep")))

;;}}}

;;{{{ code: install, mode


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-variable-convert (&optional dos-format)
  "Convert all path variables to Unix or DOS-FORMAT."
  (flet ((convert (var)
		  (if dos-format
		      (ti::file-name-backward-slashes var)
		    (ti::file-name-forward-slashes var))))
    (setq tinyperl-:pod2text-bin (convert tinyperl-:pod2text-bin))
    (setq tinyperl-:perldoc-bin  (convert tinyperl-:perldoc-bin))
    (setq tinyperl-:perl-bin     (convert tinyperl-:perl-bin))
    (setq tinyperl-:pod-path     (convert tinyperl-:pod-path))))


;;; --------------------------------------------------------------------
;;;
(defun tinyperl-executable-find-path (program old-value regexp)
  "Find path for PROGRAM with OLD-VALUE matching REGEXP."
  (if (and (ti::file-name-path-p (or old-value ""))
	   (file-exists-p old-value)
	   (not (file-directory-p old-value)))
      old-value
    (setq program
	  (if (and tinyperl-:perl-bin
		   ;;  This could return "perl5.005"
		   (string-match regexp old-value))
	      (match-string 0 old-value)
	    ;;  use default then
	    program))
    (or (executable-find program)
	;;  Only way to find Cygwin "perldoc".
	(ti::file-get-load-path program exec-path))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-perl-examine (perl)
  "Check type of PERL. Return 'win32-activestate 'win32-cygwin 'perl.
Perl is called with -v. Following properties are stored in

variable `tinyperl-:perl-bin' are set to properties:

  'version-answer   =>  The -v result string
  'type		    =>  'win32-activestate 'win32-cygwin 'perl."
  (let* ((info (ti::process-perl-version perl)))
    (put 'tinyperl-:perl-bin 'version-answer (nth 3 info))
    (put 'tinyperl-:perl-bin 'type (nth 1 info))))


;;; ----------------------------------------------------------------------
;;;
(defsubst tinyperl-perl-type ()
  "Return Perl type. Provided `tinyperl-perl-examine' has been called."
  (or (get 'tinyperl-:perl-bin 'type)
      (progn (tinyperl-perl-examine tinyperl-:perl-bin)
	     (get 'tinyperl-:perl-bin 'type))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyperl-perl-type-version-info ()
  "Return Perl -v info. Provided `tinyperl-perl-examine' has been called."
  (or (get 'tinyperl-:perl-bin 'version-answer)
      (progn (tinyperl-perl-examine tinyperl-:perl-bin)
	     (get 'tinyperl-:perl-bin 'version-answer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-install-variables-binaries (&optional force)
  "Install or FORCE setting binary variables like `tinyperl-:perl-bin'
Return:
  t      If some path needed fixing. This means that cache must be resaved."
  (interactive "P")
  (let* (ok)
    (flet ((exec-set
	    (sym bin &optional regexp)   ;; Parameters
	    (let* ((value (symbol-value sym)))
	      (when (or force
			;;  Value is set, possibly read from the cache,
			;;  but that binary does not exist any more.
			;;  Perhaps user has relocated Rerl. Deternine
			;;  new changed location.
			(and (stringp value)
			   (not (file-exists-p value)))
			;;  Value has not been set yet
			(not (stringp value)))
		(setq ok t)
		(or (tinyperl-executable-set sym bin regexp)
		    (error "TinyPerl: No binary `%s` for variable `%s' \
Check variable `exec-path'"
			   bin
			   (symbol-name sym)))))))

      ;;  `perl5' `perl5.004' ...
      ;;  If the name does not contain word perl, use "perl".

      (exec-set 'tinyperl-:perl-bin
		"perl" "perl[-.0-9]*\\.exe\\|perl[^\\/]*")

      (exec-set 'tinyperl-:perldoc-bin  "perldoc")
      (exec-set 'tinyperl-:pod2text-bin "pod2text")

      (tinyperl-perl-examine tinyperl-:perl-bin)

      ;;  Leave trace to Message buffer.

      (message "TinyPerl: [Perl version] => %s"
	       (or (tinyperl-perl-type-version-info) ""))

      ok)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-install-variables-lookup (&optional check verb)
  "Set all global lookup variables.

Input:

  CHECK     Check variable: Preserve previous content and set only
            those that do not have value.
  VERB	    Allow verbose messages

References:

  `tinyperl-:inc-path'
  `tinyperl-:inc-module-list'
  `tinyperl-:pod-path'
  `tinyperl-:pod-list'"
  (interactive)
  (flet ((set-maybe
	  (symbol eval-form)
	  (or (and check
		   (symbol-value symbol))
	      (set symbol
		   (eval eval-form)))))

    (if verb
	(message "TinyPerl: Setting up variables..."))


    (unless (set-maybe
	     'tinyperl-:inc-path
	     '(tinyperl-inc-path tinyperl-:perl-bin))
      (error "TinyPerl: Setup failure tinyperl-:inc-path,\
tinyperl-:perl-bin Unrecognized. Need Perl 5. [%s]"
	     tinyperl-:perl-bin))


    (unless (set-maybe
	     'tinyperl-:inc-module-list
	     '(tinyperl-build-list-of-inc-files
	       tinyperl-:inc-path
	       verb))
      (error "TinyPerl: Setup failure tinyperl-:inc-module-list"))

    (unless (set-maybe
	     'tinyperl-:pod-path
	     '(tinyperl-pod-path tinyperl-:perl-bin))
      (error "TinyPerl: Setup failure tinyperl-:pod-path"))

    (unless (set-maybe
	     'tinyperl-:pod-list
	     '(tinyperl-build-pod-files))
      (error "TinyPerl: Setup failure tinyperl-:pod-list"))

    (if verb
	(message "TinyPerl: Setting up variables...Done."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-install-variables-lookup-maybe (&optional verb)
  "Set all global variables only if they don't have values."
  (tinyperl-install-variables-lookup (not 'check) verb))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-install (&optional force verb)
  "Install tinyperl.

Input:

  FORCE     If non-nil, rebuild all variables and
            save new `(tinyperl-cache-file-name)'.
            If nil, read saved variables from `(tinyperl-cache-file-name)'.

  VERB      Allow verbose messaegs."
  (interactive "P")
  (ti::verb)
  (let* (stat
	 ok)

    ;;  The FORCE Flag says that we should start all over, no
    ;;  matter how broken our setup is. In case the unforunate
    ;;  accident of tinyperl-:perl-bin being in format
    ;;  e:USRLOCALBINPERLBINperl.exe  we recover the state here
    ;;  and start over.
    ;;
    ;;  If the perl exectable is not correct in the first place
    ;;  we can't continue.

    (setq stat (tinyperl-load-state-if-recent-enough))

    (setq ok (tinyperl-install-variables-binaries force))

    (if (or force
	    (null stat))
	(tinyperl-install-variables-lookup)
      (tinyperl-install-variables-lookup-maybe))

    ;;  We must use forward slashes, because When we save the cache file,
    ;;  It would look like:
    ;;  (defconst tinyperl-:perl-bin
    ;;     "e:\USR\LOCAL\BIN\PERL\BIN\perl.exe")
    ;;
    ;;   --> e:USRLOCALBINPERLBINperl.exe  when read from
    ;;

    (tinyperl-variable-convert)

    (when (or force
	      (null stat))
      (tinyperl-save-state nil verb)
      (if verb
	  (message "TinyPerl: Setting up variables...done.")))


    (put 'tinyperl-mode
	 'podchecker
	 (tinyperl-perl-module-exists-p "Pod::Checker.pm"))

    ok)) ;; install end


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-cache-file-name ()
  "Return Perl version specific cache file.

Don't touch this code unless you know what you're doing.

  We need Emacs specific cache files, because the @INC path
  names returned by Activestate Perl and Cygwin Perl are different
  under different Emacs flavors: XEmacs can be built under Cygwin and win32
  but Emacs understands only DOS paths. .. the matrix is:

  Win32 Cygwin Perl		@INC is unix style => convert to dos for Emacs
  Win32 Activestate Perl	@INC is DOS style => use as is in Emacs

  XEmacs .. eh, well, that hasn't been tackled yet. The @INC matrix
  would be:

  ygwin perl + Cygwin XEmacs plays well together
  ygwin perl + Win32 XEmacs doesn't
  ctivestate + Cygwin XEmacs doesn't
  ctivestate + Win32 XEmacs does.

References:

  `tinyperl-:cache-file-prefix'.
  `tinyperl-:cache-file-postfix'"
  (concat (if (stringp tinyperl-:cache-file-prefix)
	      (concat tinyperl-:cache-file-prefix "-")
	    "emacs-config")
	  ;; (if (win32-p) "win32-" "unix-")
	  (if (emacs-p)
	      "emacs"
	    "xemacs")
	  "-"
	  (let ((sym (tinyperl-perl-type)))
	    (if sym
		(symbol-name sym)
	      (error "TinyPerl: Perl type is not known.")))
	  (if (stringp tinyperl-:cache-file-postfix)
	      tinyperl-:cache-file-postfix
	    "")))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-load-state-if-recent-enough ()
  "Load `(tinyperl-cache-file-name)' if less than `tinyperl-:cache-file-days-old-max'"
  (interactive)
  (let ((file (tinyperl-cache-file-name)))
    (if (and (file-exists-p file)
	     (< (ti::file-days-old file)
		tinyperl-:cache-file-days-old-max))
	(tinyperl-save-state 'load 'message))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-save-state (&optional load &optional verb)
  "Save or LOAD variables to `(tinyperl-cache-file-name).'
When LOAD: If `(tinyperl-cache-file-name)' does not exist. return nil."
  (interactive)
  (ti::verb)
  (let ((file (tinyperl-cache-file-name)))

    (cond
     (load
      (when (file-exists-p file)
	(load file)
	(if verb
	    (message "TinyPerl: state restored [%s]" file))
	t))
     (t
      (ti::write-file-variable-state
       file
       "TinyPerl.el saved state"
       '(tinyperl-:inc-path
	 tinyperl-:inc-module-list
	 tinyperl-:pod-path
	 tinyperl-:pod-list
	 tinyperl-:perl-bin
	 tinyperl-:perldoc-bin
	 tinyperl-:pod2text-bin))

      (if verb
	  (message "TinyPerl: state saved [%s]" file))
      t))))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinyperl-mode-all-buffers (&optional off)
  "Turn function `tinyperl-mode' on in every perl buffer. Optionally turn OFF."
  (interactive "P")
  (ti::dolist-buffer-list
   (or
    (string-match "perl" (downcase (symbol-name major-mode)))
    (string-match "\\.pl$" (buffer-name))
    (string-match "code-perl" (or (ti::id-info) "")))
   'tmp-buffers-too
   nil
   (let ((mode (symbol-value 'tinyperl-mode)))
     ;;  We use `symbol-value' because byte compiler does not see the
     ;;  'tinyperl-mode' yet. It's defined by the minor mode wizard macro
     (if off
	 (unless (null mode)
	   (ti::funcall 'turn-off-tinyperl-mode))
       (unless mode
	 (ti::funcall 'turn-on-tinyperl-mode))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-install-hooks (&optional remove verb)
  "Install default hooks or REMOVE. VERB."
  (interactive "P")
  (ti::verb)

  (ti::add-hooks 'tinyperl-:perldoc-hook
		 '(tinyperl-pod-font-lock
		   turn-on-tinyurl-mode-1
		   ti::buffer-strip-control-m)
		 remove)

  (ti::add-hooks '(tinyperl-:pod2text-after-hook
		   tinyperl-:podchecker-after-hook)
		 '(turn-on-tinyurl-mode-1
		   turn-on-tinyperl-pod-view-mode
		   ti::buffer-strip-control-m)
		 remove)

  (ti::add-hooks 'tinyperl-:pod-view-mode-hook
		 'tinyperl-pod-font-lock
		 remove)

  (ti::add-hooks 'tinyperl-:pod-write-mode-hook
		 'tinyperl-pod-font-lock
		 remove)

  (ti::add-hooks '(perl-mode-hook
		   cperl-mode-hook)
		 'turn-on-tinyperl-mode
		 remove)


  (ti::add-hooks 'tinyperl-:mode-define-keys-hook
		 'tinyperl-mode-define-keys remove)

  (ti::add-hooks 'tinyperl-:pod-view-mode-define-keys-hook
		 'tinyperl-pod-view-mode-define-keys
		 remove)

  (ti::add-hooks 'tinyperl-:pod-write-mode-define-keys-hook
		 'tinyperl-pod-write-mode-define-keys
		 remove)

  (ti::add-hooks 'write-file-hooks
		 'tinyperl-version-stamp
		 remove)
  (if verb
      (message "TinyPerl: Hooks installed")))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-copyright ()
  "Insert copyright string fro Perl program."
  (interactive)
  (insert "Copyright (C) " (format-time-string "%Y " (current-time))
          (or (user-full-name)
              (read-string "You name: "))
          ". All rights reserved.
This program is free software; you can redistribute and/or modify program
under the same terms as Perl itself or in terms of Gnu General Public
licence v2 or later."))

;;; ..................................................... &tinyperl-mode ...

;;;###autoload (autoload 'tinyperl-mode           "tinyperl" ""	t)
;;;###autoload (autoload 'turn-on-tinyperl-mode   "tinyperl" ""	t)
;;;###autoload (autoload 'turn-off-tinyperl-mode  "tinyperl" ""	t)
;;;###autoload (autoload 'tinyperl-commentary     "tinyperl" "" t)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinyperl-" " pod" "\C-c'" "Tperl" 'TinyPerl "tinyperl-:"                 ;1-6

 "Additional commands to fetch perl module and perl manpage information

For complete on-line documentation, which is generated from the
source file itself, run command `tinyperl-version`

This minor mode is by default turned on when `[c]perl-mode' is turned on
but, you can access the Perl POD page view commands directly too even if
the minor mode is not active, Here is suggestion for global bindings that
you can put to your $HOME/.emacs startup file:

  ;;  Take global prefix key C-c p  for perl pod view commands

  (global-set-key \"\C-cpp\" 'tinyperl-pod-by-manpage)
  (global-set-key \"\C-cpP\" 'tinyperl-pod-by-module)

You can also run `perl2text' filter on any perl file with command
M-x `tinyperl-find-file' See also `tinyperl-pod-view-mode'

The function `tinyperl-pod-write-mode' will turn on additional minor
mode that might help you to write the POD dicumentation inside you
perl code. this minor mode is intended to to use only at-demand
basis, so that, when you concentrate on writing the POD page, you
turn it on, when you have finished and continue writing perl code,
you should in general turn it off.


Mode description:

\\{tinyperl-:mode-map}"

 "TinyPerl"

 nil

 "Perl buffer extras."

 (list
  tinyperl-:mode-easymenu-name
  ["Pod by module"                      tinyperl-pod-by-module     t]
  ["Pod by manpage"                     tinyperl-pod-by-manpage    t]
  ["Pod grep"                           tinyperl-pod-grep          t]

  ["Pod2text on file"                   tinyperl-pod-find-file     t]
  ["Pod2text on current buffer" tinyperl-pod-find-file-this-buffer t]

  ["Pod syntax check"
   tinyperl-pod-podchecker
   (get 'tinyperl-mode  'podchecker)]

  ["Pod switch to buffer"               tinyperl-pod-jump          t]
  ["Pod kill buffers"                   tinyperl-pod-kill-buffers  t]
  ["Pod write mode"                     tinyperl-pod-write-mode    t]
  "----"
  ["Perldoc - function help"            tinyperl-perldoc           t]
  "----"
  ["Module source find-file"            tinyperl-module-find-file  t]
  ["Module generate stubs"              tinyperl-selfstubber-stubs t]
  "----"
  ["PAUSE copy file"                    tinyperl-pause-copy-file   t]
  ["PAUSE submit page"                  tinyperl-pause-url-submit-www-page  t]
  "----"
  ["Package version"			tinyperl-version           t]
  ["Package commentary"			tinyperl-commentary        t]
  ["Mode help"                          tinyperl-mode-help         t]
  ["Mode off"                           tinyperl-mode              t])

 (progn

   (define-key map  "?"	 'tinyperl-mode-help)
   (define-key map  "Hm" 'tinyperl-mode-help)
   (define-key map  "Hc" 'tinyperl-commentary)
   (define-key map  "Hv" 'tinyperl-version)

   (define-key map "P"   'tinyperl-pod-by-manpage)
   (define-key map "p"   'tinyperl-pod-by-module)
   (define-key map "f"   'tinyperl-pod-find-file)
   (define-key map "F"   'tinyperl-pod-find-file-this-buffer)
   (define-key map "g"   'tinyperl-pod-grep)
   (define-key map "G"   'tinyperl-pod-grep-faq-answer)
   (define-key map "k"   'tinyperl-pod-kill-buffers)
   (define-key map "j"   'tinyperl-pod-jump)
   (define-key map "!"   'tinyperl-pod-podchecker)

   (define-key map "d"   'tinyperl-perldoc)
   (define-key map "m"   'tinyperl-module-find-file)

   (define-key map "?"   'tinyperl-mode-help)
   (define-key map "M"   'tinyperl-mode)

   (define-key map "S"   'tinyperl-selfstubber-stubs)

   ;; C = CPAN interface, other keys like P (PAUSE) are already reserved.

   (define-key map "Cc"   'tinyperl-pause-copy-file)
   (define-key map "Cs"   'tinyperl-pause-url-submit-www-page)
   (define-key map "Cf"   'tinyperl-pause-upload-via-ftp)

   (define-key map "W"   'tinyperl-pod-write-mode)

   ;;  Borrow some commonly used keys from the "pod-write" mode

   (define-key map "'f"   'tinyperl-pod-write-skeleton-script-function)
   (define-key map "'m"   'tinyperl-pod-write-skeleton-script-manpage)

   ;; B = Begin , E = End

   (define-key map "'F"   'tinyperl-pod-write-skeleton-module-function)
   (define-key map "'B"   'tinyperl-pod-write-skeleton-module-header)
   (define-key map "'E"   'tinyperl-pod-write-skeleton-module-footer))))


;;; ................................................... &view-pod2text ...


;;;###autoload (autoload 'tinyperl-pod-view-mode          "tinyperl" "" t)
;;;###autoload (autoload 'turn-on-tinyperl-pod-view-mode  "tinyperl" "" t)
;;;###autoload (autoload 'turn-off-tinyperl-pod-view-mode "tinyperl" "" t)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinyperl-pod-view-" " POD" "\C-c'" "POD" 'TinyPerl "tinyperl-:pod-view-"

 "View `pod2text' formatted output.
If you have manual pages in the current buffer, this mode makes
navigating the headings and topics easier.

This mode redefined the Page Up and Page down key to jump between
headings. Hold also shift or meta or control key down for other
movement controls.

Mode description:

\\{tinyperl-:pod-view-mode-map}"

 "TinyPerl Pod View"

 nil

 "POD view mode."

 (list
  tinyperl-:pod-view-mode-easymenu-name
  ["Heading forward"            tinyperl-pod-view-heading-forward     t]
  ["Heading backward"           tinyperl-pod-view-heading-backward    t]
  ["Sub Heading forward"        tinyperl-pod-view-heading-forward2    t]
  ["Sub Heading backward"       tinyperl-pod-view-heading-backward2   t]
  ["Section forward"            tinyperl-pod-view-backward            t]
  ["Section backward"           tinyperl-pod-view-backward            t]
  ["Scroll up"                  scroll-up                             t]
  ["Scroll down"                scroll-down                           t]
  "----"
  ["Exit and kill"              kill-buffer-and-window                t]
  ["Mode help"                  tinyperl-pod-view-mode-help           t]
  ["Mode off"                   tinyperl-pod-view-mode                t])

 (progn

   ;;   headings

   (define-key map "P"   'tinyperl-pod-by-manpage)
   (define-key map "p"   'tinyperl-pod-by-module)
   (define-key map "f"   'tinyperl-pod-find-file)
   (define-key map "F"   'tinyperl-pod-find-file-this-buffer)
   (define-key map "g"   'tinyperl-pod-grep)
   (define-key map "G"   'tinyperl-pod-grep-faq-answer)
   (define-key map "k"   'tinyperl-pod-kill-buffers)
   (define-key map "j"   'tinyperl-pod-jump)
   (define-key map "q"   'kill-buffer-and-window)

   (define-key root-map [(prior)]         'tinyperl-pod-view-pageup)
   (define-key root-map [(next)]          'tinyperl-pod-view-pagedown)

   ;; Sub-headings

   (define-key root-map [(shift prior)]   'tinyperl-pod-view-heading-backward2)
   (define-key root-map [(shift next)]    'tinyperl-pod-view-heading-forward2)

   ;;   Bigger steps with these

   (define-key root-map [(meta prior)]   'tinyperl-pod-view-backward)
   (define-key root-map [(meta next)]    'tinyperl-pod-view-forward)

   ;;   And original PgUp PgDown is saved under Control key

   (define-key root-map [(control prior)] 'scroll-down)
   (define-key root-map [(control next)]  'scroll-up))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-backward ()
  "Go to one topic backward."
  (interactive)
  ;;    NAME
  ;;        Net::FTP - FTP Client class
  (or (re-search-backward "^NAME[\n\r]" nil t) (ti::pmin)))


;;; ----------------------------------------------------------------------
;;;
(defun  tinyperl-pod-view-pageup ()
  "See `tinyperl-:key-pageup-control'."
  (interactive)
  (if (eq tinyperl-:key-pageup-control 'heading)
      (tinyperl-pod-view-heading-backward)
    (scroll-down)))


;;; ----------------------------------------------------------------------
;;;
(defun  tinyperl-pod-view-pagedown ()
  "See `tinyperl-:key-pageup-control'."
  (interactive)
  (if (eq tinyperl-:key-pageup-control 'heading)
      (tinyperl-pod-view-heading-forward)
    (scroll-up)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-forward ()
  "Go to one topic backward."
  (interactive)
  (end-of-line)
  (or (and (re-search-forward "^NAME[\n\r]" nil t)
           (forward-line -1))
      (ti::pmax)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-heading-backward (&optional regexp)
  "Go to one heading backward. Optionally use REGEXP."
  (interactive)
  (let* (case-fold-search)
    (or (and (re-search-backward (or regexp "^\\(  \\)?[A-Z]") nil t)
             (prog1 1 t
               (beginning-of-line)
               (skip-chars-forward " \t")))
        (ti::pmin))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-heading-forward (&optional regexp)
  "Go to one heading backward. Optionally use REGEXP."
  (interactive)
  (end-of-line)
  (let* (case-fold-search)
    (or (and (re-search-forward (or regexp "^\\(  \\)?[A-Z]") nil t)
             (prog1 t
               (beginning-of-line)
               (skip-chars-forward " \t")))
        (ti::pmax))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-heading-backward2 ()
  "Go to one sub heading backward."
  (interactive)
  (tinyperl-pod-view-heading-backward
   "\\([ \t][\r\n]\\|[\r\n][\r\n]\\)\\(  \\|    \\)?[^ \t\n\r]"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-view-heading-forward2 ()
  "Go to one sub heading backward."
  (interactive)
  (tinyperl-pod-view-heading-forward
   "\\([ \t][\r\n]\\|[\r\n][\r\n]\\)\\(  \\|    \\)?[^ \t\n\r]"))

;;}}}
;;{{{ POD write mode

;;; ....................................................... &write-pod ...

;;;###autoload (autoload 'tinyperl-pod-write-mode          "tinyperl" "" t)
;;;###autoload (autoload 'turn-on-tinyperl-pod-write-mode  "tinyperl" "" t)
;;;###autoload (autoload 'turn-off-tinyperl-pod-write-mode "tinyperl" "" t)

(eval-and-compile

(ti::macrof-minor-mode-wizard
 "tinyperl-pod-write-" " PODw" "\C-c." "PODw" 'TinyPerl "tinyperl-:pod-write-"

 "Minor mode to thelp writing POD in place.

Mode description:

\\{tinyperl-:pod-write-mode-map}"

 "TinyPerl Pod Write"

 nil

 "POD Write mode."

 (list
  tinyperl-:pod-write-mode-easymenu-name
  ["Heading forward"            tinyperl-pod-write-heading-forward    t]
  ["Heading backward"           tinyperl-pod-write-heading-backward   t]
  ["Token forward"              tinyperl-pod-write-token-forward      t]
  ["Token backward"             tinyperl-pod-write-token-backward     t]
  ["Scroll up"                  scroll-up                             t]
  ["Scroll down"                scroll-down                           t]
  "----"
  ["Skeleton script manpage"    tinyperl-pod-write-skeleton-script-manpage  t]
  ["Skeleton script function"   tinyperl-pod-write-skeleton-script-function t]
  ["Skeleton module header"     tinyperl-pod-write-skeleton-module-header   t]
  ["Skeleton module function"   tinyperl-pod-write-skeleton-module-function t]
  ["Skeleton module header"     tinyperl-pod-write-skeleton-module-footer   t]
  ["Skeleton item"              tinyperl-pod-write-skeleton-item            t]
  "----"
  ["Mode help"                  tinyperl-pod-write-mode-help                t]
  ["Mode off"                   tinyperl-pod-write-mode                     t])

 (progn
   ;;   headings

   (define-key map [(prior)]            'tinyperl-pod-write-heading-backward)
   (define-key map [(next)]             'tinyperl-pod-write-heading-forward)

   ;; Sub-headings

   (define-key map [(shift prior)]      'tinyperl-pod-write-token-backward)
   (define-key map [(shift next)]       'tinyperl-pod-write-token-forward)

   ;;   Bigger steps with these

   ;; (define-key map [(meta prior)]    'tinyperl-pod-write-backward)
   ;; (define-key map [(meta next)]     'tinyperl-pod-write-forward)

   ;;   And original PgUp PgDown is saved under Control key

   (define-key root-map [(control prior)]  'scroll-down)
   (define-key root-map [(control next)]   'scroll-up)


   ;;  S K E L E T O N -- p for pod

   (define-key map "m"      'tinyperl-pod-write-skeleton-script-manpage)
   (define-key map "f"      'tinyperl-pod-write-skeleton-script-function)
   (define-key map "i"      'tinyperl-pod-write-skeleton-item)
   (define-key map "B"      'tinyperl-pod-write-skeleton-module-header)
   (define-key map "E"      'tinyperl-pod-write-skeleton-module-footer)
   (define-key map "F"      'tinyperl-pod-write-skeleton-module-function))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-write-heading-backward  ()
  "Go to previous POD heading"
  (interactive)
  (tinyperl-pod-view-heading-backward "^=head"))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-write-heading-forward  ()
  "Go to next POD heading"
  (interactive)
  (tinyperl-pod-view-heading-forward "^=head"))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-write-token-backward  ()
  "Go to previous POD token"
  (interactive)
  (tinyperl-pod-view-heading-backward "^="))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-write-token-forward  ()
  "Go to next POD token "
  (interactive)
  (tinyperl-pod-view-heading-forward "^="))


;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-item
  "Insert =item skeleton"
  (read-string "Item: " "*")
  "
=item " str "

")

;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-script-manpage
  "Script: Insert Perl Script's manpage POD."
  (read-string "Program: " (buffer-name))
  "=pod

=head1 NAME

@(#) " str " - " (read-string "One Line description: ")

"

=head1 README

<short overall description here. This section is ripped by CPAN>

=head1 SYNOPSIS

    <program call conventions>

    program B<-V>...

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<--option-name>

=back

=head2 Miscellaneous options

=over 4

=item B<--debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<--help>

Print help

=item B<--test>

Run in test mode, do not actually do anything.

=item B<--verbose>

Print informational messages.

=item B<--Version>

Print contact and version information

=back

=head1 DESCRIPTION

<program description>

=head1 EXAMPLES

<example calls for the program in different situations>

=head1 TROUBLESHOOTING

<what to check in case of error or weird behavior>

=head1 ENVIRONMENT

<any environment variable settings>

=head1 FILES

<what files program generates uses>

=head1 SEE ALSO

<references to other programs e.g. ps(1)>

=head1 STANDARDS

<RFCs, ANSI/ISO, www.w3c.org that are related>

=head1 BUGS

<known limitations>

=head1 AVAILABILITY

"

(or tinyperl-:skeleton-script-ftp-url
    (skeleton-read "Availabillity: " "<URL Where to get the program>"))

"

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 PREREQUISITES

<what CPAN modules are needed to run this program>

=head1 COREQUISITES

<what CPAN modules are needed to run this program>

=head1 OSNAMES

C<any>

=head1 VERSION

$\Id$

=head1 AUTHOR

"

(funcall tinyperl-:copyright-function)

"

=cut
")


;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-script-function
  "Script: Insert Function banner."
  nil
  "\
# ****************************************************************************
#
#   DESCRIPTION
#
#
#
#   INPUT PARAMETERS
#
#
#
#   RETURN VALUES
#
#
#
# ****************************************************************************
")



;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-module-header
  "Module: Insert POD header; which starts the pod in module.
See function description `tinyperl-pod-write-skeleton-module-function'."
  nil
"\
# ****************************************************************************
#
#   POD HEADER
#
# ****************************************************************************

=head1 NAME

" (buffer-name) " - One line Module descriptions

=head1 REVISION

$\Id$

=head1 SYNOPSIS

    use " (replace-regexps-in-string "\\.pm" "" (buffer-name))
    "; # Import EXPORT_OK
    use "
    (replace-regexps-in-string "\\.pm" "" (buffer-name))
    " qw( :ALL ); # Import everything

=head1 DESCRIPTION



=head1 EXPORTABLE VARIABLES

If there is no special marking for the variable, it is
exported when you call `use'. The rags next to variables mean:

    [ok]    = variable is exported via list EXPORT_OK
    [tag]   = variable is exported via :TAG

=head2 $ABC_REGEXP

<description>

=head2 %ABC_HASH [ok]

<description>

=head2 $debug [ok]

Integer. If positive, activate debug with LEVEL.

<description>


=head1 INTERFACE FUNCTIONS

=for comment After this the Puclic interface functions are introduced
=for comment you close the blockquote by inserting POD footer

=for html
<BLOCKQUOTE>


=cut

")


;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-module-footer
  "Module: Insert POD footer, which starts the pod in module.
See function description `tinyperl-pod-write-skeleton-module-function'."
nil
"\
# ****************************************************************************
#
#   POD FOOTER
#
# ****************************************************************************

=pod

=for html
</BLOCKQUOTE>

=head1 KNOWN BUGS

<Limitations. How to debug problems>

=head1 AVAILABILITY

<release or where to get latest, http, ftp page>

=head1 AUTHOR

"

(funcall tinyperl-:copyright-function)

"

=cut
")

;;; ----------------------------------------------------------------------
;;;
(define-skeleton tinyperl-pod-write-skeleton-module-function
  "Module: Insert template for Puclic interface function.
Where you write Module.pm  public interface functions, document the
functions in place.

Hee is one suggestion ofr Module.pm POD layout

            P O D  H E A D E R
            NAME
            REVISION
            SYNOPSIS
            DESCRIPTION
            EXPORTABLE VARIABLES
            EXAMPLES

            #   module interface is written next

            use strict;

            BEBGIN
            {
                EXPORT          # The export interface
                EXPORT_OK
            }

            Define exported globals

            Define private variables

            P O D   I N T E R F A C E   S T A R T

            P O D  P U B L I C for public functions or methods
            sub ...

            P O D  P U B L I C for public functions or methods
            sub ...

            NORMAL banner of private function
            sub ...

            NORMAL banner of private function
            sub ...

            P O D   F O O T E R
            KNOWN BUGS
            AVAILABILITY
            AUTHOR

            1;
            __END__

"
nil
  "
=pod

=over 4

=head2 Function ()

=item Description



=item arg1:



=item arg2:


=item Return values



=back

=cut")


;;}}}


;;{{{ Perl Path functions

;;; ----------------------------------------------------------------------
;;; (tinyperl-split-win32-path "C:\\Program files\\this  c:\\temp")
;;;
(defun tinyperl-split-win32-path (string)
  "Separate different absolute directories.
\(tinyperl-split-win32-path \"C:\\Program files\\this  c:\\temp\")
-->
'(\"C:\\Program files\\this\" \"c:\\temp\")"
  (let* (locations
	 beg
	 end
	 ret
	 str)
    (with-temp-buffer
      (insert string)
      (ti::pmin)

      (while (re-search-forward "\\<[a-z]:[\\//]" nil t)
	(push (match-beginning 0) locations))

      (push (ti::pmax) locations)
      (setq locations (reverse locations))

      (while (setq beg (pop locations))
	(when (setq end (car locations))
	  (setq str (ti::string-remove-whitespace (buffer-substring beg end)))
	  (unless (ti::nil-p str)
	    (push str ret))))

      (reverse ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-path-convert-to-emacs-host (list)
  "Convert list of paths to the format that Emacs host knows.
If Emacs is win32 application, convert to DOS style paths."

  ;;  Now interesting part: If Emacs in Win32-native and user uses
  ;;  Cygwin-perl, then the situation is as follows:
  ;;
  ;;      PERL5LIB paths refer to cygwin, like /usr/share/site-perl/CPAN
  ;;
  ;;  But this is not a path that GNU Emacs know, because it is pure
  ;;  Windows application. The paths must be converted so that
  ;;
  ;;     CYGWIN-ROOT/path   or CYGWIN-MOUNT-POINT/path
  ;;
  ;;  #todo: XEmacs is different game, it can be built as Cygwin native
  ;;  #todo: How to check if running Cygwin or Win32 XEmacs ?

  (let* ((perl-type (tinyperl-perl-type)))
    (cond
     ((and (emacs-p)
	   ;;  #todo: if Emacs is built as native cygwin application,
	   ;;  this fails.
	   (eq perl-type 'win32-cygwin))
      (let (new-list)
	(dolist (path list)
	  (cond
	   ((and (string-match "^/" path)
		 ;;  Exclude Win32 UNC path formats: //SERVER/dir/dir
		 (not (string-match "^//" path)))
	    (push (w32-cygwin-path-to-dos path) new-list))
	   (t
	    ;;  the file-directory-p is checked elswhere.
	    ;;  Just return pure paths
	    (push path new-list))))
	new-list))
     (t
      list))))

;;; ----------------------------------------------------------------------
;;; (tinyperl-inc-path tinyperl-:perl-bin)
;;;
(defun tinyperl-inc-path (&optional perl)
  "Return @INC and and var PERL5LIB libs for PERL which defaults to `perl'.
References:
 `tinyperl-:inc-path-switches'"
  (let* ((perl-type (tinyperl-perl-type))
	 (path (or perl
                   (executable-find  "perl")))
         ;;    ask from perl where the paths are.
	 (result
	  (with-temp-buffer
	    (apply 'call-process
		   path
		   nil
		   (current-buffer)
		   nil
		   (append tinyperl-:inc-path-switches
			   '("-e"
			     ;; "print 11"
			     "print(qq,@INC,)")))
	    (buffer-string)))

	 ;;  We can't just explode RESULT with Emacs function `split'
	 ;;  because in Win32 it may contain spaces
	 ;;  c:\Program files\activestate\perl\lib

	 (list (when path
		 (if (and (win32-p)
			  (eq perl-type 'win32-activestate))
		     (tinyperl-split-win32-path
		      ;;  Delete current directory from the list
		      (replace-regexps-in-string " \\." "" result))
		   (split-string result))))

         ;;  The LIST test is there so that if you call this with
         ;;  perl 4, then the LIST is nil and we should not check PERL5LIB,
	 ;;  which is perl 5 only variable.

	 (lib (or (getenv "PERL5LIB")
		  (getenv "PERL5_LIB"))) ;; Win32 Activestate Perl

         (path5 (and list
		     lib
		     (split-string
		      lib
		      (if (or (string-match ";"  lib)  ;; was (if (win32-p)..
			      (string-match "[a-z]:[\\/]" lib))
			  ";"
			":"))))

         ret
	 seen)

    (when (and result
	       (string-match "warning\\|error\\|fatal" result))
      (error "TinyPerl: Reading @INC error %s" result))

    (if path5
        (setq list (append list path5)))

    (setq list (delete "." list))


    ;;  Make sure Emacs can read the Paths -- Win32 specific support

    (setq list (tinypath-path-convert-to-emacs-host list))


    (dolist (x list)
      (when (stringp x)
	(unless (member x seen)   ;; Filter out duplicates
	  (push x seen)
	  (if (file-directory-p x)
	      (push x ret)
	    ;;  Record to message, so that possible errors can be
	    ;;  traced.
	    (message "Tinyperl: invalid @INC dir %s" x)))))


    (when (and result
	       (null ret))
      (error
       (format
	(concat "TinyPerl: Can't parse @INC. Please check"
		" tinyperl-:perl-bin = %s"
		" result: %s"
		" path5: %s")
	(prin1-to-string tinyperl-:perl-bin)
	(prin1-to-string result)
	(prin1-to-string path5))))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-path (&optional perl-binary)
  "Return POD path by calling PERL-BINARY or `perl'."
  (let* ((perl (or perl-binary (executable-find "perl")))
	 (path
	  (with-temp-buffer
	    (call-process  perl
			   nil
			   (current-buffer)
			   nil
			   "-MConfig"
			   "-e"
			   ;; "print 11"
			   "print $Config{privlib}")
	    (buffer-string))))

    (when (ti::nil-p path)
      (error "TinyPerl: POD failure from Config.pm using %s" perl))

    ;;  Win32 specific Cygwin support

    (let ((path-list
	   (tinypath-path-convert-to-emacs-host (list path))))
      (setq path (car path-list)))


    (unless (file-directory-p path)
      (error "TinyPerl: Can't find pod path %s [%s]" perl path))


    ;;  Find out the Perl library path. The POD files are
    ;;  under subdir "pod" in Unix and Activestate Perl,
    ;;  but for some reason Cygwin Perl 5.6.1 changed the
    ;;  files under /pods.

    (let (correct
	  try)
      (dolist (pod '("pod/" "pods/"))
	(setq try (concat (file-name-as-directory path) pod))
	(when (and (file-directory-p try)
		   (directory-files
		    try
		    nil
		    "\\.pod$"))
	  (return (setq correct try))))
      (unless correct
	(error "TinyPerl: CAn't determine POD path %s [%s]" path perl))
      (ti::file-name-forward-slashes correct))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-build-pod-files ()
  "Build files under pod path."
  (let* ((path  (or tinyperl-:pod-path
                    (error "TinyPerl: No tinyperl-:pod-path")))
         files
         ret)
    (setq files (ti::directory-files path "\\.pod"))
    (dolist (file files)
      (push (cons file (ti::file-name-forward-slashes path)) ret))
    ret))

;;; ----------------------------------------------------------------------
;;; #todo: This should be rewritten as recursive function
;;;
(defun tinyperl-build-list-of-inc-files (&optional search-list verb)
  "Build list of files under @INC. Only 3 subdir levels are scanned.
SEARCH-LIST corresponds to `tinyperl-:inc-path'

Return:

  '((package.pm . path) (package::package.pm . path) ..)"
  (let* ((INC (or search-list
                  (error "TinyPerl: No SEARCH-LIST")))
         files
         dirs
	 dirs2
	 dirs3
	 package
         ret)
    (flet ((my-add
	    (file pfx path)
	    ;;  As long as the name of the .pl file is unique (not yet
	    ;;  added), store without leading prefix directories.
	    ;;
	    (if (and (string-match "\\.pl" file)
		     (not (assoc file ret)))
		(push (cons file path) ret)
	      (push (cons
		     (if pfx
			 (concat (file-name-nondirectory pfx) "::" file)
		       file)
		     path)
		    ret))))

    (ti::verb)

    ;;  It is unusual that Perl INC path would belonger than
    ;;  3 subdirectories, so we just check 3 levels. This is not very
    ;;  general approach to deal with the situation...
    ;;
    ;;  Font::Metrics::Courier.pm
    ;;  HTTP::Request::Common.pm

    (dolist (path INC)

      (if verb (message "TinyPerl: Reading @INC path %s" path))

      (tinyperl-directory-files files path)

      (dolist (file files)
	(push (cons file path) ret))

      (setq dirs (ti::directory-files ;;  And Level 1 directories
                  path "." 'absolute
                  '(file-directory-p arg)
                  '(string-match "\\.\\.?$" arg)))

      (dolist (dir dirs)

        (tinyperl-directory-files files dir)

        (dolist (file files)
	  (setq package (file-name-nondirectory dir))
          (my-add file package dir))

        (setq dirs2 (ti::directory-files ;;  And Level 2 directories too
                     dir "." 'absolute
                     '(file-directory-p arg)
                     '(string-match "\\.\\.?$" arg)))

        (dolist (dir1 dirs2)
	  (setq dir1 (ti::file-name-forward-slashes dir1))
          (tinyperl-directory-files files dir1)

          (dolist (file files)
	    (setq package (concat (file-name-nondirectory dir) "::"
				  (file-name-nondirectory dir1)))
	    (my-add file package dir1))

	  (setq dirs3 (ti::directory-files ;;  And Level 2 directories too
		       dir1 "." 'absolute
		       '(file-directory-p arg)
		       '(string-match "\\.\\.?$" arg)))

	  (dolist (dir2 dirs3)
	    (setq dir2 (ti::file-name-forward-slashes dir2))
	    (tinyperl-directory-files files dir2)

	    (dolist (file files)
	      (setq package (concat (file-name-nondirectory dir) "::"
				    (file-name-nondirectory dir1) "::"
				    (file-name-nondirectory dir2)))
	      (my-add file package dir2))))))
    ret)))


;;}}}
;;{{{ POD lowlevel functions


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-podchecker (file &optional buffer)
  "Run Pod::Checker/podchecker() on FILE and put output to BUFFER.
Default value for BUFFER is `tinyperl-:perldoc-buffer'."
  (let* ()
    (or (tinyperl-perl-module-exists-p "Pod::Checker.pm")
	(error "\
TinyPerl: Pod::Checker.pm is not known to this Perl version. @INC trouble?"))

    (or buffer
	(setq buffer (get-buffer-create tinyperl-:podchecker-buffer)))

    (or (get-buffer buffer)
	(setq buffer (get-buffer-create buffer)))

    (when nil ;; disabled
      (with-current-buffer buffer
	(ti::pmax)
	(run-hooks 'tinyperl-:podchecker-before-hook)
	(call-process tinyperl-:perl-bin
		      nil
		      buffer
		      nil
		      "-MPod::Checker"
		      "-e"
		      "podchecker shift, undef, -warnings => q(on)"
		      (expand-file-name file))
	(run-hooks 'tinyperl-:podchecker-after-hook)))


      (when t
	(let* (compilation-error-regexp-alist
	       ;;  `shell-quote-argument'  does not work here correctly.
	       ;;  This tackles bash.exe and  Win32 command-com
	       (quote (if (and (win32-p)
			       (string-match "cmd\\|command"
					     shell-file-name))
			  "\""
			"'"))

	       (cmd (concat
		     tinyperl-:perl-bin
		     " -MPod::Checker"
		     " -e"
		     " "
		     quote
		     "podchecker shift, undef, -warnings , q(on)"
		     quote
		     " "
		     (expand-file-name file))))

	  ;;  Keep the old values and add this regexp.
	  ;;  2 = filename, 1 = line number
	  ;; *** WARNING: 2 unescaped <> in paragraph at line 1994 in file xxx

	  (push
	   '(".*[ \t]+line[ \t]+\\([0-9]+\\)[ \t]+in[ \t]+file[ \t]+\\(.*\\)"
	     2 1)
	   compilation-error-regexp-alist)

	  (compile-internal cmd
			    "No more lines."  ;; error-message
			    nil ;; name-of-mode
			    nil ;; parser
			    nil))) ;; error-regexp-alist
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod2text (file &optional buffer)
  "Run pod on FILE and put output to BUFFER."

  (or buffer
      (setq buffer (tinyperl-pod-buffer-name
		    (file-name-nondirectory file))))

  (or (get-buffer buffer)
      (setq buffer (get-buffer-create buffer)))

  ;;  Append text to the end of buffer.

  (with-current-buffer buffer
    (setq buffer-read-only nil)

    (run-hooks 'tinyperl-:pod2text-before-hook)
    (ti::pmax)

    ;; Move point to the end of visible window
    ;; #todo: was I thinking of something here ?...

    (when nil				;disabled
      (let* ((win (get-buffer-window (current-buffer) t)))
	(when win
	  (set-window-point win (point-max)))))


    (let ((point (point)))
      (call-process tinyperl-:perl-bin
		    nil
		    buffer
		    nil
		    "-MPod::Text"
		    "-e"
		    "pod2text shift"
		    (expand-file-name file))
      (when (eq (point) point)
	(message
	 (concat "TinyPerl: pod2text was empty. Please check your perl env?"
		 "It may be broken: try running `perldoc perl'."))))

    (ti::pmin)
    (run-hooks 'tinyperl-:pod2text-after-hook)
    (setq buffer-read-only t))
  buffer)


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-manpage-to-file (pod)
  "Convert POD `perldoc.pod' or `perldoc' into absolute filename."
  (let* ((elt (assoc (ti::string-verify-ends pod ".pod")
                     (or tinyperl-:pod-list
                         (error "TinyPerl: No tinyperl-:pod-list")))))
    (when elt
      (concat (cdr elt) (car elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-read-word-module ()
  "Read word at point suitable for Perl module. Add .pm."
  (let ((word (ti::buffer-read-word "a-zA-Z:"))
	case-fold-search)
    (when (and (stringp word)
	       (or (string-match
		    ;;  English.pm
		    ;;  use English;
		    (concat
		     "^[A-Z]\\([a-z]+\\|[A-Z]+\\)$"
		     ;; use Getopt::Long;
		     ;; use HTTP::Request;
		     ;; LWP::UserAgent;
		     "\\|^[A-Z]\\([a-z]+\\|[A-Z]+\\)"
		     "\\(::[A-Z]\\([a-z]+[A-Za-z]+\\|[A-Z]+\\)\\)+$")
		    word)))
      (setq word (match-string 0 word))
      (when (not (string-match "\\.pm$" word))
	(setq word (concat word ".pm")))
      word)))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-ask-module (&optional msg)
  "Ask with MSG a module."
  (let ((word (tinyperl-read-word-module)))
    (completing-read
     (or msg "Module: ")
     (or tinyperl-:inc-module-list
	 (error "TinyPerl: No tinyperl-:inc-module-list"))
     nil
     (not 'require-match)
     (if word
	 ;;  Put point to the beginning so that user can hit C-k to kill
	 ;;  possibly unwanted word.
	 (cons word 0)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-locate-library  (module &optional no-guess)
  "Check where is MODULE. A .pl and .pm suffixes is added if needed.
Input:

  MODULE    String, name of perl module that should be along
            `tinyperl-:inc-module-list'
  NO-GUESS  Flag, if non-nil don't try searching suffixes .pm and .pl. Trus
            MODULE to be exact name.

Return:

 '(module . path)"
    (if no-guess
        (assoc module tinyperl-:inc-module-list)
      (or (assoc module tinyperl-:inc-module-list)
          (assoc (concat module ".pm") tinyperl-:inc-module-list)
          (assoc (concat module ".pl") tinyperl-:inc-module-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-library-find-file  (elt)
  "Load library pointer by ELT into emacs.
The ELT is return value from `tinyperl-locate-library'.

Return:
  buffer pointer"

  (find-file-noselect
   (format "%s/%s"
           (cdr elt)
           ;; Getopt::Long.pm --> Long.pm
           (replace-regexps-in-string  "^.*:" "" (car elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-ask-manpage ()
  "Ask pod page and Return absolute path of POD manpage."
  (tinyperl-pod-manpage-to-file
   (completing-read
    "View pod manpage: "
    tinyperl-:pod-list
    (not 'predicate)
    'match-it)))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-font-lock ()
  "Turn on `font-lock-mode' and set `tinyperl-:pod-font-lock-keywords'.
The `font-lock-mode' is turned on only if `ti::colors-supported-p'
returns non-nil."
  (interactive)
  (when (ti::colors-supported-p)
    (ti::string-syntax-kill-double-quote)
    ;;  Somehow the keywords must be setq after font-lock is turned on
    ;;  to take in effect.
    ;;
    (turn-on-font-lock)
    (setq font-lock-keywords tinyperl-:pod-font-lock-keywords)
    (font-lock-fontify-buffer)
    nil))



;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-buffer-name (module)
  "Make POD buffer name for perl module like ´English'.

Rerefences:

  `tinyperl-:pod-buffer-name'   Always is single POD buffer in effect
  `tinyperl-:pod-buffer-control'."
  (if (memq tinyperl-:pod-buffer-control '(nil one single))
      tinyperl-:pod-buffer-name
    (let* ((name module)) ;; (replace-regexps-in-string "\.pm$" "" module)))
      (concat "*pod: " name "*"))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-re-search (regexp &optional buffer)
  "Check BUFFER for REGEXP and return (buffer . point) or nil."
  (or buffer
      (setq buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (ti::pmin)
        (if (re-search-forward regexp nil t)
            (cons (current-buffer) (point)))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-pop-to-buffer (regexp &optional buffer)
  "Pop to POD buffer if REGEXP matches. Return non-nil if ok."
  (let* ((elt (tinyperl-pod-re-search regexp buffer)))
    (when elt
      (pop-to-buffer (car elt))
      (goto-char (cdr elt)))))



;;}}}
;;{{{ POD interactive


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-kill-buffers ()
  "Kill all temporary POD buffers."
  (interactive
   (progn
     (unless (y-or-n-p "Kill All temporary pod buffers ")
       (error "TinyPerl: Abort."))))
  (dolist (buffer (buffer-list))

    ;;  For each buffer that has string "*pod" and which doesn't have
    ;;  attached filename

    (when (string-match "\\*pod" (buffer-name buffer))
      (unless (with-current-buffer buffer (buffer-file-name))
        (kill-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-external-command-format (bin)
   "Determine how to call external BIN. Prepend Perl interpreter as needed.
If BIN name contain .bat .cmd etc, return BIN as it.
Otherwise prepend \"perl\" at from and return '(\"perl\" . BIN)."
   (if (string-match "\\....?$" bin)  ;; .ex or .ext
       bin
     (cons tinyperl-:perl-bin bin)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-perldoc-1 (buffer arg-list)
  "Call ´tinyperl-:perldoc-bin'. Insert results to BUFFER.
Call arguments are in ARG-LIST."
  ;; Win32 call-process fails if the binary c:\prgram files\..
  ;; name contains spaces. This is special problems for perldoc.bat
  ;; Because it is in fact full of perl code and called again. See
  ;; The source of perldoc.bat
  (cond
   ((not (win32-p))
    (apply 'call-process
	   tinyperl-:perldoc-bin
	   nil
	   buffer
	   nil
	   arg-list))
   (t
    (with-current-buffer buffer
      (let* ((perl-type (tinyperl-perl-type))
	     (cmd       (if (win32-shell-p)
			    ;;  Must not contain path name
			    ;;  I don't know if the exact problem was due to
			    ;;  SPACES in the path name.
			    "perldoc"
			  tinyperl-:perldoc-bin))
	     (call-type (tinyperl-external-command-format cmd))
	     (args (ti::list-to-string arg-list)))

	;;  Add "perl" to the front of command if it is "perldoc".
	;;  This will work under Windows/Cygwin and Unix
	(if (listp call-type)
	    (setq cmd (format "%s %s %s"
			      (car call-type)
			      (cdr call-type)
			      args))
	  (setq cmd (format "%s %s" cmd args)))

	(ti::process-perl-process-environment-macro
	    perl-type
	  ;;  At least shell command works, this a bit more expensive
	  (let ((out (shell-command-to-string cmd)))
	    (if (stringp out)
		(insert out))))))))
  buffer)

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-perldoc (string &optional force verb)
  "Run perldoc with STRING. First try with -f then without it.
Show content in `tinyperl-:perldoc-buffer'. If buffer is visible in
some other frame, the cursor is not moved there. Only contents is updated.

The last used STRING is cached and if called next time with same
string, the shell command is not called unless FORCE is non-nil.

Input:

  STRING    Seach string
  FORCE     Force calling shell although answer cached
  VERB      flag, Allow verbose messages

References:

  `tinyperl-:perldoc-hook'"
  (interactive
   (list
    (read-string "Perldoc -f: "  (ti::buffer-read-word))
    current-prefix-arg))

  (let* ((buffer (get-buffer-create tinyperl-:perldoc-buffer))
         (last   (get 'tinyperl-perldoc 'string))
         (cmd    (format
                  "%s -f %s"
                  (if (win32-shell-p)
		      ;;  Must not contain path name
		      ;;  I don't know if the exact problem was due to
		      ;;  SPACES in the path name.
		      "perldoc"
		    tinyperl-:perldoc-bin)
                  string
		  ""))
	 run
         win)

    (ti::verb)

    (when (or force
	      (and buffer
		   (with-current-buffer buffer
		     (ti::empty-buffer-p)))
              (not (stringp last))	;Show previous result
              (not (string= last string)))

      (setq run t)

      (get-buffer-create buffer)

      (with-current-buffer buffer
	(setq buffer-read-only nil)
	(erase-buffer))

      (if verb
	  (message "TinyPerl: Running %s" cmd))

      ;; Win32 call-process fails if the binary c:\prgram files\..
      ;; name contains spaces. This is special problems for perldoc.bat
      ;; Because it is in fact full of perl code and called again. See
      ;; The source of perldoc.bat

      (tinyperl-perldoc-1 buffer (list "-f" string))

      ;;  What if we had no luck? Try without "-f" then.

      (with-current-buffer buffer
        (ti::pmin)
        (when (or (looking-at "^No documentation.*for.*function\\|Can't open")
		  (ti::empty-buffer-p))
          (erase-buffer)
          (if verb
              (message "TinyPerl: No matches. Trying without -f ..."))
	  (tinyperl-perldoc-1 buffer (list string))
          (setq cmd (format "%s %s"
                            tinyperl-:perldoc-bin
                            string))
          (if verb
              (message "TinyPerl: No matches. Trying without -f ...Done."))))

      (if verb
	  (message "TinyPerl: Running %s. Done." cmd)))

    (cond
     ((setq win (or (get-buffer-window buffer t) ;In another frame
		    (get-buffer-window buffer)))
      (shrink-window-if-larger-than-buffer win)
      (raise-frame (window-frame win)))
     (t
      (display-buffer buffer)))

    (when run
      (with-current-buffer buffer
	(setq buffer-read-only nil)
	(run-hooks 'tinyperl-:perldoc-hook)
	(setq buffer-read-only t)))

    ;;  save the last query string.

    (if string
        (put 'tinyperl-perldoc 'string string))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-module-find-file (module)
  "Load Perl MODULE source."
  (interactive (list (tinyperl-ask-module "Load perl module: ")))
  (tinyperl-pod-by-module module 'load))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-by-module (module &optional mode)
  "Show pod manual page for MODULE or load MODULE.

Input:

  MODULE    The Perl module as it appears in `use' statement,
            like Getopt::Long the '.pm' is automatically added.
  MODE      If non-nil, load source file, not pod."
  (interactive
   (list (tinyperl-ask-module "View module's pod: ")
         current-prefix-arg))

  (let* ((name             (replace-regexps-in-string "\.pm$" "" module))
	 (pod-buffer-name  (tinyperl-pod-buffer-name module))
	 (pod-buffer       (get-buffer pod-buffer-name))


	 ;; Net::FTP - FTP Client class
         ;; ^^^^^^^^
         (regexp
          (concat "^[ \t]+\\(use +\\)?"
                  (regexp-quote name)
		  "[ \t]+-[ \t]+"))

         file)
    (cond
     ;; ................................................. existing POD ...
     ((and (null mode)
	   pod-buffer
	   (tinyperl-pod-pop-to-buffer regexp pod-buffer))
      nil)                               ;POD is already available

     ;; ................................... new documentation or load ...
     (t
      (if (not (string-match ".p[lm]$" module))
          (setq module (concat module ".pm")))

      (unless (setq module (tinyperl-locate-library module))
        (error "TinyPerl: Can't find module from tinyperl-:inc-module-list"))

      (setq file (ti::file-make-path
                  (cdr module)
                  ;;  Delete prefix, because (cdr path) will cnotain the
                  ;;  full directory
                  ;;
                  ;;  Getopt::Long.pm --> Long.pm
                  (replace-regexps-in-string ".*:" "" (car module))))

      (unless (file-exists-p file)
        (error "TinyPerl: Cache error, %s does not exist" file))

      (cond
       (mode
        (find-file file)
        (ti::pmin))
       (t
        (ti::pop-to-buffer-or-window (tinyperl-pod2text
				  file
				  (get-buffer-create pod-buffer-name)))
        (ti::pmin)
        (re-search-forward regexp nil t)))))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-podchecker (file)
  "Run podchecker on current file."
  (interactive
   (list
    (read-file-name
     "TinyPerl podcheck: "
     (file-name-directory (or (buffer-file-name)
			      default-directory))
     nil
     t
     (if (buffer-file-name)
	 (file-name-nondirectory (buffer-file-name))
       ""))))
  (let* ((buffer (tinyperl-podchecker file)))
    (display-buffer buffer)))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-find-file (file)
  "Run pod2text on FILE and create new buffer: '*pod' + FILE + '*'.
If file contains pod documentation section, it will be formatted nicely."
  (interactive "fFile to pod: ")
  (let* ((name   (file-name-nondirectory file))
         (buffer (get-buffer-create (concat "*pod " name "*"))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (ti::pop-to-buffer-or-window (tinyperl-pod2text file buffer))
    (ti::pmin)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-find-file-this-buffer  ()
  "Call `tinyperl-pod-find-file' with `buffer-file-name'"
  (interactive)
  (if (buffer-file-name)
      (tinyperl-pod-find-file (buffer-file-name))
    (error "TinyPerl: This buffer is not associated with file.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-jump  (module)
  "Jump to Perl MODULE POD if it exists or do nothing."
  (interactive)
  (let* ((buffer (get-buffer (tinyperl-pod-buffer-name module))))
    (when buffer
      (ti::pop-to-buffer-or-window buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-by-manpage (file)
  "Display pod for FILE."
  (interactive (list (tinyperl-pod-ask-manpage)))

  (when (ti::nil-p file)
    (error "TinyPerl: Need POD FILE, like `perldoc.pod', was `%s'" file))

  (let* ((buffer (get-buffer-create (tinyperl-pod-buffer-name
				     (file-name-nondirectory file))))
	 (beg    (with-current-buffer buffer (point-max)))

         ;; perldsc - Perl Data Structures Cookbook
         ;; ^^^^^^^

         (regexp (concat "NAME[\n\r \t]+"
                         (regexp-quote
                          (replace-regexps-in-string
                           "\.pod" ""
                           (file-name-nondirectory file)))
                         " +-+ ")))
    (or (tinyperl-pod-pop-to-buffer regexp buffer)
        (progn
          (ti::pop-to-buffer-or-window (tinyperl-pod2text file buffer))
          (goto-char beg)))))

;;}}}
;;{{{ POD grep

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-grep (regexp &optional pod-path)
  "Grep REGEXP from perl pod files.
This is your way to find what pages contain references to the items you're
looking for. However if you select the file from compile buffer, it is
in the unconverted format (.pod). A better would be to memorize the
pod file name, like

   perlre.pod:165:    \\Z       Match at only e


And call immediately \\[tinyperl-pod-by-manpage] and view `perlre' in
more pleasant manner. Few C-s searches or \\[occur] will take you
to the correct position."
  (interactive "sPod grep regexp: ")
  (or pod-path
      (setq pod-path (or tinyperl-:pod-path
			 (error "TinyPerl: No tinyperl-:pod-path"))))
  (unless (file-directory-p pod-path)
    (error "POD directory not found [%s]" pod-path))
  (let* ((grep (tinyperl-grep-program))

	 ;;  Have to set this variable, because we can't
	 ;;  allow to pass full path to the grep. in Win32 Emacs would
	 ;;  send path in DOS style, but Cygwin does not accept those;
	 ;;  only unix style paths.
	 ;:
	 ;;  So, it's enough to Emacs to do an "cd" to directory.
	 ;;
	 (default-directory (file-name-directory pod-path)))
    (setq pod-path "")
    (if (fboundp 'igrep)
        (ti::funcall 'igrep nil regexp "*.pod" pod-path)
      (grep (format "%s -n '%s' %s*pod" grep regexp pod-path)))))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-process-wait (buffer)
  "Wait until process in BUFFER has finished."
  (let (process)
    (while (or (null (get-buffer buffer))
	       (and (setq process (get-buffer-process buffer))
		    (memq (process-status process) '(run))
		    (prog1 t
		      (sit-for 0.5)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-grep-faq-data-context-1 (&optional line)
  "Read FAQ context around LINE in current buffer.
Enough context is a) FAQ entry  b) or paragraph if there
is no direct faq entry.

Return:

 '(TOPIC-HEADING TEXT-DATA)

TOPIC-HEADING  does not end to cr/lf
TEXT-DATA      ends to cr/lf"
  (flet ((context-min (point lines)
		      (goto-char point)
		      (backward-line lines)
		      (point))

	 (context-max (point lines)
		      (goto-char point)
		      (forward-line lines)
		      (point))

	 (enough-chars-found-point-p
	  (point1 point2)
	  ;;  Require at least 5 lines
	  (> (abs (- point1 point2)) (* 80 5))))

    (let (point
	  min
	  max
	  search-min
	  string
	  topic)

      ;; about 15 lines supposing 80 chars per line.
      ;; These values are rough guesses.

      (save-excursion

	(when line
	  (goto-line line))

	(setq point      (point))
	(setq min        (context-min point 7))
	(setq max        (context-max point 5))
	(setq search-min (context-min point 20)))

      (cond
       ((re-search-backward
	 ;;  FAQ topic line: perlfaq6.pod
	 "^\\(=head[0-9]?.*\\)"
	 search-min 'noerr)
	(setq min (point))
	(setq topic (match-string 1))
	;;  See if we can find next TOPIC nearby. Perhaps
	;;  this is short quote from faq.
	(forward-line 1)
	(if (re-search-forward "^=head[0-9]?\\(.*\\)"
			       max 'noerr)
	    (setq max (line-beginning-position))))
       (t
	(goto-char point)   ;; Previous search-min changed point
	(save-excursion
	  (when (re-search-backward "^=head[0-9]?" nil 'noerr)
	    (setq topic (ti::buffer-read-line))))

	;;  Excerpt enough content arount the point.
	(let (try-min
	      try-max)
	  (re-search-backward "^[ \t]*$" nil t)
	  (setq try-min (point))
	  (goto-char point)
	  (re-search-forward "^[ \t]*$" nil t)
	  (setq try-max (point))

	  ;;  Do not accept too small paragraph for an answer

	  (if (enough-chars-found-point-p try-min point)
	      (setq min try-min))
	  (if (enough-chars-found-point-p try-max point)
	      (setq max try-max)))))

      ;;  Read complete lines. Using just MIN and MAX would
      ;;  give ragged text.

      (setq string (ti::remove-properties (buffer-substring min max)))
      (goto-char point) ;; restore
      (list (ti::remove-properties topic)
	    string))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-grep-faq-data-context
  (&optional buffer grep-data line)
  "Read FAQ context around point in BUFFER.
GREP-DATA is the actual grep content.

Return:

'(absolute-file-name GREP-DATA LINE (topic context-excerpt))"
  (with-current-buffer (or buffer (current-buffer))
    (list (ti::remove-properties (buffer-file-name))
	  line
	  (and grep-data
	       (ti::remove-properties grep-data))
	  (tinyperl-pod-grep-faq-data-context-1 line))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-grep-faq-data-context-all-files
  (&optional buffer verb)
  "Read every grep in BUFFER and retun text excerpts from files.
VERB allows verbose messages.

Return:

'((absolute-file-name grep-data (topic text-data)
  (absolute-file-name grep-data (topic text-data)
  ...)"
  (let (list
	data)
    (ti::grep-output-parse-macro (or buffer (current-buffer))

      ;; Load file and goto correct line

      (let ((file (concat grep-dir grep-file)))
	(when verb
	  (message "TinyPerl: reading faq context %s" file))
	(setq buffer (find-file-noselect file)))

      (with-current-buffer buffer
	(goto-line grep-line))

      ;;  read enough context
      (when (setq data
		  (tinyperl-pod-grep-faq-data-context
		   buffer grep-data grep-line))
	(push data list)))
    (nreverse list)))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pod-grep-faq-data-insert (data &optional verb)
  "Insert faq text DATA into current buffer. VERB.

References:
    `tinyperl-pod-grep-faq-data-context-all-files'"

  (let ((colors-p (ti::colors-supported-p))
	point)

    ;; Has to disable font lock in this buffer of the
    ;; Highlighting isn't shown.

    (when colors-p
      (turn-on-font-lock))

    (dolist (elt data)
      (multiple-value-bind (file line grep-data context-data) elt
	(multiple-value-bind (topic text) context-data

	  (when verb
	    (message "TinyPerl: processing data %s"
		     (file-name-nondirectory file)))

	  (insert
	   (format "FILE: [%s]" (file-name-nondirectory file))
	   (if line
	       (format " LINE: %d\n" line)
	     "\n")
	   (make-string 70 ?-)
	   "\n"
	   (if topic
	       (format "%s\n[...cut...]\n" topic)
	     ""))

	  (setq point (point))

	  (insert text "\n")

	  (when colors-p)
	  (goto-char point)
	  ;;  Mark line that matched.
	  (ti::text-re-search-forward (regexp-quote grep-data))
	  (ti::pmax))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyperl-pod-grep-faq-answer (regexp &optional verb)
  "Grep REGEXP from perl pod files. VERB.

This function also gathers all =head topics that match the REGEXP.
You can use generated page as an answer to 'Has this this question
been answered in FAQ'"
  (interactive "sPod FAQ search regexp: ")
  (let* ((path (or tinyperl-:pod-path
                   (error "TinyPerl: No tinyperl-:pod-path")))
	 (default-directory (file-name-directory path))
         (buffer     "*grep*")
	 (out-buffer tinyperl-:faq-buffer-name)
	 (grep (tinyperl-grep-program))
	 data)

    (ti::verb)

    (setq path "")

    ;;  Grep all strings in pod files

    (grep (format "%s -n '%s' %s*pod" grep regexp path))

    ;;  Grep is asyncronousd, need sleep, and then
    ;;  wait until process finishes. Only after that we gather hits.

    (message "TinyPerl: waiting *grep* process finish...")
    (tinyperl-process-wait buffer)
    (message "TinyPerl: waiting *grep* process finish... done.")


    ;;  See if we got any faq Subject hits?
    ;;  --> put them into list '((faq-name  (topic data)) ..)

    (setq data (tinyperl-pod-grep-faq-data-context-all-files buffer verb))

    (when data
      (display-buffer (get-buffer-create out-buffer))
      (with-current-buffer out-buffer
	(erase-buffer)
	(tinyperl-pod-grep-faq-data-insert data)
	(ti::pmin)))


    (if data
	(message "TinyPerl: FAQ done.")
      (message "TinyPerl: FAQ context processing failed [no data]."))))



;;}}}

;;{{{ Misc

;;; ............................................................ &misc ...


;;; ----------------------------------------------------------------------
;;;
(put 'tinyperl-version-macro 'edebug-form-spec '(body))
(put 'tinyperl-version-macro 'lisp-indent-function 0)
(defmacro tinyperl-version-macro (&rest body)
  "Do BODY when version variable is found. Uses `save-excursion'."
  (`
   (save-excursion
     (ti::pmin)
     ;; (ti::buffer-xtra-open-outline)
     (when (tinyperl-version-stamp-re-search-forward)
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-version-stamp-re-search-forward ()
  "Search perl $VERSION variable. Match 1 will contain the version."
  (let (case-fold-search)
    (re-search-forward
     (concat
      "^[ \t]*\\$VERSION[ \t]*=[ \t]*[\"']"
      "\\([0-9][0-9][0-9][0-9]\\.[0-9][0-9][0-9][0-9]\\)[\"'][ \t]*;")
     nil
     t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-version-stamp ()
  "Find $VERSION = '1234.1234'; variable and update ISO 8601 date."
  (let* ((date (format-time-string "%Y.%m%d" (current-time))))
    (tinyperl-version-macro
     ;; Replace only if it is not current date
     (unless (save-match-data
	       (string-match (regexp-quote date) (match-string 1)))
       (replace-match date nil nil nil 1)))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pause-file-name (&optional filename use-date)
  "Generate PAUSE FILENAME: file-version.pl.
Input:

  FILENAME    `buffer-file-name'
  USE-DATE    if non-nil, use file-yyyy.mmdd.pl, otherwise
              try to guess verison number from a Perl variable in script.
              See function `tinyperl-version-stamp-re-search-forward'."
  (let* (kill
	 buffer
	 ret)

    (setq buffer (or (and filename
			  (find-buffer-visiting filename))
		     (prog1
			 (find-file-noselect filename)
		       (setq kill t))))

    (with-current-buffer buffer
      (tinyperl-version-macro
	(let* ((ver   (or (match-string 1)
			  (format-time-string "%Y.%m%d" (current-time))))
	       (name1 (file-name-nondirectory
		       (or filename
			   (buffer-file-name)
			   (error "TinyPerl: No `buffer-file-name'"))))
	       (name  (file-name-sans-extension name1))
	       (ext   (file-name-extension name1)))
	  (setq ret (format "%s-%s.%s" name ver ext)))))

    (if kill				;We loaded this from disk
	(kill-buffer buffer))

    ret))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pause-copy-file (&optional directory)
  "Copy perl script to separate directory to wait for PAUSE submission.

In order to submit code to PAUSE, it must contain version number.
The file is copied under name FILE-VERSION.pl to DIRECTORY in
this order: first

  1. DIRECTORY (available only as a lisp call)
  2. `tinyperl-:pause-directory' (user's default setting)
  3. or to current directory

References:

  `tinyperl-:pause-directory'."
  (interactive
   (let ((path tinyperl-:pause-directory))
     (list
      (read-file-name "TinyPerl: [PAUSE dir]: "
		      (and path (file-name-directory path))
		      nil   ;; users null string
		      (not 'must-match)
		      (and path
			   (file-name-nondirectory path))))))
  (let* ((from     (buffer-file-name))
	 (file     (tinyperl-pause-file-name from))
	 to)
    (unless (file-directory-p directory)
      (error "TinyPerl: Directory not found %s" directory))
    (unless file
      (message "TinyPerl: Not ready for PAUSE. No $VERSION = 'value';"))
    (setq to (concat (file-name-as-directory directory) file))
    (copy-file from to 'ok-if-already-exists)
    (message "Tinyperl: PAUSE, Copied to %s" to)
    to))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pause-upload-via-ftp (file)
  "Upload file to PAUSE server for submission.
The filename must contain version number: FILE-VERSION.pl"
  (interactive (list (buffer-file-name)))
  (let* ((ver   (tinyperl-pause-file-name file))
	 (temp  (file-name-as-directory
		 (ti::temp-directory)))
	 (upload (concat temp ver)))

    ;;  Use safety net, not just about anything shuld be uploaded.

    (unless (string-match "\\.\\(pl\\|pm\\)$" file)
      (error "TinyPerl: Only .pm or .pl files can be uploaded."))

;;;#todo: background upload not working.

;;;    (ti::file-ange-file-handle
;;;	'put
;;;	"anonymous"
;;;	"pause.perl.org"
;;;	"/incoming"
;;;	temp
;;;	(list ver)
;;;	nil ;;  Run on background
;;;	(format "TinyPerl: ange-ftp PAUSE upload completed %s" ver))

    (copy-file file upload 'ok-if-already-exists)

    (with-temp-buffer
      (insert-file upload)
      (write-file
       (concat
	"/anonymous@pause.perl.org:/incoming/"
	ver)))))


;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-pause-url-submit-www-page ()
  "Visit PAUSE WWW page where you can submit your files.
PAUSE means \"The Perl Authors Upload Server\""
  (interactive)
  (tinyurl-agent-funcall
   'url
   ;; You need to be logged, in order to use this:
   ;; https://pause.kbx.de/pause/authenquery?ACTION=add_uri
   "http://pause.kbx.de/"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyperl-selfstubber-stubs (file &optional force)
  "Generate stubs, ie. function predeclarations from FILE.
Run SelfStubber on current module, whichexpects to find functions
after __DATA__ token.

If there is entry in current buffer to read

  # BEGIN: Devel::SelfStubber
  # END:   Devel::SelfStubber

Then the generated subs are inserted into that section. Any previous
stubs are removed.

Input:

  FORCE     Flag, if nono-nil, copy the file under temp directory
            and __DATA__ token to the beginning of file do that
            all functions are shown. You can use this flag to generate
            prototypes of all functions."
  (interactive
   (list
    (read-file-name "Perl stubs from file: "
                    nil nil 'match
                    (file-name-nondirectory buffer-file-name))
    current-prefix-arg))

  (let* ((name (file-name-nondirectory file))
         tmp
         buffer
         cmd-1
         beg
         end)

    (setq file (expand-file-name file))

    (unwind-protect
        (progn

          ;; ........................................... forced insert ...

          (when force
            (unless (string-match "\\.pm$" name)
              ;;  SelfStubber expects Modules (.pm) files only
              (message "TinyPerl: %s must end to .pm, fixing..." file)
              (setq name (concat name ".pm")))

            (setq tmp (ti::temp-file name 'tmp-dir))
            (copy-file file tmp)
            (setq file tmp)

            (with-current-buffer (setq buffer (find-file-noselect file))
              (delete-matching-lines "__DATA__")
              (ti::pmin)
              (insert "use SelfLoader;\n__DATA__\n")
              (save-buffer nil)))

          ;; ............................................ perl-command ...

          (setq cmd-1
                (format
                 "Devel::SelfStubber->stub( qq{%s}, qq{%s} )"
                 (replace-regexps-in-string "\\.pm$" ""
                                            (file-name-nondirectory file))
                 (replace-regexps-in-string "[\\/]$" ""
                                            (file-name-directory file))))

          (message                      ;Record it to *Messages* buffer
           (format
            "%s -MDevel::SelfStubber -e %s"
            tinyperl-:perl-bin
            cmd-1))

          ;; ........................................... find-position ...

          (and (setq beg (ti::re-search-check
                          "BEGIN:[ \t]+Devel::SelfStubber"))
               (setq end (ti::re-search-check
                          "\n#[ \t]+END:[ \t]+Devel::SelfStubber")))

          (cond
           ((and beg end)
            (save-excursion
              (goto-char beg)
              (forward-line 1)
              (delete-region (point) end)
              (insert "\n")

              (call-process tinyperl-:perl-bin
                            nil
                            (current-buffer)
                            nil
                            "-MDevel::SelfStubber "
                            "-e"
                            cmd-1)

              (message "TinyPerl: stubs updated in buffer")))
           (t                           ;No previoous STUBS
            (call-process tinyperl-:perl-bin
                          nil
                          (current-buffer)
                          nil
                          "-MDevel::SelfStubber "
                          "-e"
                          cmd-1)))) ;; progn
      (when buffer
        (kill-buffer buffer)))))

;;}}}

(defun tinyperl-install-main (&optional uninstall)
  "The main installer."
  (tinyperl-install-hooks uninstall)

  (unless uninstall
    (tinyperl-install nil 'verb))

  (turn-on-tinyperl-mode-all-buffers uninstall)

  (ti::add-hooks '(perl-mode-hook
		   cperl-mode-hook)
		 'turn-on-tinyperl-mode
		 uninstall))

(provide   'tinyperl)
(run-hooks 'tinyperl-:load-hook)

;;; tinyperl.el ends here
