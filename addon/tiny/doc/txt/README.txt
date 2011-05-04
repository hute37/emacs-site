Table of contents

       1.0 Tiny Tools Readme

       2.0 Kit installation instructions
           2.1 Unpacking
           2.2 Quick install -- personal
           2.3 Quick install -- site wide
           2.4 Install from scratch -- without prior elisp directory
              2.4.1 Preconditions for Unix
              2.4.2 Preconditions for Win32
              2.4.3 Making Emacs aware of elisp
           2.5 Make procedure
           2.6 Autoload files
           2.7 Minimum installation
           2.8 Makefile.mak (obsolete documentation)
           2.9 Submitting bug reports

       3.0 Project information
           3.1 Project News
           3.2 Project location
           3.3 Platform support
           3.4 Maintenance note
           3.5 TinyPgp project
           3.7 Emacs resource files

       4.0 Keeping in touch
           4.1 ICQ Contact
           4.2 Mailing list
           4.3 Updates: Watching files
           4.4 Updates: Automatic fetch method
           4.5 Snapshots of forecoming releases

       5.0 About loading packages
           5.1 Dynamic byte compilation note
           5.2 Use autoloads
           5.3 Customizing packages

       6.0 History
           6.1 What does prefix "tiny" mean
           6.2 Thank you section
           6.3 About libraries

       9.0 Appendix A - Win32 and Unix beginners
           9.1 Prompt syntaxes
           9.2 Dos and Unix command equivalences
           9.3 Calling perl programs
           9.4 Changing environment settings

       7.0 Appendix B - Procmail project files

       8.0 Appendix C - Perl project files

       10.0 Appendix D - Copyright information
           10.1 Program files
           10.2 Text files and documentation

1.0 Tiny Tools Readme

        [The Sourceforge Public Summary] Emacs Tiny Tools is a collection
        of libraries and packages, which are designed to be OS and X/Emacs
        platform independent. E.g. configure load-path automatically, URL
        handler, easy-delete, mail complete and many more. This file is
        README.txt included in Emacs Tiny Tools kit. It contains
        instructions how to install a cross platform Emacs/XEmacs/
        Unix/Win32 compatible utilities for wide variety of tasks:
        diff/patch; searching, replacing, caching files, automatic
        load-path setup and more.

        .$Id: README.txt,v 1.1 2005-12-04 20:58:48 hute37 Exp $
        .$Url: http://tiny-tools.sourceforge.net/ $
        .$Contactid: jari.aalto@poboxes.com $
        .$Keywords: readme, text, emacs, perl, tiny tools $

2.0 Kit installation instructions

    2.1 Unpacking

        In _Win32_ use Winzip 8.0+ found at http://www.winzip.com/ and in
        _Unix_ use unzip available at http://www.info-zip.org/pub/infozip/
        Extract the kit and it will create directory according to the
        version number: tiny-YYYY-MM-DD/ or tiny-YYYY.MMDD/. The Kit build
        method is only for your convenience and it is not necessary to run
        the build command in order to use the Emacs Lisp files or to read
        the documentation under `/doc'.

            % mkdir -p $HOME/elisp/packages/
            % cd $HOME/elisp/packages/
            % unzip -x ~/tmp/download/tiny-tools*.zip

    2.2 Quick install -- personal

        If you just want to install the package for your
        personal use, add following lines at the start of your
        Emacs initialization file. Lines will configure your Emacs to be
        aware of all lisp paths under `$HOME/elisp' or `$HOME/lisp'.

            ;; $HOME/.emacs
            ;; PLEASE READ DOCUMENTATION OF TINYPATH FOR MORE.

            (load "ABSOLUTE-INSTALLATION-PATH-HERE/tinypath")

            ;; The rest of the code can be anywhere in .emacs.
            ;; Next, auto configure most of the packages with defaults
            ;; See M-x tiny-setup-display and C-h f tiny-setup

            (require 'tiny-setup)
            (tiny-setup 'all)

            ;; Perhaps you would like to load some package immediately.

            (require 'tinymy)

            ;; End of example

        The installation function `tiny-setup' logs actions made to Emacs
        into *Messages* or XEmacs *Message-Log* buffer. The output looks
        something like this with the default setting _all_. For more finer
        control for each individual feature, refer to `C-h' `f'
        `tiny-setup'. Messages that read "No options to configure" means
        that there is no default installation (yet) and you have to
        manually take the package into use by reading its documentation.
        The setup is very conservative: anything serious in existing emacs
        will _not_ _be_ _overridden_.

            TinySetup: tinyappend      configured with `bind'
            TinySetup: Key "C-c=" set to `tinyappend-end'.
            TinySetup: Key "C-c-" set to `tinyappend-beg'.
            TinySetup: Key "C-c_" set to `tinyappend-kill'.
            TinySetup: Key "C-c|" set to `tinyappend-yank'.
            TinySetup: tinybookmark    configured with `defalias'
            TinySetup: tinybuffer      configured with `bind'
            TinySetup: tinycache       configured with `autoload'
            TinySetup: tinychist       No options to configure.
            TinySetup: tinycomment     configured with `bind'
            TinySetup: Key [(meta 59)](M-;) set to `tinycomment-indent-for-comment' (FORCED).
            TinySetup: tinycompile     configured with `autoload'
            TinySetup: tinydesk        configured with `activate'
            TinySetup: tinydiff        configured with `autoload'
            TinySetup: `auto-mode-alist' now contains (\.diff\' . turn-on-tinydiff-mode)
            TinySetup: `auto-mode-alist' now contains (\.patch\' . turn-on-tinydiff-mode)
            TinySetup: tinydired       configured with `autoload'
            TinySetup: tinyeat         configured with `bind'
            TinySetup: Key (ESC DEL) already has has definition `backward-kill-word'. Not set to `tinyeat-erase-buffer'
            TinySetup: tinyef          configured with `autoload'
            TinySetup: tinygnus        configured with `autoload'
            TinySetup: tinyhotlist     configured with `autoload'
            TinySetup: tinyigrep       configured with `autoload'
            TinySetup: tinyindent      No options to configure.
            [And so on...]

    2.3 Quick install -- site wide

        If you are Using PC or your own Linux box, including both XEmacs
        and Emacs installations, plus lot of extra Emacs packages that are
        available form the Net, you need to tell the _different_ locations
        where the installed lisp directories are. In this case, you have to
        set the `tinypath-:load-path-root' yourself. In previous personal
        installation example, this was automatically determined.

            ;;  Your lisp/site-start.el should contain this installation code.
            ;;  STILL, IN SITE-WIDE SETUP, THE CACHE IS STORED TO EACH USER'S
            ;;  HOME DIRECTORY.
            ;;
            ;;  List of ROOT directories where all Emacs lisp
            ;;  files are located. Update list according to your site.
            ;;  No need to optimize away non-existing directories,
            ;;  because they will be ignored.

            (setq tinypath-:load-path-root
              (list

                (if (boundp 'xemacs-logo)               ;; ROOT DIR ONE
                    "Your-XEmacs-NN.NN/lisp"
                  "Your-Emacs-NN.NN/lisp")

                (if (boundp 'xemacs-logo)               ;; ROOT DIR TWO
                    "Your-XEmacs-site-list-directory"
                  "Your-Emacs-site-list-directory")

                "~/elisp"                               ;; ... and so on
                "~/lisp"))

            (load "ABSOLUTE-INSTALLATION-PATH-HERE/tinypath")

            ;; End of example

        After the code above, The site users can start configuring all the
        packages found in `load-path'. Appropriate startup code is needed
        for each user's `$HOME/.emacs' files to activate the packages,
        unless you include some site wide defaults.

    2.4 Install from scratch -- without prior elisp directory

       2.4.1 Preconditions for Unix

        If you don't have your private Emacs lisp directory yet, it's
        time to create one. Decide where you want to put future downloaded
        packages. Traditionally this has been `$HOME/elisp'. Run command:

            % mkdir $HOME/elisp

       2.4.2 Preconditions for Win32

        In Windows operating system, there is no default HOME,
        so you have to choose one directory for your personal use.
        Emacs installation in Win32 might look like this:

            C:/emacs/emacs-19.34/       # your old Emacs
            C:/emacs/emacs-20.7.1/      # the new Emacs
            C:/home/elisp               # your private elisp dir

            dos> c:
            dos> md \home
            dos> cd \home
            dos> md elisp

        At minimum, you must add environment variable *HOME* and make your
        Win98/NT/W2K have it visible at boot time. In Win9x, add this line
        to your `c:\autoexec.bat'. In Windows ME/NT/W2K, use Control Panel =>
        System => Environment. Make sure you also create the directory to
        disk. Refer to NT Emacs FAQ at
        http://www.gnu.org/software/emacs/windows/ntemacs.html
        for more complete information about installing Win32 Emacs. After this
        line, you Emacs translates tilde(~) character into reference to
        the HOME.

            set HOME=c:\home

        Your Emacs startup file must now reside at directory `$HOME/.emacs'
        which is the traditional way of saying "Emacs startup file is under HOME".
        In win32, this means that the location would be:

            c:\home\.emacs

       2.4.3 Making Emacs aware of elisp

        Emacs can't load additional packages if it doesn't know where they
        are located. The traditional way has been to add appropriate paths
        one by one to the `load-path' variable. Tiny Tools provides
        more versatile and easier path configuration as demonstrated below:

            ;; $HOME/.emacs -- Emacs startup file

            ;;  These two lines are not necessary, since tinypath.el will
            ;;  handle all this. It has been the traditional Emacs way
            ;;  to add paths in case you need to know
            ;;
            ;; (require 'cl)
            ;; (pushnew  (expand-file-name "~/elisp") load-path :test 'string=)

            (load "ABSOLUTE-INSTALLATION-PATH-HERE/tinypath")

            ;; End of $HOME/.emacs

        The "old" method required you to track every change of Emacs
        paths and manually keep it up to date. It is much easier to let
        *TinyPath* to make all this path configuration automatic.

    2.5 Make procedure

          _NOTE1:_ YOU MUST CONFIGURE `load-path.el' in the kit if you
          compile the packages. _Save_ _your_ _modified_ _copy_ for next
          installation and use the `-l' switch for the perl *makefile.pl*

          _NOTE2:_ IT IS NOT STRICTLY NEEDED TO COMPILE ANYTHING. In fact
          the maintainer would prefer if you used non-compiled versions of
          the lisp files. Bug reports from compiled files are useless for
          tracking the cause of an error. Bonus: uncompiled files can be used
          for both XEmacs and Emacs.

        Due to platform independence, Perl has been chosen for all scripting
        tasks. It offers more expressive power than `make(1)' which is old
        Unix-only solution. The older make compatible "makefile.mak" method
        is no longer supported due to Win32 incompatibility and its vulnerable
        syntax (old time Makefile users know what a missing TAB causes). The
        old makefile.mak file is preserved for educational purposes which
        you can take a look if you need makefile example for programming
        languages like Java or C++.

        You need Unix Perl 5.004+ in order to use the current build method
        or Win32 Activestate perl build 520 or never. Perl for Windows
        can be found at http://www.activestate.com/ and for other
        platforms, visit http://www.perl.com/. If you download the
        Activestate Perl 5.6 or newer (6xx branch), please pay attention
        that it uses new windows installer that you also have to download
        from Active state's page. To build the kit, change directory to
        bin/ and feed the *makefile.pl* to perl. In Win32 platform, add
        make directive `unix2dos' and in Unix add `dos2unix' which fix the
        line endings. Substitute option `emacs' with `xemacs' if you're
        compiling for XEmacs.

            % cd bin/
            % perl makefile.pl --help
            % perl makefile.pl --binary emacs --verbose 2  dos2unix all
                               |                           +======= ===
              All command line options must                |
              come BEFORE build options                    |
                                                           |
                                      Two build options here

        In case everything does not compile right, due to missing lisp
        packages, modify `load-path.el' to include any additional
        directories. Make a copy of it to yourself for later updates and
        supply your version with added option. The "\" at the end of line
        means that the line continues. In Unix, you literally type that
        character, in Win32, you just type everything as one line.

            %  perl makefile.pl --binary emacs --verbose 2  \
               --load your-modified-copy-here/load-path.el  \
               dos2unix all

        _Gnus_ _note:_ packages *tinygnus.el* and *tinymail.el* require the
        very latest development version of Gnus. Modify `load-path.el' to
        include path to the Gnus 5.8.8 (2000-11-09) or newer.

        IF YOU'RE RUNNING Unix BE SURE THAT YOU HAVE SYMBOLIC LINK IN
        `/usr/local/bin/perl' to point to perl interpreter, which has been
        the standard perl location for years. Linux Redhat may only
        have `/usr/bin/perl'. There is a build option to fix all the she-bang
        "#!" lines for the perl scripts as well if you prefer not to add
        symbolic link to tell the location of perl. Add this option to make
        the program fix all the perl files:

            --perlfix /usr/bin/perl

    2.6 Autoload files

        If you choose not to use file *tiny-setup.el* and function
        `tiny-setup', you could add following statements to your Emacs
        startup file in `$HOME/.emacs'. The autoload file provide
        "functions" that are immediately available in Emacs, but they do
        not load any packages. This is better than using `require'
        commands, because it makes your Emacs start faster.

            ;; $HOME/.emacs

            ;;  These won'y modify anything in your existing Emacs.

            (require 'tiny-autoload-loaddefs-tiny)
            (require 'tiny-autoload-loaddefs-other)

            ;;  Now, configure anything as you would like.
            ;;  Each feature must be taken into use separately.
            ;;  Read the documentation sections at the beginning of each file.

            ;; End of $HOME/.emacs

    2.7 Minimum installation

        If you are tight of space or otherwise do not need all the tools or
        if you're only interested in specific package, here are the
        instructions how you use minimum installation. The basic idea is to
        include libraries + package(s). Most of the packages require only
        four core libraries, but others may need more e.g. due to XEmacs
        compatibility.

            tinyliba.el     - autoloads
            tinylibb.el     - backward compatibility Emacs version changes
            tinylibm.el     - macros, other low level forms and defsubst's
            tinylib.el      - main library
            tinylibxe.el    - You need this if you run XEmacs

            + NOW select packages that you're interested in.

    2.8 Makefile.mak (obsolete documentation)

        The old *makefile.mak* is from very early Unix-only releases. It
        has been included, but the makefile is not supported and no fixes
        are incorporated even if suggested. The file has been kept in the
        distribution, because it may have some educational value if you
        would like to learn doing makefiles in Unix.

    2.9 Submitting bug reports

        IT IS IMPORTANT THAT YOU USE NON-COMPILED FILES AND SUBMIT THE
        *BACKTRACE* Each lisp package has one or two contact functions.
        When you find an error, immediately contact maintainer with the
        bug report function. The `XXX' is prefix of the package, like
        `tinyurl' for *tinyurl.el*

            M-x load-library RET package.el RET             # load non-compiled
            M-x turn-on-emacs-debug RET                     # From tinyliba.el

            ...  repeat what you did until the point of error ...

            M-x XXX-submit-bug-report       << prefer this if it exists >>
            M-x XXX-submit-feedback

            ... and copy paste any relevant information, like the lisp
            ... error buffer *Backtrace* that might have been generated.

3.0 Project information

    3.1 Project News

        2002-08-05 Library cleanup continues. This time tinylibo.el and
        tinylibt.el have new function prefixes. Many new bugs removed.
        Added new feature to tinyperl.el - it can now construct an
        FAQ answer by grepping all pod faq pages.

        There is new package *tinydebian.el* which hopefully will in future
        will contain lot of Debian Linux utilities. Currently the package
        mimics the `reportbug' binary program. The good thing is, that you
        can manipulate the bug report directly in Emacs and use your
        favorite Mail backend to compose mail.

        *tinypath.el* seems to need few minor fixes. See
        lisp/ChangeLog for more.

        *tiny-setup.el* is more samrter in setting up the features.

        *tinyload.el* has new user setup. Inexperienced users can
        define simple flat-file what files to lazy-load. No need
        to know lisp at all. Experienced users can take advantage
        of new lisp level `add' and `remove' functions.

        *tinypgp.el* may see a day of resurrection someday. Added
        GPG support and played with the inferface a bit. This needs
        lot more work still. Don't expect much yet.

        2002-01-02 Perl text to HTML converter was removed from the
        project. It is now hosted separately at
        http://perl-text2html.sourceforge.net/. Similarly the
        Perl webget utility was removed and it is now hosted at
        http://perl-webget.sourceforge.net/

        2001-12-31 A new RELEASE has been uploaded to sourceforge.

        2001-12-08 Name space cleanup has been completed and all the
        functions and macros from libraries are "Emacs safe", meaning that
        they should not clash Emacs's name definitions. FSF released Emacs
        21 and initial tests with the code seems promising.  No serious
        incompatibilities found. Latest CVS gnus seems to use Emacs 21.1
        specific definitions of `buffer-size` with optional argument. Added
        compatibility function to tinylib-ad.el so latest Gnus can be used
        under Emacs 20.x.

        The CVS tree has been completely redesigned. All lisp code is now
        under lisp/, but categorized further into lisp/tiny and
        lisp/other. The documentation now includes HTML and it is no longer
        generated separately. New directories include doc/txt and
        doc/html.

        2001-07-25 Work started to make all libraries "names pace clean".
        This is a pre-step to ensure that there is no incompatibilities
        with Emacs function naming and to make kit eventually an XEmacs
        package.

        2001-07-12 A new RELEASE has been uploaded to sourceforge. This is
        based on the last active snapshot 2000-04. This release contains
        renamed files, incompatible changes and whole new setup system.

        2001-05-15 The current snapshot has been in place several weeks and
        no more new features have been added. No bug reports haven't been
        reported in this period. The final release is just about ready.

        2001-03-11 New installation method provided in *tiny-setup.el*
        which is used to configure all tools without risk of changing Emacs
        environment. The methods also helps getting the the tools running
        without configuring each package separately.

        2001-03-08 Sourceforge staff has finally solved a long
        standing problem with SSH + CVS interaction that prevented using the
        CVS from the very start of the project, back to 6 months. _You_
        _can_ _use_ _cvs_ now to get newest development code.

        2001-03-04 Emacs 21.0.98 and other pretests have serious errors in
        cl-macs.el (dolist), so don't expect anything to work there. Bug
        report has been files Emacs pretest team and it will be fixed in
        official 21.x

        2001-01-22: *tinypath.el* was completely redesigned and now
        it uses external perl helper script *emacs-util.pl* which
        is much faster than the Emacs Lisp method to recurse
        deep directory structures. Took lot of work and it is not yet
        stable.

        2001-01-02: It is now possible to create *site-lisp* directory
        directly from Emacs lisp developers' home pages. The perl
        script *mywebget.pl* was enhanced in big leaps. To get all
        the latest code from Internet, refer to `bin/mywebget-emacs.conf'
        and the perl script's documentation.

        Sourceforge *emacs-elisp.html* has been completely redesigned,
        (developers are listed only in one section) and stale links
        have been removed. Index.html contains new information and
        uses BMT Internet time.

        2000-11-18: The HTML pages at sourceforge are now in place.
        Administrative tools need serious work. CVS TREE is not yet still
        functional, but a private copy is being used.

        2000-09-09: The plans to move to sourceforge and public CVS server
        has been initiated. The transition and adaption of all the building
        tools must be reprogrammed and this will take time. However,
        the official releases starting today, can be found there.

        2000-06-01: To remind all old tools users: ALL THE VARIABLE
        NAMES HAVE BEEN CHANGED form the original tar.gz distribution.
        They now reflect the PACKAGE prefix and not 3-4 letters as
        they used. (E.g. tinymail.el variables used to have timi- but
        they now have tinymail- prefix)

        At least Win32 XEmacs 21.2.20 does not have a built-in function to
        call native win32 applications, so TinyUrl package response to URLs
        is slow (external shellex.exe must be used). Get a newer XEmacs
        which has built-in function `mswindows-shell-execute'.

        2000-02-12: Emacs 20.4 comes with slightly buggy C++ mode. Get version
        5.26 or newer, otherwise loading tiny-tools will disable your C++ mode.
        http://www.python.org/emacs/cc-mode/

        2000-01-08: The Unix makefile.mak format is no longer supported,
        please use the cross platform compatible Perl makefile.pl

        2000-01-08: Don't use *tiny-tools.tar.gz*. It was last Unix based
        kit. The development continues in *tiny-tools-beta.zip* which is
        first cross-platform Win32 buildable kit. There is no support for
        the old tar.gz and it remains in place until the
        tiny-tools-beta.zip has matured enough.

    3.2 Project location

        The maintainer is <jari.aalto@poboxes.com> and the file you're
        currently reading is supposed to be `README.txt', included in the
        Tiny Tools distribution kit. The development happens at sourceforge
        and you can download the latest kit from there (Select kit by
        latest date). To monitor the kit, Select from "Latest File
        Releases" tile-box and "envelope" which reads Notes/Monitor. The
        Project WWW page and ftp download areas are below. Please point
        your web browser to these locations to see the latest news. The
        project pages contain links to all the rest of the pages.

            http://tiny-tools.sourceforge.net/
            ftp://download.sourceforge.net/pub/sourceforge/tiny-tools/

            Project_id=11049
            http://sourceforge.net/projects/tiny-tools/
            http://sourceforge.net/docman/index.php?group_id=11049

        The home page of the author is located at (may not be available)

            http://poboxes.com/jari.aalto

    3.3 Platform support

        o   Emacs : 20.4+ in Unix and Win32 platforms.
        o   XEmacs: 21.1+ in _Unix_ platform.
        o   XEmacs: 21.2+ in _Win32_ platform.

        Note: Win32 XEmacs versions prior to 21.2 are buggy. Under
        Win32 don't try to use anything older than 21.2.20. Refer to
        `emacs-elisp.txt' or web page #URL-HOME/emacs-elisp.html for
        location how to find Win32 XEmacs ports.

    3.4 Maintenance note

        IF YOU HAVE OLD tiny*el FILES, DON'T MIX THEM WITH NEW KIT.
        DON'T EVER COPY INDIVIDUAL FILES AROUND. LIBS MUST GO WITH THEM.

        Functions may have been removed from the libraries due to
        rearrangements and new coding methods. The old versions normally
        won't work with the new kit. Please don't use the old versions. The
        maintainer concentrates to questions regarding only to the latest
        released kit.

    3.5 TinyPgp project

        This note has been dated 2000-01-08. Project is stalled/broken. No
        fixes are planned in any near future.

        Years back the coding of module TinyPgp started. Lot of
        nights and not-so-well sleep went by and there were over 300
        versions made alone with the Pgp support. It was black and deep
        swamp: different MUAs (RMAIL, VM, Gnus -- beta versions every
        week), different PGP versions, 2.6.x (had so-and-so command line
        support), PGP 5.x and 6.x (that were not designed at all Unix in
        mind; they had no good batch command line support), GPG, which only
        worked at Unix at the time, and there was no way to get it compiled
        in HP-UX, also no windows version were available at that time.

        TinyPgp package offered integration to TM/SEMI to send PGP/MIME
        and supported lot of different re-mailers, direct management of
        re-mailer accounts, multiple key rings, multiple key-access methods
        to key servers and so on...

        It all worked for some time, 2 years, but the variables in this
        soup were too many for the maintainer to be able to continue
        getting them all to work together. Maintaining a single package
        drained efforts from all other projects (perl, procmail and other
        Emacs tools)

        For the time being, the TinyPgp project is in stall (perhaps not
        yet abandoned), and you can expect it to be non-functional. You can
        try it out, bug reports are welcomed, but no problems will be fixed
        in near future. All bug reports, will saved and addressed if time
        allows. There is vision to get once-excellent re-mailer
        support working again and get back to "anonymous" business in the
        spirit of pioneer anon.penet.fi

    3.7 Emacs resource files

        Some Emacs resource files (rc - startup files) has been included to
        show how Emacs can be configured for personal use. See under the
        `lisp/rc/' directory. There is no support for these files and they
        usually do not work out of the box. Read them as you see fit and
        copy code to yourself as needed. Any suggestions or comments are
        welcomed though.

4.0 Keeping in touch

    4.1 ICQ Contact

        Development location is in Europe, Finland UTC+2 and in case you use
        ICQ client from http://www.icq.com/ you can check online status
        with ICQ login "jari-aalto"/82313129

    4.2 Mailing list

        There is mailing list for the project, where you can send questions
        and bug reports. Traffic in the lists is very occasional:

            http://lists.sourceforge.net/lists/listinfo/tiny-tools-users
            http://www.geocrawler.com/lists/3/SourceForge/8673/0/

            Send a message with the word `help' as subject or in the body,
            to: <tiny-tools-users-request@lists.sourceforge.net>

            subscribe   [address=<address>]
            unsubscribe [address]

        There is also a web forum for the project, which is rather not
        recommended, because the messages stay on sourceforge forum and the
        maintainer(s) don't spend much time there. If you post to the
        mailing list, the mail arrives immediately to the contact address
        and you can expect a quick reply.

            http://sourceforge.net/forum/forum.php?forum_id=34642

    4.3 Updates: Watching files

        All the sourceforge projects can also be "watched" and make them
        send you an email whenever there is new release available. See the
        small "letter" or "paper" icon next to the the published kits at
        sourceforge `/project/tiny-tools' main page.

    4.4 Updates: Automatic fetch method

        It is best to use some general and easy update software which can
        periodically pull out the release. You may already be familiar with
        wget(1) whose binaries for Unix and Win32 can be found at:
        http://www.gnu.org/software/wget/wget.html

        There exists a cross platform Perl script which offers
        similar capabilities, including a configuration file and ability to
        find _newer_ releases from the download locations (wget only gets
        what you instruct). It may suit better for automatic retrieval of
        material with changing version numbers. Visit project
        <http://perl-webget.sourceforge.net/>. You can use the program like
        this:

            % mywebget.pl --help
            % mywebget.pl --new --overwrite --verbose HTTP-OR-FTP-FILE-LOCATION

        Define your `$HOME/config/mywebget/mywebget.conf' file where you
        cache URLs that you want to retrieve periodically and add appropriate
        entries. Start by setting these variables and put them into your startup
        file:

            tcsh% mkdir -p $HOME/elisp/packages        # drop directory
            tcsh% mkdir -p $HOME/config
            tcsh% setenv MYWEBGET_CFG $HOME/config/mywebget/mywebget.conf

        Next, add following contents to the configuration file:

            #  $HOME/conf/mywebget/mywebget.conf -- Configuration file

            ROOT  = $HOME
            ELISP = $ROOT/elisp
            EPKG  = $ELISP/packages

            tag1: my-emacs-lisp         # keep tag name prefix "my"

                lcd: $EPKG              # chdir to elisp/packages

                tag2: my-tiny-tools

                    # This does not have to be the exact file name,
                    # The name is used as template and newer fiel will be found

                    ftp://download.sourceforge.net/pub/sourceforge/tiny-tools/tiny-tools-1999.0909.zip new:

            # end of configuration file

        After putting the tags in place, you only have to remember the
        *tag* to pull off latest version of the package. Refer to Perl
        script for more. You probably want to do this bi-weekly to update
        your favorite programs by a cron. You do not have to update the
        "version name" in the file, the *new:* directive will automatically
        retrieve the latest version for you:

            % mywebget.pl -v -o -n -t tiny-tools
            % mywebget.pl -o -t tiny-tools          (or simply this)

    4.5 Snapshots of forecoming releases

        Snapshots for upcoming releases are available from CVS. The
        `cvs(1)' binary is included in Win32/Cygwin toolkit
        <http://www.cygwin.com/>. You can install Unix `cvs(1)' from
        <http://www.cvshome.com/>. Here is the command how to retrieve the
        latest version of the tools:

            $ cvs -d :pserver:anonymous@cvs.tiny-tools.sourceforge.net:/cvsroot/tiny-tools login
            password:[RET]
            $ cvs -z6 -d :pserver:anonymous@cvs.tiny-tools.sourceforge.net:/cvsroot/tiny-tools co tiny-tools

        It is wise to run CVS build with full debug on and report errors to
        the mailing list:

            bash$  perl makefile.pl --verbose 2 --binary emacs all \
                   2>&1 | tee ~/tmp/tiny-compile.log

            csh%   perl makefile.pl --verbose 2 --binary emacs all \
                   |& tee ~/tmp/tiny-compile.log

5.0 About loading packages

    5.1 Dynamic byte compilation note

          Please read these instructions with some salt, because the
          maintainer is not 100% sure of the accuracy of this explanation.
          Please feel free to suggest corrections to this text.

        All the files in Tiny Tools kit turn on the dynamic byte
        compilation flag, which appeared in Emacs 19.29 and XEmacs 19.14.
        That's something you don't have to worry about, but I think you are
        would be interested in hearing, that it has some nice benefits over
        the normal compilation. If you say

            (require 'tinylibm)

        the statement used to mean "load library tinylibm". All functions
        were imported into emacs in a whip. But that's  different now with
        dynamically compiled files. Now it means "load STUBS from library
        tinylibm". The STUBS are pointers in a table to all the rest of the
        functions in the library. So, the functions are not actually loaded
        to your Emacs; neither does your Emacs suffer from sudden memory
        bloat.

        When the actual function is called, Emacs automatically substitutes
        the STUB with the function definition by loading it from the
        library. Just that function, not the others. That's the benefit of
        the dynamic byte compilation in a nutshell.

        By the way, if you happen to need some function from the libraries,
        don't do this:

            (require 'tinylimt)     ;; mail tools

        _Do_ _not_ _do_ _this_ either, if you need something from the main
        library:

            (require 'tinylib)

        The correct way to let your emacs know about all the existing
        library functions, is to load the *m* library. It will define
        all the necessary autoloads to rest of the function and you don't
        have to worry about the location of a particular function

            ;;  Define everything, publish interfaces. Defines
            ;;  backward compatibility functions, autoloads etc.
            (require 'tinylibm)

    5.2 Use autoloads

        When you install packages, try to avoid loading them from your
        $HOME/.emacs like this

            (require 'tinytab)  ;; TAB minor mode

        Because it means that the whole package is loaded (or STUBS) if you
        do have many `require' commands, your Emacs startup time grows
        proportionally. You hate slow Emacs startup; right? Okay, there is
        much better way to load files; called `autoload'. It means that you
        tell Emacs; that "function exists in package xxx". When function or
        feature is called (and the autoload triggers), Emacs loads function
        from the package. Some day you don't use some feature in your emacs,
        sometimes you use, and Emacs grows and loads the packages when you
        happen to need them. No need to use `require' to have it all at once.

        Now, if you read carefully the *installation* section from every
        package and prefer copying the autoload setup instead, you will
        have the most optimized way to install the package to your .emacs.
        If some package doesn't have autoload choice, complain immediately
        to the Author. The package writer should have thought the autoload
        possibility too. There may also be a note that package can't be
        autoloaded, so in that case there is no other possibility that to
        `require' it. For even more slicker and faster Emacs startup
        sequence, see package *TinyLoad* which you can uset to optimise
        lisp file loadings even more.

    5.3 Customizing packages

        New Emacs releases 19.34+, 19.15+ and 20.x come with the package
        *defcustom.el* bundled. If you have limited lisp skills or if you
        want an easy interface to packages' variables, then you can
        customize the modules. The *group* is completed when you press
        `tab' key:

            M-x customize-group RET group RET

6.0 History

    6.1 What does prefix "tiny" mean

        The first package made was *tinyappend.el*, a simple utility to
        gather selected areas of text somewhere convenient. Since then the
        prefix "tiny" has been preserved in front of every lisp file.

    6.2 Thank you section

        The life with Emacs evolved in the direction called "Tiny
        Tools"; modules that got initially written in the past years
        starting with Emacs 19.28. The current code is aimed to be
        XEmacs/Emacs Unix/NT platform independent. The cross platform could
        not be possible without following testers that patiently kept
        reporting bugs and making improvement suggestions. Most sincere
        thanks to you all.

        o   Henk SPG <tank@xs4all.nl> who had courage to take early v1.6x
            TinyPgp in XEmacs. I got very good feedback and bug tracing
            assistance from him.
        o   Samuel Tardieu <sam@inf.enst.fr> has been a key figure
            in  testing the PGP interfaces with 2.6.x and 5.x in Unix.
        o   Dan Rich <drich@cisco.com> who assisted me with solving the
            XEmacs 20-21.x incompatibility problems. The correction cycle
            rolled out changes that caused rewriting my other packages too.
            In addition he suggested many other new features I didn't think
            of before. He cleared many problems by talking to XEmacs 20.x
            maintainers and if that's not all, he also made sure the
            packages worked in VM. I'm very happy that Dan came into
            picture.
        o   Sami Khoury <skhoury@omnisig.com> tried every new tiny tools
            release and reported any byte compiler errors with latest Emacs
            releases. He also suggested many improvements, many new
            features to add and sent patches. Without Sami, I would have
            not been able to know how things work in Emacs 20.x and 21.x.
            Sami was the ears and eyes in the bleeding edge Emacs
            development till the FSF opened their Emacs CVS server.
        o   David x callaway <dxc@xprt.net> joined the team somewhere 2000-10
            and kept asking, suggesting and reporting improvements in rapid
            fashion. That's what is needed to make all tools better. Thank
            you David, Don't stop bugging the maintainer *smile*.
        o   Luis Miguel Hernanz Iglesias <luish@germinus.com> got
            interested in 2000-10 and was fascinated with the TinyPath
            (new utility) which made Emacs lisp package path configuration
            a joy. He joined the development and sent lof of patches and
            squeezed many bugs from TinyPath. I'm impressed!

    6.3 About libraries

        The platform of these tools is any: Emacs and XEmacs, but and even
        the (FSF) Emacs versions differ a lot: new features are introduced,
        some made obsolete, multibyte support in 20.x changed character
        handling totally. It's hard to keep track of the changes, but that
        has been the primary goal. Whether you're running the brand new
        version or older one, the packages should work. Even if you change
        from Emacs to XEmacs, there should be no major problems. And
        nowadays the tools also support Win32 platform. All this means that
        the platform dependendent code is isolated to libraries and shared
        by all other modules. In order to reuse code, so that nothing is
        not duplicated anywhere, much of the code has been moved to the
        libraries.  It makes maintenance easier, when there is only one
        place where a possible broken function is. A constant process to
        remove old code from libraries is also in progress when new
        functions are found from standard Emacs distribution.

9.0 Appendix A - Win32 and Unix beginners

    9.1 Prompt syntaxes

        If you're just starting to use Windows or have never heard of Unix,
        here is a short course of the terms and lingo you see all the time
        in the documentation:

            %       MEANS: the command prompt where your cursor is
            $       MEANS: the command prompt as above, just different shell
            >       MEANS: the command prompt as above, again some other shell

    9.2 Dos and Unix command equivalences

        The shell (DOS in Win32) where you type the command does not usually
        matter, but the command you see may not be available in Win32. A crash
        course would be:

            Unix    Windows
            -----   ------------------------------------------------------
            cp      MEANS "copy" command
            mv      MEANS move, which is combination of "copy" and "del"
            ls      MEANS list, which is "dir"
            rm      MEANS remove, which is "del"
            ln      MEANS link, Sorry, you're out of luck in Win32.
                    --> do a complete tree or file "copy" to destination.
            chmod   MEANS change modes, ignore these in Win32
            mkdir   MEANS make directory, "md"
            --------------------------------------------------------------

        Here is an example of one instruction, similar to what you can expect.
        The explanation has been "opened" for you to the right hand in
        double quotes.

            % cp file1.txt file2.txt    "Copy file1.txt to file2.txt"

    9.3 Calling perl programs

        In Win32, the perl file (.pl) must be called differently that in Unix.
        In Unix you must make the files executable first, then just type their
        names to run them.

            % chmod +x bin/*        "Make all files in bin/* executable"
            % cd /bin
            % makefile.pl --help     "Run the perl script t2html.pl in bin/"

        In Windows, there is no such command as `chmod' and Windows does
        not know that perl scripts are runnable programs. You must prepend
        the perl interpreter in front of every call an possibly add `-S' to
        instruct to search the script along your PATH variable. The
        following calls would also work in any environment:


            dos> perl bin/t2html.pl --help     (Notice "perl" at front)
            dos> perl -S t2html.pl --help      (If you put the script along PATH)

    9.4 Changing environment settings

        The environment variables in Unix are prefixed with dollar and
        written in all caps, like talking about $PATH. Read it like this:

            % echo $PATH         (in Unix, display variable's content)
            dos> echo %PATH%     (same in Win32 dos prompt)

        If you're instructed to change any environment variables, you must
        open the `c:\autoexec.bat' in Win9x with some text editor and
        modify the contents. In WinNT/2000 you must edit the start =>
        Control Panel => System => Environment. Windows Millennium uses that
        too. Reboot after changes to make windows star using the new settings.
        WinNT/2000 is smarter, press "Apply" button in "Environment" dialog
        and you're all set to proceed.

7.0 Appendix B - Procmail project files

        Procmail development has been moved to and
        http://www.sourceforge.net/projects/ under "pm-lib" and "pm-doc".

8.0 Appendix C - Perl project files

        Perl development has been moved to CPAN
        http://cpan.perl.org/modules/by-authors/id/J/JA/JARIAALTO/
        and  http://www.sourceforge.net/projects/ under
        "perl-text2html", "perl-dyndns" and "perl-mywebget".

10.0 Appendix D - Copyright information

    10.1 Program files

        The files you see in the kit are more or less related to Emacs and
        XEmacs and they are all released under GNU general public license,
        meaning that you're free to use them as you see fit provided that you
        respect terms of GPL. Refer to http://www.gnu.org/copyleft/gpl.html to
        learn more about the GPL license.

    10.2 Text files and documentation

        All the documentation, in code, in text files or in other supplementary
        commentary files are Copyrighted. The material may be distributed only
        subject to the terms and conditions set forth in the Open Publication
        License, v1.0 or later (the latest version is presently available at
        <http://www.opencontent.org/>). Distribution of the work or
        derivative of the work in any standard (paper) book form is prohibited
        unless prior permission is obtained from the copyright holder (VI.
        LICENSE OPTIONS B).

        The clause VI.B is included to prohibit making commercial use, (in
        electronic, web based or in paper format) of the documentation without
        author's knowledge. Any free (defined here as "free of any charges" and
        "free of any access restrictions") use of the documentation is hereby
        granted, without the restrictive VI.B without any prior consultation
        with the Copyright holder.

End of file
