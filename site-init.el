;;; $Id: site-init.el,v 1.37 2008-03-05 23:32:27 hute37 Exp $

;;;
;;; $Revision: 1.37 $
;;;
;;; $Log: site-init.el,v $
;;; Revision 1.37  2008-03-05 23:32:27  hute37
;;; ubuntu source build
;;;
;;; Revision 1.36  2007-12-04 23:03:42  hute37
;;; xft, built from source
;;;
;;; Revision 1.35  2006-10-13 20:10:00  hute37
;;; dot
;;;
;;; Revision 1.34  2006/10/13 20:03:07  hute37
;;; Graphviz dot-mode
;;;
;;; Revision 1.33  2006/10/04 01:15:29  hute37
;;; toggle menubar, toolbar
;;;
;;; Revision 1.32  2006/10/03 23:33:14  hute37
;;; XEmacs fonts
;;;
;;; Revision 1.31  2006/09/30 00:44:19  hute37
;;; XEmacs/Lucid
;;;
;;; Revision 1.30  2006/01/13 00:34:59  hute37
;;; nxml nt support
;;;
;;; Revision 1.29  2006/01/12 03:47:01  hute37
;;; added J.Clark nxml-mode (incompatible with Mule-UCS)
;;;
;;; Revision 1.28  2006/01/09 22:38:32  hute37
;;; hs load
;;;
;;; Revision 1.27  2005/12/22 03:11:32  hute37
;;; hs ...
;;;
;;; Revision 1.26  2005/12/22 03:10:53  hute37
;;; hs
;;;
;;; Revision 1.25  2005/12/22 02:30:42  hute37
;;; view-mode
;;;
;;; Revision 1.24  2005/12/22 00:56:23  hute37
;;; icycles mode disabled
;;;
;;; Revision 1.23  2005/12/16 01:27:38  hute37
;;; move icicles at end
;;;
;;; Revision 1.22  2005/12/10 01:10:42  hute37
;;; *** empty log message ***
;;;
;;; Revision 1.21  2005/12/06 05:19:08  hute37
;;; *** empty log message ***
;;;
;;; Revision 1.20  2005/12/05 03:06:47  hute37
;;; mode-compile
;;;
;;; Revision 1.19  2005/12/04 23:31:49  hute37
;;; tinyperl
;;;
;;; Revision 1.18  2005/12/03 01:13:39  hute37
;;; us-keys
;;;
;;; Revision 1.17  2005/12/03 00:23:54  hute37
;;; merged with ascii
;;;
;;; Revision 1.16  2005/12/03 00:12:05  hute37
;;; mouseme !supported on ntemacs
;;;
;;; Revision 1.15  2005/11/29 03:42:23  hute37
;;; kp-keys
;;;
;;; Revision 1.14  2005/11/29 00:59:18  hute37
;;; dabbrev
;;;
;;; Revision 1.13  2005/11/28 01:11:28  hute37
;;; perldoc
;;;
;;; Revision 1.12  2005/11/28 00:46:41  hute37
;;; vcs commands
;;;
;;; Revision 1.11  2005/11/28 00:41:59  hute37
;;; test
;;;
;;; Revision 1.10  2005/11/28 00:39:43  hute37
;;; reorder fn keys
;;;
;;; Revision 1.9  2005/11/27 19:23:29  hute37
;;; *refactored*
;;;
;;; Revision 1.8  2005/11/26 02:20:37  hute37
;;; removed xemacs support
;;;
;;; Revision 1.7  2005/11/26 02:11:32  hute37
;;; clean
;;;
;;; Revision 1.6  2005/11/26 02:00:27  hute37
;;; *** empty log message ***
;;;
;;; Revision 1.5  2005/11/26 01:58:25  hute37
;;; vc-mode-line ?removed
;;;
;;; Revision 1.4  2005/11/26 01:54:37  hute37
;;; (mode-line) removed
;;;
;;; Revision 1.3  2005/11/26 01:43:39  hute37
;;; vc-mode-line
;;;
;;; Revision 1.2  2005/11/26 01:30:43  hute37
;;; add kkv
;;;
;;;
;;;

;;;////////////////////////////////////////////////////////////////
;;;  #ENTRY
;;;////////////////////////////////////////////////////////////////
(message "SITE:#ENTRY#")

;;;////////////////////////////////////////////////////////////////
;;;  @FORK
;;;////////////////////////////////////////////////////////////////

;; ---( emacs )-----------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn
    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn
    ))
 (t
  (progn
    (message (concat (format "%s" z-emacs-type) "-unknown"))
    ))
 )


;;;////////////////////////////////////////////////////////////////
;;;  @AUTOLOAD
;;;////////////////////////////////////////////////////////////////
(message "SITE:AUTOLOAD")

;; ---( server )-----------------------------------------------------

(cond
 ((eq system-type 'windows-nt) ;; WinNT
  (progn
;; *unsupported*
;;    (require 'gnuserv)
;;    (gnuserv-start)
;;    (setq gnuserv-frame (selected-frame))
    ))
 ((eq system-type 'cygwin);; GNU-Cygwin
  (progn
   ;; (server-start)
    ))
 ((eq system-type 'gnu/linux);; GNU-Linux
  (progn
    (server-start)))
 ((eq system-type 'usg-unix-v);; Sun Solaris
  (progn
    (server-start)))
 (t
  (progn
    ))
 )


;; ;; ---( tiny )---------------------------------------------------------

;; (cond (nil(progn 
;; ;;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;; (cond
;;  ((eq z-emacs-type 'xemacs) ;; XEmacs
;;   (progn
;;     ))
;;  ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
;;   (progn


;; (setq tiny-home (concat emacs-site-path "addon/tiny/" ))

;; (add-to-list 'load-path (concat tiny-home "lisp/tiny" ) t)
;; (add-to-list 'load-path (concat tiny-home "lisp/other" ) t)

;; ;; (setq load-path 
;; ;;       (append load-path 
;; ;; 	      (list 
;; ;; 	       (concat tiny-home "lisp/tiny" )
;; ;; 	       (concat tiny-home "lisp/other" ))
;; ;; 	      ))


;; (load "tinypath" )

;; ;; tiny eager load
;; ;;(require 'tiny-setup)
;; ;;(tiny-setup nil)

;; ;; tiny lazy load
;; (require 'tiny-autoload-loaddefs-tiny)
;; (require 'tiny-autoload-loaddefs-other)



;;     ))
;;  )


;; ;;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;; )))



;; ---( Package Repositories )---------------------------------------------------------
(message "SITE:PACKAGES.setup")


(cond
 ((string-lessp emacs-version "24.0") ;; 
  (progn
	(message "SITE:PACKAGES.skip")
    ))
 (t
  (progn
	(message "SITE:PACKAGES.repo")

(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
;; You don't need this one if you have marmalade:
;; (add-to-list 'package-archives
;;  '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages"))
(package-initialize)

    ))
)




;; ---( required )---------------------------------------------------------
(message "SITE:AUTOLOAD.core")

(require 'bs)

(require 'ffap)
(require 'imenu)
(require 'gud)


;;(require 'lcomp) ;;to prevent M-c remap

;; ---( EmacsWiki )---------------------------------------------------------
(message "SITE:AUTOLOAD.emacswiki")

;;(require 'ring+)
(eval-after-load "ring" '(progn (require 'ring+)))

(require 'doremi)
;;:(require 'doremi-frm)
(require 'doremi-cmd)
(require 'frame-cmds)
(require 'zoom-frm)


(message "SITE:AUTOLOAD.misc")
;; ---( all )---------------------------------------------------------

(load "all")                            ;; xedit all

;; ---( mode-compile )-----------------------------------------------------

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

;; ---( cygwin )-----------------------------------------------------

(cond
 ((eq system-type 'windows-nt) ;; WinNT
  (progn
    ))
 ((eq system-type 'cygwin);; GNU-Cygwin
  (progn
(require 'cygwin-mount)
 (cygwin-mount-activate)

    ))
 ((eq system-type 'gnu/linux);; GNU-Linux
  (progn
    ))
 ((eq system-type 'usg-unix-v);; Sun Solaris
  (progn
    ))
 (t
  (progn
    ))
 )

;;;////////////////////////////////////////////////////////////////
;;;  @THEMES
;;;////////////////////////////////////////////////////////////////
(message "SITE:THEMES")

;; ---( Custom Fonts )----------------------------------------------------

(require 'ergo-font)


;; ---( Color )-----------------------------------------------------------

(require 'color-theme)
(require 'color-theme-zcoll)
(require 'color-theme-solarized)
(setq color-theme-is-global nil)




;;;////////////////////////////////////////////////////////////////
;;;  @VC
;;;////////////////////////////////////////////////////////////////
(message "SITE:VC")

;; ---( Git )----------------------------------------------------

;; http://www.emacswiki.org/emacs/Git
;; http://www.michael-hammer.at/blog/emacs_git/
;; http://alexott.net/en/writings/emacs-vcs/EmacsGit.html

;; http://help.github.com/linux-set-up-git/
;; http://help.github.com/fork-a-repo/


;; sudo apt-get install magit

;;@TODO: move to site-start.d local directory ...

;;; Autoloads for magit

(autoload 'magit-status "magit" nil t)


;;;////////////////////////////////////////////////////////////////
;;;  @CUA
;;;////////////////////////////////////////////////////////////////
(message "SITE:CUA")

;; ---( PC Select )----------------------------------------------------


(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

(require 'pc-select)
(pc-select-mode t)

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;; pc-select
;;(require 'pc-select)
;;(pc-select-selection-keys-only t)
;;(pc-selection-mode t nil '(pc-select))
;;(setq pc-select-selection-keys-only t)
;;(pc-selection-mode)

 (if (fboundp 'pc-selection-mode)                                              
      (pc-selection-mode)                                                   
      (require 'pc-select)) ;; @todo: obsolete

 (custom-set-variables
  '(pc-selection-mode t nil (pc-select)))


    ))
 (t
  (progn
    ))
 )


;; ---( CUA )---------------------------------------------------------

(cond
 ((string-lessp emacs-version "22.0") ;; 
  (progn

    (require 'cua)
    (CUA-mode t)
    (setq CUA-mode-normal-cursor-color "red")
    (setq CUA-mode-overwrite-cursor-color "yellow")
    (setq CUA-mode-read-only-cursor-color "green")

    ;; disable in .emacs
    ;; '(cua-mode t nil (cua-base))

    ))
 (t
  (progn
    ))
)





;;;////////////////////////////////////////////////////////////////
;;;  @GLOBALS
;;;////////////////////////////////////////////////////////////////
(message "SITE:GLOBALS")

;; ---( Customize Settings )----------------------------------------------------

;; visible bell
(setq visible-bell t)

;; init message
(setq initial-scratch-message nil)


;; ---( Global Settings )----------------------------------------------------

;; Enable uppercase or lowercase conversions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Disable narrowing, whatever the hell it is.
(put 'narrow-to-region 'disabled nil)

;; Scrolling
;;(setq scroll-step 1)

;;(setq truncate-lines t)


;; ---( AutoRevert )---------------------------------------------------------

;;(autoload 'auto-revert-mode "autorevert" nil t)
;;(autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
;;(autoload 'global-auto-revert-mode "autorevert" nil t)

;; (global-auto-revert-mode 1) ;--- enable autorevert -------------

;;(add-hook 'java-mode-hook 'turn-on-auto-revert-mode)
;;(setq auto-revert-interval 5) ;--- check interval: 5 sec. -------------


;; ---( Grep )--------------------------------------------------------

;; Ignore case by default:
;;(setq igrep-options "-i")
;;(setq grep-command "egrep -n")

;; To search subdirectories by default:
;;(setq igrep-find t)

;;(setq ediff-ignore-similar-regions t)
;;(setq ediff-use-last-dir t)
;;(setq ediff-diff-options " -b ")

;; ---( i8n )----------------------------------------------------

;;;_ + mule
;;(set-terminal-coding-system 'iso-latin-1)

;;;////////////////////////////////////////////////////////////////
;;;  @CUSTOM PROPERTIES
;;;////////////////////////////////////////////////////////////////

;; ---( Misc )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn
    (customize-set-variable 'font-menu-this-frame-only-p t)
    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn
;;
    ))
 (t
  (progn
    ))
 )




;;;////////////////////////////////////////////////////////////////
;;;  @FEATURES
;;;////////////////////////////////////////////////////////////////
(message "SITE:FEATURES")


;; ---( Session )--------------------------------------------------------

;;@TODO: verify session setup ...
;;(require 'session)
;;(session-initialize)


;; ---( Backup )--------------------------------------------------------

(setq backup-by-copying t)
(require 'backups)
(move-backups t)

(setq backup-directory "~/.backups")

(or (directory-files backup-directory)
	  (make-directory backup-directory))

(require 'backup-dir)
(setq bkup-backup-directory-info
		`(
       (t ,(concat backup-directory "/") full-path prepend-name search-upward)
;      (t ,(concat backup-directory "/"))
		  ))
(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)


;; ---( HideShow )--------------------------------------------------------

(require 'hideshow)
;; (add-hook 'X-mode-hook               ; other modes similarly
;;           '(lambda () (hs-minor-mode 1)))



;;;////////////////////////////////////////////////////////////////
;;;  @FUNCTIONS
;;;////////////////////////////////////////////////////////////////
(message "SITE:FUNCIONS")

;; ---( util )---------------------------------------------------------

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))


;;;////////////////////////////////////////////////////////////////
;;;  @MACRO
;;;////////////////////////////////////////////////////////////////
(message "SITE:MACRO")


;; ---( input macros )---------------------------------------------------------

(defun scroll-up-one ( )
  "up-one."
  (interactive)
  (scroll-up 1))
(defun scroll-down-one ( )
  "down-one."
  (interactive)
  (scroll-down 1))


;; ---( grep )---------------------------------------------------------


(defvar grep-all-files-history nil)

(defvar grep-all-files-omitted-expressions
  '("*~" "#*" ".#*" ",*" "*.elc" "*.obj" "*.o" "*.exe" "*.dll" "*.lib" "*.a"
    "*.dvi" "*.class" "*.bin")
  "List of expressions matching files to be omitted in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defvar grep-all-files-omitted-directories '("CVS" "RCS" "SCCS")
  "List of directories not to recurse into in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defun construct-grep-all-files-command (find-segment grep-segment)
  (let ((omit-annoying
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -or "))
		    grep-all-files-omitted-expressions
		    "")))
    (cond ((eq grep-find-use-xargs 'gnu)
	   (format "find . %s %s -type f -print0 | xargs -0 -e %s"
		   find-segment omit-annoying grep-segment))
	  (grep-find-use-xargs
	   (format "find . %s %s -type f -print | xargs %s"
		   find-segment omit-annoying grep-segment))
	  (t
	   (format "find . %s %s -type f -exec %s {} /dev/null \\;"
		   find-segment omit-annoying grep-segment)))))

(defun grep-all-files-in-current-directory (command)
  "Run `grep' in all non-annoying files in the current directory.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function does not recurse into subdirectories.  If you want this,
use \\[grep-all-files-in-current-directory-and-below]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (require 'compile)
  (grep (construct-grep-all-files-command
	 "-name . -or -type d -prune -or" command)))

(defun grep-all-files-in-current-directory-and-below (command)
  "Run `grep' in all non-annoying files in the current directory and below.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function recurses into subdirectories.  If you do not want this,
use \\[grep-all-files-in-current-directory]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (require 'compile)
  (grep (construct-grep-all-files-command
	 ;; prune all specified directories.
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -prune -or "))
		    grep-all-files-omitted-directories
		    "")
	 command)))


;; ---( select )-------------------------------------------------------


(defun clear-select ()
  "Repeatedly select ever larger balanced expressions around the cursor.
Once you have such an expression marked, you can expand to the end of
the following expression with \\[mark-sexp] and to the beginning of the
previous with \\[backward-sexp]."
  (interactive "_") ;this means "preserve the active region after this command"
  (backward-up-list 1)
  (let ((end (save-excursion (forward-sexp) (point))))
    (push-mark end nil t)))




;; ---( wrap )---------------------------------------------------------

;; Toggles between line wrapping in the current buffer.
(defun toggle-line-wrapping ()
  "Toggles between line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil"))
  )

;; ---( kbd-macro )-----------------------------------------------------

(defun start-or-end-kbd-macro ()
  ;; A doc string.  This is optional.
  "Start defining a keyboard macro, or stop if we're already defining."
  ;; IMPORTANT: Any function bound to a key MUST have an interactive spec,
  ;; usually just the following line:
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))


;; ---( ascii )---------------------------------------------------------

(defun self-insert-backquote ( )
  "insert backquote `."
  (interactive)
  (insert-char ?` 1))

(defun self-insert-tilde ( )
  "insert tilde ~."
  (interactive)
  (insert-char ?~ 1))

;; ---( format )---------------------------------------------------------

;; Untabifies entire buffer.
(defun untabify-buffer ()
  "Untabifies entire buffer."
  (interactive)
  (point-to-register 1)
  (goto-char (point-min))
  (untabify (point-min) (point-max))
  (register-to-point 1)
  )

;; Tabifies entire buffer.
(defun tabify-buffer ()
  "Tabifies entire buffer."
  (interactive)
  (point-to-register 1)
  (goto-char (point-min))
  (tabify (point-min) (point-max))
  (register-to-point 1)
  )

(defun remove-trailing-ctl-M ()
  "Propose to remove trailing ^M from a file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (not (string-match ".gz$" (buffer-file-name)))
             (search-forward-regexp "
$" nil t))
                                        ;: a ^M is found
        (if (or (= (preceding-char) ?\^J)
                (= (following-char) ?\^J) )
            (if (y-or-n-p (format "Remove trailing ^M from %s? "
                                  (buffer-file-name)))
                (progn (goto-char (point-min))
                       (perform-replace "
" "" nil nil nil)
                       (pop-mark)
                       (save-buffer))
              (message "No transformation."))))))



;;;////////////////////////////////////////////////////////////////
;;;  @UI
;;;////////////////////////////////////////////////////////////////
(message "SITE:UI")

;; ---( Menu )-----------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    (defvar init-menubar current-menubar)
    (set-menubar nil)

    (defun toggle-menubar ()
      (interactive)
      (if (eq current-menubar nil)
	  (progn
	    (set-menubar init-menubar)
	    (redraw-display)
	    (message "Menu ON"))
	(progn 
	  (set-menubar nil)
	  (redraw-display)
	  (message "Menu OFF"))))


    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

    (menu-bar-mode 0)

    (defun toggle-menubar ()
      (interactive)
      (menu-bar-mode nil))
    
    ))
 (t
  (progn
    (defun toggle-menubar ()
      (interactive)
      (error "toggle-menubar unsupported ..."))
    ))
 )


;; ;; ---( Gutter )-----------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    (customize-set-variable 'gutter-buffers-tab-visible-p nil)

    (defun toggle-gutter ()
      (interactive)
      (customize-set-variable 'gutter-buffers-tab-visible-p 
			      (not gutter-buffers-tab-visible-p)))

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

    (defun toggle-gutter ()
      (interactive)
      (error "toggle-gutter unsupported ..."))
    
    ))
 (t
  (progn
    (defun toggle-gutter ()
      (interactive)
      (error "toggle-gutter unsupported ..."))
    ))
 )




;; ---( Toolbar )--------------------------------------------------------


(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    (customize-set-variable 'toolbar-visible-p nil)

    (defun toggle-toolbar ()
      (interactive)
      (customize-set-variable 'toolbar-visible-p 
			      (not toolbar-visible-p)))

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

    (tool-bar-mode 0)

    (defun toggle-toolbar ()
      (interactive)
      (tool-bar-mode))

    ))
 (t
  (progn
    (defun toggle-toolbar ()
      (interactive)
      (error "toggle-toolbar unsupported ..."))
    ))
 )


;; ---( Scrollbar )--------------------------------------------------------


(cond
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

    (scroll-bar-mode nil)

    (defun toggle-scroolbar ()
      (interactive)
      (scrool-bar-mode))

    ))
 (t
  (progn
    (defun toggle-toolbar ()
      (interactive)
      (error "toggle-toolbar unsupported ..."))
    ))
 )


;; ---( Modeline )-------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

 (customize-set-variable 'modeline-scrolling-method t)
 (customize-set-variable 'line-number-mode t)
 (customize-set-variable 'column-number-mode t)


    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;; no custom

    ))
 (t
  (progn
    ))
 )

;;;////////////////////////////////////////////////////////////////
;;;  @FRAME L&F
;;;////////////////////////////////////////////////////////////////
(message "SITE:FRAME")

;; ---( Look )---------------------------------------------------------------


;; ---( Feel )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

 (customize-set-variable 'auto-raise-frame t)

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;; no custom

    ))
 (t
  (progn
    ))
 )


;;;////////////////////////////////////////////////////////////////
;;;  @MOUSE INPUT MAPPINGS
;;;////////////////////////////////////////////////////////////////
(message "SITE:MOUSE")

;; ---( Wheel )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn


    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

(mouse-wheel-mode t)                      ;; enable wheel

(global-set-key [C-mouse-4] 'text-scale-decrease)
(global-set-key [C-mouse-5] 'text-scale-increase)
(global-set-key [M-mouse-4] 'bs-cycle-next)
(global-set-key [M-mouse-5] 'bs-cycle-previous)


    ))
 (t
  (progn
    ))
 )



;; ---( Meta )---------------------------------------------------------------

;; Paste at point NOT at cursor
;(setq mouse-yank-at-point 't)

;; ---( Mapping )---------------------------------------------------------------


(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

;;TODO: toolbar off

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;;(require 'mouseme)
(define-key global-map [M-S-down-mouse-3] 'imenu)
;;(define-key global-map [C-S-down-mouse-3] 'mouse-me)

    ))
 (t
  (progn
    ))
 )


;;;////////////////////////////////////////////////////////////////
;;;  @KEYBOARD INPUT MAPPINGS
;;;////////////////////////////////////////////////////////////////
(message "SITE:KEYBOARD")

;; ============================================
;; ---( Edit Keys )-----
;; ============================================

;; ---( Arrows )----------------------------------------------------

(global-set-key [(control up)] 'scroll-down-one )
(global-set-key [(control down)] 'scroll-up-one )
;; (global-set-key [(meta up)] 'backward-block-of-lines )
;; (global-set-key [(meta down)] 'forward-block-of-lines )

(global-set-key [(control right)] 'forward-word )
(global-set-key [(control left)] 'backward-word )

;; (global-set-key [(control meta right)] 'backward-sexp )
;; (global-set-key [(control meta left)] 'forward-sexp )


;; (global-set-key [(shift meta left)] 'backward-sexp-nomark )
;; (global-set-key [(shift meta right)] 'forward-sexp-nomark )
(global-set-key [(shift meta up)] 'backward-sexp-mark )
(global-set-key [(shift meta down)] 'forward-sexp-mark )
(global-set-key [(shift meta left)] '(lambda () (interactive) (other-frame -1)))
(global-set-key [(shift meta right)] '(lambda () (interactive) (other-frame +1)))
;; (global-set-key [(meta control up)] '(lambda () (interactive) (other-frame -1)))
;; (global-set-key [(meta control down)] '(lambda () (interactive) (other-frame +1)))
;; (global-set-key [(meta control right)] 'next-multiframe-window )
;; (global-set-key [(meta control left)] 'previous-multiframe-window )

(global-set-key [(meta up)] 'dired )
(global-set-key [(meta down)] 'bs-show )
(global-set-key [(meta right)] 'bs-cycle-next )
(global-set-key [(meta left)] 'bs-cycle-previous )

;; ---( Page )-------------------------------------------------------

(global-set-key [(control prior)] 'other-frame )
(global-set-key [(control next)] 'other-window )

;; (global-set-key [(meta control prior)] 'outline-previous-visible-heading )
;; (global-set-key [(meta control next)] 'outline-next-visible-heading )

;; ---( Home/End )---------------------------------------------

;;(global-set-key [home] 'beginning-of-line)
;;(global-set-key [end] 'end-of-line)

;;  (define-key global-map [(shift end)]           'end-of-line-mark)
;;  (define-key global-map [end]                   'end-of-line-nomark)
;;  (global-set-key [(shift end)]           'end-of-line-mark)
;;  (global-set-key [end]                   'end-of-line-nomark)
;;  (global-set-key [(shift control end)]          'end-of-buffer-mark)
;;  (global-set-key [(control end)]                'end-of-buffer-nomark)
;;  (global-set-key [(shift meta end)]             'end-of-buffer-mark)
;;  (global-set-key [(meta end)]                   'end-of-buffer-nomark)


;;  (define-key global-map [(shift home)]          'beginning-of-line-mark)
;;  (define-key global-map [home]                  'beginning-of-line-nomark)
;;  (global-set-key [(shift home)]          'beginning-of-line-mark)
;;  (global-set-key [home]                  'beginning-of-line-nomark)
;;  (global-set-key [(shift control home)]         'beginning-of-buffer-mark)
;;  (global-set-key [(control home)]               'beginning-of-buffer-nomark)
;;  (global-set-key [(shift meta home)]            'beginning-of-buffer-mark)
;;  (global-set-key [(meta home)]                  'beginning-of-buffer-nomark)


(global-set-key [(meta home)] 'delete-other-windows )
(global-set-key [(meta end)] 'kill-this-buffer )

;; ---( BackSpace )---------------------------------------------

(global-set-key [backspace] 'backward-delete-char )
(global-set-key [(control backspace)] 'backward-kill-word )
(global-set-key [(meta backspace)] 'undo )

;; ---( Tab )---------------------------------------------

(global-set-key [(control shift tab)]   'bs-cycle-previous)
(global-set-key [(control tab)]  'bs-cycle-next)


;; ---( Return )---------------------------------------------

;;(global-set-key [(control return)] 'cua-rect )
(global-set-key [(meta return)] 'ffap)

;; ---( Space )---------------------------------------------


;; (global-set-key [(control meta space)]
;;   '(lambda () (interactive)
;; 	  (set-mark-command nil)
;; 	  (zmacs-deactivate-region)
;; 	  ))

;;(global-set-key [(control space)] 'hippie-expand )
;;(global-set-key [(meta space)] 'imenu)
;;(define-key global-map [M-S-down-mouse-3] 'imenu))



;;;////////////////////////////////////////////////////////////////
;;;  @COMPLETION
;;;////////////////////////////////////////////////////////////////
(message "SITE:COMPLETION")

;; ---( hippie-expand )------------------------------------------
;; hippie-expand  (auto skriv resten af ord jeg har skrevet før)

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

;;TODO: completion

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;; C-\ defaults to toggle-input-method
(define-key esc-map "\\" 'dabbrev-completion)
(define-key global-map "\C-\\" 'dabbrev-expand)
;; Many people are used to typing C-SPC and getting C-@.
;;(define-key global-map [?\C- ] 'dabbrev-expand)
;;(global-set-key "\M- " 'hippie-expand) ; der står meta space!
(define-key esc-map "\C-M" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))



    ))
 (t
  (progn
    ))
 )





;;;////////////////////////////////////////////////////////////////
;;;  @MARK
;;;////////////////////////////////////////////////////////////////
(message "SITE:MARK")

;; ---( basic clipboard )------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

;;TODO: completion

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

;; (define-key global-map "\C-@" 'set-mark-command)
;; (define-key global-map "\C-@" 'cua-set-mark)
(define-key esc-map " " 'pop-global-mark)

;; ctrl-space set-mark-command
;; ctrl-y yank
;; ctrl-w cut
;; meta-w copy


    ))
 (t
  (progn
    ))
 )




;;;////////////////////////////////////////////////////////////////
;;;  @WMKEYS
;;;////////////////////////////////////////////////////////////////
(message "SITE:K-WMKEYS")

;; ---( Arrows )--------------------------------------------------

(global-set-key [(meta control up)] 'raise-frame)
(global-set-key [(meta control down)] 'lower-frame)
(global-set-key [(meta control right)] '(lambda () (interactive) (other-frame -1)))
(global-set-key [(meta control left)] '(lambda () (interactive) (other-frame +1)))

;; ---( Page )----------------------------------------------------

;; (global-set-key [(meta control prior)] 'outline-previous-visible-heading )
;; (global-set-key [(meta control next)] 'outline-next-visible-heading )

;; ---( Home/End )------------------------------------------------

(global-set-key [(meta control home)] 'delete-other-windows )
(global-set-key [(meta control end)] 'delete-window )

;; ---( Tab )-----------------------------------------------------

(global-set-key [(meta control shift tab)]  'previous-multiframe-window)
(global-set-key [(meta control tab)]  'next-multiframe-window)


;; ---( Return )---------------------------------------------------

(global-set-key [(meta control return)] 'ffap)


;; ---( Meta-Control Keys )----------------------------------------

(define-key esc-map "\C-z" 'iconify-or-deiconify-frame)
(define-key esc-map "\C-n" 'ergo-font-small-frame)







;; ============================================
;; ---( Function Keys )-----
;; ============================================
(message "SITE:K-FUNKEYS")


;; ---( F1: Help )---------------------------------------------------------


(global-set-key [f1] 'help )
(global-set-key [(shift f1)] 'woman )
(global-set-key [(control f1)] 'find-function )
(global-set-key [(meta f1)]	'function-key-error)
(global-set-key [(shift meta f1)] 'function-key-error)


;; ---( F2: Bookmarks/Breakpoints )-------------------------------------

(global-set-key [f2] ' bookmark-bmenu-list)
(global-set-key [(shift f2)] 'bookmark-set )

(global-set-key [(control f2)]
    '(lambda () (interactive)
       (if (eq hs-minor-mode nil)
	   (progn
	     (hs-minor-mode t)
	     (hs-hide-all))
	 (hs-minor-mode nil))))

(global-set-key [(meta f2)]	'hs-toggle-hiding)
(global-set-key [(shift meta f2)] 'toggle-line-wrapping )


;; ---( F3: ISearch/Find )----------------------------------------------------

(global-set-key [f3] 'isearch-repeat-forward )
(global-set-key [(shift f3)] 'isearch-repeat-backward )
(global-set-key [(control f3)] 'view-mode ) 
(global-set-key [(meta f3)] 'occur )
(global-set-key [(shift meta f3)] 'function-key-error)

;; ---( F4: Macro )----------------------------------------------------

(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [(shift f4)] 'start-or-end-kbd-macro )
(global-set-key [(control f4)] 'edit-last-kbd-macro )
(global-set-key [(meta f4)] 'kbd-macro-query )
(global-set-key [(shift meta f4)] 'edit-last-kbd-macro )
;; (global-set-key [(control f4)] 'start-kbd-macro )
;; (global-set-key [(meta f4)] 'end-kbd-macro )

;; ---( F5: Search/Grep )----------------------------------------------------

(global-set-key [f5] 'isearch-forward-regexp )
(global-set-key [(shift f5)] 'isearch-backward-regexp )
(global-set-key [(control f5)] 'find-grep-dired )
(global-set-key [(meta f5)] 'grep  )
(global-set-key [(shift meta f5)] 'function-key-error)

;; ---( F6: Replace/Ediff )----------------------------------------------------

(global-set-key [f6] 'query-replace )
(global-set-key [(shift f6)] 'query-replace-regexp )
(global-set-key [(control f6)] 'compare-windows )
(global-set-key [(meta f6)] 'ediff )
(global-set-key [(shift meta f6)] 'function-key-error)

;; ---( F7: Debug/Step )----------------------------------------------------

(global-set-key [f7] 'gud-step ) ;;@TODO: move to local mode map
(global-set-key [(control f7)] 'function-key-error)
(global-set-key [(meta f7)] 'function-key-error)
(global-set-key [(shift f7)] 'function-key-error)
(global-set-key [(shift meta f7)] 'function-key-error)

;; ---( F8: Debug/Next )----------------------------------------------------

;;(global-set-key [f8] 'function-key-error ) ;;WM expose
(global-set-key [(control f8)] 'gud-next) ;;@TODO: move to local mode map
(global-set-key [(meta f8)] 'function-key-error)
(global-set-key [(shift f8)] 'function-key-error)
(global-set-key [(shift meta f8)] 'function-key-error)

;; ---( F9: compile/run )----------------------------------------------------

;;(global-set-key [f9] 'function-key-error ) ;;WM expose
;;(global-set-key [f9] 'perldb ) ;;@TODO: move to local mode map
(global-set-key [(shift f9)] 'mode-compile-kill )
(global-set-key [(control f9)] 'mode-compile )
;;(global-set-key [(meta f9)] 'recompile )
;;(global-set-key [(shift meta f9)] 'compile)

(global-set-key [(meta f9)] 'shell)
(global-set-key [(shift meta f9)] 'list-processes)


;; ---( F10: UI )----------------------------------------------------

;;(global-set-key [f10] 'menu )
;;(global-set-key [(control f10)] 'menu-bar-mode )
(global-set-key [(control f10)] 'toggle-menubar )
(global-set-key [(shift f10)] 'toggle-toolbar )
(global-set-key [(meta f10)] 'speedbar )
(global-set-key [(shift meta f10)] 'toggle-gutter)


;; ---( F11: VCS )----------------------------------------------------

(cond
 ((fboundp 'magit-status);; Git magit
  (progn
    (global-set-key [f11] 'magit-status )
    (global-set-key [(shift meta f11)] 'vc-next-action)
    (global-set-key [(meta f11)] 'vc-diff )
    ))
 (t ;; fallback to VC bindings
  (progn
    (global-set-key [f11] 'vc-next-action )
    (global-set-key [(shift f11)] 'vc-annotate )
    (global-set-key [(control f11)] 'vc-directory )
    (global-set-key [(meta f11)] 'vc-version-diff )
    (global-set-key [(shift meta f11)] 'function-key-error)
    ))
 )





;;(define-key speedbar-key-map [button1] 'dframe-click)


;; ---( F12: Frames )----------------------------------------------------

(global-set-key [f12] 'make-frame )
(global-set-key [(control f12)] 'delete-frame )
(global-set-key [(shift f12)] 'buffer-menu )
(global-set-key [(meta f12)] 'revert-buffer )
(global-set-key [(shift meta f12)] 'function-key-error)






;; ============================================
;; ---( Numeric Keypad )-----
;; ============================================
(message "SITE:K-NUMPAD")

;; ---( center )--------------------------------------------------

(define-key global-map [begin]	'recenter)
(define-key global-map [(control kp-begin)] '(lambda () (interactive) (recenter 0)))

;; ---( comment )----------------------------------------------------

(define-key global-map [(control kp-divide)]	'comment-region)
(define-key global-map [(meta kp-divide)]	'uncomment-region)

;; ---( rect )----------------------------------------------------

(define-key global-map [(control kp-add)] 'kill-rectangle)
(define-key global-map [(control kp-subtract)] 'delete-rectangle)
(define-key global-map [(control kp-enter)] 'yank-rectangle)
(global-set-key [kp-subtract]
  '(lambda () (interactive)
	  (copy-rectangle-to-register ?r (region-beginning) (region-end) t )))
(global-set-key [kp-add]
  '(lambda () (interactive)
	  (copy-rectangle-to-register ?r (region-beginning) (region-end))))
(global-unset-key [kp-enter])
(global-set-key [kp-enter]
  '(lambda () (interactive)
	  (insert-register ?r)
	  (let ((col (current-column)))
		 (forward-line)
		 (move-to-column col t))))
(global-set-key [(meta kp-enter)]
  '(lambda () (interactive)
	  (insert-register ?r)))

;; ---( edit )----------------------------------------------------

;; (define-key global-map [kp-insert]	'yank)
;; (define-key global-map [(control kp-insert)]	'kill-ring-save)
;; (define-key global-map [(meta kp-insert)]	'overwrite-mode)

;; (define-key global-map [kp-delete]	'delete-char)
;; (define-key global-map [(control kp-delete)]	'kill-region)
;; ;; (define-key global-map [(meta kp-delete)]	'delete-frame)

;; (define-key global-map [kp-end]	'bs-show)
;; (define-key global-map [kp-home]	'delete-other-windows)

;; (define-key global-map [(control kp-home)]	'dired)
;; (define-key global-map [(control kp-end)]	'kill-this-buffer)

;; (define-key global-map [(meta kp-home)]	'make-frame)
;; (define-key global-map [(meta kp-end)]	'delete-frame)

;; ---( scroll )----------------------------------------------------

;; (define-key global-map [kp-left]	'backward-word)
;; (define-key global-map [kp-right]	'forward-word)
;; (define-key global-map [(control kp-left)]	'scroll-left)
;; (define-key global-map [(control kp-right)]	'scroll-right)
;; (define-key global-map [kp-up]	'scroll-down-one)
;; (define-key global-map [kp-down]	'scroll-up-one)

;; (define-key global-map [kp-multiply]	'scroll-left)


;; ---( WM )----------------------------------------------------

;; (define-key global-map [(control kp-prior)]	'shrink-window)
;; (define-key global-map [(control kp-next)]
;;   '(lambda () (interactive) (shrink-window -1)))
;; (define-key global-map [(meta kp-prior)]
;;   '(lambda () (interactive) (set-frame-height (selected-frame) (- (frame-height) 1))))
;; (define-key global-map [(meta kp-next)]
;;   '(lambda () (interactive) (set-frame-height (selected-frame) (+ (frame-height) 1))))
;; (define-key global-map [(control meta kp-prior)]
;;   '(lambda () (interactive) (set-frame-width (selected-frame) (- (frame-width) 1))))
;; (define-key global-map [(control meta kp-next)]
;;   '(lambda () (interactive) (set-frame-width (selected-frame) (+ (frame-width) 1))))


;; (progn
;;   (define-key global-map [(control meta kp-up)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (frame-parameter (selected-frame) 'left)
;; 			   (- (frame-parameter (selected-frame) 'top) 5))))
;;   (define-key global-map [(control meta kp-down)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (frame-parameter (selected-frame) 'left)
;; 			   (+ (frame-parameter (selected-frame) 'top) 5))))
;;   (define-key global-map [(control meta kp-left)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (- (frame-parameter (selected-frame) 'left) 5)
;; 			   (frame-parameter (selected-frame) 'top))))
;;   (define-key global-map [(control meta kp-right)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (+ (frame-parameter (selected-frame) 'left) 5)
;; 			   (frame-parameter (selected-frame) 'top))))
;;   )



;; ============================================
;; ---( ASCII Keys )-----
;; ============================================
(message "SITE:K-ASCII")

;; ---( US-Keyboard )-----------------------------------------------------

(keyboard-translate 176 ?{ ) ;; -es
(keyboard-translate 167 ?} ) ;; par
(keyboard-translate 163 ?` ) ;; backquote
(keyboard-translate 231 ?~ ) ;; ced


;; ============================================
;; ---( Control Keys )-----
;; ============================================
(message "SITE:K-CTRL")

;; ---( Undo )------------------------------------------------------------

(define-key global-map "\C-z" 'undo)


;; ---( Compile )------------------------------------------------------------

(global-set-key "\C-cc" 'mode-compile)
(global-set-key "\C-c\C-c" 'mode-compile)
(global-set-key "\C-cq" 'mode-compile-kill)
(global-set-key "\C-b" 
    '(lambda () (interactive)
       (mode-compile)))

;; ---( Search )------------------------------------------------------------

(global-set-key "\C-f" 'occur)



;; ============================================
;; ---( ESC-Maps Keys )-----
;; ============================================
(message "SITE:K-ESCMAP")

;; ---( Numeric )--------------------------------------------------------

(define-key esc-map "1" 'color-theme-select )
(define-key esc-map "2" 'ergo-font-select )
(define-key esc-map "3" 'bury-buffer)
(define-key esc-map "4" 'delete-other-windows)
(define-key esc-map "5" 'other-frame )
(define-key esc-map "6" 'other-window )
(define-key esc-map "7" 'ergo-font-small-frame ) ;;mouse-set-font
(define-key esc-map "8" 'speedbar )
(define-key esc-map "9" 'describe-mode )
(define-key esc-map "0" 'delete-other-windows )


;; ---( Jump )------------------------------------------------------------

(define-key esc-map "n" 'goto-line )

;; ---( CUA Clipboard )---------------------------------------------------

(define-key esc-map "c" 'kill-ring-save )
(define-key esc-map "v" 'yank )
(define-key esc-map "z" 'kill-region )


;; ---( Misc )------------------------------------------------------------

(define-key esc-map "o" 'dired-other-frame )


;; ---( meta f )----------------------------------------------------------

(global-unset-key [(meta f)])
(define-prefix-command 'z-meta-f-prefix)
(defvar z-meta-f-map (symbol-function 'z-meta-f-prefix))
(define-key global-map [(meta f)] 'z-meta-f-prefix)

(define-key z-meta-f-map [s] 'save-buffer)
(define-key z-meta-f-map [(meta s)] 'save-buffer)
(define-key z-meta-f-map [v]
  '(lambda ()
     (interactive) (save-some-buffers t)) )
(define-key z-meta-f-map [(meta v)]
  '(lambda ()
     (interactive) (save-some-buffers t)) )


;; ============================================
;; ---( Mode-Map Keys )-----
;; ============================================

;; ---( bookmark )--------------------------------------------------------

(define-key bookmark-map [return] 'bookmark-bmenu-select)









;;;////////////////////////////////////////////////////////////////
;;;  @LAYOUT
;;;////////////////////////////////////////////////////////////////
(message "SITE:LAYOUT")

(cond
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn





(cond  ;;--- FSF-Emacs ---------------------------------------------------
 ((eq system-type 'windows-nt) ;; WinNT
  (cond
   (t
    (progn
      (setq default-frame-alist
	    '(
	      (top . 70)
	      ;; (left . 40)
	      (width . 85)
	      (height . 40)
	      (background-color . "#303010")
	      (background-mode . dark)
	      (foreground-color . "cornsilk")
	      (border-color . "black")
	      (cursor-color . "white")
	      (mouse-color . "black")
	      ;; (font . "-*-Andale Mono-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
	      ;; (font . "-*-Lucida Sans Typewriter-normal-r-*-*-11-*-*-*-c-*-iso8859-1")
	      (font . "-*-Lucida Sans Unicode-normal-r-*-*-16-*-*-*-c-*-iso8859-1")

	      )
	    )

      (setq color-theme-is-global t)
      (color-theme-z-gnome3)
      (setq color-theme-is-global nil)
      (color-theme-z-gnome3-d)            ;; default
      (setq initial-frame-alist
	    (append
	     '(
	       (top . 30)
	       (left . 355)
	       (width . 88)
	       (height . 46)
	       ;; (height . 49)
	       ;; (background-color . "#1A4F15" )
	       ;; (foreground-color . "#CDE7CD" )
	       ;; (cursor-color . "red")
	       ;; (border-color . "blue")
	       (background-color . "#203030")
	       ;; (foreground-color . "#00D0D0")
	       (foreground-color . "#CDE7CD" )
	       (cursor-color . "red")
	       (border-color . "white")
	       ;; (border-color . "#00007F")


	       ;; (font . "-*-Raize-normal-r-normal-normal-16-120-96-96-c-90-iso8859-15")
	       ;; (font . "-*-SimSun-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
	       ;; (font . "-*-r_ansi-normal-r-*-*-16-*-*-*-c-*-iso8859-1")
	       ;; (font . "-*-Need Glasses ?-normal-r-*-*-18-*-*-*-c-*-iso8859-1")
	       ;; (font . "-outline-VAG Rounded Light-normal-r-normal-normal-19-142-96-96-p-90-iso10646-1")
	       ;; (font . "-*-r_ansi-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
	       ;; (font . "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
	       ;; (font . "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-15-*-*-*-c-*-iso8859-1")
	       (font . "-bitstream-bitstream vera sans mono-normal-r-*-*-18-*-*-*-c-*-iso8859-1")

	       )
	     initial-frame-alist ))
      )
    )))
 ((eq system-type 'cygwin);; GNU-Cygwin
  (cond
   ((string= (getenv "DISPLAY") ":9.9")
    (progn

      )
    )
   (t
    (progn
      (setq default-frame-alist
	    '(
	      (top . 60)
	      (left . 40)
	      (width . 90)
	      (height . 50)
	      (background-color . "#002035")
	      (background-mode . dark)
	      (foreground-color . "cornsilk")
	      (border-color . "black")
	      (cursor-color . "white")
	      (mouse-color . "black")
	      (font . "-b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-9")
	      )
	    )


      (setq color-theme-is-global t)
      (color-theme-z-gnome3)            ;; default

      ;; #ifdef COLOR
      ;; *customization: -color
      ;; #endif
      ;;
      ;; !!emacs*Foreground: Wheat
      ;; !!emacs*Background: Gray10
      ;; !!emacs*Foreground: Wheat
      ;; !!emacs*Background: DarkSlateGray
      ;; !!emacs*Foreground: Ivory3
      ;; !!emacs*Background: MidnightBlue
      ;; !!emacs*Foreground: DarkSeaGreen3
      ;; !!emacs*Background: Gray13
      ;; !!emacs*Foreground: PowderBlue
      ;; !!emacs*Background: Gray10
      ;; !!emacs*reverseVideo: on
      ;; emacs*pointerColor: Orchid
      ;; emacs*cursorColor: Orchid
      ;; emacs*bitmapIcon: on
      ;; !!emacs*font: -misc-screen-medium-R-normal--14-120-78-78-C-90-ISO8859-7
      ;; !!emacs*font: -greek-smserif-medium-r-semicondensed-*-*-160-*-*-m-*-iso8859-7
      ;; !!emacs*font: 10x20
      ;; !!emacs*font: -Sun-Serif-Medium-R-Normal-Serif-16-160-72-72-M-90-ISO8859-1
      ;; !!emacs*font: -adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1
      ;; emacs*font: -B&H-LucidaTypewriter-Medium-R-Normal-Sans-12-120-75-75-M-70-ISO8859-1
      ;; emacs*menubar.font:-adobe-helvetica-bold-r-normal-*-*-120-*-*-*-*-*-*
      ;; emacs.geometry: 80x25

      (setq initial-frame-alist
	    '(
	      (top . 25)
	      (left . 310)
	      (width . 85)
	      (height . 52)
	      ( background-color . "#103045" )
	      ( foreground-color . "LightCyan1" )
	      ( cursor-color . "Yellow")
	      ( background-mode . dark)
	      (font . "-b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-9")
	      ))

      (setq color-theme-is-global nil)
      (color-theme-z-gnome3-d)            ;; default

      )
    )))
 ((eq system-type 'gnu/linux);; GNU-Linux
  (cond
   ((string= (getenv "DISPLAY") ":9.9")
    (progn

      )
    )
   (t
    (progn
      (setq font-default (if (string-lessp emacs-version "23.0")
;;			"-xos4-terminus-medium-r-*-*-15-*-*-*-*-*-*-1"
			"8x13"
			"monospace-10"))
      (setq default-frame-alist
	    (if (string-lessp emacs-version "23.0")
;;; 	      (top . 80)
;;; 	      (left . 30)
;;	      (background-color . "#002035")
;;	      (background-mode . dark)
;;	      (foreground-color . "cornsilk")
;;	      (border-color . "black")
;;	      (cursor-color . "white")
;;	      (mouse-color . "black")
;;	      (font . "-b&h-lucida-medium-r-*-*-12-*-*-*-*-*-*-*")
	      ;; (font . "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1")
;;	      (font . (if (string-lessp emacs-version "23.0")
;;			"-xos4-terminus-medium-r-*-*-15-*-*-*-*-*-*-1"
;;			"monospace-10"))
	    `(
	      (width . 85)
	      (height . 60)
	      (font . ,font-default)
	      )
	    `(
	      (width . 80)
	      (height . 45)
	      (font . ,font-default)
	      )
	    ))


      (setq color-theme-is-global t)
;;      (color-theme-jonadabian)            ;; default
      (color-theme-z-gnome2)            ;; default

      ;; #ifdef COLOR
      ;; *customization: -color
      ;; #endif
      ;;
      ;; !!emacs*Foreground: Wheat
      ;; !!emacs*Background: Gray10
      ;; !!emacs*Foreground: Wheat
      ;; !!emacs*Background: DarkSlateGray
      ;; !!emacs*Foreground: Ivory3
      ;; !!emacs*Background: MidnightBlue
      ;; !!emacs*Foreground: DarkSeaGreen3
      ;; !!emacs*Background: Gray13
      ;; !!emacs*Foreground: PowderBlue
      ;; !!emacs*Background: Gray10
      ;; !!emacs*reverseVideo: on
      ;; emacs*pointerColor: Orchid
      ;; emacs*cursorColor: Orchid
      ;; emacs*bitmapIcon: on
      ;; !!emacs*font: -misc-screen-medium-R-normal--14-120-78-78-C-90-ISO8859-7
      ;; !!emacs*font: -greek-smserif-medium-r-semicondensed-*-*-160-*-*-m-*-iso8859-7
      ;; !!emacs*font: 10x20
      ;; !!emacs*font: -Sun-Serif-Medium-R-Normal-Serif-16-160-72-72-M-90-ISO8859-1
      ;; !!emacs*font: -adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1
      ;; emacs*font: -B&H-LucidaTypewriter-Medium-R-Normal-Sans-12-120-75-75-M-70-ISO8859-1
      ;; emacs*menubar.font:-adobe-helvetica-bold-r-normal-*-*-120-*-*-*-*-*-*
      ;; emacs.geometry: 80x25

      (setq font-initial (if (string-lessp emacs-version "23.0")
;;			"-xos4-terminus-medium-r-*-*-17-*-*-*-*-*-*-1"
			"9x15"
			"10x20"
			"monospace-11"
			"Droid Sans Mono-11"
;;			"Inconsolata-12"
	))
     (setq initial-frame-alist
;;	      ( background-color . "#103045" )
;;	      ( background-color . "#1f3f3f" )
;;	      ( foreground-color . "LightCyan1" )
;;	      ( background-mode . dark)
;;	      (font . "-xos4-terminus-medium-r-*-*-17-*-*-*-*-*-*-1")
	      ;; (font . "-artwiz-fkp-*-*-*-*-17-*-*-*-*-*-*-*")
	      ;; (font . "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1")
;;	      (font . (if (string-lessp emacs-version "23.0")
;;			"-xos4-terminus-medium-r-*-*-17-*-*-*-*-*-*-1"
;;			"monospace-12"))
	   (if (string-lessp emacs-version "23.0")
	    `(
	      (top . 55)
	      (left . 350)
	      (width . 85)
	      (height . 60)
	      ( cursor-color . "Yellow")
	      (font . ,font-initial)
	      )
	    `(
	      (top . 55)
	      (left . 400)
	      (width . 90)
	      (height . 60)
	      ( cursor-color . "Yellow")
	      (font . ,font-initial)
	      )
	    ))

      (setq color-theme-is-global nil)
      (color-theme-z-gnome2)            ;; default
;;      (color-theme-z-gnome3-d)            ;; default
      )
    )))
 ((eq system-type 'usg-unix-v);; Sun Solaris
  (cond
   ((string= (getenv "DISPLAY") ":0.0")
    (progn
      )
    )
   (t
    (progn
      )
    )))
 )



    ))
 (t
  (progn
    ))
 )



;;;
;;;
;;;
;;;
;;;
;;;
;;;


;;;////////////////////////////////////////////////////////////////
;;;  @SHELL
;;;////////////////////////////////////////////////////////////////
(message "SITE:SHELL")

;; ---( shell )---------------------------------------------------------------

;; requires eterm-color, eterm terminfo 
;; @see: http://www.emacswiki.org/emacs/AnsiTermHints#toc4

;; ---( shell )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn



    ))
 (t
  (progn
    ))
 )


;; ---( eshell )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn



    ))
 (t
  (progn
    ))
 )


;; ---( ansi-term )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn



    ))
 (t
  (progn
    ))
 )


;; ---( multi-term )---------------------------------------------------------------

(cond
 ((string-lessp emacs-version "22.0") ;; 
  (progn

    (require 'cua)
    (CUA-mode t)
    (setq CUA-mode-normal-cursor-color "red")
    (setq CUA-mode-overwrite-cursor-color "yellow")
    (setq CUA-mode-read-only-cursor-color "green")

    ;; disable in .emacs
    ;; '(cua-mode t nil (cua-base))

    ))
 (t
  (progn
    ))
)



;; @see: http://www.emacswiki.org/emacs/MultiTerm

(cond
 ((string-lessp emacs-version "22.0") 
  (progn

    ))
 (t
  (progn

    (require 'multi-term)
    (setq multi-term-program "/bin/bash")

 (custom-set-variables
     '(term-default-bg-color "#001000")        ;; background color (black)
     '(term-default-fg-color "#80f080"))       ;; foreground color (yellow)

(add-hook 'term-mode-hook 
	  (lambda()
	    (global-unset-key (kbd "C-r"))
	    (color-theme-z-term)
;	    (local-unset-key (kbd "C-r"))
	    (message "%s" "This is in term mode and hook enabled.")
))


(defun term-send-function-key ()
  (interactive)
  (let* ((char last-input-event)
         (output (cdr (assoc char term-function-key-alist))))
    (term-send-raw-string output)))


;; tic -o ../ emacs-color.tix1
;; tput clear
;; infocmp
;; sudo cp eterm eterm-color /usr/share/emacs/23.3/etc//e/eterm-color

;; @see: ~/.emacs-site/skel/.terminfo/e/eterm-colot.ti

;; ~/.config/mc/ini

;; [terminal:eterm-color]
;; f1=\\eOP
;; f2=\\eOQ
;; f3=\\eOR
;; f4=\\eOS
;; f5=\\e[15~
;; f6=\\e[17~
;; f7=\\e[18~
;; f8=\\e[19~
;; f9=\\e[20~
;; f10=\\e[21~
;; f11=\\e[23~
;; f12=\\e[24~



(defconst term-function-key-alist '(
				    (f1 . "\eOP")
                                    (f2 . "\eOQ")
                                    (f3 . "\eOR")
                                    (f4 . "\eOS")
                                    (f5 . "\e[15~")
                                    (f6 . "\e[17~")
                                    (f7 . "\e[18~")
                                    (f8 . "\e[19~")
                                    (f9 . "\e[20~")
                                    (f10 . "\e[21~")
                                    (f11 . "\e[23~")
                                    (f12 . "\e[24~")
))




(dolist (spec term-function-key-alist)
  (define-key term-raw-map
    (read-kbd-macro (format "<%s>" (car spec)))
    'term-send-function-key))



    ))
 )


;; ;; ---( Sunrise )---------------------------------------------------------------

;; (cond
;;  ((eq z-emacs-type 'xemacs) ;; XEmacs
;;   (progn

;;     ))
;;  ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
;;   (progn

;;     (require 'sunrise-commander)
;;     ;;(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

;;     ))
;;  (t
;;   (progn
;;     ))
;;  )








;;;////////////////////////////////////////////////////////////////
;;;  @INFO
;;;////////////////////////////////////////////////////////////////
(message "SITE:INFO")

;; ---( Infopath )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn



    ))
 (t
  (progn
    ))
 )







;;;////////////////////////////////////////////////////////////////
;;;  @INTERNET
;;;////////////////////////////////////////////////////////////////
(message "SITE:INTERNET")

;; ---( FTP )---------------------------------------------------------------

(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn

 (customize-set-variable 'efs-generate-anonymous-password "hute37@netscape.net")

    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn

    ))
 (t
  (progn
    ))
 )




;;;////////////////////////////////////////////////////////////////
;;;  @EXTRA
;;;////////////////////////////////////////////////////////////////
(message "SITE:EXTRA")


;;;////////////////////////////////////////////////////////////////
;;;  IDL (RSI)
;;;////////////////////////////////////////////////////////////////


;;(setq speedbar-supported-extension-expressions
;;		(cons ".pro" speedbar-supported-extension-expressions))


;; (setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode)
;; 			    (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist)))

;; (add-hook 'idlwave-mode-hook
;;           (function
;;            (lambda ()
;;              (local-unset-key [(meta backspace)])
;;              )))

;;;////////////////////////////////////////////////////////////////
;;;  R
;;;////////////////////////////////////////////////////////////////

;; (if (eq z-location 'home)
;;     (if (eq z-system 'windows-nt)
;; 	(progn
;; 	  (setq load-path (cons "h:/usr/share/R/ess/ess-5.1.19/lisp" load-path))
;; 	  (require 'ess-site)
;; 	  )))



;;;////////////////////////////////////////////////////////////////
;;;  AUCTEX
;;;////////////////////////////////////////////////////////////////

;; (if (eq z-location 'home)
;;     (if (eq z-system 'winnt)
;; 	(progn
;; 	  (require 'tex-site)
;; 	  ;;(setq-default TeX-master nil) ;-- Query for master file
;; 	  ;;(setq-default TeX-master "Thesis") ;-- Fixed Master File
;; 	  ;;(setq-default TeX-master "Slider") ;-- Fixed Master File
;; 	  ;;(setq-default TeX-master "Abstract") ;-- Fixed Master File
;; 	  (setq Tex-parse-self t) ; Parse on Load
;; 	  (setq Tex-auto-save t) ; Parse on save
;; 	  (setq outline-minor-mode-prefix "\C-o") ; Outline minor Prefix
;; 	  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
;; 	  )))

;; (load "preview-latex")

;;;////////////////////////////////////////////////////////////////
;;;  IMAXIMA
;;;////////////////////////////////////////////////////////////////

 (autoload 'imaxima "imaxima" "Image support for Maxima." t)


;;;////////////////////////////////////////////////////////////////
;;;  Graphviz
;;;////////////////////////////////////////////////////////////////

(load "graphviz-dot-mode") 

;;;////////////////////////////////////////////////////////////////
;;;  @PERL
;;;////////////////////////////////////////////////////////////////
(message "SITE:PERL")


;; ;; both prolog and perl files are often called .pl;
;; ;; this tries to do the right thing.
;; (defun prolog-or-perl-mode () (interactive)
;;   (if
;;       (or (string-match "/perl\\b" (buffer-string)) ; file with perl header
;;           (= 1 (point-max)))            ; new file
;;       (progn
;;         (cperl-mode)
;;         (message "Ambiguous suffix .pl resolved to perl mode."))
;;     (progn
;;       (prolog-mode)
;;       (message "Ambiguous suffix .pl resolved to prolog mode.")))
;;   (sit-for 1))



;; (autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;; (setq cperl-hairy t)
;; (setq cperl-indent-level 4)
;; (setq auto-mode-alist
;;       (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))


(cond
 ((eq z-emacs-type 'xemacs) ;; XEmacs
  (progn
    ))
 ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
  (progn


;; ;;; [[TINY PERL]]

;;(require 'tinyperl)
(autoload 'turn-on-tinyperl-mode  "tinyperl" "" t)
(add-hook 'perl-mode-hook  'turn-on-tinyperl-mode)
(add-hook 'cperl-mode-hook 'turn-on-tinyperl-mode)

;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pod\\'" . tinyperl-pod-view-mode))

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)
(defun my-cperl-mode-hook ()
  "Perl mode customizations, M-h for perl help."
  (interactive)
  (require 'cperl-mode)
  (require 'tinyperl)
  
  ;; ---( F1: Help )---------------------------------------------------------

  (local-set-key  [f1] 'cperl-info-on-command ) ;;cperl redefined
  (local-set-key [(shift f1)] 'tinyperl-pod-by-manpage ) 
  (local-set-key  [(control f1)] 'tinyperl-pod-by-module )
  ;; (define-key global-map '[(meta f1)]	'ergo-font-select)
  (local-set-key  [(shift meta f1)] 'tinyperl-pod-grep)

  ;; ---( Return )---------------------------------------------------------

  (local-set-key [(control return)] 'tinyperl-module-find-file )

  ;; ---( F9: Debug )---------------------------------------------------------

  (local-set-key [f9] 'perldb )

;;   (mosh-map-local-keys
;;      [(control up  )]   'mosh-prev-perl   ; perl-beginning-of-function
;;      [(control down)]   'mosh-next-perl   ; perl-end-of-function
;;      [(control ?j  )]   'cperl-linefeed
;;      [(alt   ?h    )]   'cperl-get-help
;;      [(super ?h    )]   'cperl-info-on-current-command
;;      [(meta  ?h    )]   'cperl-info-on-current-command ; M-h was mark-paragraph
;;      [(hyper ?h    )]   'cperl-info-on-command
;;      [       return ]   'newline-and-indent
;;      [\e ?\;        ]   'cperl-indent-for-comment
;;      [\e tab        ]   'cperl-indent-for-comment
;;   )

;;   (setq
;;         cperl-info-page         "Perl5"
;;         cperl-indent-level      4
;;         cperl-font-lock         t
;;         cperl-brace-offset     -2  ; puts '{' in same col as '}'.
;;       ; cperl-hairy             t
;;   )
;;   ; (font-lock-mode t)

  (message "my-cperl-mode-hook loaded")
)



    ))
 (t
  (progn
    ))
 )



;;;////////////////////////////////////////////////////////////////
;;;  @XML
;;;////////////////////////////////////////////////////////////////
(message "SITE:XML")


;; ---( nXML )---------------------------------------------------------

(cond
 ((string-lessp emacs-version "22.0") ;; 
  (progn



    (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
    (setq auto-mode-alist
       (nconc
         '(("\\.html$" . sgml-mode))
         '(("\\.xml$" . sgml-mode))
;;          '(("\\.html$" . xml-mode))
;;          '(("\\.xml$" . xml-mode))
         auto-mode-alist))




;;    (add-hook 'xml-mode-hook   ; XML-specific settings
    (add-hook 'sgml-mode-hook   ; XML-specific settings
	      (function (lambda()

;; 					; faces creation
;; 			  (make-face 'sgml-comment-face)
;; 			  (make-face 'sgml-start-tag-face)
;; 			  (make-face 'sgml-end-tag-face)
;; 			  (make-face 'sgml-doctype-face)

;; 					; faces definitions
;; 			  (set-face-foreground 'sgml-comment-face "SeaGreen")
;; 			  (set-face-foreground 'sgml-start-tag-face "OrangeRed")
;; 			  (set-face-foreground 'sgml-end-tag-face "OrangeRed")
;; 			  (set-face-foreground 'sgml-doctype-face "MintCream")

					; markup to face mappings
					; (see http://www.lysator.liu.se/~lenst/about_psgml/psgml.html#Highlight for details)
;; 			  (setq sgml-markup-faces
;; 				'((comment . sgml-comment-face)
;; 				  (start-tag . sgml-start-tag-face)
;; 				  (end-tag . sgml-end-tag-face)
;; 				  (doctype . sgml-doctype-face)
;; 				  )
;; 				)

(setq sgml-markup-faces '(
    (start-tag . font-lock-keyword-face)
    (end-tag . font-lock-keyword-face)
    (comment . font-lock-comment-face)
    (pi . font-lock-constant-face) ;; <?xml?>
    (sgml . font-lock-type-face)
    (doctype . bold)
    (entity . italic)
    (shortref . font-lock-reference-face)))



			  (setq sgml-auto-activate-dtd t)
			  (setq sgml-indent-data t)
					; turn faces on
			  (setq sgml-set-face t)

			  )))



    ))
 (t ;; @todo: test avail nxml
  (progn




(cond
 ((eq system-type 'windows-nt) ;; WinNT
  (progn
    (setq load-path (cons "d:/opt/emacs/emacs/site-lisp/nxml" load-path))
    (load "rng-auto.el")
    ))
 ((eq system-type 'cygwin);; GNU-Cygwin
  (progn
   ;; (server-start)
    ))
 ((eq system-type 'TODO-gnu/linux);; GNU-Linux
  (progn
    (require 'nxml-mode)
    ))
 ((eq system-type 'usg-unix-v);; Sun Solaris
  (progn
    ))
 (t
  (progn
    ))
 )

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist)) 




    ))
 )





;;; ;;;////////////////////////////////////////////////////////////////
;;; ;;;  ICYCLE
;;; ;;;////////////////////////////////////////////////////////////////

;;; ;; ---( icycles )---------------------------------------------------------

;;; (cond
;;;  ((eq z-emacs-type 'xemacs) ;; XEmacs
;;;   (progn
;;;     ))
;;;  ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
;;;   (progn



;;; ;;; ;; loaded at end, to reuse keybindings
;;; ;;; (setq icicle-mode nil) ; Prevent turning on Icicle mode.
;;; ;;; (require 'icicles)
;;; ;;; (require 'icicles)
;;; ;;; (setq icicle-prompt-suffix "")





;;;     ))
;;;  (t
;;;   (progn
;;;     ))
;;;  )




;;;////////////////////////////////////////////////////////////////
;;;  @END
;;;////////////////////////////////////////////////////////////////
(message "SITE:END")

;; ---( completed )---------------------------------------------------------

;; (cond
;;  ((eq z-emacs-type 'xemacs) ;; XEmacs
;;   (progn
;;   (message "at end - completed")
;;     ))
;;  ((eq z-emacs-type 'fsf_emacs);; GNU-Emacs
;;   (progn
;;     ))
;; (t
;;  (progn
;;    ))
;;  )

















;; ;;; [[PERL MODE]]
;; ;; See lisp/cperl-mode-bad.el:1262 /^.if window-system/
;; ;; cperl-array-face, cperl-hash-face


;;;
;;; #########################################################################################
;;; #########################################################################################
;;; ###
;;; ###  @DEPRECATED
;;; ###
;;; #########################################################################################
;;; #########################################################################################
;;;



;; ;;; [[PERL MODE]]
;; ;; See lisp/cperl-mode-bad.el:1262 /^.if window-system/
;; ;; cperl-array-face, cperl-hash-face

;; (add-hook 'cperl-mode-hook 'mosh-perl-mode)

;; (defun mosh-perl-mode ()
;;   "Perl mode customizations, M-h for perl help."
;;   (interactive)
;;   (require 'cperl-mode)

;;   (mosh-map-local-keys
;;      [(control up  )]   'mosh-prev-perl   ; perl-beginning-of-function
;;      [(control down)]   'mosh-next-perl   ; perl-end-of-function
;;      [(control ?j  )]   'cperl-linefeed
;;      [(alt   ?h    )]   'cperl-get-help
;;      [(super ?h    )]   'cperl-info-on-current-command
;;      [(meta  ?h    )]   'cperl-info-on-current-command ; M-h was mark-paragraph
;;      [(hyper ?h    )]   'cperl-info-on-command
;;      [       return ]   'newline-and-indent
;;      [\e ?\;        ]   'cperl-indent-for-comment
;;      [\e tab        ]   'cperl-indent-for-comment
;;   )

;;   (setq
;;         cperl-info-page         "Perl5"
;;         cperl-indent-level      4
;;         cperl-font-lock         t
;;         cperl-brace-offset     -2  ; puts '{' in same col as '}'.
;;       ; cperl-hairy             t
;;   )
;;   ; (font-lock-mode t)
;; )

;; ;;; [[GDB/DEBUGGER]]

;; (add-hook 'gdb-mode-hook 'mosh-gdb-mode)

;; (defun mosh-gdb-mode ()
;;   "GDB mode customization, set it in the C++ file also!"
;;   (interactive)
;;   (mosh-map-local-keys
;;        [          up]  '(if (eobp) (comint-previous-input 1) (previous-line 1))
;;        [        down]  '(if (eobp) (comint-next-input     1) (next-line     1))
;;        [         f21]  'gud-next    ; Pause/break key.
;;   )
;;   (mosh-map-global-keys
;;        [(hyper right)] 'gud-step    ; step into.
;;        [(hyper down)]  'gud-next    ; step over to next stmt.
;;        [(hyper left)]  'gud-finish  ; finish current function
;;        [(hyper  f12)]  'gud-print   ; print object under cursor.

;;        [(super left )] 'gud-up
;;        [(super right)] 'gud-down
;;   )
;;   ; (load-file "patches20.el") ; for color arrow in gdb.
;; )





;; ;;; [[GNUS]]
;; ; See http://www.cs.washington.edu/homes/voelker/ntemacs.html

;; (setq gnus-select-method '(nntp "nntp.maxim.net"
;;     (nntp-port-number 119)))

;; ;;; [[SMTP]]
;; (setenv "MAILHOST" "smtp2.maxim.net")    ; for POP3.
;; (setq smtpmail-default-smtp-server "smtp2.maxim.net")
;; (setq smtpmail-local-domain nil)
;; (setq send-mail-function 'smtpmail-send-it)
;; (require 'smtpmail)
;; (if gnuemacs (require 'mailalias))

;; ;;; [[RMAIL]]

;; (defun mosh-rmail-hook ()
;;   "Rmail setup, RMAIL options only for saspc128."
;;   (interactive)
;;   (setq rmail-display-summary t)
;;   (setq    rmail-primary-inbox-list '("po:mosh") rmail-pop-password-required t)
;;   (define-key rmail-summary-mode-map [kp-prior] 'scroll-other-window-down)
;;   (define-key rmail-summary-mode-map [kp-next ]  'scroll-other-window  )
;; )

;; ;; mailcrypt options.
;; (if (file-readable-p (concat emacs_dir "/lisp/mailcrypt.el"))
;;     (progn
;;       (require 'mailcrypt)
;;       (add-hook 'rmail-mode-hook 'mc-install-read-mode)
;;       (add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)
;;       (add-hook 'mail-setup-hook 'mc-install-write-mode)
;;       ;; (setq mc-remailer-pseudonyms '("Elvis Presley" "Vanna"))
;;     )
;; )



;; ;;; [[DESKTOP/SAVEPLACE]]
;; (require 'desktop)

;; (setq desktop-missing-file-warning nil)
;; (setq desktop-basefilename  "hist/emacs.dsk")

;; (setq desktop-globals-to-save (list
;;     'command-history
;;     'extended-command-history
;;     'file-name-history
;;     'shell-command-history
;; ))

;; (desktop-load-default)
;; (desktop-read)
;; (add-hook 'kill-emacs-hook
;;           'mosh-desktop-save
;;           'bookmark-save)

;; (defun mosh-desktop-save () (desktop-save "~/"))






;;;////////////////////////////////////////////////////////////////
;;;
;;;////////////////////////////////////////////////////////////////


;; ---( elib )---------------------------------------------------------

;; (if  (eq z-emacs-type 'fsf_emacs) ;; Emacs
;; 	 (if (eq z-location 'home)
;; 		  (progn
;; 			 (setq load-path (cons "h:/usr/share/emacs-21.1/site-lisp/elib" load-path))
;; 			 )))

;; ---( PC Select )-----------------------------------------------------------


;; (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 	 (progn
;; 		(if  (eq system-type 'usg-unix-v) ;; Sun Solaris
;; 			 (progn
;; 				(require 'pc-select)
;; 				(pc-select-mode t)
;; 				)

;; 		  (progn
;; 			 ))
;; 		)

;;   (progn
;; 	 ))


;; ---( Paren )--------------------------------------------------------

;; (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 	 (progn
;; 		(require 'paren)
;; 		;; Turn on paren mode - this highlight matching under point
;; 		;; (show-paren-mode 1)
;; 		;;(setq paren-mode nil)
;; 		;;(setq paren-mode 'blink-paren)
;; 		(setq paren-mode 'paren)
;; 		;;(setq paren-mode 'sexp)
;; 		(paren-activate)
;; 		)

;;   (progn
;; 	 (show-paren-mode)
;; 	 ))


;; ;; Make the % key jump to the matching {}[]() if on another, like VI
;; ;;(global-set-key "%" 'match-paren)
;; (defun match-paren (arg)
;;   "Go to the matching parenthesis if on parenthesis otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))


;; ---( Diary )--------------------------------------------------------

;; Tell emacs where my diary file is
;; (setq diary-file "~/diary/.diary")





; ;;;_ + escreen

; (setq escreen-prefix-char [(control backspace)])

; (load "escreen")

; (escreen-install)

; (add-hook 'escreen-goto-screen-hook
; 	  (function
; 	   (lambda ()
; 	     (if (fboundp 'eshell-refresh-windows)
; 		 (eshell-refresh-windows)))))




;; ;;; Dave Brennan <brennan@hal.com> has a copy; might as well forward
;; ;;; him any improvements as well!
;; (defun zzz-insert-braces ()
;;   "Insert matched braces, leave point inside."
;;   (interactive "*")
;;   (let (blink-paren-function) ;nil it temporarily
;;     (execute-kbd-macro
;;      (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
;; 	 "{};" "{}")))
;;   (backward-sexp 1)
;;   (if (save-excursion
;; 	(forward-char -1)
;; 	(looking-at "\\$"))
;;       nil
;;     (reindent-then-newline-and-indent)
;;     (c-indent-exp)
;;     (forward-char 1)
;;     (newline-and-indent)))

;; (defun self-insert-backquote ( )
;;   "insert backquote `."
;;   (interactive)
;;   (insert-char ?` 1))

;; (defun self-insert-tilde ( )
;;   "insert tilde ~."
;;   (interactive)
;;   (insert-char ?~ 1))

;; (defun self-insert-open-bracket ( )
;;   "inserisce carattere {."
;;   (interactive)
;;   (insert-char ?{ 1))

;; (defun self-insert-close-bracket ( )
;;   "inserisce carattere }."
;;   (interactive)
;;   (insert-char ?} 1))

;; (defvar accent-keys-bound nil
;;   "accent keys binding state.")

;; (defun accent-keys-toggle ( )
;;   "toggle accent keys binding."
;;   (interactive)
;;   (if (null accent-keys-bound)
;;       (accent-keys-bind)
;;     (accent-keys-unbind)))


;; (defun accent-keys-full-bind ( )
;;   "enable accent keys binding."
;;   (interactive)
;;   (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 		(progn
;; 		  (keyboard-translate ?? ?[ ) ;; ced
;; 		  (keyboard-translate ?? ?] ) ;; -es
;; 		  (keyboard-translate ?? ?{ ) ;; o'
;; 		  (keyboard-translate ?? ?} ) ;; a'
;; 		  (keyboard-translate ?? ?~ ) ;; i'
;; 		  (keyboard-translate ?? ?` ) ;; e'
;; 		  (keyboard-translate 232 ?` ) ;; e' gr
;; 		  (global-set-key     (quote[249]) '(lambda () (interactive) (recenter 1)) ) ;; u'
;; 		  (global-set-key     (quote[167]) 'recenter ) ;; par
;; 		  (setq accent-keys-bound t)
;; 		  )

;; 	 (progn
;; 		  (keyboard-translate 231 ?[ ) ;; ced
;; 		  (keyboard-translate 176 ?] ) ;; -es
;; 		  (keyboard-translate 242 ?{ ) ;; o'
;; 		  (keyboard-translate 224 ?} ) ;; a'
;; 		  (keyboard-translate 236 ?~ ) ;; i'
;; 		  (keyboard-translate 233 ?` ) ;; e'
;; 		  (keyboard-translate 232 ?` ) ;; e' gr
;; 		  (global-set-key     (quote[249]) '(lambda () (interactive) (recenter 1)) ) ;; u'
;; 		  (global-set-key     (quote[167]) 'recenter ) ;; par
;; 		  (setq accent-keys-bound t)
;; 		)))


;;(keyboard-translate 214 ?~ ) ;; C-

;; (defun accent-keys-tex-bind ( )
;;   "enable accent keys binding."
;;   (interactive)
;;   (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 		(progn
;; 		  (keyboard-translate ?? ?{ ) ;; ced
;; 		  (keyboard-translate ?? ?} ) ;; -es
;; 		  (keyboard-translate ?? ?{ ) ;; o'
;; 		  (keyboard-translate ?? ?} ) ;; a'
;; 		  (keyboard-translate ?? ?~ ) ;; i'
;; 		  (keyboard-translate ?? ?` ) ;; e'
;; 		  (keyboard-translate 232 ?` ) ;; e' gr
;; 		  (global-set-key     (quote[249]) '(lambda () (interactive) (recenter 1)) ) ;; u'
;; 		  (global-set-key     (quote[167]) 'recenter ) ;; par
;; 		  (setq accent-keys-bound t)
;; 		  )

;; 	 (progn
;; 		  (keyboard-translate 231 ?{ ) ;; ced
;; 		  (keyboard-translate 176 ?} ) ;; -es
;; 		  (keyboard-translate 167 ?` ) ;; -es
;; 		  (keyboard-translate 163 ?~ ) ;; -es
;; 		  (setq accent-keys-bound t)
;; 		)))

;; (defun accent-keys-unbind ( )
;;   "enable accent keys binding."
;;   (interactive)
;;   (keyboard-translate 231 231 ) ;; ced
;;   (keyboard-translate 176 176 ) ;; -es
;;   (keyboard-translate 242 242 ) ;; o'
;;   (keyboard-translate 224 224 ) ;; a'
;;   (keyboard-translate 236 236 ) ;; i'
;;   (keyboard-translate 233 233 ) ;; e'
;;   (keyboard-translate 232 232 ) ;; e' gr
;;   (global-unset-key (quote[249]) ) ;; u'
;;   (global-unset-key (quote[167]) ) ;; par
;;   (setq accent-keys-bound nil))

;; (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 	 (progn
;; 		(accent-keys-full-bind)
;; 		)

;;   (progn
;;     (accent-keys-tex-bind)
;; 	 ))

;; ;; cycle through buffers, ignoring uninteresting ones
;; (defun z-backward-buffer () (interactive)
;;   "Switch to previously selected buffer."
;;   (let* ((list (cdr (buffer-list)))
;;          (buffer (car list)))
;;     (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
;;       (progn
;;         (setq list (cdr list))
;;         (setq buffer (car list))))
;;     (bury-buffer)
;;     (switch-to-buffer buffer)))


;; (defun z-forward-buffer () (interactive)
;;   "Opposite of backward-buffer."
;;   (let* ((list (reverse (buffer-list)))
;;          (buffer (car list)))
;;     (while (and (cdr list) (string-match "\\*" (buffer-name buffer)))
;;       (progn
;;         (setq list (cdr list))
;;         (setq buffer (car list))))
;;     (switch-to-buffer buffer)))


;; (defun buffer-tab-find-prev-rec (buffer-list  prev-buffer)
;;   (when buffer-list
;; 	 (if (aref (car buffer-list) 3)
;; 		  prev-buffer
;; 		(buffer-tab-find-prev-rec (cdr buffer-list) (car buffer-list))
;; 	 )
;;   )
;; )

;; (defun buffer-tab-find-prev ()
;;   (let ((buffer-list  ( buffers-tab-items )))
;; 	 (let ((prev-buffer (buffer-tab-find-prev-rec buffer-list nil)))
;; 	 (if prev-buffer
;; 		  prev-buffer
;; 		(car (last buffer-list))
;; 	 )))
;;   )

;; (defun buffer-tab-find-next-rec (buffer-list)
;;   (when buffer-list
;; 	 (if (aref (car buffer-list) 3)
;; 		  (cadr buffer-list)
;; 		(buffer-tab-find-next-rec (cdr buffer-list))
;; 	 )
;; ))

;; (defun buffer-tab-find-next-z ()
;;   (let ((buffer-list  ( buffers-tab-items )))
;; 	 (let ((next-buffer (buffer-tab-find-next-rec buffer-list)))
;; 	 (if next-buffer
;; 		  next-buffer
;; 		(last buffer-list))
;; 	 ))
;;   )

;; (defun buffer-tab-find-next () ;; buggy
;;   (let ((buffer-list  (reverse  ( buffers-tab-items ))))
;; 	 (let ((prev-buffer (buffer-tab-find-prev-rec buffer-list nil)))
;; 	 (if prev-buffer
;; 		  prev-buffer
;; 		(car (last buffer-list))
;; 	 )))
;;   )


;; (defun buffer-tab-advance (&optional direction )
;;   "Switch to next/prev buffer in Buffer Tab List."
;;   (interactive)
;;   (let ((target-buffer
;; 		 (if direction
;; 			  (buffer-tab-find-prev)
;; 			(buffer-tab-find-next))))
;; 		 (eval (aref  target-buffer 1))))


;; (defun toggle-modeline ()
;;   (interactive)
;;   (set-specifier
;;    has-modeline-p
;;    (not (specifier-instance has-modeline-p))))

;; ;;@TODO: unused
;; (defun toggle-modeline ()
;;   (interactive)
;;   )

;;(mode-line)
;;(vc-mode-line)

;; (defun toggle-toolbar ()
;;   (interactive)
;;   (set-specifier
;;    default-toolbar-visible-p
;;    (not (specifier-instance default-toolbar-visible-p))))

;; ;;@TODO: unused
;; (defun toggle-toolbar ()
;;   (interactive)
;;   (tool-bar-mode))

;; ;; ---( Gutter )-----------------------------------------------------------

;; ;; Toggle Gutter Bar
;; (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;; 	 (defun toggle-gutter-bar ()
;; 		"Tabifies entire buffer."
;; 		(interactive)
;; 	(customize-set-variable 'gutter-buffers-tab-visible-p
;; 	(not gutter-buffers-tab-visible-p))
;; 		)
;;   )

;; (define-key global-map [kp-home]	'scroll-cursor-to-bottom)
;; (define-key global-map [kp-end]	'scroll-cursor-to-top)
;; (define-key global-map [kp-left]	'backward-char-command)
;; (define-key global-map [kp-right]	'forward-char-command)
;; (define-key global-map [(control kp-left)]	'scroll-left)
;; (define-key global-map [(control kp-right)]	'scroll-right)
;; (define-key global-map [kp-up]		'scroll-down-one)
;; (define-key global-map [kp-down]	'scroll-up-one)
;; (define-key global-map [kp-multiply]	'scroll-left)

;; (define-key global-map [kp-delete]	'bury-buffer)
;; (define-key global-map [(control kp-delete)]	'kill-this-buffer)
;; (define-key global-map [(control kp-insert)]	'other-frame)
;; (define-key global-map [(meta kp-insert)]	'make-frame)
;; (define-key global-map [(meta kp-delete)]	'delete-frame)

;; (require 'bs)
;; (define-key global-map [kp-insert]	'bs-show)


;;(global-set-key [(control tab)] 'switch-to-other-buffer )
;;(global-set-key [(control tab)]  '(lambda ()
;;	  (interactive) (buffer-tab-advance t)) )
;;(global-set-key [(control meta tab)]  '(lambda ()
;;	  (interactive) (buffer-tab-advance t)) )
;;(global-set-key [(control shift tab)]  '(lambda ()
;;	  (interactive) (buffer-tab-advance nil)) )

;; better buffer cycling
;; (global-set-key [(control return)] 'forward-buffer)
;; (global-set-key [(shift return)]  'backward-buffer)




;; (if  (eq z-emacs-type 'xemacs) ;; XEmacs
;;  (progn
;;    (define-key global-map [(control meta kp-end)]	'toggle-line-wrapping)
;; 	(setq truncate-lines t)
;; 	)

;;   (progn
;; 	(define-key global-map [(control meta kp-end)]	'toggle-truncate-lines)
;; ;	(toggle-truncate-lines)
;;  ))

;; =====================================================================
;; =====================================================================
;; Saving Emacs Sessions - Useful when you have a bunch of source
;; files open and you don't want to go and manually open each one,
;; especially when they are in various directories. Page 377 of the
;; GNU Emacs Manual says: "The first time you save the state of the
;; Emacs session, you must do it manually, with the command M-x
;; desktop-save. Once you have dome that, exiting Emacs will save the
;; state again -- not only the present Emacs session, but also
;; subsequent sessions. You can also save the state at any time,
;; without exiting Emacs, by typing M-x desktop-save again.
;; =====================================================================

;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)

;; ---( Desktop )--------------------------------------------------------

;; (require 'desktop)
;; (require 'desktop-menu)
;; (global-set-key [(control f1)]
;;  '(lambda () (interactive)
;;  (and (yes-or-no-p "Save Desktop?")
;; (desktop-save "~/"))))
;; (global-set-key [(control f2)] 'desktop-read)

;; add hook save on exit ?


; ;;;_ + desktop

; (load "desktop")
; (load "desktop-save")

; (defcustom initial-desktop-directory "~/"
;   "*The directory where the main desktop file is stored."
;   :type 'file
;   :group 'desktop)

; (setq escreen-data-file
;       (expand-file-name ".escreen" initial-desktop-directory))

; (add-hook 'desktop-save-hook 'escreen-save-state)
; (add-hook 'desktop-save-hook 'escreen-save-current-screen-configuration)

; (add-hook 'desktop-save-hook 'session-save-session)

; (add-hook 'desktop-save-hook
; 	  (function
; 	   (lambda ()
; 	     (if (fboundp 'eshell-save-some-history)
; 		 (eshell-save-some-history)))))

; (add-hook 'desktop-save-hook
; 	  (function
; 	   (lambda ()
; 	     (if (fboundp 'eshell-save-some-last-dir)
; 		 (eshell-save-some-last-dir)))))

; (defun setup-desktop ()
;   "Setup Emacs by reading from a set of persistent state files."
;   (add-hook
;    'eshell-first-time-mode-hook
;    (function
;     (lambda ()
;       (remove-hook 'kill-emacs-hook 'eshell-save-some-history)
;       (remove-hook 'kill-emacs-hook 'eshell-save-some-last-dir))))

;   (let ((default-directory initial-desktop-directory))
;     (desktop-load-default)
;     (desktop-read))

;   (if (file-readable-p escreen-data-file)
;       (escreen-restore-state escreen-data-file))

;   (add-hook 'kill-emacs-hook
; 	    (function
; 	     (lambda ()
; 	       (let ((buf (get-buffer "*Process List*")))
; 		 (when (and buf (buffer-live-p buf))
; 		   (if (get-buffer-window buf)
; 		       (delete-window (get-buffer-window buf)))
; 		   (kill-buffer buf))))))

;   (add-hook 'kill-emacs-hook 'kill-encrypted-buffers)

;   (run-with-idle-timer (* 60 5)  t 'desktop-kill)
;   (run-with-idle-timer (* 60 30) t 'desktop-kill))

; (if (and (file-exists-p (concat initial-desktop-directory
; 				desktop-basefilename))
; 	 (null noninteractive))
;     (add-hook 'after-init-hook 'setup-desktop))

;;(setq bs-default-configuration "files")


;;(global-set-key [(control f4)] 'untabify-buffer)
;;(global-set-key [(meta f4)] 'tabify-buffer)
;;(global-set-key [f5] 'tags-search)
;;(global-set-key [(shift f5)] 'tags-query-replace)
;;(global-set-key [(control f5)] 'list-tags)
;;(global-set-key [(meta f5)] 'tags-apropos)
;;(global-set-key [(control meta f5)] 'visit-tags-table)
;; Hit f9 to force a re-fontify
;;(global-set-key (quote [f9]) (quote font-lock-fontify-buffer))



;;;////////////////////////////////////////////////////////////////
;;;  @PRINT
;;;////////////////////////////////////////////////////////////////

;; This requires GhostScript which is available at this URL:
;; http://www.cs.wisc.edu/~ghost/aladdin To print, just hit C-cp. This
;; produces a file called printme.ps in the spool directory. Open this
;; in GhostView and print from there. This works even on
;; non-postscript printers.
;;  --- (setq ps-printer-name "~/spool/printme.ps")
;; --- (define-key global-map "\C-cp" 'ps-print-buffer)

;; Printing. For Windows, using w32-print. For Unix, using built-in.
;; (defun z-print-setup () ;
;; (if
;;     ;; Win32
;;     (eq system-type 'windows-nt)
;;     (progn
;;       (add-to-list 'load-path (concat my-site-lisp "/w32-print-1.3"))
;;       (setq-default
;;        ;; General
;;        w32-print-use-faces nil		; disable color, etc
;;        w32-print-zap-spool-file t	; delete temp file
;;        w32-print-paper-type 'letter
;;        w32-print-with-line-numbers t
;;        w32-print-enable-keymappings nil ; disable keyboard shortcuts
;;        ;; Postscript
;;        w32-print-ps-preview nil		; disable print preview
;;        w32-print-ps-destination (if (eq my-location 'home)
;; 				    "lpt1"
;; 				  "//romsvr1/hplj4050")
;;        w32-print-ps-text-font-size 14.0
;;        w32-print-psnup-path (concat my-site-lisp "/w32-print-1.3/bin")
;;        w32-print-psnup-margin "0in"
;;        w32-print-psnup-enable t
;;        ;; Ghostscript
;;        w32-print-gs-destination (if (eq my-location 'home)
;; 				    "lpt1"
;; 				  "//romsvr1/hplj4050")
;;        w32-print-gs-device (if (eq my-location 'home) "deskjet" "ljet4")
;;        w32-print-gs-device-dpi (if (eq my-location 'home) '300 '600)
;;        w32-print-gs-version "6.01"
;;        w32-print-gs-path (concat my-gnu-home "/ghostscript/gs6.01/bin")
;;        w32-print-gs-view-path (concat my-gnu-home "/ghostscript/gsview")
;;        ;; Plain old printer
;;        w32-print-lpr-destination (if (eq my-location 'home)
;; 				     "lpt1"
;; 				   "//romsvr1/hplj4050")
;;        w32-print-lpr-path (concat my-site-lisp "/w32-print-1.2/bin")
;;        )
;;       ;; Let's use it
;;       (load "w32-print")
;;       )
;;   ;; Unix system. Nothing to do since I can't print from unix.
;;   ()
;;   )
;; )


;;;////////////////////////////////////////////////////////////////
;;;  C/C++
;;;////////////////////////////////////////////////////////////////

;;;_ + cc-mode

;; (defconst borland-c-style
;;   '((c-backslash-column		   . 72)
;;     (c-basic-offset		   . 4)
;;     (c-block-comment-prefix	   . "")
;;     (c-hanging-semi&comma-criteria . nil)
;;     (c-comment-continuation-stars  . "")
;;     (c-comment-only-line-offset    . 0)
;;     (c-electric-pound-behavior	   . (alignleft))
;;     (c-cleanup-list
;;      . (defun-close-semi list-close-comma scope-operator))
;;     (c-hanging-colons-alist
;;      . ((inher-intro)
;; 	(member-init-intro before)
;; 	(case-label after)
;; 	(label after)
;; 	(access-key after)))
;;     (c-hanging-braces-alist
;;      . ((brace-list-open before)
;; 	(brace-list-close before)
;; 	(substatement-open before)
;; 	(substatement-case-open before)
;; 	(else-clause before)
;; 	(defun-open before)
;; 	(defun-close before)
;; 	(class-open before)
;; 	(class-close before)
;; 	(block-open after)
;; 	(block-close before)
;; 	(inline-open after)
;; 	(inline-close before)))
;;     (c-offsets-alist
;;      . ((string                . c-lineup-dont-change)
;; 	(c		       . c-lineup-C-comments)
;; 	(defun-open	       . 0)
;; 	(defun-close	       . 0)
;; 	(defun-block-intro     . +)
;; 	(class-open	       . 0)
;; 	(class-close	       . 0)
;; 	(inline-open	       . +)
;; 	(inline-close	       . 0)
;; 	(func-decl-cont        . +)
;; 	(knr-argdecl-intro     . +)
;; 	(knr-argdecl	       . 0)
;; 	(topmost-intro	       . 0)
;; 	(topmost-intro-cont    . 0)
;; 	(member-init-intro     . +)
;; 	(member-init-cont      . 0)
;; 	(inher-intro	       . +)
;; 	(inher-cont	       . c-lineup-multi-inher)
;; 	(block-open	       . 0)
;; 	(block-close	       . 0)
;; 	(brace-list-open       . 0)
;; 	(brace-list-close      . 0)
;; 	(brace-list-intro      . +)
;; 	(brace-list-entry      . 0)
;; 	(brace-entry-open      . 0)
;; 	(statement-block-intro . +)
;; 	(statement-case-intro  . +)
;; 	(statement-case-open   . 0)
;; 	(substatement	       . +)
;; 	(case-label	       . 0)
;; 	(access-label	       . -)
;; 	(label		       . 2)
;; 	(do-while-closure      . 0)
;; 	(else-clause	       . 0)
;; 	(catch-clause	       . 0)
;; 	(comment-intro	       . c-lineup-comment)
;; 	(arglist-intro	       . +)
;; 	(arglist-cont	       . 0)
;; 	(arglist-close	       . +)
;; 	(stream-op	       . c-lineup-streamop)
;; 	(inclass	       . +)
;; 	(cpp-macro	       . -1000)
;; 	(cpp-macro-cont        . c-lineup-dont-change)
;; 	(friend	               . 0)
;; 	(objc-method-intro     . -1000)
;; 	(objc-method-args-cont . c-lineup-ObjC-method-args)
;; 	(objc-method-call-cont . c-lineup-ObjC-method-call)
;; 	(extern-lang-open      . 0)
;; 	(extern-lang-close     . 0)
;; 	(inextern-lang	       . +)
;; 	(namespace-open        . 0)
;; 	(namespace-close       . 0)
;; 	(innamespace	       . +)
;; ;	(template-args-cont    . c-lineup-template-args)
;; ;	(inlambda	       . c-lineup-inexpr-block)
;; ;	(lambda-intro-cont     . +)
;; ;	(inexpr-statement      . 0)
;; ;	(inexpr-class	       . +)
;; 	(arglist-cont-nonempty . c-lineup-arglist)
;; 	(substatement-open     . 0)
;; 	(statement	       . c-lineup-runin-statements)
;; 	(statement-cont        . c-lineup-runin-statements))))
;;   "Borland C programming style")

;; (defun my-c-mode-common-hook ()
;;   (c-add-style "borland" borland-c-style t)
;;   (c-set-style "borland")

;;   (turn-on-auto-fill)
;;   (set (make-local-variable 'fill-nobreak-predicate)
;;        (function
;; 	(lambda nil
;; 	  (not (eq (get-text-property (point) (quote face))
;; 		   (quote font-lock-comment-face))))))

;;   (setq c-indent-comments-syntactically-p t
;; 	tab-width 8 indent-tabs-mode t)	; allow the use of tabs

;;   (define-key c-mode-base-map "\C-m" 'newline-and-indent)

;;   (set (make-local-variable 'comment-start) "// ")
;;   (set (make-local-variable 'comment-end) ""))

;; ;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;////////////////////////////////////////////////////////////////
;;;  PSGML/xslide
;;;////////////////////////////////////////////////////////////////

;; (if  (eq z-emacs-type 'fsf_emacs) ;; Emacs
;; 	 (if (eq z-location 'home-zzz-sgml)
;; 		  (progn
;; 			 (setq load-path (cons "h:/usr/share/emacs-21.1/site-lisp/psgml-1.2.5" load-path))
;; 			 (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;; 			 (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;; 			 (setq load-path (cons "h:/usr/share/emacs-21.1/site-lisp/xslide" load-path))
;; 			 (autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; 			 (setq load-path (cons "h:/usr/share/emacs-21.1/site-lisp/xslt-process-2.1/lisp" load-path))
;; 			 (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
;; 			 (autoload 'xslt-process-install-docbook "xslt-process"
;; 				"Register the DocBook package with XSLT-process" t)
;; 			 (add-hook 'sgml-mode-hook 'xslt-process-mode)
;; 			 (add-hook 'xml-mode-hook 'xslt-process-mode)
;; 			 (add-hook 'xsl-mode-hook 'xslt-process-mode)
;; 			 (defadvice xml-mode (after run-xml-mode-hooks act)
;; 				"Invoke `xml-mode-hook' hooks in the XML mode."
;; 				(run-hooks 'xml-mode-hook))

;; 			 (autoload 'css-mode "css-mode")
;; 			 (setq auto-mode-alist
;; 					 (cons '("\\.css\\'" . css-mode) auto-mode-alist))


;; 			 ;; Turn on font lock when in XSL mode
;; 			 (add-hook 'xsl-mode-hook
;; 						  'turn-on-font-lock)

;; 			 ;; Set up file-extension/mode associations.
;; 			 ;; Note that I use xml-mode for html... that's because i'm writing
;; 			 ;; XHTML and I want my html to conform to XML.
;; 			 (setq auto-mode-alist
;; 					 (append '(
;; 								  ("\\.sgml" . sgml-mode)
;; 								  ("\\.idd" . sgml-mode)
;; 								  ("\\.ide" . sgml-mode)
;; 								  ("\\.htm" . sgml-mode)
;; 								  ("\\.html" . sgml-mode)
;; 								  ("\\.xhtml" . xml-mode)
;; 								  ("\\.xml" . xml-mode)
;; 								  ("\\.xsd" . xml-mode)
;; 								  ("\\.xul" . xml-mode)
;; 								  ("\\.rdf" . xml-mode)
;; 								  ;;  ("\\.xsl" . xml-mode)
;; 								  ;;  ("\\.fo" . xml-mode)
;; 								  ("\\.fo" . xsl-mode)
;; 								  ("\\.xsl" . xsl-mode)
;; 								  )
;; 								auto-mode-alist
;; 								)
;; 					 )


;; 			 ;; Set up and enable syntax coloring.
;; 			 ;; Create faces  to assign markup categories.
;; 			 (make-face 'sgml-doctype-face)
;; 			 (make-face 'sgml-pi-face)
;; 			 (make-face 'sgml-comment-face)
;; 			 (make-face 'sgml-sgml-face)
;; 			 (make-face 'sgml-start-tag-face)
;; 			 (make-face 'sgml-end-tag-face)
;; 			 (make-face 'sgml-entity-face)

;; 			 ;; Assign attributes to faces. Background of white assumed.
;; ;; 			 (set-face-foreground 'sgml-doctype-face "blue1")
;; ;; 			 (set-face-foreground 'sgml-sgml-face "cyan1")
;; ;; 			 (set-face-foreground 'sgml-pi-face "magenta")
;; ;; 			 (set-face-foreground 'sgml-comment-face "purple")
;; ;; 			 (set-face-foreground 'sgml-start-tag-face "Red")
;; ;; 			 (set-face-foreground 'sgml-end-tag-face "Red")
;; ;; 			 (set-face-foreground 'sgml-entity-face "Blue")

;; 			 ;; Assign faces to markup categories.
;; 			 (setq sgml-markup-faces
;; 					 '((doctype	. sgml-doctype-face)
;; 						(pi		. sgml-pi-face)
;; 						(comment	. sgml-comment-face)
;; 						(sgml	. sgml-sgml-face)
;; 						(comment	. sgml-comment-face)
;; 						(start-tag	. sgml-start-tag-face)
;; 						(end-tag	. sgml-end-tag-face)
;; 						(entity	. sgml-entity-face)))

;; 			 ;; PSGML - enable face settings
;; 			 (setq sgml-set-face t)

;; 			 ;; Auto-activate parsing the DTD when a document is loaded.
;; 			 ;; If this isn't enabled, syntax coloring won't take affect until
;; 			 ;; you manually invoke "DTD->Parse DTD"
;; 			 (setq sgml-auto-activate-dtd t)


;; 			 ;; Set up my "DTD CATALOG".
;; 			 ;; defined in "/usr/local/lib/sgml/CATALOG.cat"
;; 			 ;; (add-to-list 'sgml-catalog-files "/usr/local/lib/sgml/HTML4/HTML4.cat")
;; 			 ;; (add-to-list 'sgml-catalog-files "/usr/local/lib/sgml/XHTML1/xhtml1.soc")

;; 			 ;; Set up my "DTD->Insert DTD" menu.

;; 			 (setq sgml-custom-dtd '
;; 					 (
;; 					  ( "DITA concept"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE concept SYSTEM \"concept.dtd\">" )
;; 					  ( "DITA task"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE task SYSTEM \"task.dtd\">" )
;; 					  ( "DITA reftopic"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE reftopic SYSTEM \"reftopic.dtd\">" )
;; 					  ( "DITA APIdesc"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE APIdesc SYSTEM \"apidesc.dtd\">" )
;; 					  ( "DITA topic"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE topic SYSTEM \"ditabase.dtd\">" )
;; 					  ( "HOD Script"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE HASCRIPT SYSTEM \"HAScript.dtd\">" )
;; 					  ( "XHTML 1.0 Strict"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"xhtml1-strict.dtd\">" )
;; 					  ( "XHTML 1.0 Transitional"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">" )
;; 					  ( "XHTML 1.0 Frameset"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"xhtml1-frameset.dtd\">" )
;; 					  ( "HTML 4.01 Transitional"
;; 						 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" )
;; 					  ( "HTML 4.01 Strict"
;; 						 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">" )
;; 					  ( "HTML 4.01 Frameset"
;; 						 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\">" )
;; 					  ( "IBMIDDoc"
;; 						 "<!DOCTYPE ibmiddoc PUBLIC \"+//ISBN 0-933186::IBM//DTD IBMIDDoc//EN\" [\n]>")
;; 					  ( "DOCBOOK XML 4.1.2"
;; 						 "<?xml version=\"1.0\"?>\n<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.1.2//EN\" \"http://www.oasis-open.org/docbook/xml/4.0/docbookx.dtd\" [\n]>")
;; 					  )
;; 					 )

;; 			 ;; From Lennart Staflin - re-enabling launch of browser (from original HTML mode)
;; 			 (defun my-psgml-hook ()
;; 				(local-set-key "\C-c\C-b" 'browse-url-of-buffer)
;; 				)

;; 			 (add-hook 'sgml-mode-hook 'my-psgml-hook)

;; 			 ;; Set up Validation support
;; 			 ;; First, for sgml-mode, if you always use the same declaration, uncomment
;; 			 ;; the following line and set the path to your declaration. If you use
;; 			 ;; more than one SGML declaration, leave it unset and use OpenSP as your
;; 			 ;; validator and include DTDDECL entries in your catalog files.
;; 			 ;; (setq sgml-declaration "<path to your SGML declaration>")

;; 			 (setq sgml-validate-command "onsgmls -s %s %s")

;; 			 ;; For xml-mode, override the default validate command by providing a
;; 			 ;; mode-hook and setting the SGML declaration to the one
;; 			 ;; provided with either SP or OpenSP.
;; 			 (defun my-psgml-xml-hook ()
;; 				(setq sgml-validate-command "onsgmls -s %s %s")
;; 				;;  (setq sgml-declaration "d:/SP/pubtext/xml.dcl")
;; 				(setq sgml-declaration "d:/OpenSP/pubtext/xml.dcl")
;; 				)
;; 			 (add-hook 'xml-mode-hook 'my-psgml-xml-hook)


;; 			 )))



;;;////////////////////////////////////////////////////////////////
;;;  COBOL
;;;////////////////////////////////////////////////////////////////

;; (if
;;     (eq z-location 'work)
;;     (progn
;; 		(autoload 'cobol-mode "cobol")

;; 		(setq auto-mode-alist
;; 				(cons '("\\.cbl\\'" . cobol-mode)
;; 						(cons '("\\.cpy\\'" . cobol-mode)
;; 								auto-mode-alist )))
;; 		(add-hook 'cobol-mode-hook 'ffap-bindings)

;; 		))



;;(setq speedbar-supported-extension-expressions
;;		(append '(".cbl" ".cpy") speedbar-supported-extension-expressions))


;;;////////////////////////////////////////////////////////////////
;;;  DELPHI
;;;////////////////////////////////////////////////////////////////

;;(autoload 'delphi-mode "delphi")
;;(setq auto-mode-alist
;;       (cons '("\\.\\(pas\\|dpr\\|dpk\\)$" . delphi-mode) auto-mode-alist))

;;;;(setq speedbar-supported-extension-expressions
;;;;		(append '(".pas" ".dpr" ".dpk") speedbar-supported-extension-expressions))

;;(add-hook 'delphi-mode-hook 'turn-on-font-lock)

;;(autoload 'delphi-mode "delphi")
;;(setq auto-mode-alist
;;       (cons '("\\.\\(pas\\|dpr\\|dpk\\)$" . delphi-mode) auto-mode-alist))

;;;;(setq speedbar-supported-extension-expressions
;;;;		(append '(".pas" ".dpr" ".dpk") speedbar-supported-extension-expressions))

;;(add-hook 'delphi-mode-hook 'turn-on-font-lock)



;; =====================================================================
;; =====================================================================
;; USEFUL NOTES AND OTHER STUFF
;; =====================================================================

;; How to record and display a keyboard macro

;; Just open a buffer and type C-x ( Then start typing in your macro.
;; Once you are finished defining your macro type C-x ) Then type M-x
;; name-last-kbd-macro. This will allow you to call your macro
;; whatever you want. Next open up your .emacs file and position your
;; cursor where you want the code for the macro to appear.  Type M-x
;; insert-kbd-macro and type in the name.  The code will automatically
;; be generated.

;; =====================================================================
;; =====================================================================

;; Use shell-command-on-region M-| to send region to external
;; process. If you use a prefix argument , C-u M-| this will replace
;; the region with the output of the external process. Good for
;; sending something to stdin and reading from stdout.

;; =====================================================================
;; =====================================================================

;; To copy to named register: C-x r s a - Where a is the name of the
;; register ( a - z ) to save the text to.

;; To paste from named register: C-x r i a - Where a is the name of
;; the register ( a - z ) to paste the saved text from.

;; To remember current point: C-x r spc a - Where a is the name of the
;; register to save point to.

;; To jump to named point: C-x r j a - Where a is the name of the
;; register holding desired point to jump to

;; =====================================================================
;; SOME GOOD URL's FOR EMACS SOURCES
;; =====================================================================

;; http://www.splode.com/users/friedman/software/emacs-lisp/
;; http://www.anc.ed.ac.uk/~stephen/emacs/ell.html

;; =====================================================================
;; =====================================================================

;;----------------------------------------------------------------------------
;; Set up hooks for often-used major modes
;;----------------------------------------------------------------------------


;; (setq backup-by-copying t)

;(require 'backups)
;(move-backups t)
;; Tell emacs where central backup directory is, and turn it on
;; (setq backup-directory "~/backups")
;; (require 'backups)
;; (move-backups t)

;;@TODO: verify current ...
;; (setq backup-directory "~/.backups")

;; (or (directory-files backup-directory)
;; 	  (make-directory backup-directory))

;; (require 'backup-dir)
;; (setq bkup-backup-directory-info
;; 		`(
;;        (t ,(concat backup-directory "/") full-path prepend-name search-upward)
;; ;      (t ,(concat backup-directory "/"))
;; 		  ))



;; (cond  ;;--- FSF-Emacs ---------------------------------------------------
;;  ((eq system-type 'windows-nt) ;; WinNT
;;   (cond
;; 	(t
;; 	 (progn
;; 		(setq backup-by-copying nil)
;; 		)
;; 	 )))
;;  ((eq system-type 'gnu/linux);; GNU-Linux
;;   (cond
;; 	(t
;; 	 (progn
;; 		(setq backup-by-copying t)
;; 		)
;; 	 )))
;;  ((eq system-type 'usg-unix-v);; Sun Solaris
;;   (cond
;; 	(t
;; 	 (progn
;; 		(setq backup-by-copying t)
;; 		)
;; 	 )))
;;  )


;;;////////////////////////////////////////////////////////////////
;;;  #EXIT
;;;////////////////////////////////////////////////////////////////
(message "SITE:#EXIT#")
