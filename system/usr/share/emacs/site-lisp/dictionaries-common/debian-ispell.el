;;File: debian-ispell.el
;;; -----------------------------------------------------------------------
;;;	$Id: debian-ispell.el,v 1.1 2008-03-07 00:28:50 hute37 Exp $	
;;; -----------------------------------------------------------------------
;;Description: Emacsen support for Debian package dictionaries-common
;;Authors: Rafael Laboissière <rafael@debian.org>
;;         Agustin Martin     <agmartin@debian.org>
;;Created on: Tue Oct 26 10:16:12 CEST 1999
;;; -----------------------------------------------------------------------

(defcustom debian-dict-common-debug nil
  "A lot of debugging info will be shown if non nil."
  :type 'boolean
  :group 'ispell)

;;; -----------------------------------------------------------------------
;;;  Initialize the alist containing all info for the different spell
;;;  emacsen entries and provide the function to populate it
;;; -----------------------------------------------------------------------

(defvar debian-ispell-dictionary-alist 
  '((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1))
  "Alist of dictionaries used internally by the Debian ispell
initialization scheme.  Its value will be used to set
`ispell-dictionary-alist' after ispell.el is loaded.

Do not change this variable directly.  Use the
`debian-ispell-add-dictionary-entry' function instead.")

(defvar debian-ispell-valid-dictionary-list nil
  "A list that will contain the list of emacsen names provided by
registered ispell or aspell dicts")

(defvar debian-ispell-availability-alist
  '((nil . nil))
  "A list that will contain a mappping of emacsen names vs the
spellchecker for which they are available. Its value will be
filled by the debian-ispell-add-dictionary-entry function")

(defun debian-ispell-add-dictionary-entry (entry &optional name)
  "Adds an ENTRY to the ispell-dictionary-alist variable. See the 
documentation of the variable ispell-dictionary-alist for the format 
of ENTRY. NAME can be ispell, aspell or all, depending on the
available spellchecker(s) for that entry"
  (set-variable 'debian-ispell-dictionary-alist
		(append (list entry) debian-ispell-dictionary-alist))
  (set-variable 'debian-ispell-valid-dictionary-list
		(add-to-list 'debian-ispell-valid-dictionary-list (car entry)))
  (if name
      (set-variable 'debian-ispell-availability-alist
		    (append (list (cons (car entry) name))
			    debian-ispell-availability-alist)))
  )

;;; ----------------------------------------------------------------------
;;;  Handle ispell.el load at startup
;;; ----------------------------------------------------------------------

(defun debian-ispell-build-startup-menu ()
;;; ----------------------------------------------------------------------
;;; Extracted from ispell.el, by Ken Stevens, part of GNU emacs.
;;; Original code released under the GNU GPL license
;;; ----------------------------------------------------------------------
  "Build startup menu, trying to not explicitely load ispell.el"
  (if ispell-menu-map-needed
      (let ((dicts (reverse mylist)))
	(setq ispell-menu-map (make-sparse-keymap "Spell"))
	;; add the dictionaries to the bottom of the list.
	(while dicts
	  (if (string-equal "default" (car dicts))
	      (define-key ispell-menu-map (vector 'default)
		(cons "Select Default Dict"
		      (cons "Dictionary for which Ispell was configured"
			    (list 'lambda () '(interactive)
				  (list
				   'ispell-change-dictionary "default")))))
	    (define-key ispell-menu-map (vector (intern (car dicts)))
	      (cons (concat "Select " (capitalize (car dicts)) " Dict")
		    (list 'lambda () '(interactive)
			  (list 'ispell-change-dictionary (car dicts))))))
	  (setq dicts (cdr dicts)))))
  
  (if ispell-menu-map-needed
      (progn
	(define-key ispell-menu-map [ispell-change-dictionary]
	  '(menu-item "Change Dictionary..." ispell-change-dictionary
		      :help "Supply explicit dictionary file name"))
	;; --
	;; (define-key ispell-menu-map [ispell-kill-ispell]
	;;   '(menu-item "Kill Process" ispell-kill-ispell
	;; 	      :enable (and (boundp 'ispell-process) ispell-process
	;; 			   (eq (ispell-process-status) 'run))
	;; 	      :help "Terminate Ispell subprocess"))
	;; --
	;; (define-key ispell-menu-map [ispell-pdict-save]
	;;   '(menu-item "Save Dictionary"
	;; 	      (lambda () (interactive) (ispell-pdict-save t t))
	;; 	      :help "Save personal dictionary"))
	;; --
	(define-key ispell-menu-map [ispell-customize]
	  '(menu-item "Customize..."
		      (lambda () (interactive) (customize-group 'ispell))
		      :help "Customize spell checking options"))
	;; --
	(define-key ispell-menu-map [ispell-help]
	  ;; use (x-popup-menu last-nonmenu-event(list "" ispell-help-list)) ?
	  '(menu-item "Help"
		      (lambda () (interactive) (describe-function 'ispell-help))
		      :help "Show standard Ispell keybindings and commands"))
	;; --
	(define-key ispell-menu-map [flyspell-mode]
	  '(menu-item "Automatic spell checking (Flyspell)"
		      flyspell-mode
		      :help "Check spelling while you edit the text"
		      :button (:toggle . (and (boundp 'flyspell-mode)
					      flyspell-mode))))
	;; --
	(define-key ispell-menu-map [ispell-complete-word]
	  '(menu-item "Complete Word" ispell-complete-word
		      :help "Complete word at cursor using dictionary"))
	;; --
	(define-key ispell-menu-map [ispell-complete-word-interior-frag]
	  '(menu-item "Complete Word Fragment" ispell-complete-word-interior-frag
		      :help "Complete word fragment at cursor"))))
  
  (if ispell-menu-map-needed
      (progn
	;; (define-key ispell-menu-map [ispell-continue]
	;;   '(menu-item "Continue Spell-Checking" ispell-continue
	;; 	      :enable (and (boundp 'ispell-region-end)
	;; 			 (marker-position ispell-region-end)
	;; 			 (equal (marker-buffer ispell-region-end)
	;; 				(current-buffer)))
	;; 	      :help "Continue spell checking last region"))
	;; --
	(define-key ispell-menu-map [ispell-word]
	  '(menu-item "Spell-Check Word" ispell-word
		      :help "Spell-check word at cursor"))
	;; --
	(define-key ispell-menu-map [ispell-comments-and-strings]
	  '(menu-item "Spell-Check Comments" ispell-comments-and-strings
		      :help "Spell-check only comments and strings"))))
  
  
  (if ispell-menu-map-needed
      (progn
	(define-key ispell-menu-map [ispell-region]
	  '(menu-item "Spell-Check Region" ispell-region
		      :enable mark-active
		      :help "Spell-check text in marked region"))
	(define-key ispell-menu-map [ispell-message]
	  '(menu-item "Spell-Check Message" ispell-message
		      :visible (eq major-mode 'mail-mode)
		      :help "Skip headers and included message text"))
	(define-key ispell-menu-map [ispell-buffer]
	  '(menu-item "Spell-Check Buffer" ispell-buffer
		      :help "Check spelling of selected buffer"))
	;;(put 'ispell-region 'menu-enable 'mark-active)
	(fset 'ispell-menu-map (symbol-value 'ispell-menu-map))))
  
  (if (and (featurep 'xemacs)
	   (featurep 'menubar)
	   ;;(null ispell-menu-xemacs)
	   (not (and (boundp 'infodock-version) infodock-version)))
      (let ((dicts mylist)
	    (current-menubar (or current-menubar default-menubar))
	    (menu
	     '(["Help"		(describe-function 'ispell-help) t]
		;;["Help"		(popup-menu ispell-help-list)	t]
		["Check Message" ispell-message (eq major-mode 'mail-mode)]
		["Check Buffer"	ispell-buffer			t]
		["Check Comments"	ispell-comments-and-strings	t]
		["Check Word"	ispell-word			t]
		["Check Region"	ispell-region  (or (not zmacs-regions) (mark))]
		;; ["Continue Check"	ispell-continue			t]
		["Complete Word Frag"ispell-complete-word-interior-frag t]
		["Complete Word"	ispell-complete-word		t]
		;; ["Kill Process"	ispell-kill-ispell		t]
		["Customize..."	(customize-group 'ispell)	t]
		;; flyspell-mode may not be bound...
		["flyspell"	flyspell-mode
		:style toggle :selected flyspell-mode ]
		"-"
		;; ["Save Personal Dict"(ispell-pdict-save t t)	t]
		["Change Dictionary" ispell-change-dictionary	t])))
	(if (null dicts)
	    (setq dicts (cons "default" nil)))
	(dolist (name dicts)
	  (setq menu (append menu
			     (list
			      (vector
			       (concat "Select " (capitalize name))
			       (list 'ispell-change-dictionary name)
			       t)))))
	(setq ispell-menu-xemacs menu)
	(if current-menubar
	    (progn
	      (if (car (find-menu-item current-menubar '("Cmds")))
		  (progn
		    ;; XEmacs 21.2
		    (delete-menu-item '("Cmds" "Spell-Check"))
		    (add-menu '("Cmds") "Spell-Check" ispell-menu-xemacs))
	      ;; previous
		(delete-menu-item '("Edit" "Spell")) ; in case already defined
		(add-menu '("Edit") "Spell" ispell-menu-xemacs))))))
  
  )

(defun debian-ispell-set-startup-menu ()
  "Make sure ispell startup menu is ready after startup.
To be run at 'after-init-hook"
  (let ((mylist (append (mapcar 'car ispell-local-dictionary-alist)
			debian-ispell-valid-dictionary-list)))
    (if (featurep 'ispell)
	(message "ispell.el is already loaded")
      (when (fboundp 'debian-ispell-build-startup-menu)
	(debian-ispell-build-startup-menu)
	(fmakunbound 'debian-ispell-build-startup-menu)
	))))
  
(add-hook 'after-init-hook 'debian-ispell-set-startup-menu)

;;; -----------------------------------------------------------------------
;;;  Guess default ispell dictionary under emacs and make ispell.el use it
;;; -----------------------------------------------------------------------

(defvar debian-ispell-dictionary 
  nil
  "The name of the ispell dictionary that will become the default after
loading of ispell.el.")

;; Load the file containing the default value for debian-ispell-dictionary

(if (file-exists-p "/var/cache/dictionaries-common/emacsen-ispell-default.el")
    (load "/var/cache/dictionaries-common/emacsen-ispell-default.el"))

;;; ----------------

(defvar debian-aspell-dictionary 
  nil
  "The name of the aspell dictionary that will become the default after
loading of ispell.el.")

(defvar debian-aspell-equivs-alist 
  '((nil . nil))
  "Alist of equivalences between locales and aspell dictionaries,
used internally by the debian ispell.el initialization scheme.
Do not change this variable directly. It is autogenerated 
from data supplied by aspell dictionaries maintainers")

;;; -------------
;;; Guess emacsen entry for aspell after LANG or other envvar
;;; Intended to be called from /var/cache/emacsen-ispell-dicts.el
;;; to set debian-aspell-dictionary if possible
;;; ---------------

(defun debian-get-aspell-default ()
  "Guess emacsen entry associated to the given aspell lang option
value. Will try calling <aspell config lang> for this and return
nil in case of error or no match be found"
  (let (prefixes
	(suffixes '("^" "@" "_"))
	debian-aspell-default
	(lang (condition-case ()
		  (with-temp-buffer
		    (call-process "aspell" nil t nil "config" "lang")
		    (car (split-string (buffer-string))))
		(error nil))))
    ;; (message "aspell-lang: %s" lang)
    (if lang
	(progn
	  (setq lang (car (split-string lang ":")))
	  (catch 'tag
	    (while suffixes
	      (setq prefixes '("" "1:"))
	      (while prefixes
		(if (setq debian-aspell-default
			  (cdr (assoc (concat (car prefixes) 
					      (car (split-string lang (car suffixes))))
				      debian-aspell-equivs-alist)))
		    (throw 'tag (car debian-aspell-default)))
		;;
		(setq prefixes (cdr prefixes))
		)
	      (setq suffixes (cdr suffixes))
	      )
	    ))
      nil)
    )
  )

;;; --------------

;;; Autoselection is currently unused, so no need to have this as a real defcustom

;;; (defcustom debian-ispell-program-name-noauto nil
;;;   "*Do not try to guess spellchecker after values registered by 
;;; Debian dict packages and user settings"
;;;   :type 'boolean
;;;   :group 'ispell)

(if (not (boundp 'debian-ispell-program-name-noauto))
    (setq debian-ispell-program-name-noauto nil))

(defun debian-set-ispell-dictionary ()
  "Set ispell default to the debconf selected one if ispell-program-name is
ispell or, when ispell-program-name is aspell, to the value guessed after
LANG if any."
  (let (debian-ispell-prefer-aspell guessed-ispell-program-name)
    
    ; Set debian-ispell-prefer-aspell 
    
    (setq debian-ispell-prefer-aspell
	  (if (or (string-equal ispell-program-name "aspell")
		  (and (boundp 'ispell-prefer-aspell)
		       ispell-prefer-aspell))
	      t
	    nil))

    ; Set value of ispell-local-dictionary if nil.
    
;;; (if (not ispell-local-dictionary)
;;;	(if (and debian-ispell-prefer-aspell debian-aspell-dictionary)
;;;	    (setq ispell-local-dictionary debian-aspell-dictionary)
;;;	  (if debian-ispell-dictionary
;;;	      (setq ispell-local-dictionary debian-ispell-dictionary)))) 
    
    (if (not ispell-local-dictionary)
	(if debian-ispell-prefer-aspell
	    (if debian-aspell-dictionary
		(setq ispell-local-dictionary debian-aspell-dictionary))
	  (if debian-ispell-dictionary
	      (setq ispell-local-dictionary debian-ispell-dictionary))))
    
    ; Look into the spellcheckers availability alist for that dictionary

    (setq guessed-ispell-program-name
	  (cdr (assoc ispell-local-dictionary debian-ispell-availability-alist)))
    
    (setq debian-ispell-program-name
	  (if (or debian-ispell-program-name-noauto
		  (not guessed-ispell-program-name))
	      ispell-program-name
	    (if (string-equal guessed-ispell-program-name "all")
		(if debian-ispell-prefer-aspell
		    "aspell"
		  "ispell")
	      guessed-ispell-program-name)))
    
    ; The debugging output if required
    
    (if debian-dict-common-debug
	(message "- dictionaries DID:%s, DAD:%s, DIPA:%s, ILD:%s, IPN:%s, DIPN:%s" 
		 debian-ispell-dictionary 
		 debian-aspell-dictionary
		 debian-ispell-prefer-aspell
		 ispell-local-dictionary
		 ispell-program-name
		 debian-ispell-program-name))
    )) ;; let and defun ends

;;; ---------------------------------------------------------------------------
;;;   Make sure patched ispell.el is first in the loadpath if not already there
;;; ---------------------------------------------------------------------------

(let ((mypath (concat "/usr/share/" 
		      (symbol-name debian-emacs-flavor) 
		      "/site-lisp/dictionaries-common")))
  (unless (member mypath load-path)
    (debian-pkg-add-load-path-item mypath)))

;;; --------------------------------------------------------------------------
;;; A home made exec-installed-p to test for {i,a}spell executable existence
;;; Implemented here to avoid apel dependency. Will be nulled at the end
;;; --------------------------------------------------------------------------

(defun debiandc-exec-installed-p (infile)
  "Checking for an executable file in the exec-path. 
Implemented here to avoid apel dependency. This is much much simpler,
we do not need all that apel's portability. Internal use funcion."
  (let (file paths)
    (setq paths exec-path)
    (catch 'tag
      (while paths
	(setq file (expand-file-name infile (car paths)))
	(if (file-executable-p file)
	    (throw 'tag t)
	  )
	(setq paths (cdr paths))
	)
      )
    )
  )

;; Fallback to aspell if ispell is not present.
;; Will be overriden by ~/.emacs selection if present

(if (not (debiandc-exec-installed-p "ispell"))
    (setq ispell-program-name "aspell"))

(fmakunbound 'debiandc-exec-installed-p)

;;; -----------------------------------------------------------------------

;; Local Variables:
;; mode: lisp
;; End:
