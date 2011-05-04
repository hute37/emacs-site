;; @(#) emacs-rc-bup.el -- Emacs rc file for custom backup settings
;; @(#) $Id: emacs-rc-bup.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;{{{  Documentation
;;; Documentation:
;;
;;  File id
;;
;;	Copyright (C) 1996-2001 Jari Aalto
;;	Author:       Jari Aalto <jari.aalto@poboxes.com>
;;	Maintainer:   Jari Aalto <jari.aalto@poboxes.com>
;;	Created:      1996
;;	keywords:     tools
;;
;;	This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that.. Just rename this file to whatever is shown
;;      in the first line.
;;
;;      The file layout is managed with tinytab.el, tinybm.el, folding.el
;;
;;  Installation
;;
;;	Put following entries entries in your .emacs
;;
;;	    (defconst my-:backup-dir "~/tmp/Autosaved" ) ;; your choice
;;	    (defconst my-:backup-os 'dos)		 ;; if you use old DOS
;;	    (load "emacs-rc-bup")
;;
;;	Exit current emacs and start fresh one. New backup is now in
;;	effect. Alternatively execute `M-x' `load-library' emacs-rc-bup.el
;;
;;  Description
;;
;;	Loading this file allows you to put all your backups in one directory.
;;	Step through the file and make your personal modifications before
;;	using this file.
;;
;;  For simpler use
;;
;;	    (defun make-backup-file-name (file)
;;	      (concat "~/tmp/Autosaved/" (file-name-nondirectory file)))
;;
;;	And forget this emacs init file alltogether. The two line lisp
;;	code puts all your files in the directory ~/tmp/Autosaved/
;;
;;  Post that I saw concerning backup
;;
;;     1995-12-18 Kevin Rodgers <kevinr@ihs.com> gnu.emacs.help
;;
;;      .>I would like to be able to have % (percent) character appended
;;      .>to my filenames for backup files, instead of the ~ (tilde) to be
;;      .>the same as those for the X editor on our system.  There appear
;;      .>to be about 15 lines in the files.el file that would need
;;      .>changing to do this.
;;
;;	Supposedly, redefining `make-backup-file-name' and
;;	`backup-file-name-p' will do the trick:
;;
;;	    (defun make-backup-file-name (file)
;;	      (concat file "%"))
;;
;;	    (defun backup-file-name-p (file)
;;            (char-equal (aref file (1- (length file))) ?%))
;;
;;     	But not entirely. There is also a function on dired.el which marks
;;     	backup files for deletion. Currently, it only looks for files which
;;     	end with a `~', and passes only those files to `backup-file-name-p'
;;     	for final verification. The comments therein say that this is for
;;     	efficiency reasons. I think the code there should be made more
;;     	general, so that if you change the default backup filename
;;     	template, you won't need changing the code there, either.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ docs

;;; .......................................................... Preface ...

;;}}}

(require 'cl)

(eval-when-compile
  (require 'advice))

(eval-and-compile
  (autoload 'win32-p "tinyliba"))


;;{{{ setup: Emacs variables

;;; .................................................... &var-settings ...
;;; You should definitely use following commands, to learn more about
;;; backup and variables:
;;;
;;;	C-h v VARIABLE
;;;	M-x apropos RET backup RET

(setq auto-save-default		t)	;I want auto-save!
(setq delete-auto-save-files    t)	;kill htem when I save a file
(setq auto-save-interval	100)	;50 char save interval
(setq auto-save-list-file-name  nil)	;do not write this list
(setq auto-save-timeout		30)	;30sec inactivity triggers auto-save

;;  Make a backup of a file the first time it is saved.

(setq make-backup-files		t)	;backups, yes!
(setq backup-inhibited		nil)	;..this overrides all. Gimme BackUp's

;;  See docs if `backup-by-copying' was set to nil
;;
;;  `backup-by-copying-when-linked'
;;  `backup-by-copying-when-mismatch'

(setq backup-by-copying		t)      ;to preserve UGO rights

;;  Install Our handler

(setq backup-enable-predicate 'my-backup-enable-predicate-check)


;;}}}
;;{{{ setup: user variables

;;; ........................................... &private-user-settings ...

(defvar my-:backup-dir
  (let ((tmp (or  (getenv "TEMPDIR")
		  (getenv "TMPDIR")))
	ret
	)
    (dolist (try (list "~/tmp/Autosaved"
		       "~/tmp"
		       "/tmp"
		       tmp
		       ))
      (when (and (stringp try)
		 (file-directory-p try))
	(setq ret try)
	(return)))
    (or ret
	(error "My: can't set value for `my-:backup-dir'")))
  "*Directory where to put emacs temporary files.
Make sure there is ending slash in the directory name.")


(defvar my-:backup-ext "~"		;I'll just stick to this..
  "*Extension used for backup files.")


(defvar my-:backup-no-regexp
  (concat
   "\\.\\(Z\\|gz\\|tar\\|tgz\\|zip\\|jpg\\|gif\\)$\\|"

   ;; "/tmp/" matches global /tmp/.. and my local ~/tmp/..
   ;;
   "/tmp/\\|"

   ;; Ignore Gnus nnfolder and archive files from backup
   ;;
   "/Mail/\\|/News/\\|newsrc|SCORE|ADAPT"
   )
  "*Which files not to backup. Set to nil if you want to backup all.")


(defvar my-:backup-os 'unix
  "*Operating system for backup.
Valid values are 'unix and 'dos, where 'dos limits file extension to 3 chars")


;;}}}

;;{{{ extra backup

;;; .................................................... &extra-backup ...

(defconst my-:backup-when-kill-buffer-table
  '(
    ".cc\\|.h\\|.el"
    ".bak"
    "~/Autosaved/"
    )
  "*When buffer is killed, should we make a \"file closed\" backup.
Notice, that if you use directory option, the files may not have unique
names, so the old .bak file is always overwritten.

 dir1/test.cc    --> ~/Autosaved/test.cc.bak
 dir2/test.cc    --> ~/Autosaved/test.cc.bak

Variable content is list:
 '(REGEXP-of-files-to-backup)
   ADDED-BACKUP-EXTENSION
   [DIRECTORY-WHERE-TO-SAVE, default to current dir]
  )")


;;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. setting  ..

(remove-hook 'kill-buffer-query-functions	          ;; Emacs 19.28 +
	     'my-kill-buffer-query-function)

(add-hook 'find-file-hooks 'my-maybe-inhibit-backup)


(when (locate-library "auto-save")
  (when (and (boundp 'auto-save-hash-p)	;PC Windows emacs, auto-save 1.26
	     (win32-p))
    (defconst auto-save-hash-p t)
    (defconst auto-save-directory my-:backup-dir)

    ;;   Is this good idea after all? If autosave fails, where it
    ;;   should write the backup then?

    (defconst auto-save-directory-fallback auto-save-directory)

    (if (not (featurep 'auto-save))
	(load "auto-save" 'noerr))
    ))

;;; .............................................. &extra-backup-funcs ...

(defun my-kill-buffer-query-function ()
  "Make backup when buffer is killed.
Reference:
  `my-:backup-when-kill-buffer-table'"
  (let* ((case-fold-search	t)
	 (list			my-:backup-when-kill-buffer-table)
	 (len			(length list))
	 (file			(buffer-file-name))
	 re ext dir
	 dest
	 mode
         )
    (when file

      (cond
       ((> len 1)
	(setq re  (nth 0 list)
	      ext (nth 1 list)
	      dir (if (eq len 3)
		      (nth 2 list)
		    (file-name-directory file))
	      dest (concat dir (file-name-nondirectory file) ext)
	      ))
       (t
	(error "Not enough parameters in LIST.")))

      (if (not (file-exists-p dir))
	  (error "No such backup dir: %s" dir))

      (when (string-match re file)
	;;    Make sure file is writable, raise +w
	(setq mode (logior (file-modes dest) 128))
	(set-file-modes dest mode)
	(if (file-exists-p dest)	;Copy barfs otw
	    (delete-file dest))
	(copy-file file dest))
      )
    t					;Make sure we return this
    ))


(defun my-maybe-inhibit-backup  ()
  "Disable backup for buffers matching `my-:backup-no-regexp'"
  (interactive)
  (if (not (funcall backup-enable-predicate buffer-file-name))
      (progn
	(make-local-variable 'backup-inhibited)
	(setq backup-inhibited t)))
  nil					;Hook return value
  )

;;}}}
;;{{{ functions

;;; ............................................. &overriden-functions ...
;;; - These allows you to say where you want the backup files to go, how to
;;;   name them, and which files not to backup...
;;; - Functions override emacs default functions. See `files.el'


(defun my-backup-enable-predicate-check (name)
  "Decides which buffers (not) to back-up. NAME is filename."
  (if (and (stringp name)
	   (string-match
	    (or my-:backup-no-regexp
		"##No-Match-here")
	    name))
      nil t))


;; 1996-03-21 Tim Anderson <taa@segue.com> comp.emacs
;; Adjustments by Jari

(defun my-dos-make-backup-file-name (file)
  (let ((ext  my-:backup-ext)
	fnse
        suf nondir dir
	)

    ;;  Get NAME without extension

    (cond
     ((fboundp 'file-name-sans-extension) ;Not in 19.28
      ;;  This hack quiets byte compiler (that function does not exist)
      (let* ((func 'file-name-sans-extension))
	(setq fnse (funcall func file))))
     (t
      (if (not (string-match ".*\\." file))
	  (setq fnse file)
	(setq fnse (substring file 0 (1- (match-end 0))))
	)))


    ;;  How should we format the extension?

    (cond
     ((< (length fnse) (length file))
      (setq suf (substring file (+ (length fnse) 1)))

      (cond ((< (length suf) 3)
	     (setq suf (concat suf ext)))
	    (t
	     (setq suf (concat (substring suf 0 2) ext))))
      (concat fnse "." suf)
      )

     (t
      (setq nondir (file-name-nondirectory file))
      (cond
       ((< (length nondir) 8)
	(concat file ext))
       ((setq dir (file-name-directory file))
	(concat dir (substring nondir 0 7) ext))
       (t
	(concat (substring nondir 0 7) ext))
       ))

     )))


;;; ....................................................... &defadvice ...

(defadvice make-backup-file-name (around my-replace act)
  "Replace original function. See variables starting my-:backup-*."
  ;;  default backup directory (better to be absolute when you su)
  (let* ((os  my-:backup-os))
    (setq ad-return-value
	  (cond
	   ((eq os 'unix)
	    (concat (file-name-as-directory my-:backup-dir)
		    (file-name-nondirectory file)
		    my-:backup-ext))
	   ((eq os 'dos)
	    (my-dos-make-backup-file-name file))
	   ))))


(defadvice backup-file-name-p (around my-replace act)
  "Replace original function. See variables starting my-:backup-*."
  ;;   returns true if file is potentially a backup
  (setq ad-return-value
	(string-match (concat (regexp-quote my-:backup-ext) "$") file)))



;;}}}


(provide 'emacs-rc-bup)

;;; emacs-rc-bup.el ends here
