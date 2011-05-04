;;; backups.el -- Save all backups in a single directory.
;;
;; Normally backups are stored in the same directory as the original file
;; with a ~ appended to it.  I personally find it very annoying to have
;; backup files allways appearing in my directory.  Often I find myself
;; deleting the files just before realizing I need them.
;;
;; This module is intended for the best of both worlds.  Instead of saving
;; every file in the current directory, it saves it in another directory.
;; That way, if you really need the backup file, you can find it, but when
;; you don't need it, it is out of the way.  It also makes it easier to
;; purge all your backups when you want to.
;;
;; To use this package, put code like this in your .emacs file:
;;
;; (require 'backups)
;; (move-backups t)
;;
;; By default, all backups are moved to the directory "~/.backups/emacs/".  To
;; change this, set the variable "backup-directory" like this:
;;
;; (setq backup-directory "~/bac")
;;
;; One thing to watch for, since all backups go to the same directory, it
;; is easy for two files to overwrite each other.  I do not see this as
;; too much of a problem because the last one used will be the one available.
;; If this is becomes a big problem, it should not be too hard to edit this
;; to make the backup files more unique.
;;

(defvar backup-directory "~/.backups/emacs/"
  "*If move backups is on, this is the directory all backup files are saved in."
  )

(fset 'orthodox-make-backup-file-name
      (symbol-function 'make-backup-file-name))

(defun file-to-backup-dir (file)
  "
Intended for \(fset 'make-backup-file-name 'file-to-backup-dir\).
The function \"move-backups\" does this for you.
"
  (if (string-match "[/\\]$" backup-directory)
      nil
    (setq backup-directory (concat backup-directory "/")))
  (concat backup-directory (file-name-nondirectory file) "~")
  )

(defun move-backups-on ()
  "Returns t if backups are being moved, nil otherwise."
  (eq (symbol-function 'make-backup-file-name) 'file-to-backup-dir))

(defun move-backups (&optional arg)
  "
Turns on and off the behavior of moving backups to their own separate directory.
If the argument is nil, backups are toggled.  Otherwise, backups are turned on
if the numeric value of the arguemnt is greater than 0.

If backups are being moved, they go to the directory specified in
\"backup-directory\".
"
  (interactive "P")
  (let ((move-on (if (null arg)
		     (not (move-backups-on))
		   (> (prefix-numeric-value arg) 0)))
	)
    (if move-on
	(progn (fset 'make-backup-file-name 'file-to-backup-dir)
	       (message "Saving all backups in %s." backup-directory)
	       )
      (fset 'make-backup-file-name 'orthodox-make-backup-file-name)
      (message "Saving all backups in current directory.")
      )
    )
  )

;;
;; Programmer's note:
;;
;; If you want emacs to recognize something that does not end with a ~ as
;; a backup, fset backup-file-name-p to a function that returns true if
;; its argument is a string that points to a backup file.  Instead of
;; changing this, I kept the ~ convention with all backups stored in that
;; backup directory.  This was not only easy, but makes it possible to
;; switch to normal backing up without confusing things.  Also, dired.el
;; assumes backups end with ~ for speed purposes.
;;

(provide 'backups)
