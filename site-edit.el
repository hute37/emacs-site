;; ---( site.view: begin )-------------------------------------------------------
(message "SITE:EDIT - begin")

;; ---( ... )--------------------------------------------------------------


;; @see: https://github.com/jwiegley/dot-emacs/blob/master/init.el

;;;_ , Enable disabled commands
(put 'downcase-region 'disabled nil) ; Let downcasing work
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil) ; Let ESC-ESC work
(put 'narrow-to-page 'disabled nil) ; Let narrowing work
(put 'narrow-to-region 'disabled nil) ; Let narrowing work
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil) ; Let upcasing work




(setq-default truncate-lines t)
(setq-default abbrev-mode 1)
(setq-default indent-tabs-mode nil)

(transient-mark-mode t)
(delete-selection-mode t)

(global-auto-revert-mode 1)

(custom-set-variables
 '(cua-mode t nil (cua-base))
)




;;;////////////////////////////////////////////////////////////////
;;;  @FEATURES
;;;////////////////////////////////////////////////////////////////
(message "SITE:FEATURES")


;; ---( Session )--------------------------------------------------------

;;@TODO: verify session setup ...
;;(require 'session)
;;(session-initialize)

;; @see: http://pages.sachachua.com/.emacs.d/Sacha.html


;; ---( Backup )--------------------------------------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; ---( Autosave )--------------------------------------------------------


(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ---( History )--------------------------------------------------------

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))




;; ---( util )---------------------------------------------------------

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))


;; ---( site.edit: end )-------------------------------------------------------
(message "SITE:EDIT - end")
