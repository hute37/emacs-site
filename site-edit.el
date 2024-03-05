;;; site-edit.el --- editor features customization in ~/.emacs config

;; Author: ht37 <hute37@gmail.com>
;; URL: https://github.com/hute37/emacs-site

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; editing defaults

;;; Code:

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
)




;;;////////////////////////////////////////////////////////////////
;;;  @FEATURES
;;;///////////////////////////////////////////////////////////////
(message "SITE:FEATURES")


;; ---( Session )--------------------------------------------------------

;;@TODO: verify session setup ...
;;(require 'session)
;;(session-initialize)

;; @see: http://pages.sachachua.com/.emacs.d/Sacha.html


;; ---( Encoding )--------------------------------------------------------

;; @see: https://gitlab.com/balajisi/emacs/blob/master/init.el

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)


(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; ---( Backup )--------------------------------------------------------


;; Save all tempfiles in $TMPDIR/emacs$UID/ -- defconsts
(setq emacs-bck-dir "~/.backups/emacs")

(make-directory emacs-bck-dir t)

(setq backup-directory-alist '(("" . "~/.backups/emacs")))

;;(setq auto-save-file-name-transforms
;;      `((".*" emacs-bck-dir t)))
;;(setq auto-save-list-file-prefix
;;      emacs-bck-dir)


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

;;;; Replace "yes or no" with y or n
;;(defun yes-or-no-p (arg)
;;  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
;;  (y-or-n-p arg))

(fset 'yes-or-no-p 'y-or-n-p)

;; ---( site.edit: end )-------------------------------------------------------
(message "SITE:EDIT - end")

(provide 'site-edit)
;;; site-edit.el ends here
