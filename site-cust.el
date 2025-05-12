;;; site-cust.el --- customization vars  -*- lexical-binding: t; -*-

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

;; legacy keymaps, after site-pkgs.org key binding
;; CUA mappings enabled

;;; Code:


;; ---( site.func: begin )-------------------------------------------------------
(message "SITE:CUST - begin")

;; ---( ... )--------------------------------------------------------------


;; @see: https://github.com/ejmr/DotEmacs/blob/master/.emacs

;;; Now comes a long section of general, global settings, minor modes,
;;; display configuration, and so on.

;; (
;;  setq inhibit-startup-message t
;;       make-backup-files nil
;;       auto-save-default t
;;       auto-save-interval 50
;;       auto-save-timeout 5
;;       delete-auto-save-files t
;;       case-fold-search t
;;       tooltip-delay 1
;;       major-mode 'text-mode
;;       imenu-sort-function 'imenu--sort-by-name
;;       kill-read-only-ok t
;;       show-trailing-whitespace t
;;       size-indication-mode t
;;       read-quoted-char-radix 16
;;       line-move-visual nil
;;       initial-scratch-message nil
;;       delete-by-moving-to-trash t
;;       visible-bell nil
;;       save-interprogram-paste-before-kill t
;;       history-length 250
;;       tab-always-indent 'complete
;;       save-abbrevs nil
;;       select-active-region t
;;       shift-select-mode nil
;;       x-select-enable-clipboard t
;;       auto-hscroll-mode nil
;;       delete-active-region 'kill)
;; (setq scroll-preserve-screen-position 'always
;;       scroll-conservatively most-positive-fixnum
;;       scroll-step 0)


;; ---( tramp )---------------------------------------------------------

;; @see: https://www.emacswiki.org/emacs/TrampMode#toc8

(setq tramp-default-method "ssh")
(setq tramp-terminal-type "tramp")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; ---( banner )---------------------------------------------------------

;;
(setq initial-scratch-message nil)



;; ---( site.func: end )-------------------------------------------------------
(message "SITE:CUST - end")

(provide 'site-cust)
;;; site-cust.el ends here
