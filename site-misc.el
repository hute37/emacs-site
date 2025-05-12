;;; site-misc.el --- misc  -*- lexical-binding: t; -*-

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


;; ---( site.misc: begin )-------------------------------------------------------
(message "SITE:MISC - begin")

;; ---( ... )--------------------------------------------------------------

;; @see: https://github.com/ejmr/DotEmacs/blob/master/.emacs

;;; Setup registers for files I commonly edit.
;; (set-register ?e '(file . "~/.emacs"))
;; (set-register ?h '(file . "~/Dropbox/Todo/mine.org"))
;; (set-register ?t '(file . "~/Dropbox/Todo/todo.org"))
;; (set-register ?m '(file . "/home/eric/Temp/mail.md"))
;; (set-register ?j '(file . "/tmp/Entry.jrnl.txt"))




;; ---( site.misc: end )-------------------------------------------------------
(message "SITE:MISC - end")

(provide 'site-misc)
;;; site-misc.el ends here
