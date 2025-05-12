;;; site-user.el --- user preferences  -*- lexical-binding: t; -*-

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


;; ---( site.user: begin )-------------------------------------------------------
(message "SITE:USER - begin")

;; ---( ... )--------------------------------------------------------------

(setq user-full-name "ht37"
      user-mail-address "hute37@gmail.com")

;; ---( site.user: end )-------------------------------------------------------
(message "SITE:USER - end")

(provide 'site-user)
;;; site-user.el ends here
