;;; site-pref.el --- preferences  -*- lexical-binding: t; -*-

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
(message "SITE:PREF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7prefs nil
  "H7 profile customization."
  :group 'convenience)

(defcustom z-use-helm nil
  "Non-nil means to activate  'helm'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-ivy nil
  "Non-nil means to activate  'ivy'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-vertico nil
  "Non-nil means to activate  'vertico'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-pdf-tools nil
  "Non-nil means to activate  'pdf-tools' instead of 'docview'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-py-jupyter nil
  "Non-nil means to activate  'jupyter'. 'zmq' binary module required, see note on zmq package."
  :type 'boolean
  :group 'h7prefs)


;; ---( ... )--------------------------------------------------------------

(defun h7/use-helm ()
 z-use-helm)

(defun h7/use-ivy ()
 z-use-ivy)

(defun h7/use-vertico ()
 z-use-vertico)


;; ---( ... )--------------------------------------------------------------

(defun h7/use-pdf-tools ()
 z-use-pdf-tools)

(defun h7/use-pdf-docview ()
 (not z-use-pdf-tools))

(defun h7/use-py-jupyter ()
 z-use-py-jupyter)


;; ---( site.func: end )-------------------------------------------------------
(message "SITE:PREF - end")

(provide 'site-pref)
;;; site-pref.el ends here
