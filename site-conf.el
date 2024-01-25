;;; site-conf.el --- local customization module in ~/.emacs config

;; Author: Giovanni Pelosi <hute37@gmail.com>
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

;; local custom config variables
;; override in ~/.emacs or in ~/.emacs-local.el

;;; Code:

;; ---( site.conf: begin )------------------------------------------
(message "SITE:CONF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7config nil
  "H7 profile configuration."
  :group 'convenience)

;; ~/work/wo/wo-note/org/ref/references.bib
(defcustom z-var-global-bibliography '("~/Dropbox/Local/data/org/ref/references.bib")
  "Global BibLaTeX bibliography file."
  :type '(repeat string)
  :group 'h7config)


;; ~/work/wo/wo-note/org/net
(defcustom z-var-roam-directory "~/Dropbox/Local/data/org/net"
  "Roan default file."
  :type 'string
  :group 'h7config)

;; ~/work/wo/wo-note/org/nat
(defcustom z-var-denote-directory "~/Dropbox/Local/data/org/nat"
  "Roan default file."
  :type 'string
  :group 'h7config)



;; ---( ... )--------------------------------------------------------------

(defun h7/var-global-bibliography ()
 (list z-var-global-bibliography))


(defun h7/var-roam-directory ()
 z-var-roam-directory)

(defun h7/var-denote-directory ()
 z-var-denote-directory)



;; ---( site.conf: end )-------------------------------------------------------
(message "SITE:CONF - end")
