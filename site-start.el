;;; .emacs-start.el --- init entry in ~/.emacs config -*- lexical-binding: t; -*-

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

;; local custom config variables
;; override in ~/.emacs or in ~/.emacs-local.el

;;; Code:
;; ---( site.init: begin )-------------------------------------------------------
(message "SITE:begin")


;; ---( logs )--------------------------------------------------------

(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;; ---( path )--------------------------------------------------------

(setq emacs-site-path (concat (getenv "HOME") "/.emacs-site/")) ;; ~/.emacs-site/

(defun emacs-site (filename)
"Expand FILENAME relative to ~/.emacs-site/ directory."
(expand-file-name filename emacs-site-path))

(defun emacs-home (filename)
"Expand FILENAME relative to ~/. directory."
(expand-file-name filename (getenv "HOME")))

(defun emacs-d (filename)
"Expand FILENAME relative to `user-emacs-directory'."
(expand-file-name filename user-emacs-directory))

;; ---( init )--------------------------------------------------------

(cond
 ((string-lessp emacs-version "24.3") ;; 
  (progn
    (message "SITE:local, ...")
    (load (emacs-site "site-local"))   ;; maint/local
    (message "SITE:local.")
    ))
 (t
  (progn
    (message "SITE:boot, ...")
    (load (emacs-site "site-boot"))   ;; master/package
    (message "SITE:boot.")
    ))
)

;; ---( site.init: end )-------------------------------------------------------
(message "SITE:end")

(provide 'site-start)
;;; site-start.el ends here
