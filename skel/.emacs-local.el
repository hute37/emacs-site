;;; .emacs-local.el --- local customization module in ~/.emacs config -*- lexical-binding: t; -*-

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
(custom-set-variables
 
 '(z-use-helm nil)
 '(z-use-pdf-tools t)
 '(z-use-py-jupyter t)
 ;; '(z-var-global-bibliography '("~/work/bv/box-up/notes/ref/references.bib"))
 '(z-var-global-bibliography '("~/work/vs/dve-sample-py/notes/ref/references.bib"))
 '(z-var-denote-directory "~/work/vs/dve-sample-py/notes/nat")
 ;;'(z-var-denote-directory "~/work/bv/box-up/notes/nat")
 )
 
