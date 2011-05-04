;;; search-citeseer.el --- Helps searching Citeseer
;;; (http://citeseer.nj.nec.com/cs) for BibTeX entries
     
;; Copyright (C) 2002 Rodrigo Araujo Real
     
;; Author: Rodrigo Araujo Real <rreal@inf.ufrgs.br>
;; Maintainer: Rodrigo Araujo Real <rreal@inf.ufrgs.br>
;; Created: 14 Aug 2002
;; Version: 0.1
;; Keywords: bibtex bibliography
;; Requires: search-citeseer perl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.    
;;
;; This file is neither part of XEmacs nor GNU Emacs. 

;;; Commentary:
;; 
;; To perform a search, you should do:
;; M-x search-citeseer
;; and follow the instructions
;;
;; Contributed to this source code:
;; Mario Domenech Goulart

(defvar search-citeseer-pathname "search-citeseer"
  "*Pathname of bib executable")

(defvar search-citeseer-buffer-name "*search-citeseer*"
  "Name of the buffer to display BibTeX results.")

(defun search-citeseer (query)
  (interactive "sSearch Expression: ")
  (start-process "search-citeseer" search-citeseer-buffer-name search-citeseer-pathname query)
  (setq orig-size (window-height))
  (set-buffer search-citeseer-buffer-name)
  (bibtex-mode)
  (goto-char (point-max))
  (setq buffer-read-only t)
  (pop-to-buffer search-citeseer-buffer-name)
  (shrink-window (- (window-height) (/ orig-size 3)))
)
