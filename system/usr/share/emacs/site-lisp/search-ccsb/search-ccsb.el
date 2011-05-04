;;; search-ccsb.el --- Helps searching the The Collection of
;; Computer Science Bibliographies provided by Alf-Christian Achilles
;; <achilles@ira.uka.de>
;; More information about the Bibliographies can be found at: 
;; http://liinwww.ira.uka.de/bibliography/index.html
     
;; Copyright (C) 2002 Rodrigo Araujo Real
     
;; Author: Rodrigo Araujo Real <rreal@inf.ufrgs.br>
;; Maintainer: Rodrigo Araujo Real <rreal@inf.ufrgs.br>
;; Created: 14 Aug 2002
;; Version: 0.1
;; Keywords: bibtex bibliography
;; Requires: search-ccsb perl

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
;; M-x search-ccsb
;; and follow the instructions
;;
;; Contributed to this source code:
;; Mario Domenech Goulart

(defvar search-ccsb-pathname "search-ccsb"
  "*Pathname of bib executable")

(defvar search-ccsb-buffer-name "*search-ccsb*"
  "Name of the buffer to display BibTeX results.")

(defun search-ccsb (query)
  (interactive "sSearch Expression: ")
  (start-process "search-ccsb" search-ccsb-buffer-name search-ccsb-pathname query)
  (setq orig-size (window-height))
  (set-buffer search-ccsb-buffer-name)
  (bibtex-mode)
  (goto-char (point-max))
  (setq buffer-read-only t)
  (pop-to-buffer search-ccsb-buffer-name)
  (shrink-window (- (window-height) (/ orig-size 3)))
)
