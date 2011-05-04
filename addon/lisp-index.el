;; lisp-index.el -- Generate a HTML index of all lisp files in a directory.

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Keywords: tools, lisp, data, convenience
;; Revision: 0.3

;; This file is [not yet] part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;  This package allows you to generate a HTML index of the
;; Lisp files in, and below, a particular directory.
;;
;;  The generated index will contain the name of the file,
;; and the description extracted from the file.
;;
;;  Originally this started out life as a Perl script that
;; do the job - but I soon realised that Emacs should be
;; used if I was going to distribute this publically ;)
;;

;;; Usage:
;;
;; * Put lisp-index.el in your emacs' site-lisp directory, or your
;;   home directory, and optionally, byte-compile it.
;;
;; * Place the following lines in your .emacs file:
;;
;;  (autoload 'lisp-index-directory "lisp-index" "Index lisp files" t)
;;
;; * To use it after that use:
;;
;;  M-x lisp-index-directory
;;   Will invoke the indexer, and prompt you for a directory.
;;
;;  Steve Kemp <skx@tardis.ed.ac.uk>
;;
;;  [new!] http://www.gnusoftware.com/


;;; History:
;;
;;  0.1  Initial implementation in Perl
;;  0.2  Lisp implementation.
;;  0.3  Made the inclusion of Links optional
;;       Ignore files that start with "."
;;       Attempt to trim description.
;;

;;; Code:
(defvar lisp-file-regexp ".*\\.el$"
  "A regexp that matches Lisp files.")

(defvar lisp-index-buffer "Index"
  "The name of the buffer in which to store the Lisp index.")

(defvar lisp-index-insert-links t
  "Controls whether we should include links to the files in
the index")


;;;###autoload
(defun lisp-index-directory (directory)
  "Create a HTML index of all the files in a directory.
This function will generate a single HTML table that contains
the names, and descriptions of all the files in a directory."
  (interactive "DStarting Directory: ")

  (kill-buffer (get-buffer-create lisp-index-buffer))
  (set-buffer (get-buffer-create lisp-index-buffer))
  (insert (concat "<HTML>\n<HEAD>\n<TITLE>Lisp index of " directory))
  (insert "</TITLE>\n</HEAD>\n<BODY>\n<P>\n<CENTER>\n")
  (insert "<TABLE WIDTH=75%>\n")

  ;; Make sure directory has trailing seperator, shouldn't be
  ;; a problem with interactive use.
  (if (string-match (concat ".*[\\/\\\\]$") directory)
      ()
    (setq directory (concat directory "/")))

  (lisp-process-directory directory 'lisp-index-function)

  (set-buffer (get-buffer-create lisp-index-buffer))
  (insert "</table>\n</body>\n</html>")
  (pop-to-buffer (get-buffer-create lisp-index-buffer)))


(defun lisp-process-directory (directory function)
  "Apply a function on all Lisp files in a directory, recursively.
Files are considered to be Lisp files if they match the `lisp-file-regexp'."
  (interactive)
  (message "Entering directory %s" directory)

  ;; Get files
  (let ((file-list (directory-files directory))
	(dir-list nil)
	(file nil)
	(dir nil))
    (setq dir-list file-list)
    (while file-list
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (if (file-directory-p (concat directory file))
	  ()
	(if (and
	     (string-match lisp-file-regexp file)
	     (not (equal 0 (string-match "\\." file))))
	    (funcall function (concat directory file)))))
    (while dir-list
      (setq dir (car dir-list))
      (setq dir-list (cdr dir-list))
      (if (and
	   (file-directory-p (concat directory dir))
	   (not (equal 0 (string-match "\\." dir))))
	  (lisp-process-directory (concat directory dir "/" ) function)))))

(defun lisp-index-function (fileName )
  "Insert the name of the found Lisp files into the index buffer."
  (let ((desc nil))
    (with-temp-buffer
      (insert-file fileName)
      (goto-char (point-min))
      (forward-line 1)
      (backward-char 1)
      (setq desc (buffer-substring 1 (point))))
    (set-buffer (get-buffer-create lisp-index-buffer))
    (if (string-match "--" desc)
	(setq desc (substring desc (+ (string-match "--" desc) 3))))
    (insert "<tr><td>")
    (if lisp-index-insert-links 
	(insert (concat "<a href=\"" fileName "\">" fileName "</a>"))
      (insert fileName))
    (insert "</td><td>")
    (insert desc)
    (insert "</td></tr>\n")))

(provide 'lisp-index)

;;; lisp-index.el ends here

