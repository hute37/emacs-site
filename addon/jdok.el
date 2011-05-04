;;; jdok.el -- Javadoc template generator
;; @(#) $Id: jdok.el,v 1.1 2005-11-26 01:15:01 hute37 Exp $

;; This file is not part of Emacs

;; Copyright (C) 1998 by David Ponce
;; Author:       David Ponce david.ponce@wanadoo.fr
;; Maintainer:   David Ponce david.ponce@wanadoo.fr
;; Created:      October 8 1998

;; LCD Archive Entry:
;; <el>|David Ponce|david.ponce@wanadoo.fr|
;; <docum>|
;; <date>|$Revision: 1.1 $|~/misc/|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:
;;
;;  This package automatically generates javadoc templates to help to document
;;  Java methods.

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add following into your
;;  ~/.emacs startup file
;;
;;      (require 'jdok)

;;; Usage:
;;
;;  M-x `jdok-generate-javadoc-template' or `C-cj'
;;     Inserts a javadoc template above the method signature at point. BEFORE
;;     EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST LINE OF
;;     THE METHOD DECLARATION. IF NOT RESULT IS UNCERTAIN.
;;
;;  In the following example, point is located at the beginning of the line,
;;  before the word `public' (but it could be anywhere on this line):
;;
;;  -|-  public
;;       void   myMethod( int  x,  int y )
;;         throws Exception
;;       {
;;         ...
;;
;;  M-x `jdok-generate-javadoc-template' will produce the following:
;;
;;       /**
;;        * Describe 'myMethod' method here.
;;        *
;;        * @param x a value of type 'int'
;;        * @param y a value of type 'int'
;;        * @exception Exception if an error occurs
;;        */
;;       public
;;       void   myMethod( int  x,  int y )
;;         throws Exception
;;       {
;;         ...

;;; Customization:
;;
;;  M-x `jdok-customize' to customize all the jdok options.
;;
;;  The following variables could be set:
;;
;;  o `jdok-load-hook'
;;        hook run when package has been loaded. The provided hook
;;        `jdok-default-load-hook' defines the default key mapping.
;;
;;  o `jdok-describe-method'
;;        Template used to describe a method.
;;
;;  o `jdok-describe-constructor'
;;        Template used to describe a constructor.
;;
;;  o `jdok-param-tag'
;;        Template used to describe a parameter.
;;
;;  o `jdok-return-tag'
;;        Template used to describe a returned value.
;;
;;  o `jdok-exception-tag'
;;        Template used to describe a exception.

;;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to David Ponce at david.ponce@wanadoo.fr.
;;
;;  This version of jdok was developed with NTEmacs 20.3.1 under MS Windows
;;  NT 4 WKS SP3 and also tested with Emacs 20.3 under Sun Solaris 2.5.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:
  
(defconst jdok-version "$Revision: 1.1 $"
  "jdok version number.")

(defconst jdok-method-type-and-name-regexp
  "\\s-*\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(.*\\)("
  "Regular expression used to find the result type and the name of a method.
See also `jdok-method-type-and-name-match'."
  )

(defconst jdok-method-type-and-name-match 3
  "Index of the parenthesized expression that match the result in
`jdok-method-type-and-name-regexp'."
  )

(defconst jdok-method-arglist-regexp
  "([ ]*\\(.*\\)[ ]*)"
  "Regular expression used to find the method arguments.
See also `jdok-method-arglist-match'."
  )

(defconst jdok-method-arglist-match 1
  "Index of the parenthesized expression that match the result in
`jdok-method-arglist-regexp'."
  )

(defconst jdok-method-throws-regexp
  "throws[ ]+\\(.*\\)[ ]*[{;]"
  "Regular expression used to find the method exceptions.
See also `jdok-method-throws-match'."
  )

(defconst jdok-method-throws-match 1
  "Index of the parenthesized expression that match the result in
`jdok-method-throws-regexp'."
  )

(defgroup jdok nil
  "jdok package customization"
  :group 'tools
  :prefix "jdok-")

(defcustom jdok-describe-method
  "* Describe '%s' method here."
  "*Template used to describe a method.
One string argument could be substituted: the name of the method."
  :group 'jdok
  :type 'string)

(defcustom jdok-describe-constructor
  "* Describe constructor here."
  "*Template used to describe a constructor.
One string argument could be substituted: the name of the constructor."
  :group 'jdok
  :type 'string)

(defcustom jdok-param-tag
  "* @param %s a value of type '%s'"
  "*Template used to describe a parameter.
Two string arguments could be substituted: the name of the parameter and its type."
  :group 'jdok
  :type 'string)

(defcustom jdok-return-tag
  "* @return a value of type '%s'"
  "*Template used to describe a returned value.
One string argument could be substituted: the type of the returned value."
  :group 'jdok
  :type 'string)

(defcustom jdok-exception-tag
  "* @exception %s if an error occurs"
  "*Template used to describe a exception.
One string argument could be substituted: the type of the exception."
  :group 'jdok
  :type 'string)

(defcustom jdok-load-hook '(jdok-default-load-hook)
   "*Hook run when package has been loaded. The default hook provided
`jdok-default-load-hook' maps the java-mode key `C-cj' to the
`jdok-generate-javadoc-template' command."
  :group 'jdok
  :type 'hook
   )

(defun jdok-customize ()
  "Customization of the group jdok."
  (interactive)
  (customize-group "jdok"))

(defun jdok-version-number ()
  "Returns jdok version number."
  (string-match "[0123456789.]+" jdok-version)
  (match-string 0 jdok-version))

(defun jdok-display-version ()
  "Displays jdok version."
  (interactive)
  (message "Using 'jdok' version %s." (jdok-version-number)))

(defun jdok-get-method-signature ()
  "Returns a method signature as a string where tab, newline characters and
extra spaces are removed. The method signature is the buffer substring
from the beginning of the current line to (and including) the next '{' or ';'
found.
In this example, point is located after the word `public'.

  public-|-
  void   myMethod( int  x,  int y )
    throws Exception
  {
    ...

The method signature returned will be the string:

  \"public void myMethod( int x, int y ) throws Exception {\"
"
  (beginning-of-line)
  (save-excursion
    (let ((start (point))
          (end (re-search-forward "[{;]" nil t)))
      (and end
           (mapconcat 'identity
                      (split-string (buffer-substring-no-properties start end)
                                    "[ \t\r\n]+")
                      " ")
           ))))

(defun jdok-extract-type-and-name (method-signature)
  "Extracts the type and the name of the method specified by the given signature.
Returns the pair '(method-type . method-name)' or nil if extraction does not
complete. For a constructor 'method-type' will be nil."
  (let ((result (and (string-match jdok-method-type-and-name-regexp method-signature)
                     (match-string jdok-method-type-and-name-match  method-signature))))
    (when result
      (setq result (split-string result "[ ]+"))
      (if (stringp (cadr result))
          (cons (car result) (cadr result))
        (cons nil (car result))))))

(defun jdok-extract-arglist (method-signature)
  "Extracts the argument list of the method specified by the given signature.
Returns a list '((arg1-type . arg1-name) ... (argN-type . argN-name))' or nil
if extraction does not complete or the method does not have any arguments."
  (let ((result (and (string-match jdok-method-arglist-regexp method-signature)
                     (match-string jdok-method-arglist-match  method-signature))))
    (when result
      (setq result (split-string result "[ ]*,[ ]*"))
      (mapcar '(lambda (x)
                 (let ((arg (split-string x "[ ]+")))
                   (rplacd arg (cadr arg))
                   arg))
              result))))

(defun jdok-extract-throws (method-signature)
  "Extracts the exceptions list of the method specified by the given signature.
Returns a list '(exception1 ... exceptionN)' or nil if extraction does not
complete or the method does not throw any exceptions."
  (let ((result (and (string-match jdok-method-throws-regexp method-signature)
                     (match-string jdok-method-throws-match  method-signature))))
    (if result
        (split-string result "\\([ ]*,[ ]*\\|[ ]+\\)"))))

(defun jdok-insert-line (template &rest args)
  "Inserts a line build from the given template in which string args
are substituted with `format'."
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (indent-according-to-mode)
  (insert (apply 'format (cons template args)))
;;  (and args (apply 'insert args))
  (reindent-then-newline-and-indent)
  )

(defun jdok-generate-javadoc-template ()
  "Generates a javadoc template to help to document the method
found at the point (see `jdok-get-method-signature' for details
on how the method signature is found).

In this example, point is located at the beginning of the line, before the
word `public':

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...
     }

M-x `jdok-generate-javadoc-template' will produce the following.

     /**
      * Describe 'myMethod' method here.
      *
      * @param x a value of type 'int'
      * @param y a value of type 'int'
      * @exception Exception if an error occurs
      */
     public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...
     }
"
  (interactive)
  (if (or (eq major-mode 'jde-mode) (eq major-mode 'java-mode))
      (let ((method-signature (jdok-get-method-signature)))
        (if method-signature
            (let* ((type-and-name (jdok-extract-type-and-name method-signature))
                   (arglist (jdok-extract-arglist method-signature))
                   (throws (jdok-extract-throws method-signature))
                   (type (car type-and-name))
                   (name (cdr type-and-name)))
              (message "method-signature: %S" method-signature)
              (jdok-insert-line "/**")
              (if type-and-name
                  (if type
                      (jdok-insert-line jdok-describe-method name)
                    (jdok-insert-line jdok-describe-constructor name)))
              (jdok-insert-line "*")
              (mapc '(lambda (x)
                       (jdok-insert-line jdok-param-tag (cdr x) (car x)))
                    arglist)
              (if (and type (not (string= type "void")))
                  (jdok-insert-line jdok-return-tag type))
              (mapc '(lambda (x)
                       (jdok-insert-line jdok-exception-tag x))
                    throws)
              (jdok-insert-line "*/")
              )
          )
        )
    (message "Invalid major mode found. Must be 'java-mode' or 'jde-mode'."))
  )

(defun jdok-default-load-hook ()
  "Hook run when package has been loaded. It maps the keys `C-cj' to
`jdok-generate-javadoc-template'."
  (define-key java-mode-map "\C-cj"  'jdok-generate-javadoc-template)
  )

(provide 'jdok)
(run-hooks 'jdok-load-hook)

;;; Change History:

;;
;; $Log: jdok.el,v $
;; Revision 1.1  2005-11-26 01:15:01  hute37
;; dot
;;
;; Revision 1.6  1998/11/05 07:58:05  ebat311
;; `jdok-get-method-signature'  and `jdok-method-throws-regexp'
;; updated to handle abstract method definitions.
;; Thanks to Jerry Boetje <jboetje@healthquality.com> for his
;; contribution.
;;
;; Revision 1.5  1998/11/04 11:12:27  ebat311
;; `jdok-get-method-signature' updated to skip CR characters.
;;
;; Revision 1.4  1998/10/21 11:42:11  ebat311
;; Fixed a problem with TAB characters in method signature. Thanks to
;; Christian Halstrick <Christian.Halstrick@dredsner-bank.com> for his help.
;;
;; Revision 1.3  1998/10/20 10:02:55  ebat311
;; Have run `untabify' on the whole source (follows a remark from ricky@siemensdc.com).
;;
;; Revision 1.2  1998/10/08 13:46:05  ebat311
;; Updated usage comments.
;;
;; Revision 1.1  1998/10/08 13:27:44  ebat311
;; Initial revision
;;
;;

;;; jdok.el ends here.
