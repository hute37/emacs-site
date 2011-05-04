;;; @(#) tinylibmenu.el --- Library for echo-area menu
;;; @(#) $Id: tinylibmenu.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1996-02
;; Keywords:	    extensions
;;
;; To get information on this program use ident(1) or do M-x ti::menu-version
;; Look at the code with folding.el

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;      (require 'tinylibmenu)
;;
;;  Or use autoload, which is prefered. Your ~/.emacs loads faster
;;
;;      (autoload 'ti::menu-menu-default "tinylibmenu" "" t)
;;
;;  To bring up the menu (or menus), bind the main function into some key:
;;
;;      (global-set-key "\C-cM"   'ti::menu-menu-default)
;;
;;  If you want to access some special menus:
;;
;;      (global-set-key "\C-M0"  'ti::menu-menu-default)  ;; the main menu
;;      (global-set-key "\C-M1" (definteractive (ti::menu-menu my-menu1 arg)))
;;
;;      (global-set-key "\C-M2" (definteractive (ti::menu-menu my-menu2 arg)))
;;
;;  Just make sure you have defined the variables `my-menu1' and `my-menu2'
;;  which hold the menu information.
;;
;;  If you have any questions, use this function to contact author
;;
;;      M-x ti::menu-submit-bug-report

;;}}}
;;{{{ docs

;;; Commentary:

;;  Overview of features
;;
;;	o   This package is a library.
;;	    Store key bindings behind echo area menu, which is similar to
;;	    menu bar.
;;	o   Regular Emacs user can also put less used binding to guided
;;	    echo menu by just defining couple of menu variables.
;;
;;  Customizing menus
;;

;;      If some package defines echo area menus and you only want to make
;;      small modifications and not to copy the whole 'defvar MENU' to your
;;      .emacs, you can use following functions to manipulate the menu
;;      items
;;
;;          ti::menu-add
;;          ti::menu-set-doc-string
;;
;;      For example, if there is menu item:
;;
;;          (defconst my-menu-sample
;;          '("?)help, 1)test1, 2)test2"
;;            ((?1 . (  (some-menu-test1 1 2 3)))
;;             (?2 . (  (some-menu-test2 1 2 3))))))
;;
;;      and you don't like keybinding '?2'. You first delete the menu item,
;;      then add yours and lastly you update the doc string that is printed
;;	in echo area. Here is how you do all these three steps.
;;
;;          (ti::menu-add 'my-menu-sample ?2  nil 'delete)
;;          (ti::menu-add 'my-menu-sample ?t '( (my-test 1 2 3)))
;;          (ti::menu-set-doc-string 'my-menu-sample
;;                                    "?)help, 1)test1, t)myTest")
;;
;;      And the modified menu looks like this
;;
;;          (defconst my-menu-sample
;;          '("?)help, 1)test1, t)myTest"
;;            ((?1 . (  (some-menu-test1 1 2 3)))
;;             (?t . (  (my-test2 1 2 3))))))
;;
;;      If you want to replace _many_ commands from the menu, it is lot
;;      easier if you copy the menu `defvar' and make direct changes there.
;;      If you want to make it all with lisp, here is example which
;;      replaces 2 items from the menu
;;
;;          (mapcar
;;            '(lambda (x)
;;               (let ((key (car x)))
;;                 (ti::menu-add 'ti::menu-:menu-sample nil 'delete) ;; Remove old
;;                 ;; Add new
;;                 (ti::menu-add 'ti::menu-:menu-sample key (cdr x))))
;;          '((?1 . ( (my-1 1 2 3)))     ;; New menu item replacements
;;            (?2 . ( (my-2 1 2 3)))))
;;
;;          (ti::menu-set-doc-string 'ti::menu-:menu-sample "?)help, 1)my1 2)my2")
;;
;;  Having a test run
;;
;;      The easiest way to get a hold on the echo menu is that you try it.
;;      Follow these steps. Then you're ready to make your own menus.
;;
;;      .   Load this file. M-x load-library tinylibmenu.el
;;      .   Start menu with `M-x' `ti::menu-menu-default'
;;      .   Press key `?' or `h' to get help and `q' to quit menu.
;;      .   Try offered choices

;;; Change Log:

;;; Code:

;;}}}
;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))


;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defvar ti::menu-:load-hook nil
  "*Hook that is run when package has been loaded.")

;;; ....................................................... &v-private ...

(defvar ti::menu-:prefix-arg  nil
  "Prefix arg when menu is called.")


;;  This is just an example, not a user variable.
;;  This is how you use the package
;;  NOTE: put the help into the documentation string. Like
;;        in variable ti::menu-:menu-mode.

(defconst ti::menu-:menu-sample
  '("?)help, 1)test1, 2)test2, m)mode, u)undefined , e)eval. q)quit"
    ((?1 . (  (ti::menu-test1 1 2 3)))      ;this does not have FLAG
     (?2 . (t (ti::menu-test2)))            ;FLAG used.
     (?m . ti::menu-:menu-mode)
     (?u . ti::menu-:menu-not-exist)        ;this variable does not exist :-)
     (?e . (t (progn
                (message "menu item evaled. Pfx: '%s' "
                         (prin1-to-string ti::menu-:prefix-arg))
                (sleep-for 1))))))
  "*This is documentation string of variable `ti::menu-:menu-sample'.
The menu help is put here.

Reserved menu keys (characters)

    `q' and `Q' are reserved for quitting the menu prompt.
    `?' anf `h' are reserved for help.

Menu structure is as follows

    FLAG is optional. It says if the menu should be shown after
    function has completed. If FLAG is missing, the menu is not displayed
    after the function call. (that is: call function and exit menu)

    The DISPLAYED-MENU-STRING is evaled, so it can contain any lisp expression
    yielding a string.

    Below you see 3 different ways to define one menu element.

    (defconst my-meny
     '(
      DISPLAYED-MENU-STRING
      ((CHARACTER-KEY  . ANOTHER-MENU-VARIABLE-SYMBOL)
       (CHARACTER-KEY  . ([FLAG] (FUNCTION-NAME PARAMETER PARAMETER...)))
       (CHARACTER-KEY  . ([FLAG] (FORM-TO-EVAL)))
       ..))
    \" MENU HELP RESIDES IN THE DOCUMENTATION STRING\")")

;; This is just an example how you could utilize the prefix arguments.
;; - Example kindly send by "Christopher J. Madsen" <madsen@computek.net>
;;
;;(defconst ti::menu-:menu-mail
;;  '((if current-prefix-arg
;;        "View mailbox read-only:  E)macs M)ailbox P)erl   R)ead/write"
;;      "View mailbox:  E)macs M)ailbox P)erl   R)ead-only")
;;    ((?e . ( (vm-visit-folder "~/mail/emacs" current-prefix-arg)))
;;     (?m . ( (call-interactively 'vm)))
;;     (?p . ( (vm-visit-folder "~/mail/perl" current-prefix-arg)))
;;     (?r . (t(setq current-prefix-arg (if current-prefix-arg nil '(4)))))
;;     ))
;;  "Select a mailbox to visit")


;; This is just an example, not a user variable.

(defconst ti::menu-:menu-mode
  '(
    "Press ?/ cC)++ l)isp tT)ext f)undamental p)icture F0ill O)font"
    ((?c . ( (c-mode)))
     (?C . ( (cc-mode)))
     (?l . ( (lisp-mode)))
     (?t . ( (text-mode)))
     (?T . ( (indented-text-mode)))
     (?f . ( (fundamental-mode)))
     (?p . ( (picture-mode)))

     (?F . (t (auto-fill-mode)))
     (?O . (t (font-lock-mode)))
     (?/ . ti::menu-:menu-sample)))           ;back to ROOT menu
  "*Menu help.
Major modes:

  c = turn on `c-mode'
  C = turn on C++ mode
  l = turn on lisp mode
  t = turn on text mode
  T = turn on indented text mode
  f = turn on fundamental mode
  p = turn on picture mode

Minor modes:

  F = turn on auto fill mode
  O = turn on f(o)nt lock mode

Special keys
  / = Return to root menu")

;;; ........................................................ &v-config ...

(defvar ti::menu-:menu 'ti::menu-:menu-sample
  "*Variable holding the default root menu.")

;;; ....................................................... &v-version ...

(eval-and-compile
(ti::macrof-version-bug-report
 "tinylibmenu.el"
 "tinylibmenu"
 tili-:version-id
 "$Id: tinylibmenu.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(ti::menu-:version-id)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ code: test funcs

;;; ....................................................... test funcs ...

(defun ti::menu-test1 (&optional arg1 arg2 arg3)
  "Sample Menu test function with ARG1 ARG2 ARG3."
  (message (format "function 1 called with args: %s %s %s" arg1 arg2 arg3)))

(defun ti::menu-test2 ()
  "Sample Menu test function."
  (message (format "function 2 called"))
  (sleep-for 1))

;;}}}
;;{{{  menu item add, delete

;;; ------------------------------------------------------------- &add ---
;;;
;;;###autoload
(defun ti::menu-add (menu-symbol ch cell &optional delete)
  "Add to menu MENU-SYMBOL elt (CH . CELL). Optionally DELETE.

Example:

  (ti::menu-add 'ti::menu-:menu-sample ?2  nil 'delete)
  (ti::menu-add 'ti::menu-:menu-sample ?t '( (my-test 1 2 3)))

Return:

  nil       no add done due to existing CELL
            no remove due to non-existing CELL"
  (let* ((menu  (symbol-value menu-symbol))
	 (doc	(nth 0 menu))
	 (list  (nth 1 menu))
         elt
         ret)
    (or (characterp ch)
	(error "CH must be character"))
    (setq elt (char-assq ch list))

    (cond

     (delete
      (when elt
	(setq ret elt)
	(setq list (delete elt list))
	(set menu-symbol (list doc list))))

     ((and (null delete)
           (not elt))                   ;not already exist?
      (setq ret (cons ch cell))
      (push ret list)
      (set menu-symbol (list doc list))))
    ret))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::menu-set-doc-string (menu-symbol doc-string)
  "Use  MENU-SYMBOL and set its DOC-STRING.

Example:

  (ti::menu-set-doc-string 'ti::menu-:menu-sample \"?=help, 1=test1, t=myTest\")"
  (let* ((menu (symbol-value menu-symbol)))

    ;;  It's better to check that the arg is right; setcar won't
    ;;  do that

    (if (not (stringp doc-string))
	(error "timu: need string."))

    (setcar menu doc-string)
    (set menu-symbol menu)))

;;}}}

;;{{{ code: menu

;;; ........................................................ menu func ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::menu-help-output  (variable-symbol)
  "Write doctring, ie Menu help, to the *Help* buffer"
  (with-output-to-temp-buffer "*Help*"
    (princ
     (documentation-property
      variable-symbol
      'variable-documentation))))

;;; ----------------------------------------------------------------------
;;; - This is only simple help. You can't resize the window etc...
;;;
(defun ti::menu-help (menu-sym)
  "Show menu help of MENU-SYM.
MENU-SYM can variable symbol, whose documentaion is displayed or
a function symbol.

The help commands are:
  n or space   = next
  p or del     = previous
  q            = end help"
  (let* ((msg     "Help: space or n = next, backspace or p = prev, q = quit")
         (oframe  (selected-frame))
         (buffer  "*help*")
         (docs    (or (documentation-property
		       menu-sym 'variable-documentation)
		      (and (fboundp menu-sym)
			   (documentation menu-sym))))
	 step
         ch)
    (cond
     ((stringp docs)
      (unwind-protect                   ;make sure the help buffer is deleted
          (save-excursion
            (save-window-excursion

              ;;  We have to save the source window config above
              ;;  Be sure this frame is non-dedicated.

              (ti::select-frame-non-dedicated)

              ;; now we may be in another frame; save it's configuration
              ;; too

              (save-window-excursion
                (with-output-to-temp-buffer buffer (princ docs))
                (select-window  (get-buffer-window buffer))

                ;;  This is simplest way to resize help window

                (balance-windows)
		(setq step (1- (window-height)))

		;;  Now scroll the help

                (while (not
			(char-in-list-case
			 (setq ch (ti::read-char-safe-until msg))
			 '(?q ?\e)))
                  (cond
                   ;;  127  = backspace in windowed
                   ;;
                   ((char-in-list-case ch '(?p ?P ?\177 ?\b))
                    (ignore-errors (scroll-down step)))

                   ((char-in-list-case ch '(?n ?N ?\ ))
                    (ignore-errors (scroll-up step))))))))

	(if (and (not (null oframe))
                 (framep oframe))
            (if (framep (setq oframe (raise-frame oframe)))
                (select-frame oframe)))
        (kill-buffer buffer)))
     (t
      (message "Sorry, no help defined.")
      (sleep-for 1)
      (message "")))

    (discard-input)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::menu-menu (menu-symbol &optional pfx-arg)
  "The menu navigation engine.

Input:

  MENU-SYMBOL           variable symbol containing menu items
  PFX-ARG               the prefix arg user ppossibly passed to menu

References:

  `ti::menu-:menu-sample'   Show how the menu is constructed.
  `ti::menu-:prefix-arg'    Copy of current prefix arg"
  (let* ((var           menu-symbol)
         (m             (eval var))     ;menu content
         (loop          t)
         (current-prefix-arg  pfx-arg)  ;set for menu functions

         prompt flag
         alist
	 ch
	 elt
         eval-form)

    (setq ti::menu-:prefix-arg pfx-arg)

    (while loop

      ;;  Substitute removes DOC ^M line endings before printing the message
      ;;  #todo: Don't know if substitute is really necessary

      (setq prompt      (subst-char-with-string (eval (nth 0 m)) ?\C-m "")
            alist       (nth 1 m))

      ;;  moving the mouse and reading with read-char would break. Use above.

      (setq ch (ti::read-char-safe-until prompt))

      (setq eval-form nil)              ;clear this always


;;;     (ti::d!! "\nMENU CH" ch "PFX" current-prefix-arg  "M" m "\nELT" elt "\n\n")

      ;; .................................................. what ch ? ...

;;;      (ti::d!! "\nTINYMENU elt ch "  elt ch)

      (cond
       ((char-in-list-case ch '(?q ?Q ?\e))	;quit
        (setq loop nil))

       ((char= ch ?? )		;handle help
        (ti::menu-help var))

       ((char= ch ?h )		;handle help
        (ti::menu-help-output var)
        (setq loop nil))

       ((setq elt (char-assq ch alist))
        (setq elt (cdr elt))

        ;; ................................. new menu or call function ...

        (cond
         ((symbolp elt)
          (if (not (boundp elt))
              (error (format "Menu variable does not exist: %s" elt))
            ;;  replace with another menu
            (setq var elt
                  m   (symbol-value elt))))
         ;; ..................................................... list ...

         ((ti::listp elt)
          (cond				;See if there is flag.
           ((and (eq 2 (length elt))
		 (equal 'quote (car elt)))
	    ;;  A menu entry is not right
	    ;;
	    ;;  '(?x . 'my-symbol)
	    ;;  --> (quote my-symbol)

	    (error
	     "Menu error, not a symbol. Use cons or list: %s" elt))

           ((eq 2 (length elt))
            (setq flag t)
            (setq elt (nth 1 elt)))

           (t
            (setq flag nil)
            (setq elt (nth 0 elt))))

          (cond
           ((fboundp (car elt))         ;is first element a function ?
            (setq eval-form elt)
            (setq loop flag))
           (t
            (error "Menu structure error %s %s"
		   (char-assq ch alist)
		   elt))))))

       (t
        ;;  ch not found from list, keep looping
        (sit-for 0.3)))          ;flash the echo area

      (message "")                      ;clear echo area

      (when eval-form
;;;	(ti::d!! "MENU EVAL-FORM" eval-form)
	(eval eval-form)))))

;;; ----------------------------------------------------------------------
;;; - This is user function
;;;
(defun ti::menu-menu-default (&optional arg)
   "Call echo area menu with prefix ARG.
Please read the documentation of variable `ti::menu-:menu-sample' to see
the structure of menu.

Menu pointed by `ti::menu-:menu' is used and PREFIX-ARG is passed to menu engine
'ti::menu-:menu'.

References:
  `ti::menu-:menu-sample'   variable. Shows how the menu is constructed."
   (interactive "P")
   (ti::menu-menu ti::menu-:menu arg))

;;}}}

(provide   'tinylibmenu)
(run-hooks 'ti::menu-:load-hook)

;;; tinylibmenu.el ends here
