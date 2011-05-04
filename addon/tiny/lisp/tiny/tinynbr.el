;;; @(#) tinynbr.el --- Number conversion minor mode oct/bin/hex
;;; @(#) $Id: tinynbr.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2002 Jari Aalto
;; Author:	    Jari Aalto <jari.aalto@poboxes.com>
;; Maintainer:	    Jari Aalto <jari.aalto@poboxes.com>
;; Created:	    1997-08
;; Keywords:	    tools
;;
;; To get information on this program use ident(1) or do M-x tinynbr-version
;; Look at the code with folding.el, tinybm.el

;; LCD Archive Entry:
;; tinynbr|Jari Aalto|jari.aalto@poboxes.com|
;; Number conversion minor mode, int --> hex,oct,bin or vice versa|
;; 2002-01-11|$Revision: 1.1 $|~/misc/tinynbr.el.Z|

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
;;  Put this file on your Emacs-Lisp load path, add following into your
;;  ~/.emacs startup file. This must be the very first entry before
;;  any keybindings take in effect.
;;
;;      (require 'tinynbr)
;;
;;  You can also use the preferred way: autoload
;;
;;_
;;*      (autoload 'tinynbr-mode "tinynbr t t)
;;*      ;;  Put all minor mode activations below C-c m map
;;*      ;;
;;*      (global-set-key "\C-cmN"  'tinynbr-mode)
;;_
;;
;;  If you have any questions, use this function to contact author
;;
;;       M-x tinynbr-submit-bug-report


;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:
;;
;;  Preface, aug 1997
;;
;;	One day in a laboratory the developer once forgot his desk
;;	calculator in another building. He was examining binary (hex)
;;	files and other electronic documents that used hex and base10
;;	numbers. He shroudly remembered that Unix included some basic
;;      calculator, but he dind't remember what was the name and how
;;	to use it. Whoops. Grin.
;;
;;	Instead of returning to get the missing calculator, he started
;;	pouring some lisp to make a simple minor mode to help
;;	to get along with the current task at hand. It didn't take
;;	long to make it, and the laboratory day was success.
;;      Ahem. Maybe should look at package calc.el someday.
;;
;;  Overview of features
;;
;;	o   Int	        --> hex,oct,bin	conversion at current point
;;	o   hex,oct,bin --> int		conversion at current point

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(ti::package-defgroup-tiny TinyNbr tinynbr-: tools
  "Number conversion minor mode oct/bin/hex.")

;;; ......................................................... &v-hooks ...

(defcustom tinynbr-:load-hook nil
  "*Hook run when file has been loaded."
    :type  'hook
    :group 'TinyNbr)

;;; ......................................................... &private ...

;;;###autoload (autoload 'tinynbr-version "tinynbr" "Display commentary." t)

(eval-and-compile
(ti::macrof-version-bug-report
 "tinynbr.el"
 "tinynbr"
 tinynbr-:version-id
 "$Id: tinynbr.el,v 1.1 2005-12-04 20:58:50 hute37 Exp $"
 '(tinynbr-:version-id
   tinynbr-:load-hook)))

;;}}}
;;{{{ Minor Mode

;;; .......................................................... &v-mode ...

;;;###autoload (autoload 'tinynbr-mode		"tinynbr" "" t)
;;;###autoload (autoload 'turn-on-tinynbr-mode	"tinynbr" "" t)
;;;###autoload (autoload 'tun-off-tinynbr-mode	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-commentary    "tinynbr" "" t)

(eval-and-compile
(ti::macrof-minor-mode-wizard
 "tinynbr-" " Tnbr" "z" "Nbr" 'Tnbr "tinynbr-:"
  "Simple number conversion minor mode.

Mode description:

\\{tinynbr-:mode-prefix-map}"

  "TinyNbr"

  nil

  "Number conversion mode"
  (list                                  ;arg 10
   tinynbr-:mode-easymenu-name
   ["int to hex"  tinynbr-int-to-hex  t]
   ["int to oct"  tinynbr-int-to-bin  t]
   ["int to bin"  tinynbr-int-to-oct  t]
   "----"
   ["hex to int"  tinynbr-hex-to-int  t]
   ["oct to int"  tinynbr-oct-to-int  t]
   ["bin to int"  tinynbr-bin-to-int  t]
   "----"
   ["Package version"	 tinynbr-version        t]
   ["Package commentary" tinynbr-commentary	t]
   ["Mode help"   tinynbr-mode-help   t]
   ["Mode off"	  tinynbr-mode	      t])

  (progn
    (define-key   map "X" 'tinynbr-hex-to-int)
    (define-key   map "B" 'tinynbr-bin-to-int)
    (define-key   map "O" 'tinynbr-oct-to-int)

    (define-key   map "x" 'tinynbr-int-to-hex)
    (define-key   map "b" 'tinynbr-int-to-bin)
    (define-key   map "o" 'tinynbr-int-to-oct)

    (define-key   map "v" 'tinynbr-version)

    (define-key map "?"  'tinynbr-mode-help)
    (define-key map "Hm" 'tinynbr-mode-help)
    (define-key map "Hc" 'tinynbr-commentary)
    (define-key map "Hv" 'tinynbr-version))))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ Code

;;; Create functions, and inform autoload generator.

;;;###autoload (autoload 'tinynbr-int-to-hex	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-int-to-oct	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-int-to-bin	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-hex-to-int	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-oct-to-int	"tinynbr" "" t)
;;;###autoload (autoload 'tinynbr-bin-to-int	"tinynbr" "" t)

(mapcar
 (function
  (lambda (x)
    (let ((sym1  (intern (format "tinynbr-%s-to-int"  (car x))))
	  (sym2  (intern (format "tinynbr-int-to-%s"  (car x))))
	  (sym3  (intern (format "int-to-%s-string" (car x))))
	  (base  (nth 1 x))
          def)
      (setq def
            (` (defun (, sym1) (&optional insert reverse)
		 "If prefix arg INSERT is non-nil, insert result to buffer."
                 (interactive "P")
                 (let* ((nbr (save-excursion
			       (if reverse
				   (ti::buffer-read-word "[0-9]+" 'strict)
				 (ti::buffer-read-word "[0-9xXa-fA-F]+" 'strict))))
			ret)
		   (when nbr
		     (if (string-match "^0[Xx]\\(.*\\)" nbr)
			 (setq nbr (match-string 1 nbr)))
		     (if (null reverse)
			 (setq ret (radix nbr (, base)))
		       (setq ret ((, sym3) (string-to-int nbr)))))

		   (cond
		    ((null nbr)
		     (message "Can't find number at current point."))
		    (t
		     (if (not insert)
			 (message "%s => %s %s"
				  nbr
				  ret
				  (cond
				   ((equal (, base)  2) (if reverse "bin - dec" "dec - bin"  ))
				   ((equal (, base)  8) (if reverse "oct - dec" "dec - oct"  ))
				   ((equal (, base) 16) (if reverse "hex - dec" "dec - hex"  ))
				   (t "")))
		       (save-excursion
			 (end-of-line)
			 (insert " " (if (numberp ret)
					 (int-to-string ret)
				       ret))))))))))
      (eval def)

      (setq def
            (` (defun (, sym2) (&optional insert)
		 "If prefix arg INSERT is non-nil, insert result to buffer."
                 (interactive "P")
		 ((, sym1) insert 'reverse))))
      (eval def))))
 '(
   (hex 16)
   (oct 8)
   (bin 2)))

;;}}}

(add-hook  'tinynbr-:mode-hook 'tinynbr-mode-define-keys)
(provide   'tinynbr)
(run-hooks 'tinynbr-:load-hook)

;;; tinynbr.el ends here
