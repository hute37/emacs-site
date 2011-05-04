;; @(#) emacs-rc-font.el -- Emacs rc file for font settings and faces (lazy-lock)
;; @(#) $Id: emacs-rc-font.el,v 1.1 2005-12-04 20:58:58 hute37 Exp $
;;
;;; Documentation:
;;
;;  File id
;;
;;      Copyright (C)   1995-2000 Jari Aalto
;;      Author:         Jari Aalto <jari.aalto@poboxes.com>
;;	Maintainer:	Jari Aalto <jari.aalto@poboxes.com>
;;	Created:	somewhere 1995
;;	Keywords:	tools
;;
;;      This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;      This is one of my personal Emacs (rc) resource files and it may be
;;      under some other name in the current directory where you found it,
;;      but do not mind that. Rename this file to whatever is shown
;;      in the first line.
;;
;;  Installation
;;
;;      Put following statement in your .emacs. Make sure $HOME is included
;;      in `load-path' list.
;;
;;          (require 'rc-set "emacs-rc-font")
;;
;;      This file may be under some other name in the current directory
;;      where you found it, but do not mind that.. Just rename this file to
;;      whatever is shown in the first line.
;;
;;      The file layout is managed with tinytf.el, tinybm.el, folding.el
;;
;;  Description
;;
;;	M-x list-faces-display
;;	M-x list-colors-display
;;
;;
;;
;;   (font-lock-add-keywords
;;      'emacs-lisp-mode
;;      '(("FIXME[:!]?" 0 'show-paren-mismatch-face prepend)))


;;; .............................................................. &my ...

;;; Change Log:
;;; Code:

;;{{{ face definition


;;; ........................................................ &my-faces ...

(defconst win32 (win32-p))

;;; ----------------------------------------------------------------------
;;; See
;;; HP:     /usr/lib/X11/rgb.txt
;;; SunOS:  /usr/openwin/lib/X11/rgb.txt
;;;
(defconst my-:face-definition-list
  '("brown"

    "yellow"
    ("yellow-inv-dark"  ("black" . "yellow"))

    "magenta"
    "pink"
    "violet"
    "brown"

    "orange"
    ("orange-inv-dark" ("black" . "orange"))

    "OrangeRed"

    ("green"	    ("green"))
    ("green-inv"    ("black" . "green"))
    "DarkOliveGreen"
    "ForestGreen"
    "LightSalmon"

    "gray"
    "grey"
    "red"
    "blue"
    "blue4"

    "SkyBlue2"
    "navy"
    "LightBlue"
    "LightSkyBlue"
;;    "DeepSkyBlue3"   ;;  <== My console colour
    "DodgerBlue1"
    "RoyalBlue"
    "CadetBlue"
    "BlueViolet"
    ;;    "LightSteelBlue3"; no steel blue is good for Light bg.

    ("cyan"	    ("Cyan" . "Violet"))

    ("pink"	   ("pink"))
    ("pink-inv"    ("black" . "pink"))

    "RosyBrown"
    "hotpink"
    "beige"
    "turquoise"
    ("paleturquoise-inv" ("black" . "paleturquoise"))

    ("turquoise-inv-dark" ("black" . "turquoise"))

    "LightGoldenRod"  ;; #eedd82
    "PaleGoldenRod"
    "DarkGoldenRod"

    "purple"
    "violet"
    "VioletRed"

    ("inv10"	    (nil . "#3070b0"))
    ("inv11"	    ("black" . "green"))
    ("inv12"	    ("black" . "ForestGreen"))
    ("inv20"	    ("black"))
    ("inv30"	    ("ForestGreen"))
    ("inv40"	    ("red" . "green"))
    ("inv50"	    ("red" . "white"))

    )
  "*List of allocalted faces and colors.")


(defvar my-bg-color-default
  (or (and window-system
	   (fboundp 'x-get-resource)
	   (if (emacs-p)
	       ;; (ATTRIBUTE CLASS &optional COMPONENT SUBCLASS)
	       (x-get-resource ".background" "*Background")
	     ;; (NAME CLASS TYPE &optional LOCALE DEVICE NO-ERROR)
	     (x-get-resource ".background" "*Background" 'string)
	     ))
      nil
      ;;      "DeepSkyBlue3"
      )
  "My background color")

;;; ................................................. &color-functions ...

(defun my-face (face color-list &optional bg)
  "Assign fg color safely to face. Optionally BG color."
  (when (face-p face)
    (setq color-list (list-make color-list))
    (cond
     ((eq bg 'bg)
      (nconcc color-list "grey")
      (ti::t-try-set-colors color-list face 'bg))
     (t
      (nconcc  color-list "white" )
      (ti::t-try-set-colors color-list face)
      ))))

;;; ----------------------------------------------------------------------
;;;
(defun my-defcolor-1 (name &optional fg bg)
  "Define variable my-face-NAME where NAME is color name.

Input:

  NAME	    string, Name of the colour
  FG	    string, nil or 'NIL. Foreground colour. If 'NIL set foreground
            color to nil.
  BG	    string, nil or 'NIL. Background colour. If 'NIL set background
            color to nil.

NAME is used as foreground color if FG is nil."
  (let* ((var  (intern (format  "my-face-%s" name)))
	 (sym  (intern (format  "my-face-%s" name)))
	 ret
	 )
    (` (defconst (, var)
	 (progn
	   (make-face (quote (, sym)))

	   (if (stringp (, fg))
	       (set-face-foreground (quote (, sym)) (, fg))
	     (if (null (, bg))
		 (set-face-foreground (quote (, sym)) (, name))
	       (set-face-foreground (quote (, sym))  (if (emacs-p) nil []))))

	   (if (stringp (, bg))
	       (set-face-background (quote (, sym)) (, bg) )
	     (set-face-background (quote (, sym))
				  (if (emacs-p) nil [])))

	   (quote (, sym))
	   )))))

;;; ----------------------------------------------------------------------
;;;
(defun my-defcolor (name &optional fg bg)
  (eval (my-defcolor-1 name fg bg)))

;;; ----------------------------------------------------------------------
;;;  (my-defcolor "blue-light" "LightBlue")
;;;
(defun my-color-define (list)
  "Define faces according to color LIST."
  (interactive)
  (dolist (color list)
    (cond
     ((stringp color)
      (my-defcolor color)
      (my-defcolor (concat color "-inv") nil color))
     (t
      (let* ((name (car color))
	     (fg   (car (nth 1 color)))
	     (bg   (cdr-safe (nth 1 color)))
	     )
	(my-defcolor name fg bg)
	)))))

(my-color-define my-:face-definition-list)

;;; ----------------------------------------------------------------------
;;;
(defun my-color (list)
  "Select most suitable color from LIST"
  (if (eq (colour-type) 'dark)
      (nth 0 list)
    (nth 1 list)))

;;; ----------------------------------------------------------------------
;;;
(defun my-font-lock-color (type choice default)
  ""
  (let* ((sym (intern-soft (format "font-lock-%s-face"
				   (symbol-name type))))
	 elt
	 ptr
	 )
    (when sym
      (setq elt (nth 1 (or (assq type choice)
			   (assq type default))))
      (setq elt (my-color elt))
      ;;  (font-lock-comment-face "OrangeRed")
      (when (and (boundp 'font-lock-face-attributes) ;19.34
		 (setq ptr (assq sym font-lock-face-attributes)))
	(setnth 1 ptr (face-foreground elt)))
      (set sym elt))
    (list type sym elt)))

;;}}}
;;{{{ face change

;;; ----------------------------------------------------------------------
;;;
(defun my-face-change (&optional name)
  "Configures faces for certain modes"
  (interactive
   (list
    (completing-read
     "mode: "
     '(
       ("pc" . 1)
       ("def" . 2)
       ("c-code" . 3)
       ("c++" . 3)
       )
     nil t
     )))

  (let* ((name     (cond
		    ((stringp name)
		     name)
		    ((symbolp name)
		     (symbol-name name))
		    (t			;default value
		     "def"
		     )))
	 )

    ;; face-foreground
    ;; face-background

    (cond
     ((equal name "def")
      ;; - To see setting (face-foreground 'region) (face-background 'region)
      ;;
      ;; - if you want Selection to show as underlined
      ;;   (set-face-underline-p 'region t)
      ;;   disable whole underline showing....
      (set-face-underline-p 'underline nil)

      (my-face 'highlight "white")
      (my-face 'highlight "blue3" 'bg)

      (ti::t-try-set-colors
       '(
	 "VioletRed3"
	 "forest green"			;same as forest
	 "DarkOliveGreen"		;a bit; bit lighter
	 "dark green"			;darkest
	 "green4"			;much lighter
	 "black"
	 )
       'region)


      (set-face-foreground 'bold "white")
      (my-face 'bold "blue3" 'bg) ;; in RMAIL too

      (when (face-p 'italic)
	(if (emacs-p) (set-face-foreground 'italic nil))
	(set-face-background 'italic "gray")
	)

      (when (face-p 'region)
	(if (emacs-p) (set-face-foreground 'region nil))
	(set-face-background 'region "gray")
	)

      (when (face-p 'bold-italic)
	(set-face-foreground 'bold-italic  "blue3"))

      )
     ((equal name "c-code" )
      ;;  in C++ code, this is comment color
      ;;  font-lock-comment-face , font-lock-type-face == 'italic
      ;;  #AdDfFf = LightBlue

      (my-face 'highlight "MediumOrchid" 'bg)
      ;; (my-face 'italic "LightBlue" 'fg)

      ;; font-lock-function-name-face , 'bold-italic
      (my-face 'bold-italic "blue3" 'fg)

      ;; font-lock-keyword-face , 'bold
      (set-face-background 'bold "#3070c0")
      (my-face 'bold "wheat" 'fg)


      ;; font-lock-string-face , 'underline

      (set-face-foreground 'underline "white")
      )
     ((equal name "pc")

      (set-default-font "7x13")

      (my-face 'default "black")
      (my-face 'default nil 'bg)	;use default color.

      (set-mouse-color "blue")		;doesn't change otw.

      ;; - To see setting (face-foreground 'region) (face-background 'region)
      ;;
      ;; - if you want Selection to show as underlined
      ;;   (set-face-underline-p 'region t)
      ;;   disable whole underline showing....

      (set-face-underline-p 'underline nil)

      (set-face-background 'highlight "blue3")   ;; in RMAIL too

      ;; (defconst rmail-highlighted-headers nil)

      (set-face-foreground 'region nil)


      ;;
      (set-face-foreground 'bold "white")
      (set-face-background 'bold "blue3")        ;; for RMAIL

      (when (face-p 'italic)
	(set-face-foreground 'italic nil)
	(set-face-background 'italic "gray")
	)

      (when (face-p 'bold-italic)
	(set-face-foreground 'bold-italic  "green"))

      )
     ((equal name "c++" )
      ;;  in C++ code, this is comment color
      ;;  font-lock-comment-face , font-lock-type-face == 'italic
      ;;  #AdDfFf = LightBlue
      (when (face-p 'italic)
	(set-face-foreground 'italic "LightBlue"))

      ;; font-lock-function-name-face , 'bold-italic
      (when (face-p 'bold-italic)
	(set-face-foreground 'bold-italic  "LightGoldenRod1"))

      ;; font-lock-keyword-face , 'bold
      (set-face-background 'bold "#3070c0")
      (set-face-foreground 'bold "wheat")


      ;; font-lock-string-face , 'underline
      ;;
      (set-face-foreground 'underline "white")
      )
     (t
      (message "No such mode"))
     )))

;;}}}
;;{{{ lazy lock

;;; ....................................................... &lazy-lck ...


(defconst my-font-lock-keywords-lisp
  (list
   (list
    (concat
     "defcustom\\|defconst\\|defvar"
     "\\|setq-default"
     "\\|mapcar"
     "\\|dolist\\|dotimes"
     "\\|unless\\|when\\|or\\|and"
     "\\|string-match"
     "\\|inline\\|proclaim\\|declaim\\|with-.*"
     )
    '(0 font-lock-keyword-face)
    ))
  "My additional lisp font lock keywords")


(defconst my-font-lock-keywords-cc-mode
  (list
   (list
    (concat
     )
    '(0 font-lock-keyword-face)
    ))
  "My additional C/C++ font lock keywords")



;;; ----------------------------------------------------------------------
;;;
(defun my-font-lock-add-mode-regexp  (mode var &optional force)
  "Add MODE REGEXP to `font-lock-defaults-alist'.

Input:

  MODE	    symbol, mode symbol, like 'lisp-mode
  VAR       symbol, symbol of the regexp that would have the keywords.
  FORCE	    flag, if non-nil reset the keywords. Otherwise, if
	    keywords have already been set, do nothing."
  (when (boundp 'font-lock-defaults-alist)
    (let* ((fl-sym (make-symbol (format "%s-defaults" mode)))
	   (my-sym (make-symbol (format "my-%s" mode)))
	   (o-sym  (make-symbol (format "my-%s-orig" mode)))
	   elt
	   var-list
	   )
      (when  (and (or force
		      (not (get 'font-lock-defaults-alist my-sym)));; only once
		  (setq elt (assq mode font-lock-defaults-alist))
		  )

	(unless (get 'font-lock-defaults-alist o-sym);;  Save original value
	  (put 'font-lock-defaults-alist o-sym elt))

	(put 'font-lock-defaults-alist my-sym 'defined)

	;;  font-lock-defaults-alist = '((mode-name ((SYM-LIST ....) ...)

	(setq var-list (car (cdr elt)));; next list
	(setq var-list (delq var var-list));; kill old
	(push var var-list);; add new
	(setcar (cdr elt) var-list);; replace this list with my keywords added
	))))


;;; ------------------------------------------------------------ &face ---
;;;
(defun my-font-lock-colors ()
  "Set font lock colors."
  (interactive)
  (let* ((default
	   '(
	     ;; my-face-pink-light
	     ;; my-face-VioletRed
	     ;; my-face-OrangeRed

	     (comment	    (my-face-LightBlue my-face-LightGoldenRod))

	     ;; my-face-BlueViolet
	     ;; my-face-violet

	     ;; (string	    (my-face-LightGoldenRod my-face-beige-inv))
	     (string	    (my-face-LightGoldenRod my-face-RosyBrown-inv))
	     (function-name (my-face-navy my-face-LightGoldenrod-inv))
	     (doc-string    (nil my-face-DodgerBlue1))
	     (warning	    (black gray))
	     (keyword	    (my-face-yellow my-face-blue))
	     (other-type    (my-face-inv20 ))
	     (type	    (my-face-cyan my-face-OrangeRed))
	     (constant	    (my-face-orange-inv))

	     ;; my-face-orange
	     ;; my-face-LightSkyBlue
	     ;; my-face-violet

	     (variable-name (my-face-LightGoldenRod my-face-LightSalmon-inv))
	     (reference     (my-face-blue my-face-blue))
	     (builtin	    (my-face-cyan my-face-OrangeRed))
	     (doc-string    (default default))
	     ))

	 (list
	  '((good-pc-19monitor
	     (
	      (variable-name (nil font-lock-variable-name-face))
	      (keyword	     (nil font-lock-keyword-face)) ;; Purple
	      (type	     (nil font-lock-type-face))    ;; Green
	      (constant	     (nil font-lock-constant-face))    ;; CadetBlue
	      (function-name (nil my-face-OrangeRed))

	      ;; DarkGoldenRod LightBlue-inv PaleGoldenRod-inv
	      (variable-name (nil font-lock-variable-name-face))
	      (string	     (nil my-face-beige-inv))

	      ;; (string    (nil my-face-navy))  ;; was RosyBrown
	      (comment      (nil my-face-DodgerBlue1))
	      ))
	    (average-pc-17monitor
	     (
	      (keyword	     (nil font-lock-keyword-face)) ;; Purple
	      (type	     (nil font-lock-type-face))    ;; Green
	      (constant	     (nil font-lock-constant-face))    ;; CadetBlue
	      (function-name (nil my-face-OrangeRed))

	      (string	     (my-face-LightGoldenRod my-face-beige-inv))

	      ;; DarkGoldenRod
	      ;; (variable-name (nil font-lock-variable-name-face))
	      (variable-name (my-face-LightGoldenRod my-face-LightSkyBlue-inv))

	      ;; (string    (nil my-face-navy))  ;; was RosyBrown
	      ;; (comment   (nil my-face-CadetBlue))
	      (comment      (nil my-face-RosyBrown))
	      ))

	    ))


	 (choice
	  (cond
	   ((string-match "picasso" (system-name))
	    (message "My: color 19 monitor")
	    (nth 1 (assq 'good-pc-19monitor list)))
	   ((string-match "tcjari" (system-name) )
	    (message "My: color 17 monitor")
	    (nth 1 (assq 'average-pc-17monitor list)))
	  ))


         )
    ;; OrangeRed  LightSalmon LightSkyBlue

    (when (colour-type)
      (when (boundp 'font-lock-doc-string-face)
	(set 'font-lock-doc-string-face 'default))
      ;;
      ;; Set all font lock faces
      ;;
      (dolist (type (mapcar 'car default))
	(d!! "\nMy face: " (my-font-lock-color type choice default)))

      )))

;;; ----------------------------------------------------------------------
;;;
(defun my-font-lock-mode-hook ()
  "Changes colors to my taste. Turns on/off lazy-lock according
 font-lock."

  (when font-lock-mode

    (my-font-lock-colors)
    (my-font-lock-buffer)

    (my-font-lock-add-mode-regexp 'lisp-mode 'my-font-lock-keywords-lisp)
    (my-font-lock-add-mode-regexp 'emacs-lisp-mode 'my-font-lock-keywords-lisp)

    ;;  29 Jan 1998, comp.emacs steve gonedes <no@address.com>
    ;;  To make font lock ignore ' and " coloring

    (when (boundp 'font-lock-defaults-alist)
      (setf (cdr (assoc 'c++-mode font-lock-defaults-alist))
	    '((c++-font-lock-keywords
	       c++-font-lock-keywords-1
	       c++-font-lock-keywords-2 c++-font-lock-keywords-3)
	      nil nil ((?\" . "_") (?_ . "w") (?~ . "w")) beginning-of-defun
	      (font-lock-comment-start-regexp . "/[*/]")
	      (font-lock-mark-block-function . mark-defun)
	      )))

    (when (fboundp 'my-fix-minor-mode-names)
      (sfuncall 'my-fix-minor-mode-names))

    )

  ;;  turn off lazy lock when font lock turns off

  (if font-lock-mode
      (if (> (buffer-size) 2000)	;Only if big
	  (my-lazy-lock-mode 1))
    (my-lazy-lock-mode 0))
  )


(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("FIXME[:!]?\\|#todo:" 0 'show-paren-mismatch-face prepend)´)))



;;; ----------------------------------------------------------------------
;;;
(defun my-lazy-lock-mode (&optional arg)
  "Try to load and turn on or off `lazy-lock-mode' with ARG."
  (interactive)
  (let* (
         )
    (unless arg				;It's nil, toggle
      (if (null (if (fboundp 'lazy-lock-mode)
		    (symbol-value 'lazy-lock-mode)))
	  (setq arg 1)
	(setq arg -1)
	;;  Turn off font-lock too
	(when font-lock-mode (font-lock-mode -1))))

    ;;  Try to load and turn on lazy lock mode, the lazy lock
    ;;  has to be different in new emacs releases.

    (cond
     ((string-match "19.2[78]" (emacs-version))
      (ignore-errors
	(if (not (featurep 'lazy-lock))
	    (load "lazy-lock.el1.13"))
	(sfuncall 'lazy-lock-mode arg)))
     (t
      (ignore-errors
	(unless (featurep 'lazy-lock)
	  (require 'lazy-lock))		;for latest emacs version
	(sfuncall 'lazy-lock-mode arg))
      ))))

;;}}}
;;{{{ fonts, sizes

;;; ........................................................... &fonts ...

;;   Couple of good fonts to use
;;   The font1-m is normal xterm font

(defconst my--font1-m
  "-bitstream-prestige-medium-r-normal--17-128-72-72-m-90-hp-roman8")

(defconst my--font1-s
  "-bitstream-prestige-medium-r-normal--16-120-72-72-m-80-iso8859-1")

(defconst my--font1-xs
  "-hewlett packard-user-medium-r-normal--13-100-72-72-m-60-iso8859-1")

(defconst my--font2-xs
  "-adobe-courier-medium-r-normal--12-120-75-75-m-70-hp-roman8")

(defconst my--font  my--font1-m)

;;}}}
;;{{{ 19.30

;;; ........................................................... &19.30 ...

(cond
 ((and (ti::xe-window-system)
       (fboundp 'global-font-lock-mode)
       )
  (sfuncall 'global-font-lock-mode t)
  (defconst font-lock-maximum-decoration
	'((t . 3)
	  (c-mode . 3)
	  (c++-mode . 3)
	  (matlab-mode . 3)
	  (emacs-lisp-mode . 3)
	  (Info-mode . 3)
	  ))
  (when nil
    (defconst font-lock-face-attributes
      (list
       '(font-lock-comment-face "Firebrick")
       '(font-lock-string-face "RosyBrown")
       '(font-lock-keyword-face "RoyalBlue")
       '(font-lock-function-name-face "Blue")
       '(font-lock-variable-name-face "DarkGoldenRod")
       '(font-lock-type-face "DarkOliveGreen")
       '(font-lock-reference-face "CadetBlue")
       )))
  ))

;;}}}
;;{{{ face set

;;; ........................................................ &face set ...

;; print status messages

(defconst font-lock-verbose	    t)
(defconst font-lock-keywords-only   'yes)
(defconst font-lock-maximum-size    nil) ;; fontify all buffers, no size limit
;; (defconst font-lock-maximum-decoration)

(defconst font-lock-support-mode    'lazy-lock-mode)
(defconst lazy-lock-stealth-time    30)

;; Use always deman driven fontification

(defconst lazy-lock-minimum-size    2000)

;;  Do not fontify immediately if I change page.

(cond
 ((boundp 'lazy-lock-defer-driven)
  (defconst lazy-lock-defer-driven    t))
 ((boundp 'lazy-lock-defer-on-scrolling)
  (defconst lazy-lock-defer-driven-on-scrolling nil))
 )

(defconst lazy-lock-defer-time	    0.5)  ;; was 0..25
(defconst lazy-lock-stealth-nice    0.25) ;; was 0.125
(defconst lazy-lock-stealth-verbose t)

(autoload 'turn-on-lazy-lock "lazy-lock" t t)

;; (defconst font-lock-mode-hook nil)


;;; 4 Sep 1996, Harald Meland <Harald.Meland@cern.ch>, gnu.emacs.help
;;;
;;;(if (fboundp 'global-font-lock-mode)
;;;    ;; Yes, 'global-font-lock-mode' does have a function definiton:
;;;    (global-font-lock-mode t)
;;;  ;; Otherwise, we do it the ancient (pre-19.31) way:
;;;  (add-hook 'find-file-hooks 'turn-on-font-lock))

(defvar font-lock-mode nil)

;; Unfortunately this hooks runs only when mode is turned on.
;; I have to use advice to get it called always.
;;
;; (add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

(defadvice font-lock-mode (after my act)
  "Run `my-font-lock-mode-hook'. Always on mode on and off basis."
  (defvar font-lock-keywords nil)
  (my-font-lock-mode-hook)
  )



;; bind the standard type-faces to the font-lock faces, some already have a
;; sensible default
;; (defconst font-lock-type-face 'bold)

;; (modify-face 'modeline "white" "blue" "y" nil nil)

(defconst my-outline-font-lock-keywords
  '(
    ;; This is for outline: Highlight headings according to the level.
    ;;
    ("^\\([*@]+\\)[ \t]*\\(.+\\)?[ \t]*$" . font-lock-function-name-face)

    ;; Highight questions and aswers
    ;;
    ("A:.*" . font-lock-comment-face)
    ("Q:.*" . font-lock-type-face)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun my-font-lock-buffer ()
  "Set suitable font lock variables."
  (interactive)
  (let* ((cs    (or comment-start-skip "[ \t]+"))
	 (file  "")
	 var
	 table
	 keywords
         )
    (when (stringp buffer-file-name)
      (setq file (or buffer-file-name "no-name?")))

    (setq
     keywords
     (cond
      ;; ........................................................ gnus. ...

      ((and (string=  "*Group*" (buffer-name))
	    (boundp 'my-:gnus-group-font-lock-keywords))
       (symbol-value 'my-:gnus-group-font-lock-keywords))

      ((and (string-match  "\\*Summary" (buffer-name))
	    (boundp 'my-:gnus-summary-font-lock-keywords))
       (ti::s-syntax-kill-double-quote)
       (symbol-value 'my-:gnus-summary-font-lock-keywords))

      ((and (string-match  "Article" (buffer-name))
	    (boundp 'my-:gnus-article-font-lock-keywords))
       (symbol-value 'my-:gnus-article-font-lock-keywords))

      ;; ................................................. buffer-menu ...

      ((string-match "\\*Message" (buffer-name))
       (setq font-lock-keywords
	     (list
	      '("error\\|warning\\|can'?t\\|can not\\|invalid"
		0 font-lock-warning-face)

	      '("\\.\\(pl\\)"		1 font-lock-type-face)
	      )))

      ;; ................................................. buffer-menu ...

      ((string-match "buffer list" (buffer-name))
       (setq font-lock-keywords
	     (list
	      '(" \\(\\.emacs-rc-\\).*  " 1 font-lock-reference-face)
	      '(" \\.emacs.\\([^ ]+\\)  " 1 my-face-inv10)
	      '(" tiny\\([^ ]+\\)\\.el"	 1 font-lock-reference-face)

	      '("dired\\|Live-Find-File\\|ange-ftp\\|C\\++"
		0 font-lock-keyword-face)

	      '("Lisp" 0 font-lock-reference-face)

	      '(" [^/]+\\(mail\\|funcs\\|gui\\)"	1 my-face-red-inv)


	      '(" wmp\\(...\\)mx\\...?.?.[< ]"
		1 font-lock-reference-face)


	      '("\\.\\(pl\\)"		1 font-lock-keyword-face)
	      '("\\.\\(rc\\) "		1 font-lock-keyword-face)
	      '("\\(pm-\\)[^ ]+   "	1 font-lock-comment-face)
	      '(" \\.\\(txt\\|cc\\)[< ]" 1 highlight)


	      ;;  wmpmagmx.h<2>
	      '("\\.\\(h\\)[< ]"	1 font-lock-reference-face)


	      '("/\\(wmpman\\)/"    	1 my-face-red-inv)
	      '("/\\(w?txt\\)/"		1 my-face-inv10)
	      '("/\\(e?lisp\\)/"    	1 font-lock-type-face)
	      '("/\\(w\\)txt/"		1 font-lock-reference-face)
	      '("/\\(vax\\)/"		1 font-lock-reference-face)
	      ))
       )

      ;; .................................................. &procmail ...

      ((string-match "procmailrc\\|pm-.*rc" file)
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-defaults nil)
       )

      ;; ....................................................... dired ...


      ((memq major-mode '(dired-mode))
       (setq font-lock-keywords
	     (list
	      '("^. +d.*" . font-lock-function-name-face) ;; Directories
	      '("^. +l.*" . font-lock-string-face)  ;; Symbolic links
	      '("^\\(. +-..[xsS]......\\|^. +-.....[xsS]...\\|^. +-........[xsS]\\).*$" . font-lock-keyword-face) ;; Executables
	      '("^  [/~].*:$" . bold-italic) ;; The directory name
	      '("^.*~$"       . font-lock-comment-face) ;; Backup files
	      '("^. total .*$"   . bold)
	      )))

      ;; ......................................................... &live ...

      ((string-match "pm-.*\\.hdr$" file)  ;;  procmail log buffer
       (ti::s-syntax-kill-double-quote)
       (setq var (ti::d-standard-date 'short))
       (setq font-lock-keywords
	     (list

	      (list (concat "^" var)	0 'font-lock-reference-face)
	      '("[^ \t\n]+@[^ \t\n]+"	0 font-lock-keyword-face)
	      '("send\\|LINT:"		0 my-face-yellow-inv-dark)
	      '("; err-"		0 my-face-LightSalmon-inv)
	      '("PLUS .*/\\(.*\\).spool" 1 my-face-navy)
	      '("exitcode"		0 my-face-navy-inv)
	      )))

      ((string-match "\\.trc$" file)
       (setq font-lock-keywords
	     (list
	      '("P_[^ \n\t]+"	     0 font-lock-keyword-face)
	      '(" ..h "		     0 font-lock-keyword-face)
	      '("/var/tmp/[^ \n\t]+" 0 font-lock-reference-face)
	      '("ORA-.*"	     0 my-face-LightSalmon-inv)
	      )))

      ((string-match "wer.*\\.log$" file)
       (setq font-lock-keywords
	     (list
	      '(".*error.*"	    0 my-face-LightSalmon-inv)
	      '("/var/tmp/[^ \n\t]+" 0 font-lock-reference-face)
	      '("program exit.*\\|.*started.*"    0 font-lock-keyword-face)
	      )))


      ((string-match "pm.*\\.log$" file)
       ;;  The strings "" in the procmail log makes font-lock crazy,
       ;;  We kill the String class from the buffer with these statements.
       ;;
       (ti::s-syntax-kill-double-quote)

       (setq font-lock-keywords
	     (list
	      (cons ": match on"	    'font-lock-function-name-face)
	      (cons (concat
		     "Opening \\|FSUBJ=.*\\|MY_SRV_DIR="
		     "\\|JA_SRV_SUBJECT="
		     "\\|^From.*"
		     )
		    'font-lock-type-face)
	      (cons "INCLUDERC.*\\|FFROM="  'my-face-yellow-inv-dark)
	      (cons "Re: \\|LINT:.*"	'font-lock-function-name-face)
	      (list (concat
		     "\\(.* error .*\\)"
		     )
		    1 'font-lock-keyword-face
		    )
	      (cons "Folder:"		    'font-lock-type-face)
	      (cons
	       (concat
		"Program failure\\|Extraneous locallockfile ignored"
		"\\|Couldn't read\\|cannot open T"
		"\\|Missing closing brace"
		"\\|Bad substitution"
		"\\|syntax error at line"
		"\\|Error while writing"
		"\\|Bypassed lock"
		"\\|Couldn't determine"
		"\\|Extraneous.*"
		"\\|Timeout.*"
		)
	       'my-face-red-inv)
	      '("stat=\\|Skipped .*"	    . my-face-red-inv)
	      (cons "action:  .*"	    'my-face-inv10)
	      ))
       )))

    (if keywords
	(setq font-lock-keywords keywords))
    ))



;;}}}
;;{{{ test

;;; ............................................................ &test ...

;; Anders Lindgren <andersl@csd.uu.se>
;; the big dufference
;; between an Emacs and a XEmacs is the way font-lock is told about the
;; presence of the keywords.
;;
;; The following is a piece of code that installs the keywords correctly
;; for both Emacs and XEmacs:

(when nil
  (let ((foo-defaults '((foo-font-lock-keywords
			 foo-font-lock-keywords-1
			 foo-font-lock-keywords-2
			 foo-font-lock-keywords-3
			 )
			nil   ;; Non-nil means don't fontify strings
			;; and comments
			nil   ;; Non-nil means ignore character case.
			((?_ . "w") (?$ . "w"))   ;; Syntax table modifications
			nil   ;; Function to move point to a safe
			;; starting point, or nil
			;; The rest of the arguments are pairs of
			;; buffer-local variables and their value.
			(font-lock-mark-block-function . mark-defun)
			)))

  (if (string-match "XEmacs" emacs-version)
      ;; XEmacs
      (put 'foo-mode 'font-lock-defaults foo-defaults)
    ;; GNU Emacs
    (let ((pair (assq 'foo-mode font-lock-defaults-alist)))
      (if pair
	  (setq font-lock-defaults-alist
		(delq pair font-lock-defaults-alist))))
    (setq font-lock-defaults-alist
	  (cons (cons 'foo-mode foo-defaults) font-lock-defaults-alist))
    )))


;;; ----------------------------------------------------------------------
;;; Direct modification to font-lock.el
;;;
(let* ((storage "auto\\|extern\\|register\\|static\\|typedef")
       (struct "struct\\|union\\|enum")
       (prefixes "signed\\|unsigned\\|short\\|long")
       (types (concat prefixes "\\|int\\|char\\|float\\|double\\|void"))
       (ctoken "[a-zA-Z0-9_:~*]+")
       (c++-things "")
       )

 (defconst c-font-lock-keywords-my
  (list


   ;; fontify preprocessor directives
   ;; - first the token, then we pick up SOME keywords
   '("^#" . font-lock-function-name-face)
   '("^#[ \t]*\\(ifndef\\|ifdef\\|if\\|elif\\|else\\|endif\\)"
     1 font-lock-function-name-face)


   ;; fontify **names** being defined.
;;:   '("^#[ \t]*\\(define\\|undef\\)[ \t]+\\(\\(\\sw\\|\\s_\\)+\\)" 2
;;:     font-lock-function-name-face)


   ;; fontify the names of functions being defined.
   (list (concat
          "^\\(" ctoken "[ \t]+\\)?"    ; type specs; there can be no
          "\\(" ctoken "[ \t]+\\)?"     ; more than 3 tokens, right?
          "\\(" ctoken "[ \t]+\\)?"
          "\\([*&]+[ \t]*\\)?"          ; pointer
          "\\(" ctoken "\\)[ \t]*(")            ; name
    5 'font-lock-function-name-face)


   ;; Fontify structure names (in structure definition form).

   (list (concat "^\\(" storage "\\)?[ \t]*\\<\\(" struct "\\)"
          "[ \t]+\\(" ctoken "\\)[ \t]*\\(\{\\|$\\)")
    3 'font-lock-function-name-face)


   ;; And likewise for structs

   (list (concat "^[ \t]*\\(\\(" storage "\\)[ \t]+\\)?\\(" struct
          "\\)[ \t]+" ctoken "[ \t]+\\(" ctoken "\\);")
    4 'font-lock-function-name-face 'keep)

   ;; Fontify case clauses.  This is fast because its anchored on the left.

   '("case[ \t]+\\(\\(\\sw\\|\\s_\\)+\\):". 1)
   '("\\<\\(default\\):". 1)

   ;; ......................................................................
   ;; My keywords

   (list
    (concat
     "\\s +\\("
     "while\\|do \\|if\\|for\\|return\\|exit"
     "\\)"
     )
    1 'font-lock-keyword-face)

   (list
    (concat
     "\\("
     "protected:\\|private:\\|public:\\|inline\\|class"
     "\\)"
     )
    1 'font-lock-keyword-face)

   (list
    (concat
     "class\\s +\\("
     ctoken
     "\\)"
     )
    1 'font-lock-function-name-face)




   ))

 )


;;}}}


(when nil
  ;; Make XXX00 stand out as big ugly warnings.

  (setq font-lock-fixme-face (make-face 'font-lock-fixme-face))
  (set-face-foreground 'font-lock-fixme-face "Red")
  (set-face-background 'font-lock-fixme-face "Yellow")

  (setq font-lock-scaffold-face (make-face 'font-lock-scaffold-face))
  (set-face-foreground 'font-lock-scaffold-face "Magenta")

  (font-lock-add-keywords
   'jde-mode
   '(("\\(^.*XXX00.*$\\)" 1 font-lock-fixme-face prepend)
     ("\\<\\(Debug.assert\\|Debug.error\\|Debug.notImplemented\\)\\>"
      . font-lock-scaffold-face)))
  )



(require 'mouse)

(when (and (boundp 'mouse-secondary-overlay) ;XEmacs...
	   (overlayp mouse-secondary-overlay))
  (overlay-put mouse-secondary-overlay 'face 'secondary-selection)
  (overlay-put mouse-drag-overlay      'face 'region)
  )

(when (featurep 'font-lock)
  (my-font-lock-colors))

(provide 'rc-font)


(defvar rc-:font-hook nil)
(run-hooks 'rc-:font-hook)

;;; emacs-rc-font.el ends here
