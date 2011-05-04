;;; ergo-font.el --- quick font changer

;; Copyright (C)  2000  Jonadab the Unsightly One <jonadab@bright.net>

;; Author: Jonadab the Unsightly One <jonadab@bright.net>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 5.0.1
;; Keywords: faces

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Sharing your current color setup:
;;
;; If you have already invested time in customizing Emacs faces, please
;; consider sharing your current setup.  Make sure that color-theme.el
;; is in your `load-path'.  Type M-x load-library RET color-theme RET to
;; load all the functions.  Type M-x ergo-font-print RET and mail the
;; result to the maintainer of this package (see above for mail addres).
;;
;; If you want to make sure that all your customization was exported,
;; type M-x list-faces-display RET to get a list of all faces currently
;; defined.  This is the list of faces that `ergo-font-print' uses.

;; Installing a color theme:
;;
;; Make sure that color-theme.el is in your `load-path'.  Type M-x
;; load-library RET color-theme RET to load all the functions.
;;
;; The main function to call is ergo-font-select.  Type M-x
;; ergo-font-select RET.  That creates a Color Theme Selection
;; buffer.  Press RET or `i' on a color theme to install it for the
;; rest of your session.
;;
;; If you want to install the color theme as soon as Emacs is started
;; up, read the description of the theme you like and remember the
;; name of the color theme function.  Press `d' on a color theme in
;; the Color Theme Selection buffer to read the description.  Assuming
;; you like the Gnome2 theme, you'll find that the function to use is
;; called `ergo-font-gnome2'.  Add the following to the end of your
;; .emacs (removing the leading `;;').
;;
;; (require 'color-theme)
;; (ergo-font-gnome2)

;; Changing fonts:
;;
;; If are using X, you can set the menu foreground and background using
;; your .Xdefaults file.  If you set emacs*Background and
;; emacs*Foreground, the first frame will be created with these
;; foreground and background colors used for the menu.  If your .emacs
;; loads a color theme, the frame foreground and background colors
;; overwrite the settings from the .Xdefaults file in the frame itself,
;; but not for the menu.  This assumes that you are not setting any menu
;; ressources for Emacs in the .Xdefaults file.  Here is a sample entry
;; for your .Xdefaults file:
;;
;;   emacs*Background:		DarkSlateGray
;;   emacs*Foreground:		wheat

;; Making a color theme work for both Emacs and XEmacs:
;;
;; The most important thing is to add missing faces for the other
;; editor.  These are the most important faces to check:
;;
;; In Emacs                       In XEmacs
;; `font-lock-builtin-face'       `font-lock-reference-face'
;; `font-lock-string-face'        `font-lock-doc-string-face'
;; `font-lock-constant-face'      `font-lock-preprocessor-face'
;; `modeline'                     `modeline-buffer-id'
;; `modeline'                     `modeline-mousable'
;; `modeline'                     `modeline-mousable-minor-mode'
;; `region'                       `primary-selection'
;; `region'                       `isearch'

;; Deriving your own color theme:
;;
;; If you want to derive your own color theme from an existing color
;; theme, press `p' in the Color Theme Selection buffer (it doesn't
;; matter where in the buffer you press `p'.  This creates a buffer with
;; the elisp code needed to install the current color theme.  Copy the
;; entire code to your .emacs and start fooling around with it.  Read
;; the documentation of ergo-font-install using C-h f
;; ergo-font-install RET.
;;
;; Note that all color themes are cumulative.  You can try to combine
;; several color themes.  This makes sense if one color theme defines
;; faces which another color theme does not.  Install both themes by
;; pressing RET or `i' on them in the Color Theme Selection buffer,
;; press `p' to get the elisp code, paste it into your .emacs and
;; start working on your masterpiece.
;;
;; If your color theme is but a variation of an existing color theme,
;; install the parent color theme, make the modifications you want,
;; and then use C-u p or C-u M-x ergo-font-print to avoid
;; duplicating settings from the parent color theme.

;;; Thanks

;; S. Pokrovsky <pok@nbsp.nsk.su> for ideas and discussion.
;; All the users that contributed their color themes.

;;; Bugs:

;; Emacs 20.6: Some faces are created using copy-face; these faces are
;; not printed correctly.  This causes the following to be non-equal:
;; (copy-face 'bold 'new-bold)
;; (equal (face-attr-construct 'bold)
;;        (face-attr-construct 'new-bold))
;; A patch was submitted to the Emacs maintainers.
;;
;; XEmacs 21.2: Not compatible with the custom-theme mode.  It should be
;; easy to transform the color-theme source into custom-theme source,
;; however.
;;
;; Note that this package includes a compatibility layer for Emacs and
;; XEmacs which fixes some bugs encountered in Emacs 20.6 (patches
;; submitted).
;;
;; If you are running XEmacs, then only foreground and background color
;; of the default face and only the background color of the text-cursor
;; face will used.  This is due to the fact that these three facts are
;; stored as frame parameters in Emacs.
;;
;; If you are running XEmacs, variables cannot have a frame-local
;; binding.  Therefore, if ergo-font-is-global is set to nil, the
;; variable settings in a color theme are ignored.
;;
;; Using Emacs and a non-nil value for ergo-font-is-global will
;; install a new color theme for all frames.  Using XEmacs and a non-nil
;; value for ergo-font-is-global will install a new color theme only
;; on those frames that are not using a local color theme.
;;
;; Tested with Emacs 20.6 and XEmacs 21.1



;;; Code:

(defvar ergo-font-xemacs-p (string-match "XEmacs" emacs-version)
  "Non-nil if running XEmacs.")


(if ergo-font-xemacs-p ;; XEmacs
	 (progn
		(require 'mouse); need face-custom-attributes-set and other functions for XEmacs
		(require 'font-menu); need face-custom-attributes-set and other functions for XEmacs
		)
  
  (progn
	 (require 'mouse); need face-custom-attributes-set and other functions for XEmacs
	 ))




;; Customization

(defgroup ergo-font nil
  "Custom Fonts for Emacs.  
A color theme consists of frame parameter settings, variable settings,
and face definitions."
  )


;; List of font commands.

(defvar ergo-command-list
  '((bury-buffer "[Quit]" "Bury this buffer.")

    ((lambda () (interactive) (call-interactively 'ergo-font-dialog )) "[Dialog]" "show font dialog.")
    ((lambda () (interactive) (call-interactively 'set-frame-font )) "[Manual]" "select frame font.")

    ((lambda () (ergo-font-set-frame-size '(40 15 nil nil))) "[40x15]" "(40 15 nil nil)")
    ((lambda () (ergo-font-set-frame-size '(75 20 5 30))) "[75x20]" "(75 20 5 30)")
    ((lambda () (ergo-font-set-frame-size '(80 25 nil nil))) "[80x25]" "(80 25 nil nil)")
    ((lambda () (ergo-font-set-frame-size '(82 35 nil nil))) "[82x35]" "(82 35 nil nil)")
    ((lambda () (ergo-font-set-frame-size '(85 43 nil 30))) "[85x43]" "(85 43 nil nil)")
    ((lambda () (ergo-font-set-frame-size '(90 50 nil nil))) "[90x50]" "(90 50 nil nil)")
    ((lambda () (ergo-font-set-frame-size '(100 30 5 30))) "[100x30]" "(100 30 5 30)")
    ((lambda () (ergo-font-set-frame-size '(110 55 5 30))) "[110x55]" "(110 55 5 30)")
    ((lambda () (ergo-font-set-frame-size '(132 65 5 30))) "[132x60]" "(132 60 5 30)")
    ((lambda () (ergo-font-set-frame-size '(150 65  5 30))) "[150x65]" "(150 65 5 30)")

    ) "List of ergo font commands.")




(defvar ergo-font-list
  (if ergo-font-xemacs-p ;; XEmacs
		(cond  
		 ((eq system-type 'windows-nt) ;; WinNT
		  (cond 
			(t
  '(
;;---Font Size---
     ("7"  (nil nil "7") (90 65 nil nil))
     ("8"  (nil nil "8") (90 55 nil nil))
     ("9"  (nil nil "9") (90 45 nil nil))
     ("10"  (nil nil "10") (80 35 nil nil))
     ("11"  (nil nil "11") (80 35 nil nil))
     ("12"  (nil nil "12") (80 35 nil nil))
     ("13"  (nil nil "13") (80 35 nil nil))
     ("14"  (nil nil "14") (80 25 nil nil))
     ("15"  (nil nil "15") (80 25 nil nil))
     ("16"  (nil nil "16") (80 25 nil nil))
     ("17"  (nil nil "17") (80 20 nil nil))
     ("18"  (nil nil "18") (70 20 nil nil))
     ("19"  (nil nil "19") (70 20 nil nil))
     ("20"  (nil nil "20") (70 20 nil nil))
;;---Font Family---
     (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
     ("Lucida Sans Typewriter"  ("Lucida Sans Typewriter" nil nil) (nil nil nil nil))
     ("Lucida Console"            ("Lucida Console" nil nil)                (nil nil nil nil))
     ("Andale Mono"                ("Andale Mono" nil nil)                (nil nil nil nil))
     ("Courier New"                ("Courier New" nil nil)                (nil nil nil nil))
     ("SimSun"                       ("SimSun" nil nil)                (nil nil nil nil))
     (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
     ("Arial"                        ("Arial" nil nil)                (nil nil nil nil))
     ("Century Gothic"            ("Century Gothic" nil nil)                (nil nil nil nil))
     ("Comic Sans MS"              ("Comic Sans MS" nil nil)                (nil nil nil nil))
     ("Dom Casual"                  ("Dom Casual" nil nil)                (nil nil nil nil))
     ("Garrison ExtraBold Sans"  ("Garrison ExtraBold Sans" nil nil)                (nil nil nil nil))
     ("Lucida Sans Unicode"      ("Lucida Sans Unicode" nil nil)                (nil nil nil nil))
     ("Monotype Corsiva"          ("Monotype Corsiva" nil nil)                (nil nil nil nil))
     ("Tahoma"                        ("Tahoma" nil nil)                (nil nil nil nil))
     ("Trebuchet MS"               ("Trebuchet MS" nil nil)                (nil nil nil nil))
     ("VAG Round"                   ("VAG Round" nil nil)                (nil nil nil nil))
     ("Verdana"                      ("Verdana" nil nil)                (nil nil nil nil))
     (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
     ("Bookman Old Style"        ("Bookman Old Style" nil nil)                (nil nil nil nil))
     ("Batang"                      ("Batang" nil nil)                (nil nil nil nil))
     ("Book Antiqua"              ("Book Antiqua" nil nil)                (nil nil nil nil))
     ("Century"                     ("Century" nil nil)                (nil nil nil nil))
     ("Garamond"                    ("Garamond" nil nil)                (nil nil nil nil))
     ("Georgia"                     ("Georgia" nil nil)                (nil nil nil nil))
     ("PMingLiU"                    ("PMingLiU" nil nil)                (nil nil nil nil))
     ("Times New Roman"           ("Times New Roman" nil nil)                (nil nil nil nil))
     ("Village Square"            ("Village Square" nil nil)                (nil nil nil nil))
    )
  )))
		 ((eq system-type 'linux);; GNU-Linux
		  (cond 
			((string= (getenv "DISPLAY") ":0.0")
			 '(
				;;---Font Size---
			 ("7"  (nil nil "7") (90 65 nil nil))
			 ("8"  (nil nil "8") (90 55 nil nil))
			 ("9"  (nil nil "9") (90 45 nil nil))
			 ("10"  (nil nil "10") (80 35 nil nil))
			 ("11"  (nil nil "11") (80 35 nil nil))
			 ("12"  (nil nil "12") (80 35 nil nil))
			 ("13"  (nil nil "13") (80 35 nil nil))
			 ("14"  (nil nil "14") (80 25 nil nil))
			 ("15"  (nil nil "15") (80 25 nil nil))
			 ("16"  (nil nil "16") (80 25 nil nil))
			 ("17"  (nil nil "17") (80 20 nil nil))
			 ("18"  (nil nil "18") (70 20 nil nil))
			 ("19"  (nil nil "19") (70 20 nil nil))
			 ("20"  (nil nil "20") (70 20 nil nil))
			 ;;---Font Family---
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Lucida Sans Typewriter"  ("Lucida Sans Typewriter" nil nil) (nil nil nil nil))
			 ("Lucida Console"            ("Lucida Console" nil nil)                (nil nil nil nil))
			 ("Andale Mono"                ("Andale Mono" nil nil)                (nil nil nil nil))
			 ("Courier New"                ("Courier New" nil nil)                (nil nil nil nil))
			 ("SimSun"                       ("SimSun" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Arial"                        ("Arial" nil nil)                (nil nil nil nil))
			 ("Century Gothic"            ("Century Gothic" nil nil)                (nil nil nil nil))
			 ("Comic Sans MS"              ("Comic Sans MS" nil nil)                (nil nil nil nil))
			 ("Dom Casual"                  ("Dom Casual" nil nil)                (nil nil nil nil))
			 ("Garrison ExtraBold Sans"  ("Garrison ExtraBold Sans" nil nil)                (nil nil nil nil))
			 ("Lucida Sans Unicode"      ("Lucida Sans Unicode" nil nil)                (nil nil nil nil))
			 ("Monotype Corsiva"          ("Monotype Corsiva" nil nil)                (nil nil nil nil))
			 ("Tahoma"                        ("Tahoma" nil nil)                (nil nil nil nil))
			 ("Trebuchet MS"               ("Trebuchet MS" nil nil)                (nil nil nil nil))
			 ("VAG Round"                   ("VAG Round" nil nil)                (nil nil nil nil))
			 ("Verdana"                      ("Verdana" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Bookman Old Style"        ("Bookman Old Style" nil nil)                (nil nil nil nil))
			 ("Batang"                      ("Batang" nil nil)                (nil nil nil nil))
			 ("Book Antiqua"              ("Book Antiqua" nil nil)                (nil nil nil nil))
			 ("Century"                     ("Century" nil nil)                (nil nil nil nil))
			 ("Garamond"                    ("Garamond" nil nil)                (nil nil nil nil))
			 ("Georgia"                     ("Georgia" nil nil)                (nil nil nil nil))
			 ("PMingLiU"                    ("PMingLiU" nil nil)                (nil nil nil nil))
			 ("Times New Roman"           ("Times New Roman" nil nil)                (nil nil nil nil))
			 ("Village Square"            ("Village Square" nil nil)                (nil nil nil nil))
			 )
			 )
			(t
			 '(
				;;---Font Size---
			 ("7"  (nil nil "7") (90 65 nil nil))
			 ("8"  (nil nil "8") (90 55 nil nil))
			 ("9"  (nil nil "9") (90 45 nil nil))
			 ("10"  (nil nil "10") (80 35 nil nil))
			 ("11"  (nil nil "11") (80 35 nil nil))
			 ("12"  (nil nil "12") (80 35 nil nil))
			 ("13"  (nil nil "13") (80 35 nil nil))
			 ("14"  (nil nil "14") (80 25 nil nil))
			 ("15"  (nil nil "15") (80 25 nil nil))
			 ("16"  (nil nil "16") (80 25 nil nil))
			 ("17"  (nil nil "17") (80 20 nil nil))
			 ("18"  (nil nil "18") (70 20 nil nil))
			 ("19"  (nil nil "19") (70 20 nil nil))
			 ("20"  (nil nil "20") (70 20 nil nil))
			 ;;---Font Family---
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Lucida Sans Typewriter"  ("Lucida Sans Typewriter" nil nil) (nil nil nil nil))
			 ("Lucida Console"            ("Lucida Console" nil nil)                (nil nil nil nil))
			 ("Andale Mono"                ("Andale Mono" nil nil)                (nil nil nil nil))
			 ("Courier New"                ("Courier New" nil nil)                (nil nil nil nil))
			 ("SimSun"                       ("SimSun" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Arial"                        ("Arial" nil nil)                (nil nil nil nil))
			 ("Century Gothic"            ("Century Gothic" nil nil)                (nil nil nil nil))
			 ("Comic Sans MS"              ("Comic Sans MS" nil nil)                (nil nil nil nil))
			 ("Dom Casual"                  ("Dom Casual" nil nil)                (nil nil nil nil))
			 ("Garrison ExtraBold Sans"  ("Garrison ExtraBold Sans" nil nil)                (nil nil nil nil))
			 ("Lucida Sans Unicode"      ("Lucida Sans Unicode" nil nil)                (nil nil nil nil))
			 ("Monotype Corsiva"          ("Monotype Corsiva" nil nil)                (nil nil nil nil))
			 ("Tahoma"                        ("Tahoma" nil nil)                (nil nil nil nil))
			 ("Trebuchet MS"               ("Trebuchet MS" nil nil)                (nil nil nil nil))
			 ("VAG Round"                   ("VAG Round" nil nil)                (nil nil nil nil))
			 ("Verdana"                      ("Verdana" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Bookman Old Style"        ("Bookman Old Style" nil nil)                (nil nil nil nil))
			 ("Batang"                      ("Batang" nil nil)                (nil nil nil nil))
			 ("Book Antiqua"              ("Book Antiqua" nil nil)                (nil nil nil nil))
			 ("Century"                     ("Century" nil nil)                (nil nil nil nil))
			 ("Garamond"                    ("Garamond" nil nil)                (nil nil nil nil))
			 ("Georgia"                     ("Georgia" nil nil)                (nil nil nil nil))
			 ("PMingLiU"                    ("PMingLiU" nil nil)                (nil nil nil nil))
			 ("Times New Roman"           ("Times New Roman" nil nil)                (nil nil nil nil))
			 ("Village Square"            ("Village Square" nil nil)                (nil nil nil nil))
			 )
			 )))
		 ((eq system-type 'usg-unix-v);; Sun Solaris
		  (cond 
			((string= (getenv "DISPLAY") ":0.0")
			 '(
				;;---Font Size---
			 ("7"  (nil nil "7") (90 65 nil nil))
			 ("8"  (nil nil "8") (90 55 nil nil))
			 ("9"  (nil nil "9") (90 45 nil nil))
			 ("10"  (nil nil "10") (80 35 nil nil))
			 ("11"  (nil nil "11") (80 35 nil nil))
			 ("12"  (nil nil "12") (80 35 nil nil))
			 ("13"  (nil nil "13") (80 35 nil nil))
			 ("14"  (nil nil "14") (80 25 nil nil))
			 ("15"  (nil nil "15") (80 25 nil nil))
			 ("16"  (nil nil "16") (80 25 nil nil))
			 ("17"  (nil nil "17") (80 20 nil nil))
			 ("18"  (nil nil "18") (70 20 nil nil))
			 ("19"  (nil nil "19") (70 20 nil nil))
			 ("20"  (nil nil "20") (70 20 nil nil))
			 ;;---Font Family---
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Lucida Sans Typewriter"  ("Lucida Sans Typewriter" nil nil) (nil nil nil nil))
			 ("Lucida Console"            ("Lucida Console" nil nil)                (nil nil nil nil))
			 ("Andale Mono"                ("Andale Mono" nil nil)                (nil nil nil nil))
			 ("Courier New"                ("Courier New" nil nil)                (nil nil nil nil))
			 ("SimSun"                       ("SimSun" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Arial"                        ("Arial" nil nil)                (nil nil nil nil))
			 ("Century Gothic"            ("Century Gothic" nil nil)                (nil nil nil nil))
			 ("Comic Sans MS"              ("Comic Sans MS" nil nil)                (nil nil nil nil))
			 ("Dom Casual"                  ("Dom Casual" nil nil)                (nil nil nil nil))
			 ("Garrison ExtraBold Sans"  ("Garrison ExtraBold Sans" nil nil)                (nil nil nil nil))
			 ("Lucida Sans Unicode"      ("Lucida Sans Unicode" nil nil)                (nil nil nil nil))
			 ("Monotype Corsiva"          ("Monotype Corsiva" nil nil)                (nil nil nil nil))
			 ("Tahoma"                        ("Tahoma" nil nil)                (nil nil nil nil))
			 ("Trebuchet MS"               ("Trebuchet MS" nil nil)                (nil nil nil nil))
			 ("VAG Round"                   ("VAG Round" nil nil)                (nil nil nil nil))
			 ("Verdana"                      ("Verdana" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Bookman Old Style"        ("Bookman Old Style" nil nil)                (nil nil nil nil))
			 ("Batang"                      ("Batang" nil nil)                (nil nil nil nil))
			 ("Book Antiqua"              ("Book Antiqua" nil nil)                (nil nil nil nil))
			 ("Century"                     ("Century" nil nil)                (nil nil nil nil))
			 ("Garamond"                    ("Garamond" nil nil)                (nil nil nil nil))
			 ("Georgia"                     ("Georgia" nil nil)                (nil nil nil nil))
			 ("PMingLiU"                    ("PMingLiU" nil nil)                (nil nil nil nil))
			 ("Times New Roman"           ("Times New Roman" nil nil)                (nil nil nil nil))
			 ("Village Square"            ("Village Square" nil nil)                (nil nil nil nil))
			 )
			 )
			(t
			 '(
				;;---Font Size---
			 ("7"  (nil nil "7") (90 65 nil nil))
			 ("8"  (nil nil "8") (90 55 nil nil))
			 ("9"  (nil nil "9") (90 45 nil nil))
			 ("10"  (nil nil "10") (80 35 nil nil))
			 ("11"  (nil nil "11") (80 35 nil nil))
			 ("12"  (nil nil "12") (80 35 nil nil))
			 ("13"  (nil nil "13") (80 35 nil nil))
			 ("14"  (nil nil "14") (80 25 nil nil))
			 ("15"  (nil nil "15") (80 25 nil nil))
			 ("16"  (nil nil "16") (80 25 nil nil))
			 ("17"  (nil nil "17") (80 20 nil nil))
			 ("18"  (nil nil "18") (70 20 nil nil))
			 ("19"  (nil nil "19") (70 20 nil nil))
			 ("20"  (nil nil "20") (70 20 nil nil))
			 ;;---Font Family---
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Lucida Sans Typewriter"  ("Lucida Sans Typewriter" nil nil) (nil nil nil nil))
			 ("Lucida Console"            ("Lucida Console" nil nil)                (nil nil nil nil))
			 ("Andale Mono"                ("Andale Mono" nil nil)                (nil nil nil nil))
			 ("Courier New"                ("Courier New" nil nil)                (nil nil nil nil))
			 ("SimSun"                       ("SimSun" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Arial"                        ("Arial" nil nil)                (nil nil nil nil))
			 ("Century Gothic"            ("Century Gothic" nil nil)                (nil nil nil nil))
			 ("Comic Sans MS"              ("Comic Sans MS" nil nil)                (nil nil nil nil))
			 ("Dom Casual"                  ("Dom Casual" nil nil)                (nil nil nil nil))
			 ("Garrison ExtraBold Sans"  ("Garrison ExtraBold Sans" nil nil)                (nil nil nil nil))
			 ("Lucida Sans Unicode"      ("Lucida Sans Unicode" nil nil)                (nil nil nil nil))
			 ("Monotype Corsiva"          ("Monotype Corsiva" nil nil)                (nil nil nil nil))
			 ("Tahoma"                        ("Tahoma" nil nil)                (nil nil nil nil))
			 ("Trebuchet MS"               ("Trebuchet MS" nil nil)                (nil nil nil nil))
			 ("VAG Round"                   ("VAG Round" nil nil)                (nil nil nil nil))
			 ("Verdana"                      ("Verdana" nil nil)                (nil nil nil nil))
			 (">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    (nil nil nil)   (nil nil nil nil))
			 ("Bookman Old Style"        ("Bookman Old Style" nil nil)                (nil nil nil nil))
			 ("Batang"                      ("Batang" nil nil)                (nil nil nil nil))
			 ("Book Antiqua"              ("Book Antiqua" nil nil)                (nil nil nil nil))
			 ("Century"                     ("Century" nil nil)                (nil nil nil nil))
			 ("Garamond"                    ("Garamond" nil nil)                (nil nil nil nil))
			 ("Georgia"                     ("Georgia" nil nil)                (nil nil nil nil))
			 ("PMingLiU"                    ("PMingLiU" nil nil)                (nil nil nil nil))
			 ("Times New Roman"           ("Times New Roman" nil nil)                (nil nil nil nil))
			 ("Village Square"            ("Village Square" nil nil)                (nil nil nil nil))
			 )
			 )))
		 )

		(cond  ;;--- fsf-emacs ---------------------------------------------------
		 ((eq system-type 'windows-nt) ;; WinNT
		  (cond 
			(t
			 '(

				("09-Monaco"  "-*-Monaco-r-*-*-*-9-*-*-c-*-iso8859-1"    (256 65 5 30) )
				("10-Monaco"  "-*-Monaco-r-*-*-*-10-*-*-c-*-iso8859-1"    (220 65 5 30) )
				("11-Monaco"  "-*-Monaco-r-*-*-*-11-*-*-c-*-iso8859-1"    (180 60 5 30) )
				("12-Monaco"  "-*-Monaco-r-*-*-*-12-*-*-c-*-iso8859-1"    (120 50 nil 30) )
				("13-Monaco"  "-*-Monaco-r-*-*-*-13-*-*-c-*-iso8859-1"    (110 47 nil 30) )
				("14-Monaco"  "-*-Monaco-r-*-*-*-14-*-*-c-*-iso8859-1"    (100 47 nil 30) )
				("15-Monaco"  "-*-Monaco-r-*-*-*-15-*-*-c-*-iso8859-1"    (100 45 nil 30) )
				("16-Monaco"  "-*-Monaco-r-*-*-*-16-*-*-c-*-iso8859-1"    ( 90 42 nil 30) )
				("17-Monaco"  "-*-Monaco-r-*-*-*-17-*-*-c-*-iso8859-1"    ( 90 37 nil 30) )
				("18-Monaco"  "-*-Monaco-r-*-*-*-18-*-*-c-*-iso8859-1"    ( 92 36 nil 30) )
				("19-Monaco"  "-*-Monaco-r-*-*-*-19-*-*-c-*-iso8859-1"    (95 36 30 30) )
				("20-Monaco"  "-*-Monaco-r-*-*-*-20-*-*-c-*-iso8859-1"    (95 32 30 30) )
				("21-Monaco"  "-*-Monaco-r-*-*-*-21-*-*-c-*-iso8859-1"    (92 32 30 30) )
				("22-Monaco"  "-*-Monaco-r-*-*-*-22-*-*-c-*-iso8859-1"    (90 28 30 30) )
				("23-Monaco"  "-*-Monaco-r-*-*-*-23-*-*-c-*-iso8859-1"    (90 28 30 30) )
				("24-Monaco"  "-*-Monaco-r-*-*-*-24-*-*-c-*-iso8859-1"    (80 25 30 30) )
				("25-Monaco"  "-*-Monaco-r-*-*-*-25-*-*-c-*-iso8859-1"    (80 25 5 30) )
				("26-Monaco"  "-*-Monaco-r-*-*-*-26-*-*-c-*-iso8859-1"    (80 25 5 30) )
				("27-Monaco"  "-*-Monaco-r-*-*-*-27-*-*-c-*-iso8859-1"    (80 24 5 30) )
				("28-Monaco"  "-*-Monaco-r-*-*-*-28-*-*-c-*-iso8859-1"    (78 23 5 30) )
				("29-Monaco"  "-*-Monaco-r-*-*-*-29-*-*-c-*-iso8859-1"    (75 23 5 30) )
				("30-Monaco"  "-*-Monaco-r-*-*-*-30-*-*-c-*-iso8859-1"    (72 23 5 30) )
				("35-Monaco"  "-*-Monaco-r-*-*-*-35-*-*-c-*-iso8859-1"    (62 19 5 30) )
					  				 
				("11-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (120 75 nil nil) )
				("12-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (100 65 nil nil) )
				("13-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (100 60 nil nil) )
				("14-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (90 55 nil nil) )
				("15-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (90 50 nil nil) )
				("16-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (90 50 nil nil) )
				("17-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (90 45 nil nil) )
				("18-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (90 40 nil nil) )
				("19-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (95 40 30 30) )
				("20-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (92 40 30 30) )
				("21-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (90 40 30 30) )
				("22-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (90 35 30 30) )
				("23-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (90 35 30 30) )
				("24-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (80 30 30 30) )
				("25-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (80 30 30 30) )
				("26-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (80 30 30 30) )
				("27-Monospace"  "-bitstream-bitstream vera sans mono-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (80 30 30 30) )

				("11-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Lucida"  "-*-Lucida Sans Unicode-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )

				("11-Terminal"  "-*-Terminal-r-*-*-*-11-*-*-*-*-*-*"    (120 75 nil nil) )
				("12-Terminal"  "-*-Terminal-r-*-*-*-12-*-*-*-*-*-*"    (100 65 nil nil) )
				("13-Terminal"  "-*-Terminal-r-*-*-*-13-*-*-*-*-*-*"    (100 60 nil nil) )
				("14-Terminal"  "-*-Terminal-r-*-*-*-14-*-*-*-*-*-*"    (90 55 nil nil) )
				("15-Terminal"  "-*-Terminal-r-*-*-*-15-*-*-*-*-*-*"    (90 50 nil nil) )
				("16-Terminal"  "-*-Terminal-r-*-*-*-16-*-*-*-*-*-*"    (90 50 nil nil) )
				("17-Terminal"  "-*-Terminal-r-*-*-*-17-*-*-*-*-*-*"    (90 45 nil nil) )
				("18-Terminal"  "-*-Terminal-r-*-*-*-18-*-*-*-*-*-*"    (90 40 nil nil) )
				("19-Terminal"  "-*-Terminal-r-*-*-*-19-*-*-*-*-*-*"    (95 40 30 30) )
				("20-Terminal"  "-*-Terminal-r-*-*-*-20-*-*-*-*-*-*"    (92 40 30 30) )
				("21-Terminal"  "-*-Terminal-r-*-*-*-21-*-*-*-*-*-*"    (90 40 30 30) )
				("22-Terminal"  "-*-Terminal-r-*-*-*-22-*-*-*-*-*-*"    (90 35 30 30) )
				("23-Terminal"  "-*-Terminal-r-*-*-*-23-*-*-*-*-*-*"    (90 35 30 30) )
				("24-Terminal"  "-*-Terminal-r-*-*-*-24-*-*-*-*-*-*"    (80 30 30 30) )
				("25-Terminal"  "-*-Terminal-r-*-*-*-25-*-*-*-*-*-*"    (80 30 30 30) )
				("26-Terminal"  "-*-Terminal-r-*-*-*-26-*-*-*-*-*-*"    (80 30 30 30) )
				("27-Terminal"  "-*-Terminal-r-*-*-*-27-*-*-*-*-*-*"    (80 30 30 30) )
					  				 												
				("11-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (90 55 nil nil) )
				("12-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (85 45 nil nil) )
				("13-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (80 45 nil nil) )
				("14-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (80 43 nil nil) )
				("15-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (80 41 nil nil) )
				("16-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (80 40 nil nil) )
				("17-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (80 35 nil nil) )
				("18-Typewriter"  "-*-Lucida Sans Typewriter-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (80 30 nil nil) )

				("20-Orator15  "  "-*-Orator15 BT-bold-r-normal-normal-27-202-96-96-c-120-iso8859-15"   (78 25 5 35) )
				("22-Orator15  "  "-*-Orator15 BT-bold-r-normal-normal-29-217-96-96-c-130-iso8859-15"   (72 23 5 35) )
				("16-Orator10  "  "-*-Orator10 BT-bold-r-normal-normal-21-157-96-96-c-140-iso8859-15"   (68 24 5 35) )
				("18-Orator10  "  "-*-Orator10 BT-bold-r-normal-normal-24-180-96-96-c-150-iso8859-15"   (64 22 5 35) )
					  						 
				("09-Tahoma"  "-*-Tahoma-normal-r-*-*-9-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("10-Tahoma"  "-*-Tahoma-normal-r-*-*-10-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("11-Tahoma"  "-*-Tahoma-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Tahoma"  "-*-Tahoma-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Tahoma"  "-*-Tahoma-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (110 55 0 90) )
				("14-Tahoma"  "-*-Tahoma-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Tahoma"  "-*-Tahoma-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Tahoma"  "-*-Tahoma-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Tahoma"  "-*-Tahoma-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Tahoma"  "-*-Tahoma-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Tahoma"  "-*-Tahoma-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Tahoma"  "-*-Tahoma-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Tahoma"  "-*-Tahoma-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Tahoma"  "-*-Tahoma-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Tahoma"  "-*-Tahoma-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Tahoma"  "-*-Tahoma-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Tahoma"  "-*-Tahoma-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Tahoma"  "-*-Tahoma-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Tahoma"  "-*-Tahoma-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (120 49 500 30))
				("16-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Trebuchet MS"  "-*-Trebuchet MS-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Comic Sans MS"  "-*-Comic Sans MS-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Marydale"  "-*-Marydale-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Marydale"  "-*-Marydale-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Marydale"  "-*-Marydale-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Marydale"  "-*-Marydale-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Marydale"  "-*-Marydale-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Marydale"  "-*-Marydale-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Marydale"  "-*-Marydale-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Marydale"  "-*-Marydale-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Marydale"  "-*-Marydale-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Marydale"  "-*-Marydale-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Marydale"  "-*-Marydale-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Marydale"  "-*-Marydale-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Marydale"  "-*-Marydale-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Marydale"  "-*-Marydale-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Marydale"  "-*-Marydale-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Marydale"  "-*-Marydale-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Marydale"  "-*-Marydale-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Marydale"  "-*-Marydale-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Marydale"  "-*-Marydale-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Marydale"  "-*-Marydale-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Marydale"  "-*-Marydale-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Marydale"  "-*-Marydale-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Marydale"  "-*-Marydale-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Marydale"  "-*-Marydale-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Marydale"  "-*-Marydale-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Marydale"  "-*-Marydale-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Marydale"  "-*-Marydale-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("20-Cage"  "-*-Cage-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Cage"  "-*-Cage-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Cage"  "-*-Cage-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Cage"  "-*-Cage-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Cage"  "-*-Cage-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Cage"  "-*-Cage-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Cage"  "-*-Cage-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Cage"  "-*-Cage-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Cage"  "-*-Cage-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Cage"  "-*-Cage-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Cage"  "-*-Cage-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Cage"  "-*-Cage-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Cage"  "-*-Cage-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Cage"  "-*-Cage-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Cage"  "-*-Cage-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Cage"  "-*-Cage-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Cage"  "-*-Cage-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Cage"  "-*-Cage-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("38-Cage"  "-*-Cage-normal-r-*-*-38-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("39-Cage"  "-*-Cage-normal-r-*-*-39-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("40-Cage"  "-*-Cage-normal-r-*-*-40-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("41-Cage"  "-*-Cage-normal-r-*-*-41-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("42-Cage"  "-*-Cage-normal-r-*-*-42-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("43-Cage"  "-*-Cage-normal-r-*-*-43-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("44-Cage"  "-*-Cage-normal-r-*-*-44-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("45-Cage"  "-*-Cage-normal-r-*-*-45-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
					  
				("20-Carolingia"  "-*-Carolingia-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Carolingia"  "-*-Carolingia-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Carolingia"  "-*-Carolingia-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Carolingia"  "-*-Carolingia-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Carolingia"  "-*-Carolingia-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Carolingia"  "-*-Carolingia-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Carolingia"  "-*-Carolingia-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Carolingia"  "-*-Carolingia-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Carolingia"  "-*-Carolingia-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Carolingia"  "-*-Carolingia-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Carolingia"  "-*-Carolingia-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Carolingia"  "-*-Carolingia-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Carolingia"  "-*-Carolingia-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Carolingia"  "-*-Carolingia-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Carolingia"  "-*-Carolingia-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Carolingia"  "-*-Carolingia-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Carolingia"  "-*-Carolingia-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Carolingia"  "-*-Carolingia-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("38-Carolingia"  "-*-Carolingia-normal-r-*-*-38-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("39-Carolingia"  "-*-Carolingia-normal-r-*-*-39-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("40-Carolingia"  "-*-Carolingia-normal-r-*-*-40-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
					  
				("11-Abscissa"  "-*-Abscissa-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Abscissa"  "-*-Abscissa-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Abscissa"  "-*-Abscissa-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Abscissa"  "-*-Abscissa-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Abscissa"  "-*-Abscissa-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Abscissa"  "-*-Abscissa-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Abscissa"  "-*-Abscissa-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Abscissa"  "-*-Abscissa-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Abscissa"  "-*-Abscissa-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Abscissa"  "-*-Abscissa-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Abscissa"  "-*-Abscissa-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Abscissa"  "-*-Abscissa-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Abscissa"  "-*-Abscissa-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Abscissa"  "-*-Abscissa-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Abscissa"  "-*-Abscissa-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Abscissa"  "-*-Abscissa-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Abscissa"  "-*-Abscissa-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Abscissa"  "-*-Abscissa-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Abscissa"  "-*-Abscissa-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Abscissa"  "-*-Abscissa-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Abscissa"  "-*-Abscissa-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Abscissa"  "-*-Abscissa-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Abscissa"  "-*-Abscissa-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Abscissa"  "-*-Abscissa-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Abscissa"  "-*-Abscissa-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Abscissa"  "-*-Abscissa-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Abscissa"  "-*-Abscissa-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Dom Casual"  "-*-Dom Casual-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Dom Casual"  "-*-Dom Casual-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Dom Casual"  "-*-Dom Casual-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Dom Casual"  "-*-Dom Casual-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Dom Casual"  "-*-Dom Casual-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Dom Casual"  "-*-Dom Casual-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Dom Casual"  "-*-Dom Casual-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Dom Casual"  "-*-Dom Casual-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Dom Casual"  "-*-Dom Casual-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Dom Casual"  "-*-Dom Casual-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Dom Casual"  "-*-Dom Casual-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Dom Casual"  "-*-Dom Casual-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Dom Casual"  "-*-Dom Casual-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Dom Casual"  "-*-Dom Casual-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Dom Casual"  "-*-Dom Casual-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Dom Casual"  "-*-Dom Casual-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Dom Casual"  "-*-Dom Casual-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Dom Casual"  "-*-Dom Casual-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Dom Casual"  "-*-Dom Casual-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Dom Casual"  "-*-Dom Casual-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Dom Casual"  "-*-Dom Casual-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Dom Casual"  "-*-Dom Casual-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Dom Casual"  "-*-Dom Casual-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Dom Casual"  "-*-Dom Casual-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Dom Casual"  "-*-Dom Casual-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Dom Casual"  "-*-Dom Casual-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Dom Casual"  "-*-Dom Casual-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Times New Roman"  "-*-Times New Roman-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Times New Roman"  "-*-Times New Roman-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Times New Roman"  "-*-Times New Roman-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Times New Roman"  "-*-Times New Roman-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Times New Roman"  "-*-Times New Roman-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Times New Roman"  "-*-Times New Roman-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Times New Roman"  "-*-Times New Roman-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Times New Roman"  "-*-Times New Roman-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Times New Roman"  "-*-Times New Roman-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Times New Roman"  "-*-Times New Roman-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Times New Roman"  "-*-Times New Roman-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Times New Roman"  "-*-Times New Roman-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Times New Roman"  "-*-Times New Roman-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Times New Roman"  "-*-Times New Roman-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Times New Roman"  "-*-Times New Roman-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Times New Roman"  "-*-Times New Roman-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Times New Roman"  "-*-Times New Roman-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Verdana"  "-*-Verdana-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Verdana"  "-*-Verdana-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Verdana"  "-*-Verdana-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Verdana"  "-*-Verdana-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Verdana"  "-*-Verdana-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Verdana"  "-*-Verdana-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Verdana"  "-*-Verdana-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Verdana"  "-*-Verdana-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Verdana"  "-*-Verdana-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Verdana"  "-*-Verdana-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Verdana"  "-*-Verdana-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Verdana"  "-*-Verdana-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Verdana"  "-*-Verdana-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Verdana"  "-*-Verdana-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Verdana"  "-*-Verdana-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Verdana"  "-*-Verdana-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Verdana"  "-*-Verdana-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Chicago"  "-*-Chicago-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Chicago"  "-*-Chicago-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Chicago"  "-*-Chicago-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Chicago"  "-*-Chicago-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Chicago"  "-*-Chicago-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Chicago"  "-*-Chicago-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Chicago"  "-*-Chicago-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Chicago"  "-*-Chicago-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Chicago"  "-*-Chicago-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Chicago"  "-*-Chicago-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Chicago"  "-*-Chicago-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Chicago"  "-*-Chicago-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Chicago"  "-*-Chicago-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Chicago"  "-*-Chicago-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Chicago"  "-*-Chicago-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Chicago"  "-*-Chicago-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Chicago"  "-*-Chicago-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Myriad Web"  "-*-Myriad Web-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Myriad Web"  "-*-Myriad Web-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Myriad Web"  "-*-Myriad Web-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Myriad Web"  "-*-Myriad Web-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Myriad Web"  "-*-Myriad Web-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Myriad Web"  "-*-Myriad Web-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Myriad Web"  "-*-Myriad Web-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Myriad Web"  "-*-Myriad Web-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Myriad Web"  "-*-Myriad Web-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Myriad Web"  "-*-Myriad Web-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Myriad Web"  "-*-Myriad Web-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Myriad Web"  "-*-Myriad Web-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Myriad Web"  "-*-Myriad Web-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Myriad Web"  "-*-Myriad Web-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Myriad Web"  "-*-Myriad Web-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Myriad Web"  "-*-Myriad Web-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Myriad Web"  "-*-Myriad Web-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("36-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-36-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("37-Myriad Condensed Web"  "-*-Myriad Condensed Web-normal-r-*-*-37-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-VTCorona"  "-*-VTCorona-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-VTCorona"  "-*-VTCorona-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-VTCorona"  "-*-VTCorona-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-VTCorona"  "-*-VTCorona-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-VTCorona"  "-*-VTCorona-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-VTCorona"  "-*-VTCorona-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-VTCorona"  "-*-VTCorona-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-VTCorona"  "-*-VTCorona-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-VTCorona"  "-*-VTCorona-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-VTCorona"  "-*-VTCorona-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-VTCorona"  "-*-VTCorona-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-VTCorona"  "-*-VTCorona-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-VTCorona"  "-*-VTCorona-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-VTCorona"  "-*-VTCorona-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-VTCorona"  "-*-VTCorona-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-VTCorona"  "-*-VTCorona-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-VTCorona"  "-*-VTCorona-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Teleprinter"  "-*-Teleprinter-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Teleprinter"  "-*-Teleprinter-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Teleprinter"  "-*-Teleprinter-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Teleprinter"  "-*-Teleprinter-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Teleprinter"  "-*-Teleprinter-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Teleprinter"  "-*-Teleprinter-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Teleprinter"  "-*-Teleprinter-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Teleprinter"  "-*-Teleprinter-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Teleprinter"  "-*-Teleprinter-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Teleprinter"  "-*-Teleprinter-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Teleprinter"  "-*-Teleprinter-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Teleprinter"  "-*-Teleprinter-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Teleprinter"  "-*-Teleprinter-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Teleprinter"  "-*-Teleprinter-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Teleprinter"  "-*-Teleprinter-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Teleprinter"  "-*-Teleprinter-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Teleprinter"  "-*-Teleprinter-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Georgia"  "-*-Georgia-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Georgia"  "-*-Georgia-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Georgia"  "-*-Georgia-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Georgia"  "-*-Georgia-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Georgia"  "-*-Georgia-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Georgia"  "-*-Georgia-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Georgia"  "-*-Georgia-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Georgia"  "-*-Georgia-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Georgia"  "-*-Georgia-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Georgia"  "-*-Georgia-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Georgia"  "-*-Georgia-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Georgia"  "-*-Georgia-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Georgia"  "-*-Georgia-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Georgia"  "-*-Georgia-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Georgia"  "-*-Georgia-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Georgia"  "-*-Georgia-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Georgia"  "-*-Georgia-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("15-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("28-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-28-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("29-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-29-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("30-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-30-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("31-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-31-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("32-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-32-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("33-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-33-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("34-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-34-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("35-Apple Chancery"  "-*-Apple Chancery-normal-r-*-*-35-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
					  
				("11-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-11-*-*-*-c-*-iso8859-1"    (150 65 nil nil) )
				("12-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-12-*-*-*-c-*-iso8859-1"    (140 55 nil nil) )
				("13-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-13-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("14-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-14-*-*-*-c-*-iso8859-1"    (130 55 nil nil) )
				("15-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-15-*-*-*-c-*-iso8859-1"    (130 50 nil nil) )
				("16-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-16-*-*-*-c-*-iso8859-1"    (130 45 nil nil) )
				("17-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-17-*-*-*-c-*-iso8859-1"    (130 43 nil nil) )
				("18-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-18-*-*-*-c-*-iso8859-1"    (110 40 nil nil) )
				("19-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-19-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("20-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-20-*-*-*-c-*-iso8859-1"    (110 38 nil nil) )
				("21-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-21-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("22-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-22-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("23-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-23-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("24-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-24-*-*-*-c-*-iso8859-1"    (100 35 30 30) )
				("25-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-25-*-*-*-c-*-iso8859-1"    (100 30 30 30) )
				("26-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-26-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
				("27-Palatino Linotype"  "-*-Palatino Linotype-normal-r-*-*-27-*-*-*-c-*-iso8859-1"    (90 30 30 30) )
					  
				("11-Raize  "  "-*-Raize-normal-r-normal-normal-14-105-96-96-c-80-iso8859-15"   (90 57 315 32) )
				("12-Raize  "  "-*-Raize-normal-r-normal-normal-16-120-96-96-c-90-iso8859-15"   (85 50 275 32) )

				("11-SimSun    "  "-*-SimSun-normal-r-*-*-11-*-*-*-c-*-iso8859-1"               (90 50 nil nil) )
				("12-SimSun    "  "-*-SimSun-normal-r-*-*-12-*-*-*-c-*-iso8859-1"               (85 50 nil nil) )
				("13-SimSun    "  "-*-SimSun-normal-r-*-*-13-*-*-*-c-*-iso8859-1"               (80 50 nil nil) )
				("14-SimSun    "  "-*-SimSun-normal-r-*-*-14-*-*-*-c-*-iso8859-1"               (80 50 nil nil) )
				("15-SimSun    "  "-*-SimSun-normal-r-*-*-15-*-*-*-c-*-iso8859-1"               (80 50 nil nil) )
				("16-SimSun    "  "-*-SimSun-normal-r-*-*-16-*-*-*-c-*-iso8859-1"               (80 45 nil nil) )
				("17-SimSun    "  "-*-SimSun-normal-r-*-*-17-*-*-*-c-*-iso8859-1"               (80 40 nil nil) )
				("18-SimSun    "  "-*-SimSun-normal-r-*-*-18-*-*-*-c-*-iso8859-1"               (80 40 nil nil) )
				("20-SimSun    "  "-*-SimSun-normal-r-*-*-20-*-*-*-c-*-iso8859-1"               (80 40 230 33) )
				("22-SimSun    "  "-*-SimSun-normal-r-*-*-22-*-*-*-c-*-iso8859-1"               (80 35 150 35) )

				("11-Ansi      "  "-*-r_ansi-normal-r-*-*-11-*-*-*-c-*-iso8859-1"               (85 55 nil nil) )
				("12-Ansi      "  "-*-r_ansi-normal-r-*-*-12-*-*-*-c-*-iso8859-1"               (85 45 nil nil) )
				("13-Ansi      "  "-*-r_ansi-normal-r-*-*-13-*-*-*-c-*-iso8859-1"               (80 47 nil nil) )
				("14-Ansi      "  "-*-r_ansi-normal-r-*-*-14-*-*-*-c-*-iso8859-1"               (80 43 nil nil) )
				("15-Ansi      "  "-*-r_ansi-normal-r-*-*-15-*-*-*-c-*-iso8859-1"               (80 43 nil nil) )
				("16-Ansi      "  "-*-r_ansi-normal-r-*-*-16-*-*-*-c-*-iso8859-1"               (80 41 nil nil) )
				("17-Ansi      "  "-*-r_ansi-normal-r-*-*-17-*-*-*-c-*-iso8859-1"               (80 39 nil nil) )
				("18-Ansi      "  "-*-r_ansi-normal-r-*-*-18-*-*-*-c-*-iso8859-1"               (80 35 100 35) )
				("19-Ansi      "  "-*-r_ansi-normal-r-*-*-19-*-*-*-c-*-iso8859-1"               (80 35 100 35) )
				("20-Ansi      "  "-*-r_ansi-normal-r-*-*-20-*-*-*-c-*-iso8859-1"               (80 30 100 35) )
				("21-Ansi      "  "-*-r_ansi-normal-r-*-*-21-*-*-*-c-*-iso8859-1"               (80 30 100 35) )
				("22-Ansi      "  "-*-r_ansi-normal-r-*-*-22-*-*-*-c-*-iso8859-1"               (80 30 100 35) )
					  						 
				("11-Andale    "  "-*-Andale Mono-normal-r-*-*-11-*-*-*-c-*-iso8859-1"               (90 45 nil nil) )
				("12-Andale    "  "-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-1"               (85 45 nil nil) )
				("13-Andale    "  "-*-Andale Mono-normal-r-*-*-13-*-*-*-c-*-iso8859-1"               (80 45 nil nil) )
				("14-Andale    "  "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1"               (80 43 nil nil) )
				("15-Andale    "  "-*-Andale Mono-normal-r-*-*-15-*-*-*-c-*-iso8859-1"               (80 41 nil nil) )
				("16-Andale    "  "-*-Andale Mono-normal-r-*-*-16-*-*-*-c-*-iso8859-1"               (80 40 nil nil) )
				("17-Andale    "  "-*-Andale Mono-normal-r-*-*-17-*-*-*-c-*-iso8859-1"               (80 35 nil nil) )
				("18-Andale    "  "-*-Andale Mono-normal-r-*-*-18-*-*-*-c-*-iso8859-1"               (80 30 nil nil) )

				("11-Console   "  "-*-Lucida Console-normal-r-*-*-11-*-*-*-c-*-iso8859-1"            (90 45 nil nil) )
				("12-Console   "  "-*-Lucida Console-normal-r-*-*-12-*-*-*-c-*-iso8859-1"            (85 45 nil nil) )
				("13-Console   "  "-*-Lucida Console-normal-r-*-*-13-*-*-*-c-*-iso8859-1"            (80 45 nil nil) )
				("14-Console   "  "-*-Lucida Console-normal-r-*-*-14-*-*-*-c-*-iso8859-1"            (80 43 nil nil) )
				("15-Console   "  "-*-Lucida Console-normal-r-*-*-15-*-*-*-c-*-iso8859-1"            (80 41 nil nil) )
				("16-Console   "  "-*-Lucida Console-normal-r-*-*-16-*-*-*-c-*-iso8859-1"            (80 40 nil nil) )
				("17-Console   "  "-*-Lucida Console-normal-r-*-*-17-*-*-*-c-*-iso8859-1"            (80 35 nil nil) )
				("18-Console   "  "-*-Lucida Console-normal-r-*-*-18-*-*-*-c-*-iso8859-1"            (80 30 nil nil) )

				("11-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-11-*-*-*-c-*-iso8859-1" (90 45 nil nil) )
				("12-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-12-*-*-*-c-*-iso8859-1" (85 45 nil nil) )
				("13-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-13-*-*-*-c-*-iso8859-1" (80 45 nil nil) )
				("14-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-14-*-*-*-c-*-iso8859-1" (85 40 nil nil) )
				("15-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-15-*-*-*-c-*-iso8859-1" (80 40 nil nil) )
				("16-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-16-*-*-*-c-*-iso8859-1" (85 38 nil nil) )
				("17-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-17-*-*-*-c-*-iso8859-1" (80 35 nil nil) )
				("18-Gothic    "  "-*-Letter Gothic 12 Pitch BT-normal-r-*-*-18-*-*-*-c-*-iso8859-1" (80 30 nil nil) )

				("11-Courier   "  "-*-Courier New-normal-r-*-*-11-*-*-*-c-*-iso8859-1"               (90 45 nil nil) )
				("12-Courier   "  "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-1"               (85 45 nil nil) )
				("13-Courier   "  "-*-Courier New-normal-r-*-*-13-*-*-*-c-*-iso8859-1"               (80 45 nil nil) )
				("14-Courier   "  "-*-Courier New-normal-r-*-*-14-*-*-*-c-*-iso8859-1"               (80 43 nil nil) )
				("15-Courier   "  "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-1"               (80 41 nil nil) )
				("16-Courier   "  "-*-Courier New-normal-r-*-*-16-*-*-*-c-*-iso8859-1"               (80 40 nil nil) )
				("17-Courier   "  "-*-Courier New-normal-r-*-*-17-*-*-*-c-*-iso8859-1"               (80 35 nil nil) )
				("18-Courier   "  "-*-Courier New-normal-r-*-*-18-*-*-*-c-*-iso8859-1"               (80 30 nil nil) )

				)
			 )))
		 ((eq system-type 'cygwin);; GNU-Cigwin
		  (cond 
			((string= (getenv "DISPLAY") ":0.0")
			 '(

				("fixed"               "fixed" (132 60 nil 40))
				("LucidaType-12"       "-b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-9" (100 55 nil 25))
				("8x13"                "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (100 35 nil nil ))
				("LucidaG-12"          "-greek-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-7" (80 50 25 25 ))
				("Screen-14"           "-misc-screen-medium-r-normal-*-*-120-*-*-c-*-iso8859-7" (90 50 nil 25 ))
				("LucidaType-14"       "-b&h-lucidatypewriter-medium-r-normal-*-*-140-*-*-m-*-iso8859-9" (80 30 nil nil))
				("9x15"                "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 24 nil 25))
				("Clean-15"            "-schumacher-clean-medium-r-normal-*-*-150-*-*-c-*-*-*" (80 30 nil 25))
				("Clean-16"            "-schumacher-clean-medium-r-normal-*-*-160-*-*-c-*-*-*" (85 43 nil 25))
				("Courier-14"          "-biznet-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-2" (85 43 nil 25))
				("SerifSC-16"          "-greek-smserif-medium-r-semicondensed-*-*-160-*-*-m-*-iso8859-7" (90 43 nil 25))
				("SerifSC-16"          "-*-*-medium-R-semicondensed-*-*-160-*-*-m-*-fontset-startup" (90 43 nil 25))
				("Courier-18"          "-biznet-courier-medium-r-normal-*-*-180-*-*-m-*-iso8859-2" (80 30 nil nil))
				("Terminal-18"         "-bitstream-terminal-medium-r-normal-*-*-140-*-*-c-*-iso8859-9" (80 40 nil 25))
				("LucidaType-18"       "-b&h-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-iso8859-9" (80 30 nil nil))
				("10x20"               "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 38 nil 30))
				("Etl-24"              "-etl-fixed-medium-r-normal-*-*-170-*-*-c-*-iso8859-7" (80 33 nil 25))
				("Courier-24"          "-adobe-courier-medium-r-normal--*-240-*-*-m-*-iso8859-1" (65 20 nil nil))

				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))

			 )
			 )
			(t
			 '(

				("fixed"               "fixed" (110 53 nil 40))
				("LucidaType-12"       "-b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-9" (90 52 nil nil))
				("8x13"                "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (85 52 nil nil ))
				("LucidaG-12"          "-greek-lucidatypewriter-medium-r-normal-*-*-120-*-*-m-*-iso8859-7" (80 50 5 nil ))
				("Screen-14"           "-misc-screen-medium-r-normal-*-*-120-*-*-c-*-iso8859-7" (80 43 nil nil ))
				("LucidaType-14"       "-b&h-lucidatypewriter-medium-r-normal-*-*-140-*-*-m-*-iso8859-9" (75 43 nil nil))
				("9x15"                "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 24 nil nil))
				("Clean-15"            "-schumacher-clean-medium-r-normal-*-*-150-*-*-c-*-*-*" (80 40 nil nil))
				("Clean-16"            "-schumacher-clean-medium-r-normal-*-*-160-*-*-c-*-*-*" (85 43 nil nil))
				("Courier-14"          "-biznet-courier-medium-r-normal-*-*-140-*-*-m-*-iso8859-2" (85 43 nil 25))
				("SerifSC-16"          "-greek-smserif-medium-r-semicondensed-*-*-160-*-*-m-*-iso8859-7" (80 35 nil 25))
				("SerifSC-16"          "-*-*-medium-R-semicondensed-*-*-160-*-*-m-*-fontset-startup" (80 35 nil 25))
				("Courier-18"          "-biznet-courier-medium-r-normal-*-*-180-*-*-m-*-iso8859-2" (75 30 nil nil))
				("Terminal-18"         "-bitstream-terminal-medium-r-normal-*-*-140-*-*-c-*-iso8859-9" (80 40 nil 25))
				("LucidaType-18"       "-b&h-lucidatypewriter-medium-r-normal-*-*-180-*-*-m-*-iso8859-9" (80 30 nil nil))
				("10x20"               "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 30 nil 30))
				("Etl-24"              "-etl-fixed-medium-r-normal-*-*-170-*-*-c-*-iso8859-7" (80 33 nil 25))
				("Courier-24"          "-adobe-courier-medium-r-normal--*-240-*-*-m-*-iso8859-1" (65 20 nil nil))

				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))

			 )
			 )))
		 ((eq system-type 'gnu/linux);; GNU-Linux
		  (cond 
			((string= (getenv "DISPLAY") ":0.0")
			 '(

				("fixed"               "fixed" (132 60 nil 40))

				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))

				("xos4-12" "-xos4-terminus-medium-r-*-*-12-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-14" "-xos4-terminus-medium-r-*-*-14-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-16" "-xos4-terminus-medium-r-*-*-16-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-17" "-xos4-terminus-medium-r-*-*-17-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-20" "-xos4-terminus-medium-r-*-*-20-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-24" "-xos4-terminus-medium-r-*-*-24-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-28" "-xos4-terminus-medium-r-*-*-28-*-*-*-*-*-*-1" (80 25 nil nil))
				("xos4-32" "-xos4-terminus-medium-r-*-*-32-*-*-*-*-*-*-1" (80 25 nil nil))
													
				("vga17" "-dosemu-vga-medium-r-normal-*-17-*-75-*-*-*-ibm-cp437" (80 25 nil nil))
				("vga19" "-dosemu-vga-medium-r-normal-*-19-*-75-*-*-*-ibm-cp437" (80 25 nil nil))
											 
				("heriliane-12" "-vh-herilane-medium-r-normal-*-12-*-75-*-*-*-iso8859-15" (80 25 nil nil))
				("heriliane-14" "-vh-herilane-medium-r-normal-*-14-*-75-*-*-*-iso8859-15" (80 25 nil nil))
														
				("Typewriter-10"   "lucidasanstypewriter-10" (80 45 nil nil) )
				("Typewriter-12"   "lucidasanstypewriter-12" (80 45 nil nil) )
				("Typewriter-14"   "lucidasanstypewriter-14" (80 43 nil nil) )
				("Typewriter-18"   "lucidasanstypewriter-18" (80 25 nil 25) )
				("Typewriter-24"   "lucidasanstypewriter-24" (60 25 nil 25) )

				("Mono-14"       "-bitstream-bitstream vera sans mono-medium-r-normal--14-*-*-*-*-*-*-*" (93 58 nil 25) )
				("Mono-15"       "-bitstream-bitstream vera sans mono-medium-r-normal--15-*-*-*-*-*-*-*" (85 53 nil 25) )
				("Mono-16"       "-bitstream-bitstream vera sans mono-medium-r-normal--16-*-*-*-*-*-*-*" (81 49 nil 25) )
				("Mono-17"       "-bitstream-bitstream vera sans mono-medium-r-normal--17-*-*-*-*-*-*-*" (81 47 nil 25) )
				("Mono-18"       "-bitstream-bitstream vera sans mono-medium-r-normal--18-*-*-*-*-*-*-*" (95 45 nil 25) )
				("Mono-19"       "-bitstream-bitstream vera sans mono-medium-r-normal--19-*-*-*-*-*-*-*" (81 43 nil 25) )
				("Mono-20"       "-bitstream-bitstream vera sans mono-medium-r-normal--20-*-*-*-*-*-*-*" (88 39 nil 25) )
				("Mono-21"       "-bitstream-bitstream vera sans mono-medium-r-normal--21-*-*-*-*-*-*-*" (88 39 5 25) )
				("Mono-22"       "-bitstream-bitstream vera sans mono-medium-r-normal--22-*-*-*-*-*-*-*" (81 37 5 25) )
				("Mono-23"       "-bitstream-bitstream vera sans mono-medium-r-normal--23-*-*-*-*-*-*-*" (82 36 5 25) )
				("Mono-24"       "-bitstream-bitstream vera sans mono-medium-r-normal--24-*-*-*-*-*-*-*" (82 34 5 25) )
				("Mono-28"       "-bitstream-bitstream vera sans mono-medium-r-normal--28-*-*-*-*-*-*-*" (69 28 5 25) )
				("Mono-32"       "-bitstream-bitstream vera sans mono-medium-r-normal--32-*-*-*-*-*-*-*" (63 24 5 25) )
				  										  						
				("Sans-14"       "-bitstream-bitstream vera sans-medium-r-normal--14-*-*-*-*-*-*-*" (44 59 nil 25) )
				("Sans-16"       "-bitstream-bitstream vera sans-medium-r-normal--16-*-*-*-*-*-*-*" (40 50 nil 25) )
				("Sans-18"       "-bitstream-bitstream vera sans-medium-r-normal--18-*-*-*-*-*-*-*" (38 45 nil 25) )
				("Sans-22"       "-bitstream-bitstream vera sans-medium-r-normal--22-*-*-*-*-*-*-*" (38 38 5 25) )
				("Sans-28"       "-bitstream-bitstream vera sans-medium-r-normal--28-*-*-*-*-*-*-*" (31 29 5 25) )
				("Sans-32"       "-bitstream-bitstream vera sans-medium-r-normal--32-*-*-*-*-*-*-*" (27 25 5 25) )
				  										  
				("Serif-14"       "-bitstream-bitstream vera serif-medium-r-normal--14-*-*-*-*-*-*-*" (38 54 nil 25) )
				("Serif-16"       "-bitstream-bitstream vera serif-medium-r-normal--16-*-*-*-*-*-*-*" (38 50 nil 25) )
				("Serif-18"       "-bitstream-bitstream vera serif-medium-r-normal--18-*-*-*-*-*-*-*" (38 43 nil 25) )
				("Serif-22"       "-bitstream-bitstream vera serif-medium-r-normal--22-*-*-*-*-*-*-*" (35 35 5 25) )
				("Serif-28"       "-bitstream-bitstream vera serif-medium-r-normal--28-*-*-*-*-*-*-*" (31 29 5 25) )
				("Serif-32"       "-bitstream-bitstream vera serif-medium-r-normal--32-*-*-*-*-*-*-*" (27 25 5 25) )
				  										  					
				("Comic-14"       "-microsoft-comic sans ms-medium-r-normal--14-*-*-*-*-*-*-*" (43 46 nil 25) )
				("Comic-15"       "-microsoft-comic sans ms-medium-r-normal--15-*-*-*-*-*-*-*" (38 38 nil 25) )
				("Comic-18"       "-microsoft-comic sans ms-medium-r-normal--18-*-*-*-*-*-*-*" (38 37 nil 25) )
				("Comic-22"       "-microsoft-comic sans ms-medium-r-normal--22-*-*-*-*-*-*-*" (40 31 nil 25) )
				("Comic-28"       "-microsoft-comic sans ms-medium-r-normal--28-*-*-*-*-*-*-*" (34 24 5 25) )
				("Comic-32"       "-microsoft-comic sans ms-medium-r-normal--32-*-*-*-*-*-*-*" (29 21 5 25) )

				("Verdana-14"       "-microsoft-verdana-medium-r-normal--14-*-*-*-*-*-*-*" (38 54 nil 25) )
				("Verdana-16"       "-microsoft-verdana-medium-r-normal--16-*-*-*-*-*-*-*" (38 50 nil 25) )
				("Verdana-18"       "-microsoft-verdana-medium-r-normal--18-*-*-*-*-*-*-*" (38 43 nil 25) )
				("Verdana-22"       "-microsoft-verdana-medium-r-normal--22-*-*-*-*-*-*-*" (35 35 5 25) )
				("Verdana-28"       "-microsoft-verdana-medium-r-normal--28-*-*-*-*-*-*-*" (27 27 5 25) )
				("Verdana-32"       "-microsoft-verdana-medium-r-normal--32-*-*-*-*-*-*-*" (23 24 5 25) )
				  										  
				("Times-14"       "-monotype-times new roman-medium-r-normal--14-*-*-*-*-*-*-*" (25 60 nil 25) )
				("Times-16"       "-monotype-times new roman-medium-r-normal--16-*-*-*-*-*-*-*" (22 54 nil 25) )
				("Times-18"       "-monotype-times new roman-medium-r-normal--18-*-*-*-*-*-*-*" (23 47 nil 25) )
				("Times-22"       "-monotype-times new roman-medium-r-normal--22-*-*-*-*-*-*-*" (21 38 5 25) )
				("Times-28"       "-monotype-times new roman-medium-r-normal--28-*-*-*-*-*-*-*" (20 30 5 25) )
				("Times-32"       "-monotype-times new roman-medium-r-normal--32-*-*-*-*-*-*-*" (17 25 5 25) )
				  						  			  
				("Andale-14"       "-monotype-andale mono-medium-r-normal--14-*-*-*-*-*-*-*" (96 57 nil 25) )
				("Andale-16"       "-monotype-andale mono-medium-r-normal--16-*-*-*-*-*-*-*" (84 50 nil 25) )
				("Andale-18"       "-monotype-andale mono-medium-r-normal--18-*-*-*-*-*-*-*" (82 47 nil 25) )
				("Andale-22"       "-monotype-andale mono-medium-r-normal--22-*-*-*-*-*-*-*" (81 35 5 25) )
				("Andale-28"       "-monotype-andale mono-medium-r-normal--28-*-*-*-*-*-*-*" (70 30 5 25) )
				("Andale-32"       "-monotype-andale mono-medium-r-normal--32-*-*-*-*-*-*-*" (62 26 5 25) )
				  						  
				("Lucida-12"       "-b&h-lucida-medium-r-*-*-12-*-*-*-*-*-*-*" (70 73 nil 25) )
				("Lucida-14"       "-b&h-lucida-medium-r-*-*-14-140-*-*-*-*-*-*" (60 58 nil 25) )
				("Lucida-17"       "-b&h-lucida-medium-r-*-*-17-*-*-*-*-*-*-*" (53 49 nil 25) )
				("Lucida-18"       "-b&h-lucida-medium-r-*-*-18-*-*-*-*-*-*-*" (50 49 nil 25) )
				("Lucida-19"       "-b&h-lucida-medium-r-*-*-19-*-*-*-*-*-*-*" (50 44 nil 25) )
				("Lucida-20"       "-b&h-lucida-medium-r-*-*-20-*-*-*-*-*-*-*" (52 41 nil 25) )
				("Lucida-24"       "-b&h-lucida-medium-r-*-*-24-*-*-*-*-*-*-*" (45 35 nil 25) )
				("Lucida-25"       "-b&h-lucida-medium-r-*-*-25-*-*-*-*-*-*-*" (40 32 nil 25) )
				("Lucida-26"       "-b&h-lucida-medium-r-*-*-26-*-*-*-*-*-*-*" (45 31 nil 25) )
				("Lucida-34"       "-b&h-lucida-medium-r-*-*-34-*-*-*-*-*-*-*" (37 24 nil 25) )

				("Terminal-14"     "-dec-terminal-medium-*-*-*-14-*-*-*-*-*-*-*" (80 25 nil nil))
				("Terminal-17"     "-bitstream-terminal-medium-*-*-*-17-*-*-*-*-*-*-*" (80 25 nil nil))

				("Clean-12"        "-schumacher-clean-medium-r-*-*-12-*-*-*-*-70-*-*" (80 25 nil nil))
				("Clean-13"        "-schumacher-clean-medium-r-*-*-13-*-*-*-*-60-*-*" (80 25 nil nil))

				("Sony-16"         "-sony-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*" (80 25 nil nil))
				("Sony-24"         "-sony-fixed-medium-r-normal-*-24-*-*-*-*-*-*-*" (100 40 nil nil))

				("Avangarde-17"    "-adobe-avantgarde-demi-r-normal-*-17-120-100-100-p-0-*-*" (36 37 nil nil))
				("Utopia-17"       "-adobe-utopia-bold-r-normal-*-17-120-100-100-p-0-*-*" (52 48 nil nil))
				("Bookman-17"      "-adobe-bookman-demi-r-normal-*-17-120-100-100-p-0-*-*" (40 35 nil nil))

				("BookMan-17"      "-urw-urw bookman l-regular-r-normal--17-120-100-100-p-0-*-*" (42 40 nil nil))
				("Palladio-17"     "-urw-urw palladio l-regular-r-normal--17-120-100-100-p-0-*-*" (43 38 nil nil))
				("ScBook-17"       "-urw-century schoolbook l-regular-r-normal--17-120-100-100-p-0-*-*" (40 38 nil nil))

				("Courier-10"      "-adobe-courier-medium-r-normal-*-10-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-12"      "-adobe-courier-medium-r-normal-*-12-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-14"      "-adobe-courier-medium-r-normal-*-14-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-17"      "-adobe-courier-medium-r-normal-*-17-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-18"      "-adobe-courier-medium-r-normal-*-18-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-24"      "-adobe-courier-medium-r-normal-*-24-*-*-*-m-*-iso8859-1" (80 40 nil nil))
				("Courier-34"      "-adobe-courier-medium-r-normal-*-34-*-*-*-m-*-iso8859-1" (61 33 nil nil))

				("Anorexia-10"     "-artwiz-anorexia-medium-r-normal--0-0-75-75-p-0-iso8859-1" (85 45 nil nil))
				("Aqui-11"         "-artwiz-aqui-medium-r-*-*-11-*-*-*-*-*-*-*" (85 45 nil nil))
				("Drift-10"        "-artwiz-drift-*-*-*-*-10-*-*-*-*-*-*-*" (165 80 nil nil))
				("Gelly-10"        "-artwiz-gelly-*-*-*-*-10-*-*-*-*-*-*-*" (85 45 nil nil))
				("FKP-17"          "-artwiz-fkp-*-*-*-*-17-*-*-*-*-*-*-*" (95 58 nil nil))
				("Mintsmild-8"     "-artwiz-mintsmild-*-*-*-*-8-*-*-*-*-*-*-*" (85 45 nil nil))
;;				("Mintsstrong-17"  "-artwiz-mintsstrong-*-*-*-*-17-*-*-*-*-*-*-*" (65 45 nil nil))
				("SmoothANSI-13"   "-artwiz-smoothansi-*-*-*-*-13-*-*-*-*-*-*-*" (120 72 5 25))



			 )
			 )
			(t
			 '(

				("fixed"               "fixed" (132 60 nil 40))

				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))

				("Lucida-10"  "lucidasanstypewriter-10" (80 45 nil nil) )
				("Lucida-12"  "lucidasanstypewriter-12" (80 45 nil nil) )
				("Lucida-14"  "lucidasanstypewriter-14" (80 43 nil nil) )
				("Lucida-18"  "lucidasanstypewriter-18" (80 25 nil 25) )
				("Lucida-24"  "lucidasanstypewriter-24" (60 15 nil 25) )

				("Terminal-14"     "-dec-terminal-medium-*-*-*-14-*-*-*-*-*-*-*" (80 25 nil nil))
				("Terminal-17"     "-bitstream-terminal-medium-*-*-*-17-*-*-*-*-*-*-*" (80 25 nil nil))

				("Clean-10"        "-schumacher-clean-medium-r-*-*-10-*-*-*-*-80-*-*" (80 25 nil nil))
				("Clean-12"        "-schumacher-clean-medium-r-*-*-12-*-*-*-*-70-*-*" (80 25 nil nil))
				("Clean-13"        "-schumacher-clean-medium-r-*-*-13-*-*-*-*-60-*-*" (80 25 nil nil))
				("Clean-14"        "-schumacher-clean-medium-r-*-*-14-*-*-*-*-80-*-*" (80 25 nil nil))
				("Clean-15"        "-schumacher-clean-medium-r-*-*-15-*-*-*-*-*-*-*" (80 25 nil nil))
				("Clean-16"        "-schumacher-clean-medium-r-*-*-16-*-*-*-*-*-*-*" (80 25 nil nil))

				("Sony-16"         "-sony-fixed-medium-r-normal-*-16-*-*-*-*-*-*-*" (80 25 nil nil))
				("Sony-24"         "-sony-fixed-medium-r-normal-*-24-*-*-*-*-*-*-*" (80 25 nil nil))

				("Avangarde-17"    "-adobe-avantgarde-demi-r-normal-*-17-120-100-100-p-0-*-*" (80 25 nil nil))
				("Bookman-17"      "-adobe-bookman-demi-r-normal-*-17-120-100-100-p-0-*-*" (80 25 nil nil))
				("Utopia-17"       "-adobe-utopia-bold-r-normal-*-17-120-100-100-p-0-*-*" (80 25 nil nil))


				("BookMan-17"      "-urw-urw bookman l-regular-r-normal--17-120-100-100-p-0-*-*" (80 25 nil nil))
				("Palladio-17"     "-urw-urw palladio l-regular-r-normal--17-120-100-100-p-0-*-*" (80 25 nil nil))
				("ScBook-17"       "-urw-century schoolbook l-regular-r-normal--17-120-100-100-p-0-*-*" (80 25 nil nil))

				("Courier-10"      "-adobe-courier-medium-r-normal-*-10-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-12"      "-adobe-courier-medium-r-normal-*-12-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-14"      "-adobe-courier-medium-r-normal-*-14-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-17"      "-adobe-courier-medium-r-normal-*-17-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-18"      "-adobe-courier-medium-r-normal-*-18-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-24"      "-adobe-courier-medium-r-normal-*-24-*-*-*-m-*-iso8859-1" (80 25 nil nil))
				("Courier-34"      "-adobe-courier-medium-r-normal-*-34-*-*-*-m-*-iso8859-1" (80 25 nil nil))

				("Anorexia-10"     "-artwiz-anorexia-medium-r-normal--0-0-75-75-p-0-iso8859-1" (85 45 nil nil))
				("Anorexia-17"     "-artwiz-anorexia-medium-r-normal--17-120-100-100-p-0-iso8859-1" (85 45 nil nil))
				("Drift-10"        "-artwiz-drift-*-*-*-*-10-*-*-*-*-*-*-*" (85 45 nil nil))
				("Gelly-10"        "-artwiz-gelly-*-*-*-*-10-*-*-*-*-*-*-*" (85 45 nil nil))
				("Gelly-17"        "-artwiz-gelly-*-*-*-*-17-*-*-*-*-*-*-*" (85 45 nil nil))
				("FKP-17"          "-artwiz-fkp-*-*-*-*-17-*-*-*-*-*-*-*" (85 45 nil nil))
				("Mintsmild-8"     "-artwiz-mintsmild-*-*-*-*-8-*-*-*-*-*-*-*" (85 45 nil nil))
				("Mintsmild-17"    "-artwiz-mintsmild-*-*-*-*-17-*-*-*-*-*-*-*" (85 45 nil nil))
				("Mintsstrong-17"  "-artwiz-mintsstrong-*-*-*-*-17-*-*-*-*-*-*-*" (85 45 nil nil))
				("SmoothANSI-13"   "-artwiz-smoothansi-*-*-*-*-13-*-*-*-*-*-*-*" (85 45 nil nil))


			 )
			 )))
		 ((eq system-type 'usg-unix-v);; Sun Solaris
		  (cond 
			((string= (getenv "DISPLAY") ":0.0")
			 '(
				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))
			 )
			 )
			(t
			 '(
				("6x10"  "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" (80 45 nil nil) )
				("7x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" (80 35 nil nil))
				("7x14"  "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" (80 25 nil nil))
				("8x13"  "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" (80 25 nil nil))
				("9x15"  "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" (80 25 nil nil))
				("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" (80 25 nil nil))
				("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" (80 25 nil nil))
				("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" (80 25 nil nil))
			 )
			 )))
		 )
		)
	  
  "List of custom fonts.

Each item is  a three element list (NAME SPEC GEOM) where GEOM is itself
a four element list (WIDTH HEIGHT LEFT TOP).

FUNC is a color theme function which does the setup.  The function
FUNC may call `ergo-font-install'.  The color theme function may be
interactive.

NAME is the name of the theme and MAINTAINER is the name and/or email of
the maintainer of the theme.

If you defined your own color theme and want to add it to this list,
use something like this:

  (add-to-list 'color-themes '(ergo-font-gnome2 \"Gnome2\" \"Alex\"))")



(defcustom ergo-font-small-frame-width 80
  "*Small Frame Width, set by ergo-font-small-frame, useful when shitching 
to large fonts."
  :group 'ergo-font
  :type 'integer)

(defcustom ergo-font-small-frame-height 24
  "*Small Frame Height, set by ergo-font-small-frame, useful when shitching 
to large fonts."
  :group 'ergo-font
  :type 'integer)




(defcustom ergo-font-legal-frame-parameters "\\(color\\|mode\\)$"
  "Regexp that matches frame parameter names.
Only frame parameter names that match this regexp can be changed as part
of a color theme."
  :type 'regexp
  :group 'ergo-font
  :link '(info-link "(elisp)Window Frame Parameters"))


(defcustom ergo-font-is-global t
  "*Determines wether a color theme is installed on all frames or not.
If non-nil, color themes will be installed for all frames.  
If nil, color themes will be installed for the selected frame only.

A possible use for this variable is dynamic binding. Here is a larger
example to put in your ~/.emacs; it will make the Blue Sea color theme
the default used for the first frame, and it will create two additional
frames with different color themes.

setup:
    \(require 'color-theme)
    ;; set default color theme
    \(ergo-font-blue-sea)
    ;; create some frames with different color themes
    \(let ((ergo-font-is-global nil))
      \(select-frame (make-frame))
      \(ergo-font-gnome2)
      \(select-frame (make-frame))
      \(ergo-font-standard))

Please note that using XEmacs and and a nil value for
ergo-font-is-global will ignore any variable settings for the color
theme, since XEmacs doesn't have frame-local variable bindings.

Also note that using Emacs and a non-nil value for ergo-font-is-global
will install a new color theme for all frames.  Using XEmacs and a
non-nil value for ergo-font-is-global will install a new color theme
only on those frames that are not using a local color theme."
  :type 'boolean
  :group 'ergo-font)

(defvar ergo-font-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'ergo-font-install-at-point)
    (define-key map (kbd "i") 'ergo-font-install-at-point)
    (define-key map (kbd "q") 'bury-buffer)
    (if ergo-font-xemacs-p
		  (define-key map (kbd "<button2>") 'ergo-font-install-at-mouse)
      (define-key map (kbd "<mouse-2>") 'ergo-font-install-at-mouse))
    map)
  "Mode map used for the buffer created by `ergo-font-select'.")

(defvar ergo-font-buffer-name "*Custom Fonts*"
  "Name of the color theme selection buffer.")

;;; List of custom fonts used to create the *Custom Fonts Selection*
;; buffer.

;;; Support Functions

(defun ergo-font-match-item (spec1 spec2)
  "D."
  (cond ((null spec1 ) t)
	((null spec1 ) t)
	(t (let ((item1 (car spec1))
		 (item2 (car spec2)))
	     (if (or (equal item1 item2)
		     (equal item1 "*")
		     (equal item2 "*"))
		 (ergo-font-match-item (cdr spec1) (cdr spec2))
	       nil)))))

(defun ergo-font-match-spec (fontspec1 fontspec2)
  "D."
  (let ((spec1 (split-string (downcase fontspec1 ) "-"))
	(spec2 (split-string (downcase fontspec2 ) "-")))
      (ergo-font-match-item spec1 spec2)))

(defun ergo-font-look-pointer (fontspec fontslist)
  "Displays a special buffer for selecting and installing a custom font."
  (if fontslist
      (if (ergo-font-match-spec fontspec (cadr (car fontslist)))
	  fontslist
	(ergo-font-look-pointer fontspec (cdr fontslist)))
    nil))

(defun ergo-font-get-pointer ()
  "Displays a special buffer for selecting and installing a custom font."
  (ergo-font-look-pointer 
   (frame-parameter (selected-frame) 'font) ergo-font-list)) 

(defun ergo-font-find (step)
  "Displays a special buffer for selecting and installing a custom font."
  (let* ((font-count (length ergo-font-list))
	 (font-pointer (ergo-font-get-pointer))
	 (font-pos   (- font-count (length font-pointer))) 
	 (new-font-item
	  (nthcdr (mod (+ step font-pos) font-count)
		 ergo-font-list)))
	 (car new-font-item)))

(defun ergo-font-set (step)
  "Displays a special buffer for selecting and installing a custom font."
  (let ((new-font-item (ergo-font-find step)))
	(if new-font-item
	    (ergo-font-do-set new-font-item))))



(if ergo-font-xemacs-p ;; XEmacs


    (defun ergo-font-dialog ()
      "Displays a dialog to select a font."
      (interactive)
      (gtk-choose-font))
  
  (progn


    (defun ergo-font-dialog ()
      "Displays a dialog to select a font."
      (interactive)
      (call-interactively 
       ;;     'ergo-alt-mouse-set-font 
       'ergo-mouse-set-font 
       ))


    (defun ergo-mouse-set-font (&rest fonts)
      "Select an emacs font from a list of known good fonts and fontsets."
      (interactive
       (x-popup-menu
	t
	;; Append list of fontsets currently defined.
	(append x-fixed-font-alist (list (generate-fontset-menu)))))
      (if fonts
	  (let (font)
	    (while fonts
	      (condition-case nil
		  (progn
		    (set-default-font (car fonts))
		    (setq font (car fonts))
		    (setq fonts nil))
		(error
		 (setq fonts (cdr fonts)))))
	    (if (null font)
		(error "Font not found")))))
    
;;     (require 'alt-font-menu)
;;     ;; (autoload 'alt-mouse-set-font "alt-font-menu"
;;     ;;      "interactively choose font using mouse" t)
;;     (defun ergo-alt-mouse-set-font (&rest fonts)
;;       "Select an emacs font from a list of existent fonts and fontsets.
;; Attempts to resize the frame so that it is no larger than the
;; original."
;;       (interactive
;;        (x-popup-menu
;; 	;;  last-nonmenu-event 
;; 	t
;; 	x-font-alist))
;;       (if fonts
;; 	  (let (font)
;; 	    (while fonts
;; 	      (condition-case nil
;; 		  (progn
;; 		    ;; change the font
;; 		    (afm-set-font (car fonts))
;; 		    (setq font (car fonts))
;; 		    (setq fonts nil))
;; 		(error
;; 		 (setq fonts (cdr fonts)))))
;; 	    (if (null font)
;; 		(error "font not found")))))


    ))





(defun ergo-font-do-set (fontdesc)
  "Displays a special buffer for selecting and installing a custom font."
  (let ((spec (nth 1 fontdesc))
	(geo  (nth 2 fontdesc)))
	 (if ergo-font-xemacs-p ;; XEmacs
		  (progn
			 (font-menu-set-font (nth 0 spec) (nth 1 spec) (nth 2 spec))
			 (ergo-font-set-frame-size geo)
			 )
		(progn
		  (set-frame-font spec)
		  (ergo-font-set-frame-size geo)
		  (message (frame-parameter (selected-frame) 'font))
		  ))
	 ))



(defun ergo-font-set-frame-size (geo)
  "Set selected frame size."
  (let ((l nil))
    (if (nth 0 geo)
	(setq l (append l (list (cons 'width (nth 0 geo))))))
    (if (nth 1 geo)
	(setq l (append l (list (cons 'height (nth 1 geo))))))
    (if (nth 2 geo)
	(setq l (append l (list (cons 'left (nth 2 geo))))))
    (if (nth 3 geo)
	(setq l (append l (list (cons 'top (nth 3 geo))))))
    (if l
	(modify-frame-parameters 
	 (selected-frame) l))))


;;; Keyboard Bindings

(if ergo-font-xemacs-p ;; XEmacs
	 (defun ergo-font-default-binding ()
		"Default key-bindings for quick font selection."
		(interactive)
		(define-key global-map '[(control kp-multiply)]	'ergo-font-select)
		)
  
  (progn
	 (defun ergo-font-default-binding ()
		"Default key-bindings for quick font selection."
		(interactive)
		(define-key global-map '[(control kp-multiply)]	'ergo-font-select)
		(define-key global-map '[(control multiply)]	'ergo-font-select)
		(define-key global-map '[(meta f1)]	'ergo-font-select)
		)
	 ))



;;; Keyboard Functions

(defun ergo-font-prev ()
  "Select previous (smaller) font in ergo-font-list for selected frame."
  (interactive)
  (ergo-font-set -1))

(defun ergo-font-next ()
  "Select next (larger) font in ergo-font-list for selected frame."
  (interactive)
  (ergo-font-set +1))

(defun ergo-font-small-frame ()
  "Set selected frame size to a (small) default, useful after selecting large fonts."
  (interactive)
  (ergo-font-set-frame-size 
   (list 
    ergo-font-small-frame-width 
    ergo-font-small-frame-height)))



;;; Select Functions

(defun ergo-font-select ()
  "Displays a special buffer for selecting and installing a custom font."
  (interactive)
  (switch-to-buffer (get-buffer-create ergo-font-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)

  (let ((commands ergo-command-list))
    (while commands
      (let* ((command (car commands))
	     (func (nth 0 command))
	     (name (nth 1 command))
	     (text (nth 2 command))
	     (desc))
	(setq desc (format "%-23s %s" name text))
	(put-text-property 0 (length desc) 'font-command func desc)
	(put-text-property 0 (length name) 'face 'bold desc)
	(put-text-property 0 (length name) 'mouse-face 'highlight desc)

        (insert desc)
	(newline))
      (setq commands (cdr commands))))


  (let ((fonts ergo-font-list))
    (while fonts
      (let* ((font (car fonts))
	     (name (nth 0 font))
	     (spec (nth 1 font))
	     (geom (nth 2 font))
	     (desc))
		  (if ergo-font-xemacs-p ;; XEmacs
				(progn
				  (setq desc (format "%-23s" name))
				  )
			 
			 (progn
				(setq desc (format "%-23s (%dX%d)  %s" name (nth 0 geom) (nth 1 geom) spec))
				))
	(put-text-property 0 (length desc) 'font-desc font desc)
	(put-text-property 0 (length name) 'face 'bold desc)
	(put-text-property 0 (length name) 'mouse-face 'highlight desc)
        (insert desc)
	(newline))
      (setq fonts (cdr fonts))))
  (beginning-of-buffer)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (ergo-font-mode))

(defun ergo-font-mode ()
  "Major mode to select and install color themes.

Use \\[ergo-font-install-at-point] to install a color theme: the changes are applied to all
frames.

Note that the changes are applied on top of your current setup.  This is
a feature.

Note that some of the themes should be considered extensions to the
standard color theme; they modify only a limited number of faces.  To
verify the final look of a color theme, install the standard color
theme, then install the other color theme.  This is a feature; it allows
you to mix several color themes.

Use \\[ergo-font-describe] to read more about the color theme function at point.
If you want to install the color theme permanently, put the call to the
color theme function into your ~/.emacs.

Example:
    \(require 'color-theme)
    \(ergo-font-gnome2)

Note that the Emacs menu is not affected by color themes within Emacs.
Depending on the toolkit you used to compile Emacs, you might have to
set specific X ressources.  See the info manual for more information.

Example in your ~/.Xdefaults:
    emacs*Background: DarkSlateGray
    emacs*Foreground: wheat

\\{ergo-font-mode-map}

The color themes are listed in `color-themes', which see."
  (kill-all-local-variables)
  (setq major-mode 'ergo-font-mode)
  (setq mode-name "Custom Fonts")
  (use-local-map ergo-font-mode-map)
  (when (functionp 'goto-address); Emacs
    (goto-address)))

;;; Commands in Custom Fonts Selection mode


(defun ergo-font-install-at-mouse (event)
  "Install color theme clicked upon using the mouse.
First argument EVENT is used to set point.  Then
`ergo-font-install-at-point' is called."
  (interactive "e")
  (save-excursion
    (if (and (functionp 'event-start); Emacs
	     (functionp 'posn-window)
	     (functionp 'posn-point))
	(let ((posn (event-start event)))
	  (set-buffer (window-buffer (posn-window posn)))
	  (goto-char (posn-point posn)))
      (let ((posn (event-point event))); XEmacs
	(set-buffer (event-buffer event))
	(goto-char posn)))
    (ergo-font-install-at-point)))

(defun ergo-font-install-at-point ()
  "Install color theme at point.
This calls the value of the text-property `color-theme' at point.
The text-property `color-theme' should be a color theme function.
See `color-themes'."
  (interactive)
  (let ((font-desc (get-text-property (point) 'font-desc)))
    (if font-desc
	(ergo-font-do-set font-desc)))
  (let ((func (get-text-property (point) 'font-command)))
    (if func
	(funcall func))))


(defun ergo-font-filter (old-list regexp &optional exclude)
  "Filter OLD-LIST.
The resulting list only contains elements with names matching REGEXP.
If the element is a list, then the name of the car of the list is used.
Therefore, OLD-LIST may also be an alist -- in that case the keys are
used to determine wether an element stays or goes.

If the optional argument EXCLUDE is non-nil, then the sense is
reversed: only non-matching elements will be retained."
  (let ((elem) (new-list))
    (while old-list
      (setq elem (car old-list))
      (setq name (symbol-name (if (listp elem) (car elem) elem)))
      (setq old-list (cdr old-list))
      (if (or (and (not exclude)
		   (string-match regexp name))
	      (and exclude
		   (not (string-match regexp name))))
	    (add-to-list 'new-list elem)))
    new-list))






;;; The color theme functions

(provide 'ergo-font)

;;; ergo-font.el ends here
