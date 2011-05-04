;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux octave package
;;
;; Originally contributed by Nils Naumann <naumann@unileoben.ac.at>
;; Modified by Dirk Eddelbuettel <edd@debian.org>

;; The Octave package follows the Debian/GNU Linux 'emacsen' policy and
;; byte-compiles its elisp files for each 'emacs flavour' ({,x}emacs{19,20}).
;; The compiled code is then installed in a subdirectory of the of the
;; respective site-lisp directory. We have to add this back to the load-path:
;; Modified per Rafael's patch to only execute for emacs19

(if (string-match "^19." emacs-version)
    (setq load-path (nconc load-path
			   (list (concat "/usr/share/"
					 (symbol-name debian-emacs-flavor)
					 "/site-lisp/octave")))))

;; The Octave mode calls this file
(autoload 'octave-mode "octave-mod" nil t)
(autoload 'octave-help "octave-hlp" nil t)

(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

; (add-hook 'octave-mode-hook
;           (lambda ()
;             (abbrev-mode 1)
;             (auto-fill-mode 1)
;             (if (eq window-system 'x)
; 		(font-lock-mode 1))))

(autoload 'run-octave "octave-inf" nil t)
(autoload 'inferior-octave "octave-inf" nil t)
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
;            (define-key inferior-octave-mode-map [up]
;              'comint-previous-input)
;            (define-key inferior-octave-mode-map [down]
;              'comint-next-input)
	    ))
