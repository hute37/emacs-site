;; ---( site.keys: begin )-------------------------------------------------------
(message "SITE:KEYS - begin")


;; @see: https://github.com/jwiegley/dot-emacs/blob/master/init.el


;;;////////////////////////////////////////////////////////////////
;;;  @MOUSE INPUT MAPPINGS
;;;////////////////////////////////////////////////////////////////
(message "SITE:MOUSE")

;; ---( Wheel )---------------------------------------------------------------

(mouse-wheel-mode t)                      ;; enable wheel

(global-set-key [C-mouse-4] 'text-scale-decrease)
(global-set-key [C-mouse-5] 'text-scale-increase)
(global-set-key [M-mouse-4] 'bs-cycle-next)
(global-set-key [M-mouse-5] 'bs-cycle-previous)

;;(require 'mouseme)
;;(define-key global-map [M-S-down-mouse-3] 'imenu)

;; ---( Meta )---------------------------------------------------------------

;; Paste at point NOT at cursor
;(setq mouse-yank-at-point 't)


;;;////////////////////////////////////////////////////////////////
;;;  @KEYBOARD INPUT MAPPINGS
;;;////////////////////////////////////////////////////////////////
(message "SITE:KEYBOARD")


;; ============================================
;; ---( Custom Maps )-----
;; ============================================

;; ---( comma )----------------------------------------------------

(defvar ctl-h7-map)
(define-prefix-command 'ctl-h7-map)
(bind-key "C-," 'ctl-h7-map)


;; ============================================
;; ---( Edit Keys )-----
;; ============================================

;; ---( Arrows )----------------------------------------------------

(global-set-key [(control up)] 'scroll-down-one )
(global-set-key [(control down)] 'scroll-up-one )
(global-set-key [(control kp-up)] 'scroll-down-one )
(global-set-key [(control kp-down)] 'scroll-up-one )
;; (global-set-key [(meta up)] 'backward-block-of-lines )
;; (global-set-key [(meta down)] 'forward-block-of-lines )

(global-set-key [(control right)] 'forward-word )
(global-set-key [(control left)] 'backward-word )
(global-set-key [(control kp-right)] 'forward-word )
(global-set-key [(control kp-left)] 'backward-word )

;; (global-set-key [(control meta right)] 'backward-sexp )
;; (global-set-key [(control meta left)] 'forward-sexp )


;; (global-set-key [(shift meta left)] 'backward-sexp-nomark )
;; (global-set-key [(shift meta right)] 'forward-sexp-nomark )
(global-set-key [(shift meta up)] 'backward-sexp-mark )
(global-set-key [(shift meta down)] 'forward-sexp-mark )
(global-set-key [(shift meta left)] '(lambda () (interactive) (other-frame -1)))
(global-set-key [(shift meta right)] '(lambda () (interactive) (other-frame +1)))
;; (global-set-key [(meta control up)] '(lambda () (interactive) (other-frame -1)))
;; (global-set-key [(meta control down)] '(lambda () (interactive) (other-frame +1)))
;; (global-set-key [(meta control right)] 'next-multiframe-window )
;; (global-set-key [(meta control left)] 'previous-multiframe-window )

(global-set-key [(meta up)] 'dired )
(global-set-key [(meta down)] 'bs-show )
(global-set-key [(meta right)] 'bs-cycle-next )
(global-set-key [(meta left)] 'bs-cycle-previous )
(global-set-key [(meta kp-up)] 'dired )
(global-set-key [(meta kp-down)] 'bs-show )
(global-set-key [(meta kp-right)] 'bs-cycle-next )
(global-set-key [(meta kp-left)] 'bs-cycle-previous )

;; ---( Page )-------------------------------------------------------

(global-set-key [(control prior)] 'other-frame )
(global-set-key [(control next)] 'other-window )
(global-set-key [(control kp-prior)] 'other-frame )
(global-set-key [(control kp-next)] 'other-window )

;; (global-set-key [(meta control prior)] 'outline-previous-visible-heading )
;; (global-set-key [(meta control next)] 'outline-next-visible-heading )

;; ---( Home/End )---------------------------------------------

;;(global-set-key [home] 'beginning-of-line)
;;(global-set-key [end] 'end-of-line)

;;  (define-key global-map [(shift end)]           'end-of-line-mark)
;;  (define-key global-map [end]                   'end-of-line-nomark)
;;  (global-set-key [(shift end)]           'end-of-line-mark)
;;  (global-set-key [end]                   'end-of-line-nomark)
;;  (global-set-key [(shift control end)]          'end-of-buffer-mark)
;;  (global-set-key [(control end)]                'end-of-buffer-nomark)
;;  (global-set-key [(shift meta end)]             'end-of-buffer-mark)
;;  (global-set-key [(meta end)]                   'end-of-buffer-nomark)


;;  (define-key global-map [(shift home)]          'beginning-of-line-mark)
;;  (define-key global-map [home]                  'beginning-of-line-nomark)
;;  (global-set-key [(shift home)]          'beginning-of-line-mark)
;;  (global-set-key [home]                  'beginning-of-line-nomark)
;;  (global-set-key [(shift control home)]         'beginning-of-buffer-mark)
;;  (global-set-key [(control home)]               'beginning-of-buffer-nomark)
;;  (global-set-key [(shift meta home)]            'beginning-of-buffer-mark)
;;  (global-set-key [(meta home)]                  'beginning-of-buffer-nomark)


(global-set-key [(meta home)] 'delete-other-windows )
(global-set-key [(meta end)] 'kill-this-buffer )
(global-set-key [(meta kp-home)] 'delete-other-windows )
(global-set-key [(meta kp-end)] 'kill-this-buffer )

;; ---( BackSpace )---------------------------------------------

(global-set-key [backspace] 'backward-delete-char )
(global-set-key [(control backspace)] 'backward-kill-word )
(global-set-key [(meta backspace)] 'undo )

;; ---( Tab )---------------------------------------------

(global-set-key [(control shift tab)]   'bs-cycle-previous)
(global-set-key [(control tab)]  'bs-cycle-next)


;; ---( Return )---------------------------------------------

;;(global-set-key [(control return)] 'cua-rect )
;;(global-set-key [(meta return)] 'ffap)

;; ---( Space )---------------------------------------------


;; (global-set-key [(control meta space)]
;;   '(lambda () (interactive)
;; 	  (set-mark-command nil)
;; 	  (zmacs-deactivate-region)
;; 	  ))

;;(global-set-key [(control space)] 'hippie-expand )
;;(global-set-key [(meta space)] 'imenu)
;;(define-key global-map [M-S-down-mouse-3] 'imenu))



;;;////////////////////////////////////////////////////////////////
;;;  @COMPLETION
;;;////////////////////////////////////////////////////////////////
(message "SITE:COMPLETION")

;; ---( hippie-expand )------------------------------------------
;; hippie-expand  (auto skriv resten af ord jeg har skrevet før)

(cond
 ((string-lessp emacs-version "29.3")
  (progn

;;TODO: completion

    ))
 (t
  (progn

;; C-\ defaults to toggle-input-method
(define-key esc-map "\\" 'dabbrev-completion)
(define-key global-map "\C-\\" 'dabbrev-expand)
;; Many people are used to typing C-SPC and getting C-@.
;;(define-key global-map [?\C- ] 'dabbrev-expand)
;;(global-set-key "\M- " 'hippie-expand) ; der står meta space!
(define-key esc-map "\C-M" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))



    ))
 )




(cond
 ((string-lessp emacs-version "29.3")
  (progn


    ))
 (t
  (progn


    ))
 )


;;;////////////////////////////////////////////////////////////////
;;;  @MARK
;;;////////////////////////////////////////////////////////////////
(message "SITE:MARK")

;; ---( basic clipboard )------------------------------------------


(cond
 ((string-lessp emacs-version "20.3")
  (progn


    ))
 (t
  (progn

;; (define-key global-map "\C-@" 'set-mark-command)
;; (define-key global-map "\C-@" 'cua-set-mark)
(define-key esc-map " " 'pop-global-mark)

;; ctrl-space set-mark-command
;; ctrl-y yank
;; ctrl-w cut
;; meta-w copy


    ))
 )






;;;////////////////////////////////////////////////////////////////
;;;  @WMKEYS
;;;////////////////////////////////////////////////////////////////
(message "SITE:K-WMKEYS")

;; ---( Arrows )--------------------------------------------------

(global-set-key [(meta control up)] 'raise-frame)
(global-set-key [(meta control down)] 'lower-frame)
(global-set-key [(meta control right)] '(lambda () (interactive) (other-frame -1)))
(global-set-key [(meta control left)] '(lambda () (interactive) (other-frame +1)))

;; ---( Page )----------------------------------------------------

;; (global-set-key [(meta control prior)] 'outline-previous-visible-heading )
;; (global-set-key [(meta control next)] 'outline-next-visible-heading )

;; ---( Home/End )------------------------------------------------

(global-set-key [(meta control home)] 'delete-other-windows )
(global-set-key [(meta control end)] 'delete-window )

;; ---( Tab )-----------------------------------------------------

(global-set-key [(meta control shift tab)]  'previous-multiframe-window)
(global-set-key [(meta control tab)]  'next-multiframe-window)


;; ---( Return )---------------------------------------------------

(global-set-key [(meta control return)] 'ffap)

(global-set-key [(meta return)] 'toggle-fullscreen)


;; ---( Meta-Control Keys )----------------------------------------

(define-key esc-map "\C-z" 'iconify-or-deiconify-frame)
(define-key esc-map "\C-n" 'ergo-font-small-frame)







;; ============================================
;; ---( Function Keys )-----
;; ============================================
(message "SITE:K-FUNKEYS")


;; ---( F1: Help )---------------------------------------------------------


(global-set-key [f1] 'help )
(global-set-key [(shift f1)] 'woman )
(global-set-key [(control f1)] 'find-function )
(global-set-key [(meta f1)]	'function-key-error)
(global-set-key [(shift meta f1)] 'function-key-error)


;; ---( F2: Bookmarks/Breakpoints )-------------------------------------

(global-set-key [f2] ' bookmark-bmenu-list)
(global-set-key [(shift f2)] 'bookmark-set )

(global-set-key [(control f2)]
    '(lambda () (interactive)
       (if (eq hs-minor-mode nil)
	   (progn
	     (hs-minor-mode t)
	     (hs-hide-all))
	 (hs-minor-mode nil))))

(global-set-key [(meta f2)]	'hs-toggle-hiding)
(global-set-key [(shift meta f2)] 'toggle-line-wrapping )


;; ---( F3: ISearch/Find )----------------------------------------------------

(global-set-key [f3] 'isearch-repeat-forward )
(global-set-key [(shift f3)] 'isearch-repeat-backward )
(global-set-key [(control f3)] 'view-mode ) 
(global-set-key [(meta f3)] 'occur )
(global-set-key [(shift meta f3)] 'function-key-error)

;; ---( F4: Macro )----------------------------------------------------

(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [(shift f4)] 'start-or-end-kbd-macro )
(global-set-key [(control f4)] 'edit-last-kbd-macro )
(global-set-key [(meta f4)] 'kbd-macro-query )
(global-set-key [(shift meta f4)] 'edit-last-kbd-macro )
;; (global-set-key [(control f4)] 'start-kbd-macro )
;; (global-set-key [(meta f4)] 'end-kbd-macro )

;; ---( F5: Search/Grep )----------------------------------------------------

(global-set-key [f5] 'isearch-forward-regexp )
(global-set-key [(shift f5)] 'isearch-backward-regexp )
(global-set-key [(control f5)] 'find-grep-dired )
(global-set-key [(meta f5)] 'grep  )
(global-set-key [(shift meta f5)] 'function-key-error)

;; ---( F6: Replace/Ediff )----------------------------------------------------

(global-set-key [f6] 'query-replace )
(global-set-key [(shift f6)] 'query-replace-regexp )
(global-set-key [(control f6)] 'compare-windows )
(global-set-key [(meta f6)] 'ediff )
(global-set-key [(shift meta f6)] 'function-key-error)

;; ---( F7: Debug/Step )----------------------------------------------------

(global-set-key [f7] 'gud-step ) ;;@TODO: move to local mode map
(global-set-key [(control f7)] 'function-key-error)
(global-set-key [(meta f7)] 'function-key-error)
(global-set-key [(shift f7)] 'function-key-error)
(global-set-key [(shift meta f7)] 'function-key-error)

;; ---( F8: Debug/Next )----------------------------------------------------

;;(global-set-key [f8] 'function-key-error ) ;;WM expose
(global-set-key [(control f8)] 'gud-next) ;;@TODO: move to local mode map
(global-set-key [(meta f8)] 'function-key-error)
(global-set-key [(shift f8)] 'function-key-error)
(global-set-key [(shift meta f8)] 'function-key-error)

;; ---( F9: compile/run )----------------------------------------------------

;;(global-set-key [f9] 'function-key-error ) ;;WM expose
;;(global-set-key [f9] 'perldb ) ;;@TODO: move to local mode map
(global-set-key [(shift f9)] 'mode-compile-kill )
(global-set-key [(control f9)] 'mode-compile )
;;(global-set-key [(meta f9)] 'recompile )
;;(global-set-key [(shift meta f9)] 'compile)

(global-set-key [(meta f9)] 'shell)
(global-set-key [(shift meta f9)] 'list-processes)


;; ---( F10: UI )----------------------------------------------------

;;(global-set-key [f10] 'menu )
;;(global-set-key [(control f10)] 'menu-bar-mode )
(global-set-key [(control f10)] 'toggle-menubar )
(global-set-key [(shift f10)] 'toggle-toolbar )
(global-set-key [(meta f10)] 'speedbar )
(global-set-key [(shift meta f10)] 'toggle-gutter)


;; ---( F11: VCS )----------------------------------------------------

(cond
 ((fboundp 'magit-status);; Git magit
  (progn
    (global-set-key [f11] 'magit-status )
    (global-set-key [(shift meta f11)] 'vc-next-action)
    (global-set-key [(meta f11)] 'vc-diff )
    (global-set-key [(control meta f11)] 'toggle-fullscreen )
    ))
 (t ;; fallback to VC bindings
  (progn
    (global-set-key [f11] 'vc-next-action )
    (global-set-key [(shift f11)] 'vc-annotate )
    (global-set-key [(control f11)] 'vc-directory )
    (global-set-key [(meta f11)] 'vc-version-diff )
    (global-set-key [(shift meta f11)] 'function-key-error)
    (global-set-key [(control meta f11)] 'toggle-fullscreen )
    ))
 )





;;(define-key speedbar-key-map [button1] 'dframe-click)


;; ---( F12: Frames )----------------------------------------------------

(global-set-key [f12] 'make-frame )
(global-set-key [(control f12)] 'delete-frame )
(global-set-key [(shift f12)] 'buffer-menu )
(global-set-key [(meta f12)] 'revert-buffer )
(global-set-key [(shift meta f12)] 'function-key-error)






;; ============================================
;; ---( Numeric Keypad )-----
;; ============================================
(message "SITE:K-NUMPAD")

;; ---( center )--------------------------------------------------

(define-key global-map [begin]	'recenter)
(define-key global-map [(control kp-begin)] '(lambda () (interactive) (recenter 0)))

;; ---( comment )----------------------------------------------------

(define-key global-map [(control kp-divide)]	'comment-region)
(define-key global-map [(meta kp-divide)]	'uncomment-region)

;; ---( rect )----------------------------------------------------

(define-key global-map [(control kp-add)] 'kill-rectangle)
(define-key global-map [(control kp-subtract)] 'delete-rectangle)
(define-key global-map [(control kp-enter)] 'yank-rectangle)
(global-set-key [kp-subtract]
  '(lambda () (interactive)
	  (copy-rectangle-to-register ?r (region-beginning) (region-end) t )))
(global-set-key [kp-add]
  '(lambda () (interactive)
	  (copy-rectangle-to-register ?r (region-beginning) (region-end))))
(global-unset-key [kp-enter])
(global-set-key [kp-enter]
  '(lambda () (interactive)
	  (insert-register ?r)
	  (let ((col (current-column)))
		 (forward-line)
		 (move-to-column col t))))
(global-set-key [(meta kp-enter)]
  '(lambda () (interactive)
	  (insert-register ?r)))

;; ---( edit )----------------------------------------------------

;; (define-key global-map [kp-insert]	'yank)
;; (define-key global-map [(control kp-insert)]	'kill-ring-save)
;; (define-key global-map [(meta kp-insert)]	'overwrite-mode)

;; (define-key global-map [kp-delete]	'delete-char)
;; (define-key global-map [(control kp-delete)]	'kill-region)
;; ;; (define-key global-map [(meta kp-delete)]	'delete-frame)

;; (define-key global-map [kp-end]	'bs-show)
;; (define-key global-map [kp-home]	'delete-other-windows)

;; (define-key global-map [(control kp-home)]	'dired)
;; (define-key global-map [(control kp-end)]	'kill-this-buffer)

;; (define-key global-map [(meta kp-home)]	'make-frame)
;; (define-key global-map [(meta kp-end)]	'delete-frame)

;; ---( scroll )----------------------------------------------------

;; (define-key global-map [kp-left]	'backward-word)
;; (define-key global-map [kp-right]	'forward-word)
;; (define-key global-map [(control kp-left)]	'scroll-left)
;; (define-key global-map [(control kp-right)]	'scroll-right)
;; (define-key global-map [kp-up]	'scroll-down-one)
;; (define-key global-map [kp-down]	'scroll-up-one)

;; (define-key global-map [kp-multiply]	'scroll-left)


;; ---( WM )----------------------------------------------------

;; (define-key global-map [(control kp-prior)]	'shrink-window)
;; (define-key global-map [(control kp-next)]
;;   '(lambda () (interactive) (shrink-window -1)))
;; (define-key global-map [(meta kp-prior)]
;;   '(lambda () (interactive) (set-frame-height (selected-frame) (- (frame-height) 1))))
;; (define-key global-map [(meta kp-next)]
;;   '(lambda () (interactive) (set-frame-height (selected-frame) (+ (frame-height) 1))))
;; (define-key global-map [(control meta kp-prior)]
;;   '(lambda () (interactive) (set-frame-width (selected-frame) (- (frame-width) 1))))
;; (define-key global-map [(control meta kp-next)]
;;   '(lambda () (interactive) (set-frame-width (selected-frame) (+ (frame-width) 1))))


;; (progn
;;   (define-key global-map [(control meta kp-up)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (frame-parameter (selected-frame) 'left)
;; 			   (- (frame-parameter (selected-frame) 'top) 5))))
;;   (define-key global-map [(control meta kp-down)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (frame-parameter (selected-frame) 'left)
;; 			   (+ (frame-parameter (selected-frame) 'top) 5))))
;;   (define-key global-map [(control meta kp-left)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (- (frame-parameter (selected-frame) 'left) 5)
;; 			   (frame-parameter (selected-frame) 'top))))
;;   (define-key global-map [(control meta kp-right)]
;;     '(lambda () (interactive)
;;        (set-frame-position (selected-frame)
;; 			   (+ (frame-parameter (selected-frame) 'left) 5)
;; 			   (frame-parameter (selected-frame) 'top))))
;;   )



;; ============================================
;; ---( ASCII Keys )-----
;; ============================================
(message "SITE:K-ASCII")

;; ---( US-Keyboard )-----------------------------------------------------

(keyboard-translate 176 ?{ ) ;; -es
(keyboard-translate 167 ?} ) ;; par
(keyboard-translate 163 ?` ) ;; backquote
(keyboard-translate 231 ?~ ) ;; ced


;; ============================================
;; ---( Control Keys )-----
;; ============================================
(message "SITE:K-CTRL")

;; ---( Undo )------------------------------------------------------------

;;(define-key global-map "\C-z" 'undo)


;; ---( Compile )------------------------------------------------------------

;; (global-set-key "\C-cc" 'mode-compile)
;; (global-set-key "\C-c\C-c" 'mode-compile)
;; (global-set-key "\C-cq" 'mode-compile-kill)
;; (global-set-key "\C-b" 
;;     '(lambda () (interactive)
;;        (mode-compile)))

;; ---( Search )------------------------------------------------------------

(global-set-key "\C-f" 'occur)



;; ============================================
;; ---( ESC-Maps Keys )-----
;; ============================================
(message "SITE:K-ESCMAP")

;; ---( Numeric )--------------------------------------------------------

;;(define-key esc-map "1" 'color-theme-select )
;;(define-key esc-map "2" 'ergo-font-select )
;;(define-key esc-map "3" 'bury-buffer)
;;(define-key esc-map "4" 'delete-other-windows)
;;(define-key esc-map "5" 'other-frame )
;;(define-key esc-map "6" 'other-window )
;;(define-key esc-map "7" 'ergo-font-small-frame ) ;;mouse-set-font
;;(define-key esc-map "8" 'speedbar )
;;(define-key esc-map "9" 'describe-mode )
;;(define-key esc-map "0" 'delete-other-windows )


;; ---( Jump )------------------------------------------------------------


;;(define-key esc-map "n" 'goto-line )
;;(define-key esc-map "g" 'goto-line )

;; ---( CUA Clipboard )---------------------------------------------------

(define-key esc-map "c" 'kill-ring-save )
(define-key esc-map "v" 'yank )
(define-key esc-map "z" 'kill-region )


;; ---( Misc )------------------------------------------------------------

(define-key esc-map "o" 'dired-other-frame )


;; ---( meta f )----------------------------------------------------------

(global-unset-key [(meta f)])
(define-prefix-command 'z-meta-f-prefix)
(defvar z-meta-f-map (symbol-function 'z-meta-f-prefix))
(define-key global-map [(meta f)] 'z-meta-f-prefix)

(define-key z-meta-f-map [s] 'save-buffer)
(define-key z-meta-f-map [(meta s)] 'save-buffer)
(define-key z-meta-f-map [v]
  '(lambda ()
     (interactive) (save-some-buffers t)) )
(define-key z-meta-f-map [(meta v)]
  '(lambda ()
     (interactive) (save-some-buffers t)) )


;; ============================================
;; ---( Mode-Map Keys )-----
;; ============================================

;; ---( bookmark )--------------------------------------------------------

(define-key bookmark-map [return] 'bookmark-bmenu-select)




;; ---( site.func: end )-------------------------------------------------------
(message "SITE:KEYS - end")