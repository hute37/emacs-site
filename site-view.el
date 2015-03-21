;; ---( site.view: begin )-------------------------------------------------------
(message "SITE:VIEW - begin")

;; ---( ... )--------------------------------------------------------------

;; @see: https://github.com/ejmr/DotEmacs/blob/master/.emacs

(when window-system
;;  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
;;  (scroll-bar-mode -1)
  )

(blink-cursor-mode 1)


(electric-pair-mode 0)
(setq visible-bell t)


(column-number-mode t)
(show-paren-mode t)
(global-hi-lock-mode 1)
(which-function-mode t)


;;;////////////////////////////////////////////////////////////////
;;;  @DISPLAY
;;;////////////////////////////////////////////////////////////////
(message "SITE:VIEW:display - begin")


;; ---( Version )-------------------------------------------------------------

(defvar running-alternate-emacs nil)

;; ---( Screen )--------------------------------------------------------------

(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defvar display-name
  (let ((width (display-pixel-width)))
    (cond ((= width 2560) 'retina-imac)
          ((= width 1440) 'retina-macbook-pro))))

;; ---( site.view:display end )-------------------------------------------------------
(message "SITE:VIEW:display - end")


;;;////////////////////////////////////////////////////////////////
;;;  @FONT
;;;////////////////////////////////////////////////////////////////
(message "SITE:VIEW:font - begin")

;; ---( defaut )--------------------------------------------------------------

;;; Default Font:
(add-to-list 'default-frame-alist '(font . "Monospace-12"))


;; (defvar emacs-min-font
;;   (cond
;;    ((eq display-name 'retina-imac)
;;     (if running-alternate-emacs
;;         "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
;;       "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"))
;;    (t
;;     (if running-alternate-emacs
;;         "-*-Myriad Pro-normal-normal-normal-*-17-*-*-*-p-0-iso10646-1"
;;       "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))))


;; ---( site.view:font end )-------------------------------------------------------
(message "SITE:VIEW:font - end")

;;;////////////////////////////////////////////////////////////////
;;;  @FRAME
;;;////////////////////////////////////////////////////////////////
(message "SITE:VIEW:frame - begin")

;; ---( Geometry )---------------------------------------------------------

(defvar emacs-min-top 23)
(defvar emacs-min-left
  (cond ((eq display-name 'retina-imac) 975)
        (t 521)))

(defvar emacs-min-height
  (cond ((eq display-name 'retina-imac) 55)
        (t 44)))

(defvar emacs-min-width 100)

;; ---( Window )--------------------------------------------------------------

;; (let ((frame-alist
;;        (list (cons 'top emacs-min-top)
;;              (cons 'left emacs-min-left)
;;              (cons 'height emacs-min-height)
;;              (cons 'width emacs-min-width)
;;              ;;(cons 'font emacs-min-font)
;;              )))
;;   (setq initial-frame-alist frame-alist))


;; ---( Presets )--------------------------------------------------------------

;; (defun emacs-min ()
;;   (interactive)
;;   (set-frame-parameter (selected-frame) 'fullscreen nil)
;;   (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
;;   (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
;;   (set-frame-parameter (selected-frame) 'top emacs-min-top)
;;   (set-frame-parameter (selected-frame) 'left emacs-min-left)
;;   (set-frame-parameter (selected-frame) 'height emacs-min-height)
;;   (set-frame-parameter (selected-frame) 'width emacs-min-width)
;;   ;;(set-frame-font emacs-min-font)
;;   ;; (when running-alternate-emacs
;;   ;;   (set-background-color "grey85")
;;   ;;   (set-face-background 'fringe "gray80"))
;;   )

;; (if window-system
;;     (add-hook 'after-init-hook 'emacs-min))

;; (defun emacs-max ()
;;   (interactive)
;;   (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
;;   (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
;;   (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

;; (defun emacs-toggle-size ()
;;   (interactive)
;;   (if (> (cdr (assq 'width (frame-parameters))) 100)
;;       (emacs-min)
;;     (emacs-max)))

;; (bind-key "C-c m" 'emacs-toggle-size)

;; ---( site.view:frame end )-------------------------------------------------------
(message "SITE:VIEW:frame - end")


;; ---( site.view: end )-------------------------------------------------------
(message "SITE:FUNC - end")
