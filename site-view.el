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

;;; Default Font:
(add-to-list 'default-frame-alist '(font . "Monospace-12"))



;; ---( site.view: end )-------------------------------------------------------
(message "SITE:FUNC - end")
