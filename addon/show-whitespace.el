;;; show-whitespace.el --- Simple mode to highlight whitespace in all modes.
;;;

;; Show leading and trailing whitespace
(cond (window-system
    (progn
      (add-hook 'font-lock-mode-hook
		'(lambda()
		   (setq font-lock-keywords
			 (append font-lock-keywords
				 '(
				   ;; any space, underline (those at start or end of line will be overriden
				   ;; by lines below...)
				   ;;		   ("[ ]+" (0 'secondary-selection t))
				   ;; leading spaces, light blue
				   ("^[ ]+" (0 'secondary-selection t))
				   ;; spaces immediately after tab[s] (and the tab[s] but those get
				   ;; marked by the next line which overrides...), light blue
				   ("\t+[ ]+" (0 'secondary-selection t))
				   ;; any tabs, light green
				   ("[\t]+" (0 'secondary-selection t))
				   ;; trailing tabs or spaces, red
				   ("[\t ]+$" (0 'secondary-selection t))
				   ))))))))

;;  Allow this feature to be used.
(provide 'show-whitespace)