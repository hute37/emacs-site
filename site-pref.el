;; ---( site.func: begin )-------------------------------------------------------
(message "SITE:PREF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7prefs nil
  "H7 profile customization."
  :group 'convenience)

(defcustom z-use-helm nil
  "Non-nil means to activate  'helm' instead of 'ivy'."
  :type 'boolean
  :group 'h7prefs)


;; ---( ... )--------------------------------------------------------------

(defun h7/use-helm ()
 z-use-helm)

(defun h7/use-ivy ()
 (not z-use-helm))


;; ---( site.func: end )-------------------------------------------------------
(message "SITE:PREF - end")
