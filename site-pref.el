;; ---( site.func: begin )-------------------------------------------------------
(message "SITE:PREF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7prefs nil
  "H7 profile customization."
  :group 'convenience)

(defcustom z-use-helm nil
  "Non-nil means to activate  'helm'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-ivy nil
  "Non-nil means to activate  'ivy'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-vertico nil
  "Non-nil means to activate  'vertico'."
  :type 'boolean
  :group 'h7prefs)

(defcustom z-use-pdf-tools nil
  "Non-nil means to activate  'pdf-tools' instead of 'docview'."
  :type 'boolean
  :group 'h7prefs)


;; ---( ... )--------------------------------------------------------------

(defun h7/use-helm ()
 z-use-helm)

(defun h7/use-ivy ()
 z-use-ivy)

(defun h7/use-vertico ()
 z-use-vertico)


;; ---( ... )--------------------------------------------------------------

(defun h7/use-pdf-tools ()
 z-use-pdf-tools)

(defun h7/use-pdf-docview ()
 (not z-use-pdf-tools))


;; ---( site.func: end )-------------------------------------------------------
(message "SITE:PREF - end")

(provide 'site-pref)
;;; site-pref.el ends here
