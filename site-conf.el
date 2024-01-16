;; ---( site.conf: begin )------------------------------------------
(message "SITE:CONF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7config nil
  "H7 profile configuration."
  :group 'convenience)

;; ~/bib/references.bib
(defcustom z-var-global-bibliography '("~/Dropbox/Local/data/org/ref/references.bib")
  "Global BibLaTeX bibliography file."
  :type '(string)
  :group 'h7config)



;; ---( ... )--------------------------------------------------------------

(defun h7/var-global-bibliography ()
 z-var-global-bibliography)



;; ---( site.conf: end )-------------------------------------------------------
(message "SITE:CONF - end")
