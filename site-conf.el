;; ---( site.conf: begin )------------------------------------------
(message "SITE:CONF - begin")

;; ---( ... )--------------------------------------------------------------

(defgroup h7config nil
  "H7 profile configuration."
  :group 'convenience)

;; ~/work/wo/wo-note/org/ref/references.bib
(defcustom z-var-global-bibliography '("~/Dropbox/Local/data/org/ref/references.bib")
  "Global BibLaTeX bibliography file."
  :type '(repeat string)
  :group 'h7config)


;; ~/work/wo/wo-note/org/net
(defcustom z-var-roam-directory "~/Dropbox/Local/data/org/net"
  "Roan default file."
  :type 'string
  :group 'h7config)

;; ~/work/wo/wo-note/org/nat
(defcustom z-var-denote-directory "~/Dropbox/Local/data/org/nat"
  "Roan default file."
  :type 'string
  :group 'h7config)



;; ---( ... )--------------------------------------------------------------

(defun h7/var-global-bibliography ()
 (list z-var-global-bibliography))


(defun h7/var-roam-directory ()
 z-var-roam-directory)

(defun h7/var-denote-directory ()
 z-var-denote-directory)



;; ---( site.conf: end )-------------------------------------------------------
(message "SITE:CONF - end")
