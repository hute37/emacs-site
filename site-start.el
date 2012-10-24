;; ---( site.init: begin )-------------------------------------------------------
(message "SITE:begin")

;; ---( System )--------------------------------------------------------

(defvar z-emacs-type
  (if (string-match "XEmacs\\|Lucid" (emacs-version))
      'xemacs
    'fsf_emacs)
  "The type of emacs running, either 'fsf_emacs' or 'xemacs'. Uses
'emacs-version' to determine type.")

(defvar z-location
  (cond 
	((eq system-type 'windows-nt)
	 (cond 
	  ((string= (downcase (system-name)) "phoenix") 'home)
	  ((string= (downcase (system-name)) "neptune") 'home)
	  ((string= (downcase (system-name)) "altair") 'home)
	  (t 'work)))
	((eq system-type 'cygwin)
	 (cond 
	  ((string= (downcase (system-name)) "phoenix") 'home)
	  ((string= (downcase (system-name)) "neptune") 'home)
	  ((string= (downcase (system-name)) "altair") 'home)
	  (t 'work)))
	(t
	 (cond 
	  ((string= (downcase (system-name)) "tethys") 'home)
	  ((string= (downcase (system-name)) "helios") 'home)
	  ((string= (downcase (system-name)) "mercury") 'home)
	  ((string= (downcase (system-name)) "rigel") 'home)
	  (t 'work)))
	)
  "My physical location, either 'work' or 'home'. Uses 'system-name' to
determine this.")

(defvar z-system
  (if (eq system-type 'windows-nt)
      'winnt		; NT system
    'unix)			; unix/cygwin system
  "My current OS, either 'win' or 'unix'.")




;; ---( Load Path )--------------------------------------------------------

;;(setenv "HOME" "h:/home/gio/.xemacs")


  (cond 
	((eq system-type 'windows-nt)
	 (progn 
		(setq emacs-drive (getenv "EMACS_DRIVE"))
		(setenv "HOME"  "d:\\home\\gio")
	  ))
	((eq system-type 'cygwin)
	 (progn 
		(setq emacs-drive "")
	  ))
	(t
	 (progn 
		(setq emacs-drive "")
	  ))
	)


(and (>= emacs-major-version 20)   ;; /usr/local Xft build
 (add-to-list 'load-path "/usr/share/emacs/site/lisp" t))
 


;;(setq emacs-drive (getenv "EMACS_DRIVE"))
;;(setq emacs-site-base (concat emacs-drive "/usr/local"))
(setq emacs-site-base (concat emacs-drive "/usr/local/share/emacs/"))
(setq emacs-site-home (concat emacs-site-base "emacs-share/"))
(setq emacs-site-path (concat emacs-site-home "emacs-site/"))


(setq load-path 
      (append load-path 
	      (list 
	       emacs-site-path 
	       (concat emacs-site-path "addon" )
	       ;;(concat emacs-site-path "addon/magit/magit-1.1.1" )
	       (concat emacs-site-path "addon/magit/magit-1.2.0" )
	       )
	      ))

(setq Info-default-directory-list
      (append Info-default-directory-list 
	      (list 
	       emacs-site-path 
	       (concat emacs-site-path "addon" )
	       ;;(concat emacs-site-path "addon/magit/magit-1.1.1" )
	       (concat emacs-site-path "addon/magit/magit-1.2.0" )
	       "~/.info"
	       )
	      ))


;; ---( Autoload )---------------------------------------------------------

(load (concat emacs-site-path "site-custom")) ;; default custom
(load (concat emacs-site-path "site-init"))   ;; default init

;; ---( site.init: end )-------------------------------------------------------
(message "SITE:end")
