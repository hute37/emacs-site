;; ---( site.boot: begin )-------------------------------------------------------
(message "SITE:BOOT - begin")


;; ---( init support )-------------------------------------------------------

(defun h7/load (filename)
"#H load init file."
 (progn
    (message "SITE:" filename ", ...")
    (load (emacs-site (concat filename ".el")))
    (message "SITE:" filename ".")
    ))

(defun h7/home (filename)
"#H load home file."
 (progn
    (message "SITE:~/" filename ", ...")
    (load (emacs-home (concat  "."  filename ".el")))
    (message "SITE:~/" filename ".")
    ))

(defun h7/clear ()
"#H clear initial settings."
 (progn
    (message "SITE: Clear, ...")
    (global-unset-key [(meta c)]) ;; meta-C alias for Ctrl-C
    (message "SITE: Clear.")
    ))

(defun h7/local ()
"#H clear initial settings."
 (progn
   (message "SITE: Local, ...")
   (setq h7-local-conf "~/.emacs-local.el")
   (cond 
    ((file-exists-p h7-local-conf)
	 (progn 
           (message "SITE: Local: " h7-local-conf ", ...")
           (load h7-local-conf)
           (message "SITE: Local: " h7-local-conf ".")
           (setq h7-locals t)
	  ))
	(t
	 (progn 
           (setq h7-locals t)
	  ))
	)

    (message "SITE: Local.")
    ))

(defun h7/path (directory)
"#H add load path."
    (message "SITE: path " directory ", ...")
    (progn
      (setq load-path 
            (append load-path 
                    (list directory )
                    ))
    (message "SITE: path " directory ".")
    ))



;; ---( init sequence )-------------------------------------------------------

(defun h7/init ()
    "#H main init sequence."
  (progn
    
    (h7/clear)
    
    (h7/load "site-pref")
    (h7/load "site-conf")

    (h7/local)
    
    (h7/load "site-pkgs")
    (h7/load "site-func")
    (h7/load "site-cust")
    (h7/load "site-misc")
    (h7/load "site-edit")
    (h7/load "site-view")
    (h7/load "site-keys")
    (h7/load "site-tool")
    
    (h7/load "site-user")
    
    (h7/path "~/.emacs-site/addon")
    
;;    (h7/home "emacs-user")
    
    ))

;; ---( site.boot: end )-------------------------------------------------------

(cond
 ((string-lessp emacs-version "24.3") ;; 
  (progn
    (message "SITE:empty, ...")
    (h7/load "site-load")
    (message "SITE:empty.")
    ))
 (t
  (progn
    (message "SITE:load, ...")
    (h7/init) 
    (message "SITE:load.")
    ))
)


;; ---( site.boot: end )-------------------------------------------------------
(message "SITE:BOOT - end")
