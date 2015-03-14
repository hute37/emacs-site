;; ---( site.init: begin )-------------------------------------------------------
(message "SITE:begin")

;; ---( System )--------------------------------------------------------


(setq emacs-site-path (concat (getenv "HOME") "/.emacs-site/")) ;; ~/.emacs-site/

(cond
 ((string-lessp emacs-version "29.3") ;; 
  (progn
    (message "SITE:local, ...")
    (load (concat emacs-site-path "site-local"))   ;; maint/local
    (message "SITE:local.")
    ))
 (t
  (progn
    (message "SITE:boot, ...")
    (load (concat emacs-site-path "site-boot"))   ;; master/package
    (message "SITE:boot.")
    ))
)

;; ---( site.init: end )-------------------------------------------------------
(message "SITE:end")
