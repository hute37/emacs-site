;; ---( site.init: begin )-------------------------------------------------------
(message "SITE:begin")


;; ---( logs )--------------------------------------------------------

(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

;; ---( path )--------------------------------------------------------

(setq emacs-site-path (concat (getenv "HOME") "/.emacs-site/")) ;; ~/.emacs-site/

(defun emacs-site (filename)
"Expand FILENAME relative to ~/.emacs-site/ directory."
(expand-file-name filename emacs-site-path))

(defun emacs-home (filename)
"Expand FILENAME relative to ~/. directory."
(expand-file-name filename (getenv "HOME")))

(defun emacs-d (filename)
"Expand FILENAME relative to `user-emacs-directory'."
(expand-file-name filename user-emacs-directory))

;; ---( init )--------------------------------------------------------

(cond
 ((string-lessp emacs-version "24.3") ;; 
  (progn
    (message "SITE:local, ...")
    (load (emacs-site "site-local"))   ;; maint/local
    (message "SITE:local.")
    ))
 (t
  (progn
    (message "SITE:boot, ...")
    (load (emacs-site "site-boot"))   ;; master/package
    (message "SITE:boot.")
    ))
)

;; ---( site.init: end )-------------------------------------------------------
(message "SITE:end")

(provide 'site-start)
;;; site-start.el ends here
