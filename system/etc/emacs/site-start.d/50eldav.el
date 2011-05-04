;; -*-emacs-lisp-*-
;;
;; Emacs startup file for the Debian GNU/Linux eldav package
;;

(let ((dir (concat "/usr/share/" (symbol-name flavor) "/site-lisp/eldav")))
  (if (file-exists-p dir)
      (progn
	(setq load-path (cons dir load-path))
	(require 'eldav))))

;; proxy configuration
(setq eldav-proxy (or (getenv "http_proxy") (getenv "HTTP_PROXY")))
; (setq eldav-use-vc t)
