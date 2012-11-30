;;
;;
;;


;; ---( tramp )-----------------------------------------------------------------



;;(eval-after-load "tramp"
;;  '(when (boundp 'tramp-default-proxies-alist)
;;     (add-to-list 'tramp-default-proxies-alist
;;		  '("\\`example.com\\'" "\\`root\\'" "/ssh:%h:")))) 

(eval-after-load "tramp"
  '(when (boundp 'tramp-default-proxies-alist)
    (progn
      (setq tramp-verbose 10)
      (setq tramp-debug-buffer t)
      (add-to-list 'tramp-default-proxies-alist
		   '("dba-x1" "root" "/ssh:root@91.212.167.228:"))
      (add-to-list 'tramp-default-proxies-alist
		   '("dba-x2" "root" "/ssh:root@91.212.167.228:"))
;;      (add-to-list 'tramp-default-proxies-alist
;;		   '("\\`.\\'" nil nil))
      )))

;; (require 'tramp)

;; (add-to-list 'tramp-default-proxies-alist
;;                  '("dba-x\\." "root" "/ssh:root@91.212.167.228:"))
;;(add-to-list 'tramp-default-proxies-alist
;;                  '("\\." nil nil))


;; ---( server )-----------------------------------------------------

;; @see: http://lists.gnu.org/archive/html/emacs-devel/2010-10/msg00957.html
;;
;; cat  ~/.emacs.d/server/server
;; ssh -R 13502:localhost:13501 xxx@firewall
;; ssh -L 13501:localhost:13502 address@hidden
;;
;;  scp ~/.emacs.d/server/server xxx@firewall:.emacs.d/server/server 
;;  ssh xxx@firewall scp /home/xxx/.emacs.d/server/server address@hidden:.emacs.d/server/server 
;;
;;  ssh -t xxx@firewall ssh address@hidden \
;;  emacsclent -nw 
;;




(cond
 ((eq system-type 'windows-nt) ;; WinNT
  (progn
;; *unsupported*
;;    (require 'gnuserv)
;;    (gnuserv-start)
;;    (setq gnuserv-frame (selected-frame))
    ))
 ((eq system-type 'cygwin);; GNU-Cygwin
  (progn
    ;set server-host to be the name of the machine Emacs server will run on
    (setq server-host "127.0.0.1")
    ;set server-use-tcp to t
    (setq server-use-tcp t)
    (server-start)
    ))
 ((eq system-type 'gnu/linux);; GNU-Linux
  (progn
    (server-start)))
 ((eq system-type 'usg-unix-v);; Sun Solaris
  (progn
    (server-start)))
 (t
  (progn
    ))
 )



