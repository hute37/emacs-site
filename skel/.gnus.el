;; GNUS Config (NNTP)

(setq user-mail-address "you@somewhere.com")
(setq user-full-name "Your Name")

;;(setq gnus-nntp-server "news.eternal-september.org" nntp-port-number "563")
;;(setq gnus-select-method '(nntp "news.eternal-september.org"))
;;(setq gnus-select-method '(nntp "news.eternal-september.org"))
;;(setq gnus-select-method '(nntp "nntp.idecnet.com"))
;;(setq gnus-select-method '(nntp "news.gmane.org"))
;;(setq gnus-select-method '(nntp "news.cu.mi.it"))

;;(setq gnus-select-method '(nntp "news.eternal-september.org" (nntp-port-number 563) ))
(setq gnus-select-method '(nntp "nntp.aioe.org" (nntp-port-number 119) ))


;;(add-to-list 'gnus-secondary-select-methods '(nntp "localhost"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
     
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;;(add-to-list 'gnus-secondary-select-methods '(nnml ""))
     
;;(setq mail-sources '((pop :server "pop.provider.org" :user "you" :password "secret")))

;;(setq send-mail-function 'smtpmail-send-it)
;;(setq message-send-mail-function 'smtpmail-send-it)
;;(setq smtpmail-default-smtp-server "your.smtp-server.de")


;;(setq smtpmail-auth-credentials '(("hostname" "port" "username" "password")))
;;(setq smtpmail-starttls-credentials '(("hostname" "port" nil nil)))


