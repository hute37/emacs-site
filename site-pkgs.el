;; ---( site.func: begin )-------------------------------------------------------
(message "SITE:FUNC - begin")

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @PACKAGES
;; ;;;////////////////////////////////////////////////////////////////

;; ---( Boot )--------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(package-initialize)

;; Bootstrap `use-package'
(unless (and
	 (package-installed-p 'bind-key)
	 (package-installed-p 'diminish)
	 (package-installed-p 'use-package)
	 (package-installed-p 'req-package)
	 )
  (package-refresh-contents)
  (package-install 'bind-key)
  (package-install 'diminish)
  (package-install 'use-package)
  (package-install 'req-package)
  )


;; @see: https://github.com/jwiegley/dot-emacs/blob/master/init.el

(eval-and-compile
  (defvar use-package-verbose t))


(require 'bind-key)
(require 'use-package)
(require 'req-package)



(use-package req-package)

;; ---( ... )--------------------------------------------------------------

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @BASIC
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ... )--------------------------------------------------------------

(use-package bs)

;; ---( ... )--------------------------------------------------------------

(use-package pretty-lambdada :init (dolist (hook '(lisp-mode-hook))) :config (dolist (global-pretty-lambda-mode)))
;;(use-package pretty-lambdada)

(use-package jumpc :config (dolist (jumpc-bind-vim-key)))

;; ---( powerline )--------------------------------------------------------------

;; (use-package powerline
;; 	     :init (dolist

;; 			 ;; powerine
;; 			 ;; smart-mode-line
;; 			 ;; smart-mode-line-powerline-theme
;; 			 ;;(require 'powerline)

;; 		       (set-face-attribute 'mode-line nil
;; 					   :foreground "Black"
;; 					   :background "DarkOrange"
;; 					   :box nil)

;; 		       (powerline-default-theme)

;; 		       ;;    (require 'smart-mode-line)
;; 		       ;;    (setq sml/theme 'powerline)
;; 		       ;;    (load-theme 'smart-mode-line-powerline t)
;; 		       ;;    (sml/setup)

;; 		       ;;(sml/apply-theme 'powerline)
;; 		       ;;(sml/apply-theme 'dark)
;; 		       ;;(sml/apply-theme 'light)
;; 		       ;;(sml/apply-theme 'respectful)
;; 		       ;;(sml/apply-theme 'automatic)

;; 		       ;;(powerline-default-theme)
		       
;; 		       (powerline-reset)
		       
;; 		       )
;; 	     )


;; ---( whitespace )--------------------------------------------------------------

(use-package whitespace
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(conf-mode-hook))
;;  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @MAGIT
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( magit )--------------------------------------------------------------

(use-package magit)



(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))




;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @HELM
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( helm )--------------------------------------------------------------

(use-package helm)


;; (use-package helm
;;   :ensure helm
;;   :diminish helm-mode
;;   :init
;;   (progn
;;     (require 'helm-config)
;;     (setq helm-candidate-number-limit 100)
;;     ;; From https://gist.github.com/antifuchs/9238468
;;     (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;           helm-input-idle-delay 0.01  ; this actually updates things
;;                                         ; reeeelatively quickly.
;;           helm-quick-update t
;;           helm-M-x-requires-pattern nil
;;           helm-ff-skip-boring-files t)
;;     (helm-mode))
;;   :bind (("C-c h" . helm-mini)
;;          ("C-h a" . helm-apropos)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x b" . helm-buffers-list)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x)
;;          ("C-x c o" . helm-occur)
;;          ("C-x c s" . helm-swoop)
;;          ("C-x c b" . my/helm-do-grep-book-notes)
;;          ("C-x c SPC" . helm-all-mark-rings)))
;; (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; ;; ---( helm-descbinds )--------------------------------------------------------------

;; (use-package helm-descbinds
;;   :defer t
;;   :bind (("C-h b" . helm-descbinds)
;;          ("C-h w" . helm-descbinds)))

;; ;; ---( helm local )--------------------------------------------------------------

;; ;; (defvar my/book-notes-directory "~/Dropbox/books")
;; ;; (defun my/helm-do-grep-book-notes ()
;; ;;   "Search my book notes."
;; ;;   (interactive)
;; ;;   (helm-do-grep-1 (list my/book-notes-directory)))

;; ---( site.func: end )-------------------------------------------------------
(message "SITE:FUNC - end")
