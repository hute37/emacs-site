;; ---( site.pkgs: begin )-------------------------------------------------------
(message "SITE:PKGS - begin")

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @PACKAGES
;; ;;;////////////////////////////////////////////////////////////////

;; @see: https://github.com/jwiegley/use-package
;; @see: http://pages.sachachua.com/.emacs.d/Sacha.html
;; @see: https://github.com/bdd/.emacs.d/blob/master/packages.el
;; @see: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; @see: https://ladicle.com/post/config/

;; ---( Install )--------------------------------------------------------------

;;(fset 'h7/ensure 't)
;;(fset h7/ensure nil)

;; ---( Boot )--------------------------------------------------------------

;; (setq debug-on-error t)

(require 'package)
;;(nconc package-archives
;;       '(("melpa" . "http://melpa.org/packages/")
;;         ("org" . "http://orgmode.org/elpa/")))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; You don't need this one if you have marmalade:
;; (add-to-list 'package-archives '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages"))

(setq package-enable-at-startup nil)

;; (setq 
;;  load-prefer-newer t
;;  package-user-dir "~/.emacs.d/elpa"
;;  package--init-file-ensured t
;;  package-enable-at-startup nil)

;; (unless (file-directory-p package-user-dir)
;;   (make-directory package-user-dir t))    

;;(package-initialize)


;; (unless (package-installed-p 'use-package)
;;   (progn
;;     (package-refresh-contents)
;;     (package-install 'use-package)))
;; (eval-when-compile
;;   (eval-after-load 'advice
;;     '(setq ad-redefinition-action 'accept))
;;   (require 'use-package))
;; (require 'diminish)
;; (require 'bind-key)

;; (require 'package)
;; (setq package-enable-at-startup nil)

;; ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (unless (assoc-default "melpa" package-archives)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; (package-initialize)

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

;;(require 'req-package)
;;(use-package req-package)

;; ---( ... )--------------------------------------------------------------

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @BASIC
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ... )--------------------------------------------------------------

;;(use-package bs
;;  :ensure t)

;; ---( ... )--------------------------------------------------------------

;; @see: http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/

;; (use-package pretty-symbols
;;   :ensure t)

;; (use-package pretty-lambdada
;;   :ensure t
;;   :init (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook)))
;;   :config (dolist (global-pretty-lambda-mode)))
;; ;;(use-package pretty-lambdada)

(use-package jumpc
  :disabled t
  :config (progn (jumpc-bind-vim-key)))

;; ---( powerline )--------------------------------------------------------------

(use-package powerline
  :ensure t
  :init (powerline-default-theme)
)


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



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @MAGIT
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( magit )--------------------------------------------------------------

(use-package magit
  :ensure t
  :config
  (setenv "EDITOR" "emacsclient")
  (setenv "GIT_EDITOR" "emacsclient"))

;; git config --global core.editor "`which emacsclient` -t -s $EMACS_SERVER_FILE"

(use-package magit-zzz
  :disabled t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+?\\)\\*" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))
        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))
  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))
  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))
  (defun eshell/git (&rest args)
    (cond
     ((or (null args)
          (and (string= (car args) "status") (null (cdr args))))
      (magit-status-internal default-directory))
     ((and (string= (car args) "log") (null (cdr args)))
      (magit-log "HEAD"))
     (t (throw 'eshell-replace-command
               (eshell-parse-command
                "*git"
                (eshell-stringify-list (eshell-flatten-list args)))))))
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setenv "GIT_PAGER" "")
  (use-package magit-backup
    :diminish magit-backup-mode)
  (use-package magit-review
    :disabled t
    :commands magit-review
    :config (require 'json))
  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
  (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode)))
  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t))))

;; ---( projectile )--------------------------------------------------------------

(use-package git-timemachine
  :disabled t)

;; (use-package git-timemachine
;;   :ensure t
;;   :bind (("s-g" . git-timemachine)))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @PROJECT
;; ;;;////////////////////////////////////////////////////////////////


;; ---( projectile )--------------------------------------------------------------

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (emacs-d "var/projectile.cache")
        projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld"))
  :config
  (projectile-global-mode))





;; ---( etags )--------------------------------------------------------------

(use-package etags
  :bind ("M-T" . tags-search))

;; ---( gtags )--------------------------------------------------------------

(use-package gtags
  :disabled t
  :commands gtags-mode
  :diminish gtags-mode
  :config
  (bind-key "C-c t ." 'gtags-find-rtag)
  (bind-key "C-c t f" 'gtags-find-file)
  (bind-key "C-c t p" 'gtags-parse-file)
  (bind-key "C-c t g" 'gtags-find-with-grep)
  (bind-key "C-c t i" 'gtags-find-with-idutils)
  (bind-key "C-c t s" 'gtags-find-symbol)
  (bind-key "C-c t r" 'gtags-find-rtag)
  (bind-key "C-c t v" 'gtags-visit-rootdir)
  (bind-key "<mouse-2>" 'gtags-find-tag-from-here gtags-mode-map)
  (use-package helm-gtags
    :bind ("M-T" . helm-gtags-select)
    :config
    (bind-key "M-," 'helm-gtags-resume gtags-mode-map))
  )


;; ;; ---( ack )--------------------------------------------------------------

(use-package ack
  :disabled t)
;;(use-package ack-and-a-half)

;; ---( ag )--------------------------------------------------------------

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer )
  (twgrep-change-readonly-file t))

(use-package ag
  :custom
  (ag-highligh-search )
  (tag-reuse-buffers )
  (tag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))


;; ---( grep )--------------------------------------------------------------

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)
         ("M-s p" . find-grep-in-project))
  :init
  (defun find-grep-in-project (command-args)
    (interactive
     (let ((default (thing-at-point 'symbol)))
       (list (read-shell-command "Run find (like this): "
                                 (cons (concat "git --no-pager grep -n "
                                               default)
                                       (+ 24 (length default)))
                                 'grep-find-history))))
    (if command-args
        (let ((null-device nil)) ; see grep
          (grep command-args))))
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))
  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (if nil
      (progn
        (setq-default grep-first-column 1)
        (grep-apply-setting
         'grep-find-command
         '("ag --noheading --nocolor --smart-case --nogroup --column -- "
           . 61)))
    (grep-apply-setting
     'grep-find-command
     '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49))))


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @TEXT
;; ;;;////////////////////////////////////////////////////////////////


;; ---( markdown )--------------------------------------------------------------


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
;;   :init
;;   (add-hook 'markdown-mode-hook 'spell-check-and-wrap-at-80)
;;   :config
;;   (progn
;;     (let ((preferred-markdown-impl "peg-markdown"))
;;       (when (executable-find preferred-markdown-impl)
;;         (setq markdown-command preferred-markdown-impl)))))


;; ---( yaml )--------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :defer t)


;; ---( css )--------------------------------------------------------------

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

;; ;; @see: https://gitea.petton.fr/nico/emacs.d/src/commit/8ae2b902c916600c9296d967f36ed69ad50e8199/init.el?lang=sv-SE
;; (use-package rainbow-mode
;;   :disabled t	     
;;   :config
;;   (add-hook 'css-mode-hook 'rainbow-mode)
;;   (add-hook 'less-mode-hook 'rainbow-mode))



;; ---( json )--------------------------------------------------------------

(use-package json-mode
  :mode "\\.json\\'")


;; ---( nxml )--------------------------------------------------------------

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
)


;; (use-package nxml-mode
;;   :commands nxml-mode
;;   :init
;;   (defalias 'xml-mode 'nxml-mode)
;;   :config
    
;;   (defun my-nxml-mode-hook ()
;;     (bind-key "<return>" 'newline-and-indent nxml-mode-map))
;;   (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
;;   (defun tidy-xml-buffer ()
;;     (interactive)
;;     (save-excursion
;;       (call-process-region (point-min) (point-max) "tidy" t t nil
;;                            "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))
;;   (bind-key "C-c M-h" 'tidy-xml-buffer nxml-mode-map)
;;   (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers)))


;; (use-package web-mode
;;   :disabled t	     
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;   (setq web-mode-css-indent-offset 2))

  







;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @LANG
;; ;;;////////////////////////////////////////////////////////////////


;; ---( R )--------------------------------------------------------------

(use-package ess
  :if (version<= "25.1" emacs-version)
  :defer t
  ;; :ensure t
  
  ;;:load-path "site-lisp/ess/lisp/"
  ;;:config (ess-toggle-underscore nil)
  :init
  (add-hook 'ess-mode-hook
            (lambda ()

              (ess-set-style 'RStudio)
            
              ;; Replace \C-c with \M-c for CUA and ctrl key swap
              
              ;; ;; By popular demand:
              ;;(define-key map "\C-m"             'ess-newline-and-indent); = [RETURN]
              ;;(define-key map [remap yank]       'ess-yank)

              (define-key ess-mode-map (kbd "M-c M-c")      'ess-eval-region-and-go)
              
              (define-key ess-mode-map (kbd "M-c C-r")      'ess-eval-region)
              (define-key ess-mode-map (kbd "M-c M-r")      'ess-eval-region-and-go)
              (define-key ess-mode-map (kbd "M-c C-b")      'ess-eval-buffer)
              (define-key ess-mode-map (kbd "M-c M-b")      'ess-eval-buffer-and-go)
              (define-key ess-mode-map (kbd "M-c C-<up>")   'ess-eval-buffer-from-beg-to-here)
              (define-key ess-mode-map (kbd "M-c C-<down>") 'ess-eval-buffer-from-here-to-end)
              (define-key ess-mode-map (kbd "M-c C-f")      'ess-eval-function)
              (define-key ess-mode-map (kbd "M-c M-f")      'ess-eval-function-and-go)
              (define-key ess-mode-map (kbd "M-c C-c")      'ess-eval-region-or-function-or-paragraph-and-step)
              (define-key ess-mode-map (kbd "M-c C-p")      'ess-eval-paragraph-and-step)
              (define-key ess-mode-map (kbd "M-c M-p")      'ess-eval-paragraph-and-go)
              (define-key ess-mode-map (kbd "M-c M-x")      'ess-eval-region-or-function-or-paragraph)
              (define-key ess-mode-map (kbd "M-c M-n")      'ess-eval-line-and-step)
              (define-key ess-mode-map (kbd "M-c M-j")      'ess-eval-line)
              (define-key ess-mode-map [(control return)]   'ess-eval-region-or-line-and-step)
              (define-key ess-mode-map (kbd "M-c M-j")      'ess-eval-line-and-go)
              ;; the next three can only work in S/R - mode {FIXME}
              (define-key ess-mode-map (kbd "M-c M-a")      'ess-goto-beginning-of-function-or-para)
              (define-key ess-mode-map (kbd "M-c M-e")      'ess-goto-end-of-function-or-para)
              (define-key ess-mode-map "\C-xnd"             'ess-narrow-to-defun-or-para)
              (define-key ess-mode-map "\C-xnf"             'ess-narrow-to-defun-or-para)
              (define-key ess-mode-map (kbd "M-c M-y")      'ess-switch-to-ESS-deprecated)
              (define-key ess-mode-map (kbd "M-c M-z")      'ess-switch-to-inferior-or-script-buffer)
              (define-key ess-mode-map (kbd "M-c C-z")      'ess-switch-to-inferior-or-script-buffer)
              (define-key ess-mode-map (kbd "C-c C-z")      'ess-switch-to-inferior-or-script-buffer)
              (define-key ess-mode-map (kbd "C-c M-l")      'ess-load-file)
              (define-key ess-mode-map (kbd "M-c M-l")      'ess-load-file); alias, as in 'iESS' where C-c C-l is comint-list-*
              (define-key ess-mode-map (kbd "M-c M-v")      'ess-display-help-on-object)
              ;;(define-key ess-mode-map "\C-c5\C-d"'ess-dump-object-into-edit-buffer-other-frame)
              (define-key ess-mode-map (kbd "M-c M-s")      'ess-switch-process) ; use a
              
              ;; different process for the buffer.
              ;; (define-key map "\C-c\C-t"        'ess-execute-in-tb)
              ;;(define-key ess-mode-map (kbd "M-c \t")     'ess-complete-object-name-deprecated)
              ;; (define-key ess-mode-map "\C-c\t"        'comint-dynamic-complete-filename)
              
              (unless (and (featurep 'emacs) (>= emacs-major-version 24))
                (define-key ess-mode-map (kbd "M-c <tab>")  'comint-dynamic-complete))
              (define-key ess-mode-map (kbd "M-c .")        'ess-list-object-completions)
              
              ;; wrong here (define-key ess-mode-map "\C-c\C-k" 'ess-request-a-process)
              (define-key ess-mode-map (kbd "M-c M-k")      'ess-force-buffer-current)
              (define-key ess-mode-map (kbd "M-c `")        'ess-show-traceback)
              (define-key ess-mode-map (kbd "M-c \\")       'ess-show-call-stack)
              
              ;;(define-key ess-mode-map (kbd "M-c .")      (lambda () (interactive) (message "ess-set-style moved to C-c C-e C-s. Sorry for the inconvenience")))
              
              ;;(define-key ess-mode-map "{"                'ess-electric-brace)
              ;;(define-key ess-mode-map "}"                'ess-electric-brace)
              
              (define-key ess-mode-map (kbd "M-c M-q")      'ess-indent-exp)
              (define-key ess-mode-map (kbd "<M-S-right>")  'ess-mark-function-or-para)
              (if (featurep 'xemacs) ;; work around Xemacs bug (\C-\M-h redefines M-BS):
                  (define-key ess-mode-map [(meta backspace)] 'backward-kill-word))
              ;;(define-key ess-mode-map [delete]           'backward-delete-char-untabify)
              
              ;;(define-key ess-mode-map "\t"               'ess-indent-or-complete)
              (define-key ess-mode-map (kbd "M-c C-q")      'ess-quit)
              (define-key ess-mode-map (kbd "M-c M-r")      'ess-use-this-dir)

              ;; smart operators; most likely will go in the future into a separate local map
              ;;(define-key map ","          'ess-smart-comma)

              (define-key ess-mode-map (kbd "M-c M-d")       'ess-doc-map)
              (define-key ess-mode-map (kbd "M-c M-e")       'ess-extra-map)
              (define-key ess-mode-map (kbd "M-c M-t")       'ess-dev-map)
              (define-key ess-mode-map (kbd "M-c C-d")       'ess-doc-map)
              (define-key ess-mode-map (kbd "M-c C-e")       'ess-extra-map)
              (define-key ess-mode-map (kbd "M-c C-t")       'ess-dev-map)

              
;;            (ess-toggle-underscore nil))
               ;; (define-key ess-mode-map (kbd "M-c M-c") 
               ;;   'ess-eval-region-and-go)
            ))
  (add-hook 'inferior-ess-mode-hook
            '(lambda nil
               (define-key inferior-ess-mode-map [\C-up]
                 'comint-previous-matching-input-from-input)
               (define-key inferior-ess-mode-map [\C-down]
                 'comint-next-matching-input-from-input)
               (define-key inferior-ess-mode-map [\C-x \t]
                 'comint-dynamic-complete-filename)
               )
            )
  :commands R)

(use-package ess-R-data-view
  :defer t)

(use-package ess-R-object-popup
  :defer t)

(use-package ess-R-data-smart-equals
  :disabled t)

(use-package ess-R-data-smart-underscore
  :disabled t)



;; ---( polymode )--------------------------------------------------------------

;; (use-package polymode
;;   :ensure t
;;   :mode (
;;          ("\\.md$" . poly-markdown-mode)
;;          ("\\.Snw$" . poly-noweb+r-mode)
;;          ("\\.Rnw$" . poly-noweb+r-mode)
;;          ("\\.Rmd$" . poly-markdown+r-mode)
;;          ("\\.rapport$" . poly-rapport-mode)
;;          ("\\.Rhtml$" . poly-html+r-mode)
;;          ("\\.Rbrew$" . poly-brew+r-mode)
;;          ("\\.Rcpp$" . poly-r+c++-mode)
;;          ("\\.cppR$" . poly-c++r-mode)
;;          )
;; )




;; ---( python )--------------------------------------------------------------


(use-package elpy
  :ensure t
  :preface
  
  ;; @see: https://elpy.readthedocs.org/en/latest/
  ;; @see: https://github.com/jorgenschaefer/elpy
  ;; @see: https://youtu.be/0kuCeS-mfyc
  
  (defvar elpy-mode-map
    (let ((map (make-sparse-keymap)))
      ;; Alphabetical order to make it easier to find free C-c C-X
      ;; bindings in the future. Heh.

      ;; (define-key map (kbd "<backspace>") 'python-indent-dedent-line-backspace)
      ;; (define-key map (kbd "<backtab>")   'python-indent-dedent-line)

      ;; (define-key map (kbd "C-M-x")   'python-shell-send-defun)
      
      (define-key map (kbd "M-c <")   'python-indent-shift-left)
      (define-key map (kbd "M-c >")   'python-indent-shift-right)
      
      (define-key map (kbd "M-c RET") 'elpy-importmagic-add-import)
      (define-key map (kbd "M-c M-b") 'elpy-nav-expand-to-indentation)
      (define-key map (kbd "M-c M-c") 'elpy-shell-send-region-or-buffer)
      (define-key map (kbd "M-c M-d") 'elpy-doc)
      (define-key map (kbd "M-c M-e") 'elpy-multiedit-python-symbol-at-point)
      (define-key map (kbd "M-c M-f") 'elpy-find-file)
      (define-key map (kbd "M-c M-n") 'elpy-flymake-next-error)
      (define-key map (kbd "M-c M-o") 'elpy-occur-definitions)
      (define-key map (kbd "M-c M-p") 'elpy-flymake-previous-error)
      (define-key map (kbd "M-c M-s") 'elpy-rgrep-symbol)
      (define-key map (kbd "M-c M-t") 'elpy-test)
      (define-key map (kbd "M-c M-v") 'elpy-check)
      (define-key map (kbd "M-c M-z") 'elpy-shell-switch-to-shell)
      (define-key map (kbd "M-c M-r i") 'elpy-importmagic-fixup)
      (define-key map (kbd "M-c M-r p") 'elpy-autopep8-fix-code)
      (define-key map (kbd "M-c M-r r") 'elpy-refactor)

      ;; (define-key map (kbd "<S-return>") 'elpy-open-and-indent-line-below)
      ;; (define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-above)

      ;; (define-key map (kbd "<C-return>") 'elpy-shell-send-current-statement)

      ;; (define-key map (kbd "<C-down>") 'elpy-nav-forward-block)
      ;; (define-key map (kbd "<C-up>") 'elpy-nav-backward-block)
      ;; (define-key map (kbd "<C-left>") 'elpy-nav-backward-indent)
      ;; (define-key map (kbd "<C-right>") 'elpy-nav-forward-indent)

      ;; (define-key map (kbd "<M-down>") 'elpy-nav-move-line-or-region-down)
      ;; (define-key map (kbd "<M-up>") 'elpy-nav-move-line-or-region-up)
      ;; (define-key map (kbd "<M-left>") 'elpy-nav-indent-shift-left)
      ;; (define-key map (kbd "<M-right>") 'elpy-nav-indent-shift-right)

      ;; (define-key map (kbd "M-.")     'elpy-goto-definition)
      ;; (define-key map (kbd "M-TAB")   'elpy-company-backend)
    
      (define-key map (kbd "<S-return>") 'elpy-open-and-indent-line-below)
      (define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-above)

      (define-key map (kbd "<C-return>") 'elpy-shell-send-current-statement)

      (define-key map (kbd "<M-right>") 'elpy-nav-forward-block)
      (define-key map (kbd "<M-left>") 'elpy-nav-backward-block)
      ;; (define-key map (kbd "<C-S-left>") 'elpy-nav-backward-indent)
      ;; (define-key map (kbd "<C-S-right>") 'elpy-nav-forward-indent)

      ;; (define-key map (kbd "<M-S-down>") 'elpy-nav-move-line-or-region-down)
      ;; (define-key map (kbd "<M-S-up>") 'elpy-nav-move-line-or-region-up)
      (define-key map (kbd "<M-S-left>") 'elpy-nav-indent-shift-left)
      (define-key map (kbd "<M-S-right>") 'elpy-nav-indent-shift-right)

      (define-key map [(meta prior)]    'elpy-goto-definition)
      (define-key map [(meta next)]     'pop-tag-mark)
      
      (define-key map [(control menu)]   'elpy-company-backend)

      map)
    "Key map for the Emacs Lisp Python Environment.")
  :config
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt")
  ;; (elpy-use-ipython "ipython3") 
  (defalias 'workon 'pyvenv-workon))

(use-package ein
  :unless (version< emacs-version "25.1")
  :defer t
  ;; :ensure t
  
  :config
  (defalias 'eip 'ein:notebooklist-open))


(use-package pipenv
  :unless (version< emacs-version "25.1")
  :defer t
  ;; :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; (use-package python-mode
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :config
;;   (defvar python-mode-initialized nil)
;;   (defun my-python-mode-hook ()
;;     (unless python-mode-initialized
;;       (setq python-mode-initialized t)
;;       (info-lookup-add-help
;;        :mode 'python-mode
;;        :regexp "[a-zA-Z_0-9.]+"
;;        :doc-spec
;;        '(("(python)Python Module Index" )
;;          ("(python)Index"
;;           (lambda
;;             (item)
;;             (cond
;;              ((string-match
;;                "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
;;               (format "%s.%s" (match-string 2 item)
;;                       (match-string 1 item)))))))))
;;     (setq indicate-empty-lines t)
;;     (set (make-local-variable 'parens-require-spaces) nil)
;;     (setq indent-tabs-mode nil)
;;     (bind-key "C-c C-z" 'python-shell python-mode-map)
;;     (unbind-key "C-c c" python-mode-map))
;;   (add-hook 'python-mode-hook 'my-python-mode-hook))



;; ---( ruby )--------------------------------------------------------------

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  ;; :functions inf-ruby-keys
  ;; :config
  ;; (use-package yari
  ;;   :init
  ;;   (progn
  ;;     (defvar yari-helm-source-ri-pages
  ;;       '((name . "RI documentation")
  ;;         (candidates . (lambda () (yari-ruby-obarray)))
  ;;         (action ("Show with Yari" . yari))
  ;;         (candidate-number-limit . 300)
  ;;         (requires-pattern . 2)
  ;;         "Source for completing RI documentation."))
  ;;     (defun helm-yari (&optional rehash)
  ;;       (interactive (list current-prefix-arg))
  ;;       (when current-prefix-arg (yari-ruby-obarray rehash))
  ;;       (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))
  ;; (defun my-ruby-smart-return ()
  ;;   (interactive)
  ;;   (when (memq (char-after) '(?\| ?\" ?\'))
  ;;     (forward-char))
  ;;   (call-interactively 'newline-and-indent))
  ;; (defun my-ruby-mode-hook ()
  ;;   (require 'inf-ruby)
  ;;   (inf-ruby-keys)
  ;;   (bind-key "<return>" 'my-ruby-smart-return ruby-mode-map)
  ;;   (bind-key "C-h C-i" 'helm-yari ruby-mode-map))
  ;; (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
  )



;; ---( puppet )--------------------------------------------------------------

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp$" . puppet-mode)
  ;; :config
  ;; (use-package puppet-ext
  ;;   :ensure t)
  )



;; ---( scala )--------------------------------------------------------------

;; @see: https://gitlab.com/balajisi/emacs/blob/master/init.el


;; requires: sbt-plugin
;;
;; cat > ~/.sbt/0.13/plugins/plugin.sbt <<EOF
;;
;; resolvers += Resolver.sonatypeRepo("snapshots")
;; addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5-SNAPSHOT")
;;
;; EOF
;;
;; and sbt gen-ensime to generate .ensime config in project root
;;

(use-package ensime
  :disabled t
  :pin melpa)

;;(use-package ensime
;;  :pin melpa-stable)




(use-package scala-mode2
  :disabled t
  :defer t
  :init
  (progn
    (use-package ensime
      :ensure
      :commands ensime-scala-mode-hook     
      ;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
      :init
      (progn
        (add-hook 'scala-mode-hook
                  (lambda ()
                    (ensime)
                    (ensime-scala-mode)
                    ))
        )
      :config
            (progn
              (define-key ensime-mode-map (kbd "M-c M-c")    'ensime-inf-eval-region)
              (define-key ensime-mode-map (kbd "<C-return>") 'ensime-inf-eval-region)
              ))
    (use-package sbt-mode
      :ensure)))


;; ---( haskell )--------------------------------------------------------------

;; @see: https://gitlab.com/balajisi/emacs/blob/master/init.el
;; @see: https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;;;; Haskell Modes - Haskell, GHC, SHM, Idris etc.
(use-package haskell-mode
  :ensure

  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))

  :config
  (use-package ghc
    :ensure)
  (use-package flycheck-haskell
    :ensure)
  )

(defun balaji/haskell-mode-hook ()
  (turn-on-haskell-indentation)
  (ghc-init)
  (lambda () (add-to-list 'ac-sources 'ac-source-ghc))
  )

(add-hook 'haskell-mode-hook 'balaji/haskell-mode-hook)

(use-package idris-mode
  :ensure
  :disabled t)

;; ---( drools )--------------------------------------------------------------

;; @see: https://github.com/pdorrell/rules-editing-mode
;; @see: https://github.com/pdorrell/rules-editing-mode/blob/master/my-drools.el

;;;; Drools Mode


(autoload 'drools-mode "drools-mode")

(defun set-extension-mode (extension mode)
  (setq auto-mode-alist
	(cons (cons (concat "\\" extension "\\'") mode)
	      auto-mode-alist) ) )

(set-extension-mode ".drl" 'drools-mode)
(set-extension-mode ".dslr" 'drools-mode)

(add-hook 'drools-mode-hook 'my-drools-hook)

(defun drools-return-and-indent()
  (interactive)
  (newline) (indent-for-tab-command) )

(defun my-drools-hook ()
  (setq indent-tabs-mode nil)
(local-set-key [?\C-m] 'drools-return-and-indent) )



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @VM
;; ;;;////////////////////////////////////////////////////////////////


;; ---( docker )--------------------------------------------------------------


(use-package docker
  :ensure t
  :defer t)

(use-package docker-tramp
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @TEX
;; ;;;////////////////////////////////////////////////////////////////


;; ---( LaTeX )--------------------------------------------------------------

;; @see: https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org

;;   :ensure t

(use-package auctex
  :defer t)

(use-package auctex-latexmk
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))

(use-package cdlatex
  :ensure t
  :defer t)

(use-package company-auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

(use-package tex
  :defer t
  :init
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
        TeX-PDF-mode t
        ;; Synctex support
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server nil
        ;; Setup reftex style (RefTeX is supported through extension)
        reftex-use-fonts t
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)
  (defvar latex-nofill-env '("equation"
                             "equation*"
                             "align"
                             "align*"
                             "tabular"
                             "tikzpicture")
    "List of environment names in which `auto-fill-mode' will be inhibited.")
  (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'latex-math-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;; (add-hook 'LaTeX-mode-hook 'my/latex-mode-defaults)

  :config
  ;; (defun my/latex-mode-defaults ()
  ;;   (visual-line-mode +1)
  ;;   (yas-minor-mode -1))

  (defun latex//autofill ()
    "Check whether the pointer is ucrrently inside on the
environments described in `latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
    (let ((do-auto-fill t)
          (current-environment "")
          (level 0))
      (while (and do-auto-fill (not (string= current-environment "document")))
        (setq level (1+ level)
              current-environment (LaTeX-current-environment level)
              do-auto-fill (not (member current-environment latex-nofill-env))))
      (when do-auto-fill
        (do-auto-fill))))

  (defun latex/auto-fill-mode ()
    "Toggle uato-fill-mode using the custom auto-fill function."
    (interactive)
    (auto-fill-mode)
    (setq auto-fill-function 'latex//autofill))

  ;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  ;; (add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . tex-mode))

  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection
          '((output-dvi "DVI Viewer")
            (output-pdf "PDF Viewer")
            (output-html "HTML Viewer")))

    (setq TeX-view-program-list
          '(("DVI Viewer" "open %o")
            ("PDF Viewer" "open %o")
            ("HTML Viewer" "open %o")))))



;; (use-package tex-site
;;   :disabled t
;;   :load-path "site-lisp/auctex/preview/"
;;   :defines (latex-help-cmd-alist latex-help-file)
;;   :mode ("\\.tex\\'" . TeX-latex-mode)
;;   :config
;;   (defun latex-help-get-cmd-alist () ;corrected version:
;;     "Scoop up the commands in the index of the latex info manual.
;; The values are saved in `latex-help-cmd-alist' for speed."
;;     ;; mm, does it contain any cached entries
;;     (if (not (assoc "\\begin" latex-help-cmd-alist))
;;         (save-window-excursion
;;           (setq latex-help-cmd-alist nil)
;;           (Info-goto-node (concat latex-help-file "Command Index"))
;;           (goto-char (point-max))
;;           (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
;;             (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
;;                   (value (buffer-substring (match-beginning 2)
;;                                            (match-end 2))))
;;               (add-to-list 'latex-help-cmd-alist (cons key value))))))
;;     latex-help-cmd-alist)


;;   (use-package latex-mode
;;     :defer t
;;     :config
;;     (progn
;;       (use-package preview)
;;       (use-package ac-math)
;;       (defun ac-latex-mode-setup ()
;;         (nconc ac-sources
;;                '(ac-source-math-unicode ac-source-math-latex
;;                                         ac-source-latex-commands)))
;;       (add-to-list 'ac-modes 'latex-mode)
;;       (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
;;       (info-lookup-add-help :mode 'latex-mode
;;                             :regexp ".*"
;;                             :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
;;                             :doc-spec '(("(latex2e)Concept Index" )
;;                                         ("(latex2e)Command Index")))))
;;   )


;; ---( pdf )--------------------------------------------------------------

(use-package pdf-tools
  :ensure f
  ;; :config
  ;; (pdf-tools-install)
)


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @FONT
;; ;;;////////////////////////////////////////////////////////////////


;; ---( ligatures )--------------------------------------------------------------

(cond
 ((string-lessp emacs-version "27.1") ;;
  (progn
    (message "SITE:font-legacy, ...")
    (message "SITE:font-legacy.")
    ))
 (t
  (progn
    (message "SITE:font-ligatures, ...")


(use-package fira-code-mode
  :ensure t
  :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
  :hook prog-mode)                                         ; mode to enable fira-code-mode in

    
    (message "SITE:font-ligatures.")
    ))
)




;; @see: https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632

;; (use-package ligature
;;   ;;:load-path "path-to-ligature-repo"
;;   :config
;;   ;; Enable the "www" ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable traditional ligature support in eww-mode, if the
;;   ;; `variable-pitch' face supports it
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\" "://"))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @ORG
;; ;;;////////////////////////////////////////////////////////////////


;; ---(org-ref)------------------------------------------------------------------------

(use-package org-ref
  :after org
  :disabled t
;;  :ensure t
  :init
  (setq reftex-default-bibliography '("~/Dropbox/Local/data/org/ref/references.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/Local/data/org/ref/notes.org"
        org-ref-default-bibliography '("~/Dropbox/Local/data/org/ref/references.bib")
        org-ref-pdf-directory "~/Dropbox/Local/docs/papers/")

  (setq helm-bibtex-bibliography "~/Dropbox/Local/data/org/ref/references.bib")
  (setq helm-bibtex-library-path "~/Dropbox/Local/docs/papers/")

  (setq helm-bibtex-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath)))

  (setq helm-bibtex-notes-path "~/Dropbox/Local/data/org/ref/notes.org")
  :config
  (key-chord-define-global "uu" 'org-ref-cite-hydra/body)
  ;; variables that control bibtex key format for auto-generation
  ;; I want firstauthor-year-title-words
  ;; this usually makes a legitimate filename to store pdfs under.
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

(use-package org-autolist
  :after org
  :ensure t
  :config
  (org-autolist-mode +1))

(use-package doi-utils
  :after org
  :disabled t
;;  :ensure t
  )

(use-package org-ref-bibtex
  :after org
  :disabled t
;;  :ensure t
  :init
  (setq org-ref-bibtex-hydra-key-binding "\C-cj"))

;; ---( org-mode )--------------------------------------------------------------

;; @see: https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org


(use-package org
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (require 'ox-md)
  (unbind-key "C-c ;" org-mode-map)

  ;;file to save todo items
  (setq org-agenda-files (quote ("~/Dropbox/Local/data/org/all/todo.org")))


  ;;set priority range from A to C with default A
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)


  ;;set colours for priorities
  (setq org-priority-faces '((?A . (:foreground "OliveDrab" :weight bold))
                             (?B . (:foreground "LightSteelBlue"))
                             (?C . (:foreground "#F0DFAF"))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-mode agenda options                                                ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)

  ;;don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;;don't give awarning colour to tasks with impending deadlines
  ;;if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  ;;don't show tasks that are scheduled or have deadlines in the
  ;;normal todo list
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))

  ;;sort tasks in order of when they are due and then by priority

  (setq org-agenda-sorting-strategy
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))))

  (setq org-capture-templates
        '(("t" "todo" entry (file+headline "~/Dropbox/Local/data/org/all/todo.org" "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))


  (defun my/org-mode-defaults ()
    ;; (turn-on-org-cdlatex)
    ;; (diminish 'org-cdlatex-mode "")
    (turn-on-auto-fill)

    ;; make `company-backends' local is critcal
    ;; or else, you will have completion in every major mode, that's very annoying!
    (make-local-variable 'company-backends)
    ;; company-ispell is the plugin to complete words
    (add-to-list 'company-backends 'company-ispell))

  (add-hook 'org-mode-hook 'my/org-mode-defaults)

  ;; Fontify org-mode code blocks
  (setq org-src-fontify-natively t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "|" "CANCELLED(c@/!)" "DONE(d)"))))

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "green" :weight bold))
          ("NEXT" :foreground "blue" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  (setq org-enforce-todo-dependencies t)
  (setq org-src-tab-acts-natively t)

  (setq org-latex-pdf-process
        (quote ("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                "bibtex $(basename %b)"
                "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))

  (setq org-latex-create-formula-image-program 'imagemagick)

  ;; Tell the latex export to use the minted package for source
  ;; code coloration.
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (require 'ox-latex)
  (setq org-latex-listings 'minted)

  ;; (setq org-latex-minted-options
  ;;       '(("frame" "lines") ("framesep" "6pt")
  ;;         ("mathescape" "true") ("fontsize" "\\small")))

  (setq org-confirm-babel-evaluate nil)

  ;; execute external programs.
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (ditaa . t)
           (python . t)
           ;; (ruby . t)
           ;; (R . t)           
           (gnuplot . t)
           ;; (clojure . t)
           (shell . t)
           ;; (haskell . t)
           (octave . t)
           (org . t)
           (plantuml . t)
           ;; (scala . t)
           (sql . t)
           (latex . t))))

  (eval-after-load 'org-src
    '(define-key org-src-mode-map
       "\C-x\C-s" #'org-edit-src-exit)))



;; ---( org-mode-v1 )--------------------------------------------------------------

;; ;; @see: https://github.com/anschwa/emacs.d

;; ;; (use-package async
;; ;;   :ensure t
;; ;;   :demand
;; ;;   :init (setq async-bytecomp-allowed-packages '(all))
;; ;;   :config (async-bytecomp-package-mode 1))


;; (use-package org
;;   :defer
;;   :ensure org-plus-contrib
;;   :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
;;   :config
;;   (require 'ob)
;;   ;; (require 'ob-async)
;;   (require 'ob-python)
;;   ;; (require 'ob-clojure)
;;   ;; (require 'ob-perl)
;;   ;; (require 'ob-dot)
;;   ;; (require 'ob-R)
;;   ;; (require 'ob-gnuplot)
;;   ;; (require 'ob-lisp)
;;   (require 'ob-org)
;;   ;; (require 'ob-screen)
;;   ;; (require 'ob-calc)
;;   ;; (require 'ob-js)
;;   ;; (require 'ob-latex)
;;   ;; (require 'ob-plantuml)
;;   (require 'ob-shell)
;;   ;; (require 'ob-ditaa)
;;   ;; (require 'ob-awk)
;;   ;; (require 'ob-octave)
;;   ;; (require 'ob-sed)
;;   ;; (require 'ob-sql)
;;   ;; (require 'ob-sqlite)

;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '(
;;       ;; (perl . t)
;;       ;; (dot . t)
;;       ;; (R . t)
;;       ;; (gnuplot . t)
;;       ;; (clojure . t)
;;       ;; (graphviz . t)
;;       ;; (lisp . t)
;;       ;; (stan . t)
;;       (org . t)
;;       ;; (screen . t)
;;       ;; (calc . t)
;;       ;; (js . t)
;;       ;; (latex . t)
;;       ;; (plantuml . t)
;;       ;; (ruby . t)
;;       (shell . t)
;;       (python . t)
;;       (emacs-lisp . t)
;;       ;; (ditaa . t)
;;       ;; (awk . t)
;;       ;; (octave . t)
;;       ;; (sed . t)
;;       ;; (sql . t)
;;       ;; (sqlite . t)
;;       ))
;;   )
  
;;   ;; (setq org-confirm-babel-evaluate nil
;;   ;;       org-export-babel-evaluate 'inline-only)
;;   ;; (org-babel-do-load-languages
;;   ;;  'org-babel-load-languages
;;   ;;  '((emacs-lisp . t)
;;   ;;    (org . t)
;;   ;;    (shell . t)
;;   ;;    (makefile . t)
;;   ;;    (latex . t)
;;   ;;    (fortran . t)
;;   ;;    (gnuplot . t)
;;   ;;    (python . t))))

;; (use-package ob-http
;;   :after org
;;   :ensure t
;;   :config
;;   (add-to-list 'org-babel-load-languages '(http . t))
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages org-babel-load-languages))



;; ---(org-misc)------------------------------------------------------------------------

;; @see: https://github.com/anschwa/emacs.d

;; (use-package ob-core)
;; ;;(use-package ob-R)
;; ;;(use-package ob-http)
;; ;;(use-package ob-restclient)
;; (use-package ox-md)
;; (use-package ox-man)
;; (use-package ox-latex)
;; (use-package ox-beamer)


;; (use-package org
;;   :ensure t
;;   :defer 30
;;   :init
;;   (progn
;;     ;; Fontify org-mode code blocks
;;     (setq org-src-fontify-natively t)

;;     ;; Essential Settings
;;     (setq org-log-done 'time)
;;     (setq org-html-doctype "html5")
;;     (setq org-export-headline-levels 6)
;;     (setq org-export-with-smart-quotes t)

;;     ;; Configure Mobile Org
;;     ;; Set to the location of your Org files on your local system
;;     ; (setq org-directory "~/Dropbox/Development/Org")
;;     ;; Set to <your Dropbox root directory>/MobileOrg.
;;     ; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;;     ;; Set to the name of the file where new notes will be stored
;;     ; (setq org-mobile-inbox-for-pull "~/Dropbox/Development/Org/inbox.org")

;;     ;; Custom TODO keywords
;;     (setq org-todo-keywords
;;           '((sequence "TODO(t)" "NOW(n@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;;     ;; Set up latex
;;     (setq org-export-with-LaTeX-fragments t)
;;     (setq org-latex-create-formula-image-program 'imagemagick)

;;     ;; Tell the latex export to use the minted package for source
;;     ;; code coloration.
;;     ; (setq org-latex-listings 'minted)

;;     ;; Add minted to the defaults packages to include when exporting.
;;     ; (add-to-list 'org-latex-packages-alist '("" "minted"))

;;     ;; local variable for keeping track of pdf-process options
;;     (setq pdf-processp nil))
;;   :config
;;   (progn
;;     ;; Unbind from org-mode only
;;     (unbind-key "<C-S-up>" org-mode-map)
;;     (unbind-key "<C-S-down>" org-mode-map)
;;     ;; Bind new keys to org-mode only
;;     (bind-key "<s-up>" 'org-metaup org-mode-map)
;;     (bind-key "<s-down>" 'org-metadown org-mode-map)
;;     (bind-key "<s-left>" 'org-promote-subtree org-mode-map)
;;     (bind-key "<s-right>" 'org-demote-subtree org-mode-map)

;;     ;; Let the exporter use the -shell-escape option to let latex
;;     ;; execute external programs.
;;     (defun toggle-org-latex-pdf-process ()
;;       "Change org-latex-pdf-process variable.

;;       Toggle from using latexmk or pdflatex. LaTeX-Mk handles BibTeX,
;;       but opens a new PDF every-time."
;;       (interactive)
;;       (if pdf-processp
;;           ;; LaTeX-Mk for BibTex
;;           (progn
;;             (setq pdf-processp nil)
;;             (setq org-latex-pdf-process
;;                   '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f' -gg -pdf -bibtex-cond -f %f"))
;;             (message "org-latex-pdf-process: latexmk"))
;;         ;; Plain LaTeX export
;;         (progn
;;           (setq pdf-processp t)
;;           (setq org-latex-pdf-process
;;                 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;           (message "org-latex-pdf-process: xelatex"))))

;;     ;; Call toggle-org-latex-pdf-process
;;     (toggle-org-latex-pdf-process)

;;     ;; Set up babel source-block execution
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      '((emacs-lisp . t)
;; ;;     (R . t)
;;        (python . t)
;;        (haskell . t)
;;        (sh . t)
;;        (scheme . t)
;;        (ledger . t)                     ; for finances
;;        (C . t)
;; ;;     (http . t)
;;        ))

;;     ;; Prevent Weird LaTeX class issue
;;     (unless (boundp 'org-latex-classes)
;;       (setq org-latex-classes nil))
;;     (add-to-list 'org-latex-classes
;;                  '("per-file-class"
;;                    "\\documentclass{article}
;;                         [NO-DEFAULT-PACKAGES]
;;                         [EXTRA]"))

;;     (defun myorg-update-parent-cookie ()
;;       (when (equal major-mode 'org-mode)
;;         (save-excursion
;;           (ignore-errors
;;             (org-back-to-heading)
;;             (org-update-parent-todo-statistics)))))

;;     (defadvice org-kill-line (after fix-cookies activate)
;;       (myorg-update-parent-cookie))

;;     (defadvice kill-whole-line (after fix-cookies activate)
;;       (myorg-update-parent-cookie))))

;; (use-package org
;;   :ensure t
;;   :defer 30
;;   )


;;(use-package dot-org
;;  :ensure t
;;  :defer 30)

;; (use-package dot-org
;;   :commands my-org-startup
;;   :bind (("M-C" . jump-to-org-agenda)
;;          ("M-m" . org-smart-capture)
;;          ("M-M" . org-inline-note)
;;          ("C-c a" . org-agenda)
;;          ("C-c S" . org-store-link)
;;          ("C-c l" . org-insert-link)
;;          ("C-. n" . org-velocity-read))
;;   :defer 30
;;   :config
;;   (when (not running-alternate-emacs)
;;     (run-with-idle-timer 300 t 'jump-to-org-agenda)
;;     (my-org-startup))
;;   (bind-key "<tab>" 'smart-tab org-mode-map))




;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @UTIL
;; ;;;////////////////////////////////////////////////////////////////


;; ---( regex )--------------------------------------------------------------


(use-package regex-tool
  :ensure t
  :defer t)


;; ---( guide-key )--------------------------------------------------------------

;; (use-package guide-key
;;   :ensure t
;;   :defer t
;;   :diminish guide-key-mode
;;   :idle
;;   (progn
;;     (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
;;     (guide-key-mode 1)))


;; ---( windmove )--------------------------------------------------------------

;; (use-package windmove
;;   :ensure t
;;   :defer t
;;   :bind
;;   (("<f2> <right>" . windmove-right)
;;    ("<f2> <left>" . windmove-left)
;;    ("<f2> <up>" . windmove-up)
;;    ("<f2> <down>" . windmove-down)
;;    ))


;; ---( whitespace )--------------------------------------------------------------

;; (use-package whitespace
;;   :ensure t
;;   :bind (("C-c T w" . whitespace-mode))
;;   :init
;;   (dolist (hook '(conf-mode-hook))
;; ;;  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
;;     (add-hook hook #'whitespace-mode))
;;   :config (setq whitespace-line-column nil)
;;   :diminish whitespace-mode)

;; (use-package whitespace
;;   :diminish (global-whitespace-mode
;;              whitespace-mode
;;              whitespace-newline-mode)
;;   :commands (whitespace-buffer
;;              whitespace-cleanup
;;              whitespace-mode)
;;   :defines (whitespace-auto-cleanup
;;             whitespace-rescan-timer-time
;;             whitespace-silent)
;;   :preface
;;   (defun normalize-file ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (whitespace-cleanup)
;;       (delete-trailing-whitespace)
;;       (goto-char (point-max))
;;       (delete-blank-lines)
;;       (set-buffer-file-coding-system 'unix)
;;       (goto-char (point-min))
;;       (while (re-search-forward "\r$" nil t)
;;         (replace-match ""))
;;       (set-buffer-file-coding-system 'utf-8)
;;       (let ((require-final-newline t))
;;         (save-buffer))))
;;   (defun maybe-turn-on-whitespace ()
;;     "Depending on the file, maybe clean up whitespace."
;;     (let ((file (expand-file-name ".clean"))
;;           parent-dir)
;;       (while (and (not (file-exists-p file))
;;                   (progn
;;                     (setq parent-dir
;;                           (file-name-directory
;;                            (directory-file-name
;;                             (file-name-directory file))))
;;                     ;; Give up if we are already at the root dir.
;;                     (not (string= (file-name-directory file)
;;                                   parent-dir))))
;;         ;; Move up to the parent dir and try again.
;;         (setq file (expand-file-name ".clean" parent-dir)))
;;       ;; If we found a change log in a parent, use that.
;;       (when (and (file-exists-p file)
;;                  (not (file-exists-p ".noclean"))
;;                  (not (and buffer-file-name
;;                            (string-match "\\.texi\\'" buffer-file-name))))
;;         (add-hook 'write-contents-hooks
;;                   #'(lambda () (ignore (whitespace-cleanup))) nil t)
;;         (whitespace-cleanup))))
;;   :init
;;   (hook-into-modes 'whitespace-mode '(prog-mode-hook c-mode-common-hook))
;;   (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)
;;   :config
;;   (remove-hook 'find-file-hooks 'whitespace-buffer)
;;   (remove-hook 'kill-buffer-hook 'whitespace-buffer)
;;   ;; For some reason, having these in settings.el gets ignored if whitespace
;;   ;; loads lazily.
;;   (setq whitespace-auto-cleanup t
;;         whitespace-line-column 80
;;         whitespace-rescan-timer-time nil
;;         whitespace-silent t
;;         whitespace-style '(face trailing lines space-before-tab empty)))

;; ---( autorevert )--------------------------------------------------------------

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))


;; ---( hilit-chg )--------------------------------------------------------------

;; (use-package hilit-chg
;;   :ensure t
;;   :bind ("M-o C" . highlight-changes-mode))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @SERVER
;; ;;;////////////////////////////////////////////////////////////////


;; ---( server )--------------------------------------------------------------

;;
;; @see: http://babbagefiles.blogspot.it/2017/03/take-elfeed-everywhere-mobile-rss.html
;;
;;  ~/.config/systemd/user/emacs.service
;;
;; ------------------------------------------------------------------
;; [Unit]
;; Description=Emacs: the extensible, self-documenting text editor
;;
;; [Service]
;; Type=forking
;; ExecStart=/usr/bin/emacs --daemon
;; ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
;; Restart=always
;;
;; [Install]
;; WantedBy=default.target
;; ------------------------------------------------------------------
;;
;; systemctl --user enable --now emacs
;; loginctl enable-linger USERNAME
;;

(use-package edit-server
  :ensure t
  :if window-system
;;  :load-path "site-lisp/emacs_chrome/servers/"
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @SHELL
;; ;;;////////////////////////////////////////////////////////////////


;; ---( eshell )--------------------------------------------------------------

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return] 'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace] 'eshell-isearch-delete-char)
      (define-key map [delete] 'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (add-hook 'eshell-mode-hook
            '(lambda()
               (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))
    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)
    (defun ss (server)
      (interactive "sServer: ")
      (call-process "spawn" nil nil nil "ss" server))
    (eval-after-load "em-unix"
      '(progn
         (unintern 'eshell/su nil)
         (unintern 'eshell/sudo nil))))
  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
  (use-package esh-toggle
    :bind ("C-x C-z" . eshell-toggle)))


;; ---( multi-term )--------------------------------------------------------------

(use-package multi-term
  :disabled t
  :bind (("C-. t" . multi-term-next)
         ("C-. T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer)))
  :config
  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)
  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))
  (require 'term)
  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map "\177" 'term-pager-back-page)))

;; ---( vterm )--------------------------------------------------------------

(cond
 ((string-lessp emacs-version "27.1") ;;
  (progn
    (message "SITE:term-legacy, ...")
    (setq h7/term-vterm-enabled nil)
    (message "SITE:term-legacy.")
    ))
 (t
  (progn
    (message "SITE:term-libvterm, ...")

(use-package vterm
  :bind (("C-v" . vterm-yank))
         ;; ("[kp-enter]" . vterm-yank)
         ;; ("[kp-divide]" . vterm-yank-pop)
         ;; ("[kp-multiply]" . vterm-copy-mode))
  :defer
  :ensure t)

;; @see: https://lupan.pl/dotemacs/
;; (use-package vterm-toggle
;;   :bind (("H-z" . vterm-toggle)
;;          ("H-F" . vterm-toggle-forward)
;;          ("H-B" . vterm-toggle-backward)))
    
(setq h7/term-vterm-enabled t)

    (message "SITE:term-libvterm.")
    ))
)




;; ---( sh-script )--------------------------------------------------------------

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))
  (add-hook 'shell-mode-hook 'initialize-sh-script))

;; ---( sh-toggle )--------------------------------------------------------------

(use-package sh-toggle
  :disabled t
  :bind ("C-. C-z" . shell-toggle)
  )


;; ---( sunrise-commander )--------------------------------------------------------------


(use-package sunrise-commander
  :bind (("C-c j" . my-activate-sunrise)
	 ("C-c C-j" . sunrise-cd))
  :commands sunrise
  :defines sr-tabs-mode-map
  :preface
  (defun my-activate-sunrise ()
    (interactive)
    (let ((sunrise-exists
	   (loop for buf in (buffer-list)
		 when (string-match " (Sunrise)$" (buffer-name buf))
		 return buf)))
      (if sunrise-exists
	  (call-interactively 'sunrise)
	(sunrise "~/dl/" "~/Archives/"))))
  :config
  (require 'sunrise-x-modeline)
  (require 'sunrise-x-tree)
  (require 'sunrise-x-tabs)
  (bind-key "/" 'sr-sticky-isearch-forward sr-mode-map)
  (bind-key "<backspace>" 'sr-scroll-quick-view-down sr-mode-map)
  (bind-key "C-x t" 'sr-toggle-truncate-lines sr-mode-map)
  (bind-key "q" 'sr-history-prev sr-mode-map)
  (bind-key "z" 'sr-quit sr-mode-map)
  (unbind-key "C-e" sr-mode-map)
  (unbind-key "C-p" sr-tabs-mode-map)
  (unbind-key "C-n" sr-tabs-mode-map)
  (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)
  (bind-key "M-[" 'sr-tabs-prev sr-tabs-mode-map)
  (bind-key "M-]" 'sr-tabs-next sr-tabs-mode-map)
  (defun sr-browse-file (&optional file)
    "Display the selected file with the default appication."
    (interactive)
    (setq file (or file (dired-get-filename)))
    (save-selected-window
      (sr-select-viewer-window)
      (let ((buff (current-buffer))
	    (fname (if (file-directory-p file)
		       file
		     (file-name-nondirectory file)))
	    (app (cond
		  ((eq system-type 'darwin) "open %s")
		  ((eq system-type 'windows-nt) "open %s")
		  (t "xdg-open %s"))))
	(start-process-shell-command "open" nil (format app file))
	(unless (eq buff (current-buffer))
	  (sr-scrollable-viewer (current-buffer)))
	(message "Opening \"%s\" ..." fname))))
  (defun sr-goto-dir (dir)
    "Change the current directory in the active pane to the given one."
    (interactive (list (progn
			 (require 'lusty-explorer)
			 (lusty-read-directory))))
    (if sr-goto-dir-function
	(funcall sr-goto-dir-function dir)
      (unless (and (eq major-mode 'sr-mode)
		   (sr-equal-dirs dir default-directory))
	(if (and sr-avfs-root
		 (null (posix-string-match "#" dir)))
	    (setq dir (replace-regexp-in-string
		       (expand-file-name sr-avfs-root) "" dir)))
	(sr-save-aspect
	 (sr-within dir (sr-alternate-buffer (dired dir))))
	(sr-history-push default-directory)
	(sr-beginning-of-buffer)))))


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @SEC3ET
;; ;;;////////////////////////////////////////////////////////////////


;; ---( pass )--------------------------------------------------------------

;; @see: https://gitea.petton.fr/nico/emacs.d/src/commit/8ae2b902c916600c9296d967f36ed69ad50e8199/init.el?lang=sv-SE

;; (use-package pass
;;   :mode ("org/reference/password-store/" . pass-view-mode)
;;   :bind ("C-x p" . pass))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @EVIL
;; ;;;////////////////////////////////////////////////////////////////


;; ---( evil )--------------------------------------------------------------

;; @see: https://raw.githubusercontent.com/noctuid/evil-guide/master/README.org

(use-package evil
  :disabled t
  :defer 30
  )


;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @JUMP
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( hydra )--------------------------------------------------------------

(use-package hydra
  :ensure t)


;; ;; ---( ace )--------------------------------------------------------------


(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))


;; ;; ---( avy )--------------------------------------------------------------

(use-package avy
  :ensure t)



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @COMPLETION
;; ;;;////////////////////////////////////////////////////////////////


;; ---( autocomplete )--------------------------------------------------------------

(use-package auto-complete
  :disabled t
  :diminish auto-complete-mode
  :init
  (use-package pos-tip)
  (require 'auto-complete-config)
  (ac-config-default)
  :config
  ;; @see: http://auto-complete.org/doc/manual.html
  ;;(ac-set-trigger-key "<backtab>")
  ;;(ac-set-trigger-key "TAB")
  (setq ac-ignore-case 'smart)
  (setq ac-auto-start nil)
  (setq ac-use-menu-map t)
  ;;(define-key ac-mode-map (kbd "M-SPC") 'auto-complete)
  (define-key ac-mode-map  [(control menu)] 'auto-complete)
  (ac-set-trigger-key "TAB")
  ;; (define-key ac-completing-map "\M-/" 'ac-stop)
  ;; (define-key ac-completing-map "\t" 'ac-complete)
  ;; (define-key ac-completing-map "\r" nil)
  ;; (setq ac-use-menu-map t)
  ;; (define-key ac-menu-map "\C-n" 'ac-next)
  ;; (define-key ac-menu-map "\C-p" 'ac-previous)
  ;; (setq ac-use-quick-help nil)
  ;; (setq ac-menu-height 20)
  ;; (setq ac-show-menu-immediately-on-auto-complete t)
  ;; (setq ac-auto-show-menu 0.8)
  ;; (setq ac-delay 0.4)
  
  ;; (setq-default ac-sources '(ac-source-filename
  ;;                            ac-source-functions
  ;;                            ac-source-yasnippet
  ;;                            ac-source-variables
  ;;                            ac-source-symbols
  ;;                            ac-source-features
  ;;                            ac-source-abbrev
  ;;                            ac-source-words-in-same-mode-buffers
  ;;                            ac-source-dictionary))
  
  ;; (defun ac-emacs-lisp-mode-setup ()
  ;;   (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers)))
  ;; (add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
  
  ;; (bind-key "A-M-?" 'ac-last-help)
  ;; (unbind-key "C-s" ac-completing-map)
  
  )


;; ---( company )--------------------------------------------------------------

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :bind ("<C-menu>" . company-complete)
  :init
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'lisp-interaction-mode-hook 'company-mode)
  (add-hook 'ielm-mode-hook 'company-mode)
  (add-hook 'json-mode-hook 'company-mode)
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  ;; (defadvice company-pseudo-tooltip-unless-just-one-frontend
  ;;     (around only-show-tooltip-when-invoked activate)
  ;;   (when (company-explicit-action-p)
  ;;     ad-do-it))
  (use-package helm-company
    :disabled t))


;; ---( yasnippet )--------------------------------------------------------------

(use-package yasnippet
  :disabled t
  :config
  (yas-reload-all))



;; ---( ido )--------------------------------------------------------------

(use-package ido
  :disabled t
  :defer 5
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window)
         ("M-x" . ido-hacks-execute-extended-command))
  :preface
  (eval-when-compile
    (defvar ido-require-match)
    (defvar ido-cur-item)
    (defvar ido-show-confirm-message)
    (defvar ido-selected)
    (defvar ido-final-text))
  (defun ido-smart-select-text ()
    "Select the current completed item. Do NOT descend into directories."
    (interactive)
    (when (and (or (not ido-require-match)
                   (if (memq ido-require-match
                             '(confirm confirm-after-completion))
                       (if (or (eq ido-cur-item 'dir)
                               (eq last-command this-command))
                           t
                         (setq ido-show-confirm-message t)
                         nil))
                   (ido-existing-item-p))
               (not ido-incomplete-regexp))
      (when ido-current-directory
        (setq ido-exit 'takeprompt)
        (unless (and ido-text (= 0 (length ido-text)))
          (let ((match (ido-name (car ido-matches))))
            (throw 'ido
                   (setq ido-selected
                         (if match
                             (replace-regexp-in-string "/\\'" "" match)
                           ido-text)
                         ido-text ido-selected
                         ido-final-text ido-text)))))
      (exit-minibuffer)))
  :config
  (ido-mode 'buffer)
  (use-package ido-hacks
    :config
    (ido-hacks-mode 1))
  (use-package ido-vertical-mode
    :disabled t
    :config
    (ido-vertical-mode 1))
  (use-package flx-ido
    :disabled t
    :config
    (flx-ido-mode 1))
  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" 'ido-smart-select-text
                          ido-file-completion-map))))

;; ---( helm )--------------------------------------------------------------

;; @see: https://pages.sachachua.com/.emacs.d/Sacha.html#org04e47b9

(message "#helm(0): '( (h7/use-helm . %s) )" (h7/use-helm)) 
(use-package helm
  :if (h7/use-helm)
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (message "#helm(1): '( (h7/use-helm . %s) )" (h7/use-helm)) 
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :if (h7/use-helm)
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))


;; ;; @see: https://lupan.pl/dotemacs/

;; (use-package helm
;;   :if (h7/use-helm)
;;   :ensure t
;;   :demand
;;   :init
;;   (setq helm-split-window-default-side 'other
;;         helm-split-window-inside-p t
;;         helm-swoop-split-with-multiple-windows t        
;;         helm-command-prefix-key "s-c")
;;   :config
;;   (require 'helm-config)              ; required to setup "s-c" keymap
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)
;;   ;; Only rebind M-x and C-x C-f on successful load of helm to remain
;;   ;; this basic operations if helm is not installed.
;;   (bind-key "M-x" #'helm-M-x)
;;   (bind-key "C-x C-f" #'helm-find-files)
;;   :bind
;;   (("M-y" . helm-show-kill-ring)
;;    ("C-c o" . helm-occur)
;;    ("C-x b" . helm-mini)
;;    ("C-x r b" . helm-bookmarks)
;;    ("C-h a" . helm-apropos)
;;    ("C-h d" . helm-info-at-point)
;;    ("C-c a" . helm-all-mark-rings)
;;    ("C-c h e" . helm-info-emacs)
;;    ("C-c h g" . helm-info-gnus)
;;    ("C-c R" . helm-register)
;;    ("s-P" . helm-run-external-command)
;;    ;; More key bindings in "s-c" keymap
;;    :map helm-find-files-map
;;    ("<backtab>" . helm-select-action)
;;    ("C-i" . helm-execute-persistent-action)))


;; (use-package helm
;;   :if (h7/use-helm)
;;   :ensure t
;; ;;  :demand t
;;   :bind ("M-x" . helm-M-x)
;;   :config
;;   (use-package helm-descbinds
;;     :config (helm-descbinds-mode))
;;   (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
;;   (helm-mode 1)
;;   )

;; (use-package helm
;;   :if (h7/use-helm)
;;   :ensure t
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




;; ;; ---( helm - alter )--------------------------------------------------------------

;; (use-package helm-mode
;;   :defer 15
;;   :commands helm--completing-read-default)
;; (use-package helm-grep
;;   :commands helm-do-grep-1
;;   :bind (("M-s f" . my-helm-do-grep-r)
;;          ("M-s g" . my-helm-do-grep))
;;   :preface
;;   (defun my-helm-do-grep ()
;;     (interactive)
;;     (helm-do-grep-1 (list default-directory)))
;;   (defun my-helm-do-grep-r ()
;;     (interactive)
;;     (helm-do-grep-1 (list default-directory) t)))
;; (use-package helm-swoop
;;   :bind (("M-s o" . helm-swoop)
;;          ("M-s /" . helm-multi-swoop)))
;; (use-package helm-descbinds
;;   :bind ("C-h b" . helm-descbinds)
;;   :init
;;   (fset 'describe-bindings 'helm-descbinds)
;;   :config
;;   (require 'helm-config))
;; (use-package helm-config
;;   :defer 10
;;   :bind (("C-c h" . helm-command-prefix)
;;          ("C-h a" . helm-apropos)
;;          ("C-h e a" . my-helm-apropos)
;;          ("C-x f" . helm-multi-files)
;;          ;; ("C-x C-f" . helm-find-files)
;;          ("M-s F" . helm-for-files)
;;          ("M-s b" . helm-occur)
;;          ("M-s n" . my-helm-find)
;;          )
;;   :preface
;;   (defun my-helm-find ()
;;     (interactive)
;;     (helm-find nil))
;;   :config
;;   (use-package helm-commands)
;;   (use-package helm-files)
;;   (use-package helm-buffers)
;;   (use-package helm-ls-git)
;;   (use-package helm-match-plugin)
;;   (helm-match-plugin-mode t)
;;   (helm-autoresize-mode t)
;;   (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
;;   (bind-key "C-i" 'helm-execute-persistent-action helm-map)
;;   (bind-key "C-z" 'helm-select-action helm-map)
;;   (bind-key "A-v" 'helm-previous-page helm-map)
;;   (when (executable-find "curl")
;;     (setq helm-google-suggest-use-curl-p t)))



;; ---( ivy )--------------------------------------------------------------

;; @see: https://blog.jft.rocks/emacs/helm-to-ivy.html
;; @see: https://truthseekers.io/lessons/how-to-use-ivy-swiper-counsel-in-emacs-for-noobs/
;; @see: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
;; @see: https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-ivy.el
;; @see: https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/

;; .........................................................................

;; :defer 0.1

;; (use-package ivy
;;   :if (h7/use-ivy)
;;   :ensure t
;;   :diminish
;;   :bind (("C-c C-r" . ivy-resume)
;;          ("C-x B" . ivy-switch-buffer-other-window))
;;   :custom
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-use-virtual-buffers t)
;;   :config (ivy-mode))

;; (use-package ivy-rich
;;   :if (h7/use-ivy)
;;   :after ivy
;;   :ensure t
;;   :custom
;;   (ivy-virtual-abbreviate 'full
;;                           ivy-rich-switch-buffer-align-virtual-buffer t
;;                           ivy-rich-path-style 'abbrev)
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                'ivy-rich-switch-buffer-transformer))

;; (use-package swiper
;;   :if (h7/use-ivy)
;;   :after ivy
;;   :bind (("C-s" . swiper)
;;          ("C-r" . swiper)))


;; .........................................................................
(message "#ivy(0): '( (h7/use-ivy . %s) )" (h7/use-ivy)) 
 
(use-package counsel
  :if (h7/use-ivy)
  :ensure t
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :if (h7/use-ivy)
  :ensure t
  :after ivy
  :bind (("C-s" . swiper))
  )

(use-package counsel-projectile
  :if (h7/use-ivy)
  :disabled t
;; :ensure t
  :after (counsel projectile))

(use-package ivy-hydra
  :if (h7/use-ivy)
  :ensure t
  :after (ivy hydra))

;; @see: https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-ivy.el
;; Time-stamp: <2018-12-05 12:06:58 kmodi>

;; Ivy (better than ido in my opinion)

(use-package ivy
  :if (h7/use-ivy)

  ;; (*) Error (use-package): ivy/:catch: Symbol’s value as variable is void: modi-mode-map
  
  ;;  :bind (:map modi-mode-map
  ;;         ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
  ;;         ("C-c w" . ivy-push-view) ;Push window configuration to `ivy-views'
  ;;         ("C-c W" . ivy-pop-view)) ;Remove window configuration from `ivy-views'
  
  :config
  (progn
    (message "#ivy(1): '( (h7/use-ivy . %s) )" (h7/use-ivy))
    
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))
    
     ;; Show recently killed buffers when calling `ivy-switch-buffer'
     (setq ivy-use-virtual-buffers t)
     (setq ivy-virtual-abbreviate 'full) ;Show the full virtual file paths
 
     (setq ivy-count-format "%d/%d ")
    
    ;; - (setq ivy-re-builders-alist '((t . ivy--regex-plus))) ;Default
    ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

    ;; Do not show "./" and "../" in the `counsel-find-file' completion list
    ;;(setq ivy-extra-directories nil)    ;Default value: ("../" "./")

     ;; https://github.com/abo-abo/swiper/blob/master/ivy-hydra.el
     (use-package ivy-hydra
       :if (h7/use-ivy)
       :ensure t
       :config
       (progn
         ;; Re-define the `hydra-ivy' defined in `ivy-hydra' package.
         (defhydra hydra-ivy (:hint nil
                              :color pink)
           "
  ^ _,_ ^      _f_ollow      occ_u_r      _g_o          ^^_c_alling: %-7s(if ivy-calling \"on\" \"off\")      _w_(prev)/_s_(next)/_a_(read) action: %-14s(ivy-action-name)
  _p_/_n_      _d_one        ^^           _i_nsert      ^^_m_atcher: %-7s(ivy--matcher-desc)^^^^^^^^^^^^      _C_ase-fold: %-10`ivy-case-fold-search
  ^ _._ ^      _D_o it!      ^^           _q_uit        _<_/_>_ shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^       _t_runcate: %-11`truncate-lines
 "
           ;; Arrows
           ("," ivy-beginning-of-buffer)      ;Default h
           ("p" ivy-previous-line)            ;Default j
           ("n" ivy-next-line)                ;Default k
           ("." ivy-end-of-buffer)            ;Default l
           ;; Quit ivy
           ("q" keyboard-escape-quit :exit t) ;Default o
           ("C-g" keyboard-escape-quit :exit t)
           ;; Quit hydra
           ("i" nil)
           ("C-o" nil)
           ;; actions
           ("f" ivy-alt-done :exit nil)
           ;; Exchange the default bindings for C-j and C-m
           ("C-m" ivy-alt-done :exit nil) ;RET, default C-j
           ("C-j" ivy-done :exit t)       ;Default C-m
           ("d" ivy-done :exit t)
           ("D" ivy-immediate-done :exit t)
           ("g" ivy-call)
           ("c" ivy-toggle-calling)
           ("m" ivy-rotate-preferred-builders)
           (">" ivy-minibuffer-grow)
           ("<" ivy-minibuffer-shrink)
           ("w" ivy-prev-action)
           ("s" ivy-next-action)
           ("a" ivy-read-action)
           ("t" (setq truncate-lines (not truncate-lines)))
           ("C" ivy-toggle-case-fold)
           ("u" ivy-occur :exit t)
           ("?" (ivy-exit-with-action    ;Default D
                 (lambda (_) (find-function #'hydra-ivy/body))) "Definition of this hydra" :exit t))
 
         (bind-keys
          :map ivy-minibuffer-map
          ("C-t" . ivy-rotate-preferred-builders)
          ("C-o" . hydra-ivy/body)
          ("M-o" . ivy-dispatching-done-hydra))))

     (bind-keys
      :map ivy-minibuffer-map
      ;; Exchange the default bindings for C-j and C-m
      ("C-m" . ivy-alt-done)             ;RET, default C-j
      ("C-j" . ivy-done)                 ;Default C-m
      ("C-S-m" . ivy-immediate-done))

     (bind-keys
      :map ivy-occur-mode-map
      ("n" . ivy-occur-next-line)
      ("p" . ivy-occur-previous-line)
      ("b" . backward-char)
      ("f" . forward-char)
      ("v" . ivy-occur-press)            ;Default f
      ("RET" . ivy-occur-press))

     (with-eval-after-load 'setup-windows-buffers
       (bind-keys
        :map ivy-minibuffer-map
        ("C-x k" . modi/kill-buffer-dwim) ;Aborts recursive edit
        ("C-)" . modi/kill-buffer-dwim))) ;Aborts recursive edit

     ;;(key-chord-define ivy-minibuffer-map "m," #'ivy-beginning-of-buffer)
     ;;(key-chord-define ivy-minibuffer-map ",." #'ivy-end-of-buffer)

     ;; Bind C-k to kill a buffer directly from the list shown on doing M-x ivy-switch-buffer.
     ;; https://github.com/abo-abo/swiper/issues/164
     (defun modi/ivy-kill-buffer ()
       (interactive)
       (ivy-set-action 'kill-buffer)
       (ivy-done))
     (bind-keys
      :map ivy-switch-buffer-map
      ("C-k" . modi/ivy-kill-buffer))
  ))

 ;; https://github.com/Yevgnen/ivy-rich
 ;; Richer "C-x b" buffer-switching Ivy interface.
 (use-package ivy-rich
   :if (h7/use-ivy)
   :after ivy
   :ensure t
   :config
   (progn
     (ivy-rich-mode)))


;; --- (provide 'setup-ivy)

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
;;
;; |----------------------------+----------------+------------------------------------------------------|
;; | Command                    | ivy map        | Function                                             |
;; |                            | Bindings       |                                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-done                   | C-j            | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-m) | Try to leave `ivy' as soon as possible.              |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-alt-done               | C-m or RET     | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-j) | When ARG is t, acts like `ivy-immediate-done'.       |
;; |                            |                | Try NOT to leave `ivy' at the soonest. For           |
;; |                            |                | instance, if a directory name completion is          |
;; |                            |                | possible, do that and list that directory's          |
;; |                            |                | content in `ivy' instead of opening that dir         |
;; |                            |                | in `dired'.                                          |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-immediate-done         | C-S-m          | Exit the minibuffer with the current text,           |
;; |                            |                | ignoring the candidates.                             |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-partial-or-done        | TAB            | Attempts partial completion, extending current line  |
;; |                            |                | input as much as possible. "TAB TAB" is the same as  |
;; |                            |                | `ivy-alt-done'.                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-call                   | C-M-m          | Call the current action without exiting completion.  |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-next-line-and-call     | C-M-n          | Move cursor vertically down ARG candidates.          |
;; |                            |                | Call the permanent action if possible.               |
;; | ivy-previous-line-and-call | C-M-p          | Move cursor vertically up ARG candidates.            |
;; |                            |                | Call the permanent action if possible.               |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-dispatching-done       | M-o            | Presents valid actions from which to choose. When    |
;; |                            |                | only one action is available, there is no difference |
;; |                            |                | between this and `ivy-done'.                         |
;; |----------------------------+----------------+------------------------------------------------------|

;; Switch to any of the saved `ivy-views' using `M-x ivy-switch-buffer'.
;; When `ivy-mode' is enabled, binding for `switch-to-buffer' is remapped to
;; `ivy-switch-buffer'.

;; .........................................................................

;; # (use-package counsel
;; #   :ensure t
;; #   :after ivy
;; #   :config (counsel-mode))
;; # 
;; # (use-package ivy
;; #   :ensure t
;; #   :defer 0.1
;; #   :diminish
;; #   :bind (("C-c C-r" . ivy-resume)
;; #          ("C-x B" . ivy-switch-buffer-other-window)
;; #          ;; :map ivy--minibuffer-map
;; #          ;; ("<left>" . ivy-backward-kill-work)
;; #          ;; ("<right>" . ivy-alt-done)
;; #          )
;; #   :custom
;; #   (ivy-count-format "(%d/d%)")
;; #   (ivy-use-virtual-buffers t)
;; #   (ivy-height 20)
;; #   :config
;; #   (ivy-mode)
;; #   )
;; # 
;; # (use-package ivy-rich
;; #   :ensure t
;; #   :after ivy
;; #  ;; :custom
;; #  ;;   (ivy-virtual-abbreviate 'full
;; #  ;;                           ivy-rich-switch-buffer-align-virtual-buffer t
;; #  ;;                           ivy-rich-path-style 'abbrev)
;; #   :config
;; #  ;;   (ivy-set-display-transformer 'ivy-switch-buffer
;; #  ;;                                'ivy-rich-switch-buffer-transformer)
;; #   (ivy-rich-mode)
;; #   )
;; # 
;; # (use-package swiper
;; #   :ensure t
;; #   :after ivy
;; #   :bind (("C-s" . swiper))
;; #   )
;; # 
;; # 
;; # (use-package counsel-projectile
;; #   :ensure t
;; #   :after (counsel projectile))
;; # 
;; # (use-package ivy-hydra
;; #   :ensure t
;; #   :after (ivy hydra))





;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @NET
;; ;;;////////////////////////////////////////////////////////////////


;; ---( restclient )------------------------------------------------------

;; @see: https://github.com/pashky/restclient.el

(use-package restclient
  :ensure t
  :defer 30
  :init
    (progn
      ;; (unless restclient-use-org
      ;;   (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
      ;; (spacemacs/set-leader-keys-for-major-mode 'restclient-mode
      ;;   "n" 'restclient-jump-next
      ;;   "p" 'restclient-jump-prev
      ;;   "s" 'restclient-http-send-current-stay-in-window
      ;;   "S" 'restclient-http-send-current
      ;;   "r" 'spacemacs/restclient-http-send-current-raw-stay-in-window
      ;;   "R" 'restclient-http-send-current-raw
      ;;   "y" 'restclient-copy-curl-command)
      ) 
  )


;; ---( ob-http )------------------------------------------------------

;; @see: https://github.com/zweifisch/ob-http
;; @see: https://emacs.stackexchange.com/questions/2427/how-to-test-rest-api-with-emacs

;; (use-package ob-http
;;   :ensure t
;;   :defer 30
;;   )





;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @INET
;; ;;;////////////////////////////////////////////////////////////////


;; ---( w3m )------------------------------------------------------

(use-package w3m
  :disabled t
  :commands (w3m-search w3m-find-file)
  :bind (("C-. u" . w3m-browse-url)
         ("C-. U" . w3m-browse-url-new-session)
         ("C-. A-u" . w3m-browse-chrome-url-new-session)
         ("C-. w" . show-browser)
         ("A-M-e" . goto-emacswiki)
         ("A-M-g" . w3m-search)
         ("A-M-w" . wikipedia-query))
  :init
  (setq w3m-command "w3m")
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)
  (autoload 'w3m-session-crash-recovery-remove "w3m-session")
  (defun show-browser ()
    (interactive)
    (let ((w3m-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*w3m" (buffer-name buf))
                   (throw 'found buf))))))
      (if w3m-buf
          (switch-to-buffer-other-window w3m-buf)
        (call-interactively 'w3m-find-file))))
  (defun wikipedia-query (term)
    (interactive (list (read-string "Wikipedia search: " (word-at-point))))
    (require 'w3m-search)
    (w3m-search "en.wikipedia" term))
  (eval-when-compile
    (autoload 'w3m-search-escape-query-string "w3m-search"))
  (defun wolfram-alpha-query (term)
    (interactive (list (read-string "Ask Wolfram Alpha: " (word-at-point))))
    (require 'w3m-search)
    (w3m-browse-url (concat "http://m.wolframalpha.com/input/?i="
                            (w3m-search-escape-query-string term))))
  (defun goto-emacswiki ()
    (interactive)
    (w3m-browse-url "http://www.emacswiki.org"))
  (defun w3m-browse-url-new-session (url)
    (interactive (progn
                   (require 'browse-url)
                   (browse-url-interactive-arg "Emacs-w3m URL: ")))
    (w3m-browse-url url t))
  (defun w3m-browse-chrome-url-new-session ()
    (interactive)
    (let ((url (do-applescript
                (string-to-multibyte "tell application \"Google Chrome\"
URL of active tab of front window
end tell"))))
      (w3m-browse-url (substring url 1 (1- (length url))) t)))
  :config
  (let (proxy-host proxy-port)
    (with-temp-buffer
      (shell-command "scutil --proxy" (current-buffer))
      (when (re-search-forward "HTTPPort : \\([0-9]+\\)" nil t)
        (setq proxy-port (match-string 1)))
      (when (re-search-forward "HTTPProxy : \\(\\S-+\\)" nil t)
        (setq proxy-host (match-string 1))))
    (if (and proxy-host proxy-port)
        (setq w3m-command-arguments
              (nconc w3m-command-arguments
                     (list "-o" (format "http_proxy=http://%s:%s/"
                                        proxy-host proxy-port)))))
    (use-package w3m-type-ahead
      :requires w3m
      :init
      (add-hook 'w3m-mode-hook 'w3m-type-ahead-mode))
    (add-hook 'w3m-display-hook
              (lambda (url)
                (let ((buffer-read-only nil))
                  (delete-trailing-whitespace))))
    (defun my-w3m-linknum-follow ()
      (interactive)
      (w3m-linknum-follow))
    (bind-key "k" 'w3m-delete-buffer w3m-mode-map)
    (bind-key "i" 'w3m-view-previous-page w3m-mode-map)
    (bind-key "p" 'w3m-previous-anchor w3m-mode-map)
    (bind-key "n" 'w3m-next-anchor w3m-mode-map)
    (defun dka-w3m-textarea-hook()
      (save-excursion
        (while (re-search-forward "\r\n" nil t)
          (replace-match "\n" nil nil))
        (delete-other-windows)))
    (add-hook 'w3m-form-input-textarea-mode-hook 'dka-w3m-textarea-hook)
    (bind-key "<return>" 'w3m-view-url-with-external-browser
              w3m-minor-mode-map)
    (bind-key "S-<return>" 'w3m-safe-view-this-url w3m-minor-mode-map)))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @NEWS
;; ;;;////////////////////////////////////////////////////////////////


;; ---( elfeed )--------------------------------------------------------------

;; @see: https://github.com/skeeto/elfeed

;; (use-package elfeed
;;   :ensure t
;;   :defer 30
;;   )

;; @see: http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
;; @see: https://github.com/danlamanna/.emacs.d/blob/master/init.el
;; @see: http://feedly.com/i/opml

(use-package elfeed-org
  :disabled t
;;:ensure t
  :config (progn
            (use-package elfeed
              :ensure t
              :config (progn
                        (custom-set-variables
                         ;; oldest articles should be at the top
                         '(elfeed-sort-order 'ascending))))

            (use-package elfeed-goodies
              :ensure t
              :config (progn
                        (elfeed-goodies/setup)))

            (setq rmh-elfeed-org-files (list "~/.rss/elfeed.org"))
            (elfeed-org)))
  

;; @see: https://github.com/algernon/elfeed-goodies
;; (use-package elfeed-goodies
;;   :ensure t
;;   :defer 30
;;   )

;; ---( GNus )--------------------------------------------------------------

;; @see: https://www.emacswiki.org/emacs/GnusRss



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @TWITTER
;; ;;;////////////////////////////////////////////////////////////////


;; ---( twittering-mode )--------------------------------------------------------------

(use-package twittering-mode
  :disabled t
  :defer 30
  )
;; (use-package twittering-mode
;;   :disabled t
;;   :commands twit
;;   :config
;;   (setq twittering-use-master-password t))


;; ;; ---( hackernews )------------------------------------------------------

;; (use-package hackernews
;;   :defer t
;;   )




;; ---( site.pkgs: end )-------------------------------------------------------
(message "SITE:PKGS - end")
