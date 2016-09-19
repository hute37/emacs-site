;; ---( site.pkgs: begin )-------------------------------------------------------
(message "SITE:PKGS - begin")

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @PACKAGES
;; ;;;////////////////////////////////////////////////////////////////

;; @see: https://github.com/jwiegley/use-package
;; @see: http://pages.sachachua.com/.emacs.d/Sacha.html
;; @see: https://github.com/bdd/.emacs.d/blob/master/packages.el
;; @see: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;; ---( Install )--------------------------------------------------------------

;;(fset 'h7/ensure 't)
;;(fset h7/ensure nil)

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

;;(require 'req-package)
;;(use-package req-package)

;; ---( ... )--------------------------------------------------------------

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @BASIC
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ... )--------------------------------------------------------------

(use-package bs
  :ensure t)

;; ---( ... )--------------------------------------------------------------

(use-package pretty-lambdada
  :ensure t
  :init (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook)))
  :config (dolist (global-pretty-lambda-mode)))
;;(use-package pretty-lambdada)

(use-package jumpc
  :ensure t
  :config (dolist (jumpc-bind-vim-key)))

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

;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @ACE
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( ace )--------------------------------------------------------------


(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @MAGIT
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( magit )--------------------------------------------------------------

(use-package magit
  :ensure t)


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
  :ensure t)
;;(use-package ack-and-a-half)

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



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @TEXT
;; ;;;////////////////////////////////////////////////////////////////


;; ---( markdown )--------------------------------------------------------------


(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'spell-check-and-wrap-at-80)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))))



;; ---( LaTeX )--------------------------------------------------------------


(use-package tex-site
  :disabled t
  :load-path "site-lisp/auctex/preview/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (defun latex-help-get-cmd-alist () ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)


  (use-package latex-mode
    :defer t
    :config
    (progn
      (use-package preview)
      (use-package ac-math)
      (defun ac-latex-mode-setup ()
        (nconc ac-sources
               '(ac-source-math-unicode ac-source-math-latex
                                        ac-source-latex-commands)))
      (add-to-list 'ac-modes 'latex-mode)
      (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
      (info-lookup-add-help :mode 'latex-mode
                            :regexp ".*"
                            :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                            :doc-spec '(("(latex2e)Concept Index" )
                                        ("(latex2e)Command Index")))))
  )


;; ---( yaml )--------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :defer t)


;; ---( css )--------------------------------------------------------------

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

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


;; ---( yaml )--------------------------------------------------------------

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
)





;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @LANG
;; ;;;////////////////////////////////////////////////////////////////


;; ---( R )--------------------------------------------------------------

(use-package ess
  :ensure t
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
  (elpy-use-ipython)
  (defalias 'workon 'pyvenv-workon))

(use-package ein
  :ensure t
  :config
  (defalias 'eip 'ein:notebooklist-open))




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
  :ensure t
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

(use-package whitespace
  :ensure t
  :bind (("C-c T w" . whitespace-mode))
  :init
  (dolist (hook '(conf-mode-hook))
;;  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

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

(use-package hilit-chg
  :ensure t
  :bind ("M-o C" . highlight-changes-mode))



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @SERVER
;; ;;;////////////////////////////////////////////////////////////////


;; ---( server )--------------------------------------------------------------

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

;; ---( twitter )------------------------------------------------------

(use-package twittering-mode
  :disabled t
  :commands twit
  :config
  (setq twittering-use-master-password t))

;; ---( hackernews )------------------------------------------------------

(use-package hackernews
  :defer t
  )



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @HELM
;; ;;;////////////////////////////////////////////////////////////////


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

(use-package helm
  :ensure helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
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
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

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



;; ;;;////////////////////////////////////////////////////////////////
;; ;;;  @ORG
;; ;;;////////////////////////////////////////////////////////////////


;; ---( org-mode )--------------------------------------------------------------

(use-package org
  :ensure t
  :defer 30
  )

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
;; ;;;  @HYDRA
;; ;;;////////////////////////////////////////////////////////////////


;; ;; ---( hydra )--------------------------------------------------------------

(use-package hydra
  :ensure t)


;; ---( site.pkgs: end )-------------------------------------------------------
(message "SITE:PKGS - end")
