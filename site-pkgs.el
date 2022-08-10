;; Log: start
;; #+NAME: log-start

;; [[file:site-pkgs.org::log-start][log-start]]
;; ---( site.pkgs: begin )-------------------------------------------------------
(message "SITE:PKGS - begin")
;; log-start ends here

;; References
;; #+NAME: references

;; [[file:site-pkgs.org::references][references]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @REFERENCES
;; ;;;////////////////////////////////////////////////////////////////

;; ---( dotfiles )--------------------------------------------------------------

;; @see: https://sachachua.com/dotemacs#%s
;; @see: http://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/
;; @see: https://elpy.readthedocs.org/en/latest/
;; @see: https://emacs-lsp.github.io/lsp-mode/page/installation/
;; @see: https://gitea.petton.fr/nico/emacs.d/src/commit/8ae2b902c916600c9296d967f36ed69ad50e8199/init.el?lang=sv-SE
;; @see: https://github.com/bdd/.emacs.d/blob/master/packages.el
;; @see: https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org
;; @see: https://github.com/cgroll/dot_emacs.d/blob/master/init.el
;; @see: https://github.com/jidicula/dotfiles/blob/main/init.el?utm_source=pocket_mylist
;; @see: https://github.com/jorgenschaefer/elpy
;; @see: https://github.com/JuliaEditorSupport/julia-emacs
;; @see: https://github.com/jwiegley/use-package
;; @see: https://github.com/nnicandro/emacs-jupyter
;; @see: https://github.com/pdorrell/rules-editing-mode
;; @see: https://github.com/pdorrell/rules-editing-mode/blob/master/my-drools.el
;; @see: https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; @see: https://github.com/tpapp/julia-repl
;; @see: https://gitlab.com/balajisi/emacs/blob/master/init.el
;; @see: https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535?utm_source=pocket_mylist
;; @see: https://julia-users-zurich.github.io/talks/talk-2018-04/emacs.html
;; @see: https://ladicle.com/post/config/
;; @see: https://sgtpeacock.com/dot-files/Emacs.html#org66117b2
;; @see: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;; @see: https://youtu.be/0kuCeS-mfyc
;; @see: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;; }}}  .references
;; references ends here

;; Use-Package

;; #+NAME: startup

;; [[file:site-pkgs.org::startup][startup]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @PACKAGES
;; ;;;////////////////////////////////////////////////////////////////

;; ---( Install )--------------------------------------------------------------

;;(fset 'h7/ensure 't)
;;(fset h7/ensure nil)

;; ---( Boot )--------------------------------------------------------------

;; (setq debug-on-error t)


;; @see: https://github.com/radian-software/straight.el
;; @see: 
;; @see: https://youtu.be/UmbVeqphGlc

;; (setq straight-use-package-by-default t)
;; (setq use-package-always-ensure t)
;; 
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;; 
;; (setq package-enable-at-startup nil)
;; (straight-use-package 'use-package)
;; (eval-when-compile (require 'use-package))


;; @see: https://ianyepan.github.io/posts/setting-up-use-package/
;; @see: https://www.reddit.com/r/emacs/comments/dfcyy6/how_to_install_and_use_usepackage/
;; @see: https://framagit.org/steckerhalter/steckemacs.el/-/blob/master/steckemacs.el

(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")) ; Org-mode's repository
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu"   . "https://elpa.nongnu.org/nongnu/"))
  ;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
  ;;(package-initialize)
  ;; i always fetch the archive contents on startup and during compilation, which is slow
  ;; (package-refresh-contents)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (setq use-package-verbose t)
  (require 'use-package)
  ;; i don't really know why this isn't the default...
  ;;(setf use-package-always-ensure t)

  ;;(use-package use-package-ensure
  ;;  :config  (setq use-package-always-ensure t))

  (use-package quelpa
    :ensure t)
  (use-package quelpa-use-package
    :ensure t)
  (quelpa-use-package-activate-advice)
  (use-package auto-compile
    :ensure t
    :config (auto-compile-on-load-mode))
  (setq load-prefer-newer t)

  ;;   (unless (package-installed-p 'quelpa)
  ;;     (with-temp-buffer
  ;;       (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
  ;;       (eval-buffer)
  ;;       (quelpa-self-upgrade)))
  ;;   (quelpa
  ;;    '(quelpa-use-package
  ;;      :fetcher git
  ;;      :url "https://github.com/quelpa/quelpa-use-package.git"))
  ;;   (require 'quelpa-use-package)
  ;;
  )

;; ;; @see:  https://framagit.org/steckerhalter/steckemacs.el/-/blob/master/steckemacs.el

;; ;;; initialization
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
;; (package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;; (package-install 'use-package)
;; (use-package use-package-ensure
;;   :config  (setq use-package-always-ensure t))

;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)


;;(require 'package)
;; ;;(nconc package-archives
;; ;;      '(("melpa" . "http://melpa.org/packages/")
;; ;;        ("org" . "http://orgmode.org/elpa/")))
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; You don't need this one if you have marmalade:
;; (add-to-list 'package-archives '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages"))

;;(setq package-enable-at-startup nil)

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

;;(package-initialize)

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

;; (straight-use-package 'bind-key)
;; (straight-use-package 'diminish)

;; @see: https://github.com/jwiegley/dot-emacs/blob/master/init.el

(eval-and-compile
  (defvar use-package-verbose t))


(require 'bind-key)
(require 'use-package)

;; use-package-ensure-system-package
;; provides way to define system package dependencies for Emacs packages
(use-package use-package-ensure-system-package
  :ensure t)

(require 'req-package)
;;(use-package req-package)

;; @see: https://github.com/noctuid/general.el
(use-package general
  :ensure t)

;; ---( ... )--------------------------------------------------------------

;; }}}  .packages
;; startup ends here

;; Basic

;; #+NAME: basic

;; [[file:site-pkgs.org::basic][basic]]
;; ;;;////////////////////////////////////////////////////////////////
     ;; {{{  @BASIC
     ;; ;;;////////////////////////////////////////////////////////////////

     ;; ---( ... )--------------------------------------------------------------

     ;; ---( ... )--------------------------------------------------------------

     ;;(use-package bs
     ;;  :ensure t)

     ;; ---( ... )--------------------------------------------------------------

     ;; (use-package pretty-symbols
     ;;   :ensure t)

     ;; (use-package pretty-lambdada
     ;;   :ensure t
     ;;   :init (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook)))
     ;;   :config (dolist (global-pretty-lambda-mode)))


   ;; (use-package jumpc
   ;;   :disabled t
   ;;   :config (progn (jumpc-bind-vim-key)))

   ;; (use-package rainbow-delimiters
   ;;   :disabled t
   ;;   :hook (prog-mode . rainbow-delimiters-mode))

   ;; ---( undo-tree )--------------------------------------------------------------

(use-package undo-tree
  :ensure t
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (progn
    (global-undo-tree-mode)
    (unbind-key "M-_" undo-tree-map)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))


;; ---( dash )--------------------------------------------------------------

;; ~dash.el~ :: A modern list API for Emacs. No 'cl required.  (See https://github.com/magnars/dash.el/)
(use-package dash
  :ensure t)

;; ---( f )--------------------------------------------------------------

;; ~f.el~ :: A modern API for working with files and directories in Emacs. (See https://github.com/rejeep/f.el/)
(use-package f
  :ensure t)

;; ---( s )--------------------------------------------------------------

;; ~s.el~ :: The long lost Emacs string manipulation library.  (See https://github.com/magnars/s.el/)
(use-package s
  :ensure t)


  ;; }}}  .packages
;; basic ends here

;; UI

;; #+NAME: ui

;; [[file:site-pkgs.org::ui][ui]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @UI
;; ;;;////////////////////////////////////////////////////////////////

;; ---( mode-line )--------------------------------------------------------------

(use-package minions
  :ensure t
  :config (minions-mode 1)
  )

(use-package mood-line
  :ensure t
  :config (mood-line-mode 1)
  )

;; delight
;; hides modeline displays
(use-package delight
  :ensure t)
(require 'delight)                ;; if you use :delight
(require 'bind-key)                ;; if you use any :bind variant

;; ;; Required to hide the modeline 
;; (use-package hide-mode-line
;;   :ensure t
;;   :defer t)

;; (use-package all-the-icons)
;; (use-package doom-modeline
;;   :after eshell
;;   :init (doom-modeline-mode 1))

;; (use-package powerline
;;   :ensure t
;;   :init (powerline-default-theme)
;; )


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

;; ---( highlight )--------------------------------------------------------------

(use-package hl-todo
  :ensure t
  :config (minions-mode 1)
  )

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; (define-key hl-todo-mode-map (kbd "C-c p") #'hl-todo-previous)
  ;; (define-key hl-todo-mode-map (kbd "C-c n") #'hl-todo-next)
  ;; (define-key hl-todo-mode-map (kbd "C-c o") #'hl-todo-occur)
  ;; (define-key hl-todo-mode-map (kbd "C-c i") #'hl-todo-insert  
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; ---( dashboard )--------------------------------------------------------------

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (agenda . 5)))
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))


;; }}}  .ui
;; ui ends here

;; Magit
;; #+NAME: magit

;; [[file:site-pkgs.org::magit][magit]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @MAGIT
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

;; ---( git-timemachine )------------------------------------------------------------

(use-package git-timemachine
  :disabled t)

;; (use-package git-timemachine
;;   :ensure t
;;   :bind (("s-g" . git-timemachine)))

;; ---( vdiff )------------------------------------------------------------

(use-package vdiff
  :ensure t)

(use-package vdiff-magit
  :ensure t
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

  ;; This flag will default to using ediff for merges.
  ;; (setq vdiff-magit-use-ediff-for-merges nil)

  ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
  ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
  ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
  ;; when point is on an uncommitted hunk.
  ;; (setq vdiff-magit-dwim-show-on-hunks nil)

  ;; Whether vdiff-magit-show-stash shows the state of the index.
  ;; (setq vdiff-magit-show-stash-with-index t)

  ;; Only use two buffers (working file and index) for vdiff-magit-stage
  ;; (setq vdiff-magit-stage-is-2way nil)

  )

;; }}}  .magit
;; magit ends here

;; Dired
;; #+NAME: dired

;; [[file:site-pkgs.org::dired][dired]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @DIRED
;; ;;;////////////////////////////////////////////////////////////////


;; ---( dired )--------------------------------------------------------------


(use-package dired
  ;; :straight (:type built-in)
  ;; :ensure t 
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :custom
  ;; (require 'ls-lisp)
  (ls-lisp-dirs-first t)
  (ls-lisp-ignore-case nil)
  (ls-lisp-use-insert-directory-program nil)

  (dired-listing-switches "-alvhp --dired --group-directories-first")

  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t) ;;use to copy to the next buffer visible
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  ;;(image-dired-external-viewer (executable-find "sxiv"))
  )

;; @see: https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-dired.el

;; dired-x: to hide uninteresting files in dired
(use-package dired-x
  ;; :straight nil
  ;; :ensure t 
  :bind ("C-x C-j" . dired-jump)
  :hook ((dired-mode . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil)

  ;; hide backup, autosave, *.*~ files
  ;; omit mode can be toggled using `C-x M-o' in dired buffer.
  ;;(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")
  (setq dired-omit-files "^\\.?#\\|^\\.$")
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|^.git$"))
  )

;; }}}  .dired
;; dired ends here

;; Project
;; #+NAME: project

;; [[file:site-pkgs.org::project][project]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @PROJECT
;; ;;;////////////////////////////////////////////////////////////////


;; ---( projectile )--------------------------------------------------------------

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (emacs-d "var/projectile.cache")
        projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld"))
  (make-directory (emacs-d "var") t)
  :config
  (projectile-global-mode))


;; ---( treemacs )--------------------------------------------------------------

;; Provides workspaces with file browsing (tree file viewer)
;; and project management when coupled with `projectile`.

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
          treemacs-width 24)
  :bind ("C-c t" . treemacs))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


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



;; }}}  .project
;; project ends here

;; Grep
;; #+NAME: grep

;; [[file:site-pkgs.org::grep][grep]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @GREP
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ack )--------------------------------------------------------------

(use-package ack
  :disabled t)
;;(use-package ack-and-a-half)

;; ---( ag )--------------------------------------------------------------

(use-package wgrep
  :ensure t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer )
  (twgrep-change-readonly-file t))

(use-package ag
  :ensure t
  :custom
  (ag-highligh-search )
  (tag-reuse-buffers )
  (tag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag
    :ensure t))


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


;; }}}  .grep
;; grep ends here

;; Text/begin
;; #+NAME: text-begin

;; [[file:site-pkgs.org::text-begin][text-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @TEXT
;; ;;;////////////////////////////////////////////////////////////////
;; text-begin ends here

;; Markdown
;; #+NAME: markdown

;; [[file:site-pkgs.org::markdown][markdown]]
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
;; markdown ends here

;; Markup
;; #+NAME: markup

;; [[file:site-pkgs.org::markup][markup]]
;; ---( css )--------------------------------------------------------------

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

;; (use-package rainbow-mode
;;   :disabled t	     
;;   :config
;;   (add-hook 'css-mode-hook 'rainbow-mode)
;;   (add-hook 'less-mode-hook 'rainbow-mode))

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
;; markup ends here

;; Text/end
;; #+NAME: text-end

;; [[file:site-pkgs.org::text-end][text-end]]
;; }}}  .text
;; text-end ends here

;; Lang/begin
;; #+NAME: lang-begin

;; [[file:site-pkgs.org::lang-begin][lang-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @LANG
;; ;;;////////////////////////////////////////////////////////////////
;; lang-begin ends here

;; Lang: LSP.mode
;; #+NAME: lang-lsp.mode

;; [[file:site-pkgs.org::lang-lsp.mode][lang-lsp.mode]]
;; ---( LSP mode )------------------------------------------------------------

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  ;;(setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger

;; Provides visual help in the buffer 
;; For example definitions on hover. 
;; The `imenu` lets me browse definitions quickly.
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
            lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-imenu)))
;; lang-lsp.mode ends here

;; Lang: LSP.dap
;; #+NAME: lang-lsp.mode.dap

;; [[file:site-pkgs.org::lang-lsp.mode.dap][lang-lsp.mode.dap]]
;; ---( dap )--------------------------------------------------------------

;; Integration with the debug server 
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; lang-lsp.mode.dap ends here

;; Lang: R/ess
;; #+NAME: lang-r.ess

;; [[file:site-pkgs.org::lang-r.ess][lang-r.ess]]
;; ---( R )--------------------------------------------------------------

  (use-package ess
;;   :if (version<= "25.1" emacs-version)
;;  :defer t
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
;; lang-r.ess ends here

;; Lang: R/polymode
;; #+NAME: lang-r.ess.polymode

;; [[file:site-pkgs.org::lang-r.ess.polymode][lang-r.ess.polymode]]
;; ---( polymode )--------------------------------------------------------------


(use-package polymode
  :ensure t
  :commands (poly-markdown+r-mode)
  :mode (("\\.rmd\\'" . poly-markdown+r-mode)
	 ("\\.Rmd\\'" . poly-markdown+r-mode))

  :init
  (autoload 'r-mode "ess-site.el" "Major mode for editing R source." t)
)


(use-package poly-markdown
  :ensure t
  :mode (
	 ("\\.md" . poly-markdown-mode)
  )
)

(use-package poly-R
  :ensure t
)
;; lang-r.ess.polymode ends here

;; Lang: Python/mode
;; #+NAME: lang-python.mode

;; [[file:site-pkgs.org::lang-python.mode][lang-python.mode]]
;; ---( python )--------------------------------------------------------------

;; @see: https://gitlab.com/nathanfurnal/dotemacs/-/snippets/2060535?utm_source=pocket_mylist
;; @see: https://github.com/jidicula/dotfiles/blob/main/init.el?utm_source=pocket_mylist


;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python 
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))


;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to hide the modeline 
(use-package hide-mode-line
  :ensure t
  :defer t)



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
;; lang-python.mode ends here

;; Lang: Python/env
;; #+NAME: lang-python.env

;; [[file:site-pkgs.org::lang-python.env][lang-python.env]]
;; ---( pyvenv )--------------------------------------------------------------

;; Required to easily switch virtual envs 
;; via the menu bar or with `pyvenv-workon` 
;; Setting the `WORKON_HOME` environment variable points 
;; at where the envs are located. I use (miniconda ^H) poetry. 
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  ;;(setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  (setenv "WORKON_HOME" (expand-file-name "~/.cache/pypoetry/virtualenvs"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))


;; ---( poetry )-------------------------------------------------------------

(use-package poetry
  :ensure t
  ;; :init
  ;; imperfect tracking strategy causes lags in builds
  ;; (setq poetry-tracking-strategy 'switch-buffer)
  :hook
  ;; activate poetry-tracking-mode when python-mode is active
  (python-mode . poetry-tracking-mode)
  )

;; (use-package poetry
;;   :ensure t
;;   :config
;;   (add-hook 'poetry-tracking-mode-hook (lambda () (remove-hook 'post-command-hook 'poetry-track-virtualenv)))
;;   (add-hook 'python-mode-hook 'poetry-track-virtualenv)
;;   (add-hook 'projectile-after-switch-project-hook 'poetry-track-virtualenv))


;; ---( pipenv )-------------------------------------------------------------
;;
;; (use-package pipenv
;;   :unless (version< emacs-version "25.1")
;;   :defer t
;;   ;; :ensure t
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))
;; lang-python.env ends here

;; Lang: Python/lsp
;; #+NAME: lang-python.lsp

;; [[file:site-pkgs.org::lang-python.lsp][lang-python.lsp]]
;; ---( lsp-pyright )--------------------------------------------------------------

;; Language server for Python 
;; Read the docs for the different variables set in the config.
(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  ;;(setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-pyright-disable-language-service nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        ;;lsp-pyright-venv-path "~/miniconda3/envs")
        lsp-pyright-venv-path "~/.cache/pypoetry/virtualenvs")
  :hook ((python-mode . (lambda () 
                          (require 'lsp-pyright) (lsp-deferred)))))
;; lang-python.lsp ends here

;; Lang: Python/tools
;; #+NAME: lang-python.tools

;; [[file:site-pkgs.org::lang-python.tools][lang-python.tools]]
;; ---( yapfify )-------------------------------------------------------------

;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))


;; ---( python-black )--------------------------------------------------------------

(use-package python-black
  ;;:delight python-black-on-save-mode "⚫️"
  :ensure t
  :hook
  (python-mode . python-black-on-save-mode)
  :init
  (put 'python-black-command 'safe-local-variable #'stringp)
  (put 'python-black-extra-args 'safe-local-variable #'stringp)
  (put 'python-black-on-save-mode 'safe-local-variable #'booleanp)
  )
;; lang-python.tools ends here

;; Lang: Python/elpy
;; #+NAME: lang-python.elpy

;; [[file:site-pkgs.org::lang-python.elpy][lang-python.elpy]]
;; ---( python: elpy )--------------------------------------------------------------

(use-package elpy
  :disabled t
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

      (define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-below)
      ;;(define-key map (kbd "<C-S-return>") 'elpy-open-and-indent-line-above)

      ;;(define-key map (kbd "<C-return>") 'elpy-shell-send-current-statement)

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

(setenv "PYTHONIOENCODING" "utf-8")
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))
;; lang-python.elpy ends here

;; Lang: Python/ein
;; #+NAME: lang-python.ein

;; [[file:site-pkgs.org::lang-python.ein][lang-python.ein]]
;; ---( python: ein )--------------------------------------------------------------


(use-package ein
  :unless (version< emacs-version "25.1")
  ;; :defer t
  :ensure t
  :init
  (progn
    (with-eval-after-load 'ein-notebooklist
      (define-key ein:notebooklist-mode-map (kbd "<S-return>") 'ein:worksheet-execute-cell-and-goto-next-km)
      (define-key ein:notebooklist-mode-map (kbd "<C-return>") 'ein:worksheet-execute-cell)
      ))
  :config
  (defalias 'eip 'ein:notebooklist-open))



;; (use-package ein
;;   :unless (version< emacs-version "25.1")
;;   :ensure t
;;   :defer t
;;   :commands ein:notebooklist-open
;;   :init
;;   ;; (progn
;;   ;;   (with-eval-after-load 'ein-notebooklist
;;   ;;     ;; removing keybindings
;;   ;;     (define-key ein:notebook-mode-map (kbd "M-p") nil)
;;   ;;     (define-key ein:notebook-mode-map (kbd "<M-up>") nil)
;;   ;;     (define-key ein:notebook-mode-map (kbd "<M-down>") nil)
;;   ;;     ;; changing keybinding
;;   ;;     (define-key ein:notebook-mode-map (kbd "C-s") 'ein:notebook-save-notebook-command)
;;   ;;     (define-key ein:notebook-mode-map (kbd "<M-S-up>") 'ein:worksheet-move-cell-up)
;;   ;;     (define-key ein:notebook-mode-map (kbd "<M-S-down>") 'ein:worksheet-move-cell-down)))
;;   :config
;;   (defalias 'einp 'ein:notebooklist-open)
;;   (defalias 'eins 'ein:jupyter-server-start)
;;   )
;; lang-python.ein ends here

;; Lang: Julia
;; #+NAME: lang-julia

;; [[file:site-pkgs.org::lang-julia][lang-julia]]
;; ---( julia )--------------------------------------------------------------

;; @see: https://github.com/JuliaEditorSupport/julia-emacs
;; @see: https://github.com/tpapp/julia-repl
;; @see: https://github.com/nnicandro/emacs-jupyter
;; @see: https://julia-users-zurich.github.io/talks/talk-2018-04/emacs.html
;; @see: https://github.com/cgroll/dot_emacs.d/blob/master/init.el

(use-package julia-mode
   :ensure t
   :defer t
   :commands julia-mode
   :mode ("\\.jl$" . julia-mode)
   :init
   (progn
      (autoload 'julia-mode "julia-mode" nil t)
      (setq inferior-julia-program-name "julia")
      )
   :config
   (progn
      (setq inferior-julia-program-name "julia")
      )
   )


(use-package julia-repl
   :ensure t
   :defer t
   :config
   (progn
     (add-to-list 'julia-mode-hook 'julia-repl-mode)
     )
   )

;; ;; allow julia to be loaded through call to julia-mode or
;; ;; ess-inferior process
;; ;; follow-ups: etags?
;; (use-package julia-mode
;;    :defer t
;;    :commands julia-mode
;;    :mode ("\\.jl$" . julia-mode)
;;    :init
;;    (progn
;;       (autoload 'julia-mode "julia-mode" nil t)
;;       (setq inferior-julia-program-name "/usr/bin/julia")
;;       )
;;    :config
;;    (progn
;;       (add-to-list 'julia-mode-hook 'cg/modify-current-syntax-table)
;;       (setq inferior-julia-program-name "/usr/bin/julia")
;;       (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
;;       ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)      
;;       )
;;    )

;; (use-package ess-julia.el
;;    :defer t
;;    :commands julia
;;    :init                                ; run before actual loading
;;    (progn
;;       (autoload 'julia "ess-julia.el" nil t)
;;       (setq inferior-julia-program-name "/usr/bin/julia")
;;       )
;;    :config
;;    (progn
;;       (require 'ess-site)
;;       (setq inferior-julia-program-name "/usr/bin/julia")
;;       (setq ess-tracebug-prefix "\M-c")   ; define debug-mode starting key
;;       (setq ess-use-tracebug t)           ; tracebug is called for R
;;                                         ; AND JULIA!!
;;       (setq ess-tracebug-inject-source-p t)
;;       (add-to-list 'julia-mode-hook 'cg/command-line-keybindings)
;;       ;; (add-to-list 'inferior-ess-mode-hook 'cg/command-line-keybindings)            
;;       )
;;    )
;; ;; in order to add ess-process afterward, apply julia-mode again on
;; ;; open buffers - probably ess-julia.el has to be loaded again also:
;; ;; M-x load-file ess-julia.el
;; lang-julia ends here

;; Lang: Ruby
;; #+NAME: lang-ruby

;; [[file:site-pkgs.org::lang-ruby][lang-ruby]]
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
;; lang-ruby ends here

;; Lang: Scala
;; #+NAME: lang-scala

;; [[file:site-pkgs.org::lang-scala][lang-scala]]
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
;; lang-scala ends here

;; Lang: Haskell
;; #+NAME: lang-haskell

;; [[file:site-pkgs.org::lang-haskell][lang-haskell]]
;; ---( haskell )--------------------------------------------------------------

;; @see: https://gitlab.com/balajisi/emacs/blob/master/init.el
;; @see: https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md

;;;; Haskell Modes - Haskell, GHC, SHM, Idris etc.
(use-package haskell-mode
  :ensure

  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode))

  :config
  ;; (use-package ghc
  ;;   :ensure)
  (use-package flycheck-haskell
    :ensure)
  )

;; (defun balaji/haskell-mode-hook ()
;;   (turn-on-haskell-indentation)
;;   ;; (ghc-init)
;;   (lambda () (add-to-list 'ac-sources 'ac-source-ghc))
;;   )

;; (add-hook 'haskell-mode-hook 'balaji/haskell-mode-hook)

(use-package idris-mode
  :ensure
  :disabled t)
;; lang-haskell ends here

;; Lang: Drools
;; #+NAME: lang-drools

;; [[file:site-pkgs.org::lang-drools][lang-drools]]
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
;; lang-drools ends here

;; Lang/end
;; #+NAME: lang-end

;; [[file:site-pkgs.org::lang-end][lang-end]]
;; }}}  .lang
;; lang-end ends here

;; REST/begin
;; #+NAME: rest-begin

;; [[file:site-pkgs.org::rest-begin][rest-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @REST
;; ;;;////////////////////////////////////////////////////////////////
;; rest-begin ends here

;; Json
;; #+NAME: rest-json

;; [[file:site-pkgs.org::rest-json][rest-json]]
;; ---( json )--------------------------------------------------------------

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package json-reformat
  :ensure t
  :after json-mode
  :init (setq json-reformat:indent-width 2))
;; rest-json ends here

;; Yaml
;; #+NAME: rest-yaml

;; [[file:site-pkgs.org::rest-yaml][rest-yaml]]
;; ---( yaml )--------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :defer t)
;; rest-yaml ends here

;; TODO Request 
;; #+NAME: rest-request

;; [[file:site-pkgs.org::rest-request][rest-request]]
;; ---( request )--------------------------------------------------------------

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
;; rest-request ends here

;; Rest/end
;; #+NAME: rest-end

;; [[file:site-pkgs.org::rest-end][rest-end]]
;; }}}  .rest
;; rest-end ends here

;; OP/begin
;; #+NAME: op-begin

;; [[file:site-pkgs.org::op-begin][op-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @OP
;; ;;;////////////////////////////////////////////////////////////////
;; op-begin ends here

;; Ansible
;; #+NAME: vm-ansible

;; [[file:site-pkgs.org::vm-ansible][vm-ansible]]
;; @see: https://emacs-lsp.github.io/lsp-mode/page/lsp-ansible/

(use-package ansible
  :commands ansible
  :ensure t
  :custom
  (ansible-vault-password-file "~/.ans-wall.asc")
  )


(use-package ansible-doc
  :after ansible
  :diminish ansible-doc-mode
  :ensure t

  :commands
  (ansible-doc
   ansible-doc-mode))

;; (use-package ansible-vault
;;   :after ansible
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'ansible
;;     (defun ansible-vault-mode-maybe ()
;;       (when (ansible-vault--is-vault-file)
;;         (ansible-vault-mode 1))))
;;   (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe))


(use-package company-ansible
  :after ansible
  :commands company-ansible
  :ensure t

  :init
  (with-eval-after-load 'company
    (defun gr/setup-company-ansible ()
      (set (make-local-variable 'company-backends) '(company-ansible)))
    (add-hook 'ansible-hook 'gr/setup-company-ansible)))

(use-package jinja2-mode
  :after ansible
  :ensure t)


(use-package poly-ansible
  :after polymode
  :ensure t

  :preface
  (eval-when-compile
    (defvar pm-inner/jinja2 nil))

  :mode
  ("playbook\\.ya?ml\\'" . poly-ansible-mode)
  ("/ansible/.*\\.ya?ml\\'" . poly-ansible-mode)
  ("/\\(?:group\\|host\\)_vars/" . poly-ansible-mode)

  :init
  (with-eval-after-load 'fill-column-indicator
    (add-hook 'ansible-hook 'fci-mode))

  :config
  (setq pm-inner/jinja2
    (pm-inner-chunkmode :mode #'jinja2-mode
                        :head-matcher "{[%{#][+-]?"
                        :tail-matcher "[+-]?[%}#]}"
                        :head-mode 'body
                        :tail-mode 'body
                        :head-adjust-face nil
                        :tail-adjust-face nil)))
;; vm-ansible ends here

;; Terraform
;; #+NAME: op-terraform

;; [[file:site-pkgs.org::op-terraform][op-terraform]]
;; ---( terraform )--------------------------------------------------------------

(use-package company-terraform
  :ensure t
  :defer t)
;; op-terraform ends here

;; OP/end
;; #+NAME: op-end

;; [[file:site-pkgs.org::op-end][op-end]]
;; }}}  .op
;; op-end ends here

;; VM/begin
;; #+NAME: vm-begin

;; [[file:site-pkgs.org::vm-begin][vm-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @VM
;; ;;;////////////////////////////////////////////////////////////////
;; vm-begin ends here

;; Docker
;; #+NAME: vm-docker

;; [[file:site-pkgs.org::vm-docker][vm-docker]]
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
;; vm-docker ends here

;; VM/end
;; #+NAME: vm-end

;; [[file:site-pkgs.org::vm-end][vm-end]]
;; }}}  .vm
;; vm-end ends here

;; TeX/begin
;; #+NAME: tex-begin

;; [[file:site-pkgs.org::tex-begin][tex-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @TEX
;; ;;;////////////////////////////////////////////////////////////////
;; tex-begin ends here

;; LaTeX
;; #+NAME: tex-latex

;; [[file:site-pkgs.org::tex-latex][tex-latex]]
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
;; tex-latex ends here

;; PDF
;; #+NAME: tex-pdf

;; [[file:site-pkgs.org::tex-pdf][tex-pdf]]
;; ---( pdf )--------------------------------------------------------------

(message "#pdf-tools(0): '( (h7/use-pdf-tools . %s) )" (h7/use-pdf-tools)) 

;; (use-package pdf-tools
;;   ;;  :if (h7/use-pdf-tools)
;;   :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools")
;;   :ensure t
;;   :pin manual ;; don't reinstall when package updates
;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq pdf-annot-activate-created-annotations t)
;;   (require 'pdf-occur)
;;   (pdf-tools-install :no-query)
;;   )

(use-package pdf-tools
  :if (h7/use-pdf-tools)
  :ensure t
  :config
  (pdf-tools-install t)
  (quelpa '(pdf-continuous-scroll-mode
          :fetcher github
          :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

  (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode))


(use-package saveplace-pdf-view
  :if (h7/use-pdf-tools)
)

(use-package pdfgrep
  :ensure t
)

(use-package paperless
  :ensure t
)

;; (use-package toc-mode
;;   :disabled t
;; )

;; (use-package biblithek
;;   :disabled t
;; )
;; tex-pdf ends here

;; TeX/end
;; #+NAME: tex-end

;; [[file:site-pkgs.org::tex-end][tex-end]]
;; }}}  .tex
;; tex-end ends here

;; Fonts/begin
;; #+NAME: fonts-begin

;; [[file:site-pkgs.org::fonts-begin][fonts-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @FONT
;; ;;;////////////////////////////////////////////////////////////////
;; fonts-begin ends here

;; Faces
;; #+NAME: fonts-faces

;; [[file:site-pkgs.org::fonts-faces][fonts-faces]]
;; ---( faces )--------------------------------------------------------------

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Source Code Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 135 :width normal)))))

;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "Source Sans Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 135 :width normal))))
;;    '(fixed-pitch ((t ( :family "JetBrains Mono Medium")))))
;; fonts-faces ends here

;; Ligatures
;; #+NAME: fonts-ligatures

;; [[file:site-pkgs.org::fonts-ligatures][fonts-ligatures]]
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

    (setq ligature-path (expand-file-name "local/repos/ligatures.el" user-emacs-directory))
    (let ((ligature-source (expand-file-name "ligatures.el" ligature-path)))
      (unless (file-exists-p ligature-source)
        (progn
          (make-directory ligature-path t)
          (url-copy-file "https://raw.githubusercontent.com/mickeynp/ligature.el/master/ligature.el" ligature-source t))))

(load-library "~/.emacs.d/local/repos/ligatures.el/ligatures")

(use-package ligature
;;  :load-path "local/repos/ligatures.el/ligature"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))



;; (use-package fira-code-mode
;;   :ensure t
;; ;; :disabled t
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
;;   :hook prog-mode)                                         ; mode to enable fira-code-mode in


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
;; fonts-ligatures ends here

;; Fonts/end
;; #+NAME: fonts-end

;; [[file:site-pkgs.org::fonts-end][fonts-end]]
;; }}}  .fonts
;; fonts-end ends here

;; Org/begin
;; #+NAME: org-begin

;; [[file:site-pkgs.org::org-begin][org-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @ORG
;; ;;;////////////////////////////////////////////////////////////////
;; org-begin ends here

;; org-mode
;; #+NAME: org-mode

;; [[file:site-pkgs.org::org-mode][org-mode]]
;; ---( org-mode )--------------------------------------------------------------

;; @see: https://github.com/bixuanzju/emacs.d/blob/master/emacs-init.org
;; @see: https://stackoverflow.com/questions/45041399/proper-configuration-of-packages-in-gnu-emacs
;; @see: https://hugocisneros.com/org-config/

(use-package org
  :ensure t
  ;; :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ([(meta up)] . nil)    ;; was 'org-metaup
         ([(meta down)] . nil)  ;; was 'org-metadown
         )
  :init 
  ;;keymap conflicts
  (setq org-CUA-compatible t)
  (setq org-support-shift-select t) ;; were 'org-shiftup+dpwn+left+right
  (setq org-replace-disputed-keys t)

  :hook (org-mode . h7/org-mode-setup)

  :config
  (require 'ox-md)
  (unbind-key "C-c ;" org-mode-map)

  ;;keymap conflicts
  (setq org-CUA-compatible t)
  (setq org-support-shift-select t) ;; were 'org-shiftup+dpwn+left+right
  (setq org-replace-disputed-keys t)


  ;; --[org-mode options] ----------------------------------------------------------

  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)

  ;; (setq org-modules
  ;;   '(org-crypt
  ;;       org-habit
  ;;       org-bookmark
  ;;       org-eshell
  ;;       org-irc))


  ;; --[org-mode faces] ----------------------------------------------------------

  ;; Fontify org-mode code blocks
  (setq org-src-fontify-natively t)
  (setq org-fontify-quote-and-verse-blocks t)

  ;; Stop the org-level headers from increasing in height relative to the other text.
  ;;(set-face-attribute 'org-block nil :weight 'semi-bold :height 1.3)

  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)

  (dolist (face '(org-document-info-keyword))
    (set-face-attribute face nil :weight 'bold :height 1.1))

  (dolist (face '(org-document-title))
    (set-face-attribute face nil :weight 'bold :height 1.2))
  (dolist (face '(org-document-info))
    (set-face-attribute face nil :weight 'semi-bold :height 1.2))


  (dolist (face '(org-level-1))
    (set-face-attribute face nil :weight 'semi-bold :height 1.3))

  (dolist (face '(org-level-2))
    (set-face-attribute face nil :weight 'semi-bold :height 1.2))

  (dolist (face '(org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.1))

  (dolist (face '(org-block-begin-line
                  org-block-end-line
                  org-meta-line))
    (set-face-attribute face nil :weight 'bold :height 0.9))





  ;; --[org-mode todo] ----------------------------------------------------------

  ;; @see: https://github.com/daviwil/dotfiles/blob/master/Workflow.org

  (setq org-store-todo "~/Dropbox/Local/data/org/all")

  (setq org-agenda-files
        (mapcar (lambda(x) (mapconcat 'identity (list org-store-todo x) "/"))
                '("task.org"
                  "milk.org"
                  "read.org"
                  "dots.org")))        

  (setq org-capture-templates
        `(
          ("t" "task" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ACTIVE")
           "** TODO [#C] %? [/]\n   %a\n   + [ ] ...\n\n")
          ("n" "task (next)" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ACTIVE")
           "** NEXT [#B] %? [/]\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n   %a\n   + [ ] ...\n\n")
          ("b" "backlog" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "BACKLOG")
           "* BACK [#C] %? [/]\n   %a\n   + [ ] ...\n\n")
          ("p" "attic" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ATTIC")
           "* PAST [#C] %?\n")
          ("m" "milk" entry (file+headline ,(format "%s/%s" org-store-todo "milk.org") "ACTIVE")
           "** TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
          ("r" "read" entry (file+headline ,(format "%s/%s" org-store-todo "read.org") "BACKLOG")
           "** READ [#C] %?\n\n")
          ("e" "dots" entry (file+headline ,(format "%s/%s" org-store-todo "dots.org") "ACTIVE")
           "** EDIT [#C] %?\n   - %a\n\n")
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "DOING(x)" "|" "UNDO(u)" "DONE(d)")
          (sequence "READY(w)" "|" "BACK(b)" "WAIT(w)" "HOLD(w)" "CANCELLED(c@/!)")
          (sequence "EDIT(e)" "|" "READ(r)" "NOP(o)" "ACK(a)" "NACK(k)")
          (sequence "|" "PAST(p)" "LOST(l)")))

  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "green" :weight bold))
          ("NEXT" :foreground "orange" :weight bold)
          ("DOING" :foreground "yellow" :weight bold)
          ("UNDO" :foreground "SteelBlue" :weight bold)
          ("READY" :foreground "orange red" :weight bold)
          ("BACK" :foreground "MediumPurple3" :weight bold)
          ("WAIT" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("PAST" :foreground "Silver" :weight bold)
          ("PAST" :foreground "Dark Silver" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))

  ;; Configure common tags
  (setq org-tag-alist
        '((:startgroup) ; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("batch" . ?b)
          ("next" . ?n)
          ("followup" . ?f)
          ("recurring" . ?r)))

  ;;set priority range from A to C with default A
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  ;;set colours for priorities
  (setq org-priority-faces '((?A . (:foreground "OliveDrab" :weight bold))
                             (?B . (:foreground "LightSteelBlue"))
                             (?C . (:foreground "#F0DFAF"))))

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  (setq org-enforce-todo-dependencies t)

  ;; --[org-mode agenda] ----------------------------------------------------------

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


  ;; --[org-mode latex] ----------------------------------------------------------

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

  ;; --[org-mode babel] ----------------------------------------------------------

  (setq org-confirm-babel-evaluate nil)

  ;; execute external programs.
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (dot . t)
           (ditaa . t)
           (python . t)
           (ruby . t)
           (R . t)           
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


  (defun h7/org-mode-setup ()

    ;;keymap conflicts
    (local-set-key [(meta up)] 'dired)
    (local-set-key [(meta down)] 'bs-show)

    ;; (turn-on-org-cdlatex)
    ;; (diminish 'org-cdlatex-mode "")
    (turn-on-auto-fill)

    ;; make `company-backends' local is critcal
    ;; or else, you will have completion in every major mode, that's very annoying!
    (make-local-variable 'company-backends)
    ;; company-ispell is the plugin to complete words
    (add-to-list 'company-backends 'company-ispell))

  (eval-after-load 'org-src
    '(define-key org-src-mode-map
       "\C-x\C-s" #'org-edit-src-exit)))
;; org-mode ends here

;; Org extras
;; #+NAME: org-extras

;; [[file:site-pkgs.org::org-extras][org-extras]]
;; ---(org-superstar)------------------------------------------------------------------------

;; Nice bullet points. Retires org-bullets.
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•))))


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
;; org-extras ends here

;; Org roam
;; #+NAME: org-roam

;; [[file:site-pkgs.org::org-roam][org-roam]]
;; ---(org-roam)------------------------------------------------------------------------

;; @see: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Dropbox/Local/data/org/net"))
  ;;(org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ;; :map org-mode-map
         ;; ("C-M-i" . completion-at-point)
         ;; :map org-roam-dailies-map
         ;; ("Y" . org-roam-dailies-capture-yesterday)
         ;; ("T" . org-roam-dailies-capture-tomorrow)
         )
  ;; :bind-keymap
  ;; ("C-c n d" . org-roam-dailies-map)
  :config
  ;; (require 'org-roam-dailies) ;; Ensure the keymap is available
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "system" plain
           "* Info\n\n- Module: ${title}\n- OS: %^{os}\n- Layer: %^{layer}\n- Zone: %^{zone}\n- Version: %^{version}\n\n* Related:\n\n- \n\n* Bindings:\n\n- %?\n\n* References:\n\n- "
           :if-new (file+head "system/%<%Y%m%d%H%M%S>-${slug}.org" "\n#+STARTUP: showeverything\n\n#+title: ${title}\n")
           :unnarrowed t)
          ("v" "devel" plain
           "* Info\n\n- Module: ${title}\n- Lang: %^{lang}\n- Frame: %^{frame}\n- Context: %^{context}\n- Version: %^{version}\n\n* Related:\n\n- \n\n* Bindings:\n\n- %?\n\n* References:\n\n- "
           :if-new (file+head "devel/%<%Y%m%d%H%M%S>-${slug}.org" "\n#+STARTUP: showeverything\n\n#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "refs/%<%Y%m%d%H%M%S>-b-${slug}.org" "\n#+STARTUP: showeverything\n\n#+title: ${title}\n")
           :unnarrowed t)
          ("l" "Online link" plain
           "\n* Source\n\nTitle: ${title}\nURL: %^{URL}\n\n* Summary\n\n%?"
           :if-new (file+head "refs/%<%Y%m%d%H%M%S>-l-${slug}.org" "\n#+STARTUP: showeverything\n\n#+title: ${title}\n")
           :unnarrowed t)
          ))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; ---(http server)------------------------------------------------------------------------

(use-package websocket
  :ensure t
  :after org-roam
  ;; :straight (:host github :repo "ahyatt/emacs-websocket" :branch "main")
  )

(use-package simple-httpd
  :ensure t
  :after org-roam
  )

;; ---(org-roam-ui)------------------------------------------------------------------------

(use-package org-roam-ui
  :ensure t
  ;; :straight
  ;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :quelpa (org-roam-ui :fetcher github :repo "org-roam/org-roam-ui")
  :after org-roam
  ;; ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;; ;; a hookable mode anymore, you're advised to pick something yourself
  ;; ;; if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
;; org-roam ends here

;; Org samples
;; #+NAME: org-samples

;; [[file:site-pkgs.org::org-samples][org-samples]]
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
;; org-samples ends here

;; Org/end
;; #+NAME: org-end

;; [[file:site-pkgs.org::org-end][org-end]]
;; }}}  .org
;; org-end ends here

;; Utils/begin
;; #+NAME: utils-begin

;; [[file:site-pkgs.org::utils-begin][utils-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @UTIL
;; ;;;////////////////////////////////////////////////////////////////
;; utils-begin ends here

;; Utils/Search
;; #+NAME: util-search

;; [[file:site-pkgs.org::util-search][util-search]]
;; ---( regex )--------------------------------------------------------------


(use-package regex-tool
  :ensure t
  :defer t)
;; util-search ends here

;; Utils/Help
;; #+NAME: util-help

;; [[file:site-pkgs.org::util-help][util-help]]
;; ---( guide-key )--------------------------------------------------------------

;; (use-package guide-key
;;   :ensure t
;;   :defer t
;;   :diminish guide-key-mode
;;   :idle
;;   (progn
;;     (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
;;     (guide-key-mode 1)))

;; ---( which-key )--------------------------------------------------------------

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

(use-package which-key
  :delight
  :ensure t
  :init
  (which-key-mode)
  )



;; ---( helpful )--------------------------------------------------------------

;; @see: https://sgtpeacock.com/dot-files/Emacs.html#org66117b2

(use-package helpful
  :ensure t
  :general
  (:states '(normal visual emacs)
           :prefix "SPC"

           "d" '(:ignore t :wk "Describe")
           "d." 'helpful-symbol
           "df" 'helpful-function
           "dv" 'helpful-variable
           "dk" 'helpful-key
           "dc" 'helpful-command)
  :config
  (defvar read-symbol-positions-list nil))
;; util-help ends here

;; Utils/Misc
;; #+NAME: util-misc

;; [[file:site-pkgs.org::util-misc][util-misc]]
;; ---( popper )--------------------------------------------------------------

(use-package popper
  :ensure t
  ;; :general
  ;; (:states '(normal visual emacs)
  ;;          :prefix "SPC"
  ;;          "`" 'popper-toggle-latest
  ;;          "~" 'popper-cycle)
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode
                              eldoc-mode))
  (popper-window-height 30)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

;; ---( comint )--------------------------------------------------------------

(use-package comint
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))

;; ---( environment )--------------------------------------------------------------

;; Restart Emacs from inside Emacs with `M-x restart-emacs`
(use-package restart-emacs
  :defer t)

;; use-package-ensure-system-package
;; provides way to define system package dependencies for Emacs packages
(use-package use-package-ensure-system-package
  :ensure t)

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


;; ---( folding )--------------------------------------------------------------

(use-package vimish-fold
  :ensure t
  :hook ((
          terraform-mode
          yaml-mode
          text-mode
          ) . vimish-fold-mode)
)

;;        markdown-mode


;; (use-package folding
;;   :ensure t
;; )

;; ---( yasnippet )--------------------------------------------------------------

(use-package yasnippet
  :disabled t
  :config
  (yas-reload-all))
;; util-misc ends here

;; Utils/end
;; #+NAME: util-end

;; [[file:site-pkgs.org::util-end][util-end]]
;; }}}  .util
;; util-end ends here

;; Server/begin
;; #+NAME: server-begin

;; [[file:site-pkgs.org::server-begin][server-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @SERVER
;; ;;;////////////////////////////////////////////////////////////////
;; server-begin ends here

;; Server Control
;; #+NAME: server-control

;; [[file:site-pkgs.org::server-control][server-control]]
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
;; server-control ends here

;; Server/end
;; #+NAME: server-end

;; [[file:site-pkgs.org::server-end][server-end]]
;; }}}  .server
;; server-end ends here

;; Shell/begin
;; #+NAME: shell-begin

;; [[file:site-pkgs.org::shell-begin][shell-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @SHELL
;; ;;;////////////////////////////////////////////////////////////////
;; shell-begin ends here

;; eshell
;; #+NAME: shell-eshell

;; [[file:site-pkgs.org::shell-eshell][shell-eshell]]
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
;; shell-eshell ends here

;; vterm
;; #+NAME: shell-vterm

;; [[file:site-pkgs.org::shell-vterm][shell-vterm]]
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
  :bind (("C-<F9>" . vterm)
             ;; :straight (:post-build (cl-letf (((symbol-function #'pop-to-buffer)
             ;;                        (lambda (buffer) (with-current-buffer buffer (message (buffer-string))))))
             ;;               (setq vterm-always-compile-module t)
             ;;               (require 'vterm)))
         :map vterm-mode-map
         ("C-v" . vterm-yank)
         ("S-<insert>" . vterm-yank)
         ([kp-enter] . vterm-yank)
         ([kp-divide] . vterm-yank-pop)
         ([kp-multiply] . vterm-copy-mode))
  :ensure t)


(use-package multi-vterm
  :bind (("C-S-<f9>" . multi-vterm)
         :map vterm-mode-map
         ("C-<f7>" . multi-vterm-prev)
         ("C-<f8>" . multi-vterm-next))
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


;; ---( multi-term )--------------------------------------------------------------

;; (use-package multi-term
;;   :disabled t
;;   :bind (("C-. t" . multi-term-next)
;;          ("C-. T" . multi-term))
;;   :init
;;   (defun screen ()
;;     (interactive)
;;     (let (term-buffer)
;;       ;; Set buffer.
;;       (setq term-buffer
;;             (let ((multi-term-program (executable-find "screen"))
;;                   (multi-term-program-switches "-DR"))
;;               (multi-term-get-buffer)))
;;       (set-buffer term-buffer)
;;       ;; Internal handle for `multi-term' buffer.
;;       (multi-term-internal)
;;       ;; Switch buffer
;;       (switch-to-buffer term-buffer)))
;;   :config
;;   (defalias 'my-term-send-raw-at-prompt 'term-send-raw)
;;   (defun my-term-end-of-buffer ()
;;     (interactive)
;;     (call-interactively #'end-of-buffer)
;;     (if (and (eobp) (bolp))
;;         (delete-char -1)))
;;   (require 'term)
;;   (defadvice term-process-pager (after term-process-rebind-keys activate)
;;     (define-key term-pager-break-map "\177" 'term-pager-back-page)))
;; shell-vterm ends here

;; Scripts
;; #+NAME: shell-script

;; [[file:site-pkgs.org::shell-script][shell-script]]
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
;; shell-script ends here

;; Ranger
;; #+NAME: shell-ranger

;; [[file:site-pkgs.org::shell-ranger][shell-ranger]]
;; ---( ranger )--------------------------------------------------------------

(use-package ranger
  :ensure t
  ;;:bind ("C-, C-," . ranger)
  )

;; (use-package ranger
;;   :custom
;;   (;; (ranger-override-dired mode t)
;;    (ranger-cleanup-on-disable t)
;;    (ranger-dont-show-binary t)))

;; ;; ---( sunrise-commander )--------------------------------------------------------------


;; (use-package sunrise-commander
;;   :bind (("C-c j" . my-activate-sunrise)
;; 	 ("C-c C-j" . sunrise-cd))
;;   :commands sunrise
;;   :defines sr-tabs-mode-map
;;   :preface
;;   (defun my-activate-sunrise ()
;;     (interactive)
;;     (let ((sunrise-exists
;; 	   (loop for buf in (buffer-list)
;; 		 when (string-match " (Sunrise)$" (buffer-name buf))
;; 		 return buf)))
;;       (if sunrise-exists
;; 	  (call-interactively 'sunrise)
;; 	(sunrise "~/dl/" "~/Archives/"))))
;;   :config
;;   (require 'sunrise-x-modeline)
;;   (require 'sunrise-x-tree)
;;   (require 'sunrise-x-tabs)
;;   (bind-key "/" 'sr-sticky-isearch-forward sr-mode-map)
;;   (bind-key "<backspace>" 'sr-scroll-quick-view-down sr-mode-map)
;;   (bind-key "C-x t" 'sr-toggle-truncate-lines sr-mode-map)
;;   (bind-key "q" 'sr-history-prev sr-mode-map)
;;   (bind-key "z" 'sr-quit sr-mode-map)
;;   (unbind-key "C-e" sr-mode-map)
;;   (unbind-key "C-p" sr-tabs-mode-map)
;;   (unbind-key "C-n" sr-tabs-mode-map)
;;   (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)
;;   (bind-key "M-[" 'sr-tabs-prev sr-tabs-mode-map)
;;   (bind-key "M-]" 'sr-tabs-next sr-tabs-mode-map)
;;   (defun sr-browse-file (&optional file)
;;     "Display the selected file with the default appication."
;;     (interactive)
;;     (setq file (or file (dired-get-filename)))
;;     (save-selected-window
;;       (sr-select-viewer-window)
;;       (let ((buff (current-buffer))
;; 	    (fname (if (file-directory-p file)
;; 		       file
;; 		     (file-name-nondirectory file)))
;; 	    (app (cond
;; 		  ((eq system-type 'darwin) "open %s")
;; 		  ((eq system-type 'windows-nt) "open %s")
;; 		  (t "xdg-open %s"))))
;; 	(start-process-shell-command "open" nil (format app file))
;; 	(unless (eq buff (current-buffer))
;; 	  (sr-scrollable-viewer (current-buffer)))
;; 	(message "Opening \"%s\" ..." fname))))
;;   (defun sr-goto-dir (dir)
;;     "Change the current directory in the active pane to the given one."
;;     (interactive (list (progn
;; 			 (require 'lusty-explorer)
;; 			 (lusty-read-directory))))
;;     (if sr-goto-dir-function
;; 	(funcall sr-goto-dir-function dir)
;;       (unless (and (eq major-mode 'sr-mode)
;; 		   (sr-equal-dirs dir default-directory))
;; 	(if (and sr-avfs-root
;; 		 (null (posix-string-match "#" dir)))
;; 	    (setq dir (replace-regexp-in-string
;; 		       (expand-file-name sr-avfs-root) "" dir)))
;; 	(sr-save-aspect
;; 	 (sr-within dir (sr-alternate-buffer (dired dir))))
;; 	(sr-history-push default-directory)
;; 	(sr-beginning-of-buffer)))))
;; shell-ranger ends here

;; Other
;; #+NAME: shell-other

;; [[file:site-pkgs.org::shell-other][shell-other]]
;; ---( sh-toggle )--------------------------------------------------------------

(use-package sh-toggle
  :disabled t
  :bind ("C-. C-z" . shell-toggle)
  )
;; shell-other ends here

;; Shell/end
;; #+NAME: shell-end

;; [[file:site-pkgs.org::shell-end][shell-end]]
;; }}}  .shell
;; shell-end ends here

;; Sec3et/begin
;; #+NAME: sec3et-begin

;; [[file:site-pkgs.org::sec3et-begin][sec3et-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @SEC3ET
;; ;;;////////////////////////////////////////////////////////////////
;; sec3et-begin ends here

;; Pass
;; #+NAME: sec3et-pass

;; [[file:site-pkgs.org::sec3et-pass][sec3et-pass]]
;; ---( pass )--------------------------------------------------------------

;; @see: https://gitea.petton.fr/nico/emacs.d/src/commit/8ae2b902c916600c9296d967f36ed69ad50e8199/init.el?lang=sv-SE

;; (use-package pass
;;   :mode ("org/reference/password-store/" . pass-view-mode)
;;   :bind ("C-x p" . pass))
;; sec3et-pass ends here

;; Sec3et/end
;; #+NAME: sec3et-end

;; [[file:site-pkgs.org::sec3et-end][sec3et-end]]
;; }}}  .sec3et
;; sec3et-end ends here

;; Evil/begin
;; #+NAME: evil-begin

;; [[file:site-pkgs.org::evil-begin][evil-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @EVIL
;; ;;;////////////////////////////////////////////////////////////////
;; evil-begin ends here

;; Evil Mode
;; #+NAME: evil-mode

;; [[file:site-pkgs.org::evil-mode][evil-mode]]
;; ---( evil )--------------------------------------------------------------

;; @see: https://raw.githubusercontent.com/noctuid/evil-guide/master/README.org

(use-package evil
  :ensure t
  :defer 30
  )
;; evil-mode ends here

;; EVIL/end
;; #+NAME: evil-end

;; [[file:site-pkgs.org::evil-end][evil-end]]
;; }}}  .evil
;; evil-end ends here

;; Jump/begin
;; #+NAME: jump-begin

;; [[file:site-pkgs.org::jump-begin][jump-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @JUMP
;; ;;;////////////////////////////////////////////////////////////////
;; jump-begin ends here

;; Hydra
;; #+NAME: jump-hydra

;; [[file:site-pkgs.org::jump-hydra][jump-hydra]]
;; ;; ---( hydra )--------------------------------------------------------------

(use-package hydra
  :ensure t)
;; jump-hydra ends here

;; Ace
;; #+NAME: jump-ace

;; [[file:site-pkgs.org::jump-ace][jump-ace]]
;; ---( ace )--------------------------------------------------------------


(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))
;; jump-ace ends here

;; Avy
;; #+NAME: jump-avy

;; [[file:site-pkgs.org::jump-avy][jump-avy]]
;; ;; ---( avy )--------------------------------------------------------------

(use-package avy
  :ensure t)
;; jump-avy ends here

;; Jump/end
;; #+NAME: jump-end

;; [[file:site-pkgs.org::jump-end][jump-end]]
;; }}}  .jump
;; jump-end ends here

;; Completion/begin
;; #+NAME: comp-ap-begin

;; [[file:site-pkgs.org::comp-ap-begin][comp-ap-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @COMPLETION "AT POINT"
;; ;;;////////////////////////////////////////////////////////////////
;; comp-ap-begin ends here

;; Company
;; #+NAME: comp-ap-company

;; [[file:site-pkgs.org::comp-ap-company][comp-ap-company]]
;; ---( company )--------------------------------------------------------------

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :bind ("<C-space>" . company-complete)
  :init
  ;; (add-hook 'clojure-mode-hook 'company-mode)
  ;; (add-hook 'cider-repl-mode-hook 'company-mode)
  ;; (add-hook 'lisp-mode-hook 'company-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'company-mode)
  ;; (add-hook 'lisp-interaction-mode-hook 'company-mode)
  ;; (add-hook 'ielm-mode-hook 'company-mode)
  ;; (add-hook 'json-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t)  
  ;; (use-package helm-company :disabled t)
  :hook (
         (text-mode . company-mode)
         (prog-mode . company-mode)
         )
  )

;; @see: https://cloudnine.github.io/science/2020-07-27-emacs-company-mode/
;; @see: https://github.com/mswift42/.emacs.d/blob/master/init.el
;; @see: https://medium.com/helpshift-engineering/configuring-emacs-from-scratch-use-package-c30382297877
;; (use-package company
;;   :bind (:map company-active-map
;;          ("C-n" . company-select-next)
;;          ("C-p" . company-select-previous))
;;   :config
;;   (setq company-idle-delay 0.3)
;;   (global-company-mode t))

  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  ;; (defadvice company-pseudo-tooltip-unless-just-one-frontend
  ;;     (around only-show-tooltip-when-invoked activate)
  ;;   (when (company-explicit-action-p)
  ;;     ad-do-it))
;; comp-ap-company ends here

;; Auto-Complete
;; #+NAME: comp-ap-autocomplete

;; [[file:site-pkgs.org::comp-ap-autocomplete][comp-ap-autocomplete]]
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
;; comp-ap-autocomplete ends here

;; IDO
;; #+NAME: comp-ap-ido

;; [[file:site-pkgs.org::comp-ap-ido][comp-ap-ido]]
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
;; comp-ap-ido ends here

;; Completion/end
;; #+NAME: comp-ap-end

;; [[file:site-pkgs.org::comp-ap-end][comp-ap-end]]
;; }}}  .comp-ap
;; comp-ap-end ends here

;; Completion/begin
;; #+NAME: comp-mb-begin

;; [[file:site-pkgs.org::comp-mb-begin][comp-mb-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @COMPLETION "PROMPT"
;; ;;;////////////////////////////////////////////////////////////////
;; comp-mb-begin ends here

;; Vertico*/begin
;; #+NAME: comp-mb-ver-begin

;; [[file:site-pkgs.org::comp-mb-ver-begin][comp-mb-ver-begin]]
;; ===( vertico )=============================================================

 ;; @see: https://kristofferbalintona.me/posts/202202211546/

(message "#vertico(0): '( (h7/use-vertico . %s) )" (h7/use-vertico))
;; comp-mb-ver-begin ends here

;; Marginalia
;; #+NAME: comp-mb-ver-marginalia

;; [[file:site-pkgs.org::comp-mb-ver-marginalia][comp-mb-ver-marginalia]]
;; ---( marginalia )--------------------------------------------------------------

(use-package marginalia
  :ensure t
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  ;;(marginalia-align 'right)
  :config
  (set-face-attribute 'marginalia-documentation nil :underline nil)
  :init
  (marginalia-mode))

;; (use-package marginalia
;;   :general
;;   (:keymaps 'minibuffer-local-map
;;             "M-A" 'marginalia-cycle)
;;   :custom
;;   (marginalia-max-relative-age 0)
;;   (marginalia-align 'right)
;;   :init
;;   (marginalia-mode))

;; icon fonts: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  )


(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
;; comp-mb-ver-marginalia ends here

;; Vertico
;; #+NAME: comp-mb-ver-vertico

;; [[file:site-pkgs.org::comp-mb-ver-vertico][comp-mb-ver-vertico]]
;; ---( vertico )--------------------------------------------------------------

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; (use-package vertico
;;   :custom
;;   (vertico-count 13)                    ; Number of candidates to display
;;   (vertico-resize t)
;;   (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
;;   :general
;;   (:keymaps 'vertico-map
;;             "<tab>" #'vertico-insert  ; Insert selected candidate into text area
;;             "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
;;             ;; NOTE 2022-02-05: Cycle through candidate groups
;;             "C-M-n" #'vertico-next-group
;;             "C-M-p" #'vertico-previous-group)
;;   :config
;;   (vertico-mode))
;; comp-mb-ver-vertico ends here

;; Consult
;; #+NAME: comp-mb-ver-consult

;; [[file:site-pkgs.org::comp-mb-ver-consult][comp-mb-ver-consult]]
;; ---( consult )--------------------------------------------------------------


  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; @see: https://github.com/minad/consult
  ;; @see: https://gitlab.com/to1ne/temacco/-/blob/main/README.org#L749

  ;; Example configuration for Consult
  (use-package consult
    :ensure t
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("<help> a" . consult-apropos)            ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s d" . consult-find)
           ("M-s D" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
    ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
;; comp-mb-ver-consult ends here

;; Orderless
;; #+NAME: comp-mb-ver-orderless

;; [[file:site-pkgs.org::comp-mb-ver-orderless][comp-mb-ver-orderless]]
;; ---( orderless )--------------------------------------------------------------

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)))

;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless))      ; Use orderless
;;   (completion-category-defaults nil)    ; I want to be in control!
;;   (completion-category-overrides
;;    '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
;;                    orderless)))))
;; comp-mb-ver-orderless ends here

;; Embark
;; #+NAME: comp-mb-ver-embark

;; [[file:site-pkgs.org::comp-mb-ver-embark][comp-mb-ver-embark]]
;; ---( embark )--------------------------------------------------------------

  (use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

   :init

   ;; Optionally replace the key help with a completing-read interface
   (setq prefix-help-command #'embark-prefix-help-command)

   :config

   ;; Hide the mode line of the Embark live/completions buffers
   (add-to-list 'display-buffer-alist
                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters (mode-line-format . none))))

  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package embark
;;   :ensure t)

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t only necessary if you have the hook below
;;   if you want to have consult previews as you move around an
;;   auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))
;; comp-mb-ver-embark ends here

;; Savehist
;; #+NAME: comp-mb-ver-savehist

;; [[file:site-pkgs.org::comp-mb-ver-savehist][comp-mb-ver-savehist]]
;; ---( savehist )--------------------------------------------------------------

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


(recentf-mode)

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; comp-mb-ver-savehist ends here

;; Vertico*/end
;; #+NAME: comp-mb-ver-end

;; [[file:site-pkgs.org::comp-mb-ver-end][comp-mb-ver-end]]
(message "#vertico(0): '( (h7/use-vertico . %s) )" (h7/use-vertico)) 
;; .........................................................................
;; comp-mb-ver-end ends here

;; Completion/end
;; #+NAME: comp-mb-end

;; [[file:site-pkgs.org::comp-mb-end][comp-mb-end]]
;; }}}  .comp-mb
;; comp-mb-end ends here

;; Net/begin
;; #+NAME: net-begin

;; [[file:site-pkgs.org::net-begin][net-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @NET
;; ;;;////////////////////////////////////////////////////////////////
;; net-begin ends here

;; w3m
;; #+NAME: net-web-w3m

;; [[file:site-pkgs.org::net-web-w3m][net-web-w3m]]
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
;; net-web-w3m ends here

;; elfeed
;; #+NAME: net-news-elfeed

;; [[file:site-pkgs.org::net-news-elfeed][net-news-elfeed]]
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
;; net-news-elfeed ends here

;; GNus
;; #+NAME: net-news-gnus

;; [[file:site-pkgs.org::net-news-gnus][net-news-gnus]]
;; ---( GNus )--------------------------------------------------------------

;; @see: https://www.emacswiki.org/emacs/GnusRss
;; net-news-gnus ends here

;; twittering
;; #+NAME: net-twitter-twittering

;; [[file:site-pkgs.org::net-twitter-twittering][net-twitter-twittering]]
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
;; net-twitter-twittering ends here

;; Net/end
;; #+NAME: net-end

;; [[file:site-pkgs.org::net-end][net-end]]
;; }}}  .net
;; net-end ends here

;; Log: finish
;; #+NAME: log-finish

;; [[file:site-pkgs.org::log-finish][log-finish]]
;; vim: noet sw=4 ts=4 fdm=marker foldcolumn=0

;; ---( site.pkgs: end )-------------------------------------------------------
(message "SITE:PKGS - end")
;; log-finish ends here
