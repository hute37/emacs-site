;;; site-pkgs.el --- literal emacs package configuration module in ~/.emacs config -*- lexical-binding: t; -*-

;; Author: ht37 <hute37@gmail.com>
;; URL: https://github.com/hute37/emacs-site

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; included  by site-boot.el
;; tangled from site-pkgs.org

;;; Code:

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
  ;; @see: https://blog.sumtypeofway.com/posts/emacs-config.html
  ;; @see: https://github.com/daedreth/UncleDavesEmacs
  ;; @see: https://github.com/PythonNut/quark-emacs
  ;; @see: https://tecosaur.github.io/emacs-config/config.html
  ;; @see: https://pages.sachachua.com/.emacs.d/Sacha.html
  ;; @see: 

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
  (add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/")) ; Org-mode's repository
  (add-to-list 'package-archives '("gnu"       . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa"     . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu"    . "https://elpa.nongnu.org/nongnu/"))
;;(add-to-list 'package-archives '("jcs-elpa"  . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

  (setq package-archive-priorities '(("melpa"    . 5)
                                  ;; ("jcs-elpa" . 0)
                                     ))
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

;; ---( ... )--------------------------------------------------------------

(require 'bind-key)
(require 'use-package)

;; use-package-ensure-system-package
;; provides way to define system package dependencies for Emacs packages

(use-package use-package-ensure-system-package
  :ensure f)

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

;; I avoid defining too many custom helpers, =dir-concat= is an exception. Emacs
;; 28 provides =file-name-concat=, but I'm on 27.2 some of the time.
(use-package emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))
  (defun dir-mk (dir)
    "ensure existing dir and rec creation if missing"
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir)

  ;; Set directory
  ;; (setq default-directory
  ;;       (cond ((equal (system-name) "surface")
  ;;              "/cygdrive/c/Users/karth/OneDrive/Documents/")
  ;;             ((equal system-type 'nt)
  ;;              "/cygdrive/c/Users/karth/OneDrive/Documents/")
  ;;             (t "~/")))

  ;; Adds ~/.emacs.d to the load-path
  ;; (push (dir-concat user-emacs-directory "plugins/") load-path)
  ;; (push (dir-concat user-emacs-directory "lisp/") load-path)
  ;; (defvar user-cache-directory "~/.cache/emacs/"
  ;; "Location where files created by emacs are placed."))
  
  ;; ---( cache )--------------------------------------------------------------
  
  (defvar user-cache-directory (dir-mk "~/.backups/")
  "Location where files created by emacs are placed.")

  (defvar user-profile-directory (dir-mk "~/.emacs-site/")
  "Location where emacs profiles are placed.")

  (defvar user-plugins-directory (dir-mk "~/.emacs-site/plugins")
  "Location where emacs roeming plugins placed.")


  )

;; ---( autosave/backups )-------------------------------------------------------

;; @see: https://github.com/karthink/.emacs.d/blob/master/init.el#L373

(setq auto-save-interval 2400)
(setq auto-save-timeout 300)
(setq auto-save-list-file-prefix
      (dir-concat user-cache-directory (dir-mk "auto-save-list/.saves-")))
(setq backup-directory-alist
      `(("." . ,(dir-mk (dir-concat user-cache-directory "backup"))))
      backup-by-copying t ; Use copies
      version-control t ; Use version numbers on backups
      delete-old-versions t ; Automatically delete excess backups
      kept-new-versions 10 ; Newest versions to keep
      kept-old-versions 5 ; Old versions to keep
      )

;; ---( undo-tree )--------------------------------------------------------------

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  ;; :bind (("C-c _" . undo-tree-visualize))
  :config
  (progn
    (global-undo-tree-mode)
    ;; (unbind-key "M-_" undo-tree-map)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-history-directory-alist
          `(("." . ,(dir-mk (dir-concat user-cache-directory "undo-tree")))))))

;; =C-x u= to browse the tree with =f=, =b=, =n=, =p=, =RET=.
;; (use-package vundo
;;   :ensure t
;;   :config
;;   (setq vundo-glyph-alist vundo-unicode-symbols)
;;   :bind
;;   ("C-x u" . vundo))


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



;; }}}  .packages
;; basic ends here

;; Config

;; #+NAME: config

;; [[file:site-pkgs.org::config][config]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @CONFIG
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ... )--------------------------------------------------------------

;; Enable local variables
(setq-default enable-local-variables t)

;; For lazy typists
(setq use-short-answers t)

;; Move the mouse away if the cursor gets close
;; (mouse-avoidance-mode 'animate)

;; highlight the current line, as in Matlab
;; (global-hl-line-mode)

;; Confirm when killing Emacs
;; (setq confirm-kill-emacs (lambda (prompt)
;;                            (y-or-n-p-with-timeout prompt 2 nil)))


;; ---( ... )--------------------------------------------------------------


;; }}}  .packages
;; config ends here

;; System

;; #+NAME: system

;; [[file:site-pkgs.org::system][system]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @SYSTEM
;; ;;;////////////////////////////////////////////////////////////////

;; ---( ... )--------------------------------------------------------------

;; tune gc
(setq gc-cons-threshold 100000000)

;; ---( ... )--------------------------------------------------------------


;; }}}  .packages
;; system ends here

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

  ;; icon fonts: M-x all-the-icons-install-fonts
  (use-package all-the-icons
    :ensure t
    )

   (use-package solaire-mode
     :ensure t
     :init (solaire-global-mode +1))


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


  ;; }}}  .ui
;; ui ends here

;; Utils/begin
;; #+NAME: utils-begin

;; [[file:site-pkgs.org::utils-begin][utils-begin]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @UTIL
  ;; ;;;////////////////////////////////////////////////////////////////
;; utils-begin ends here

;; Utils/Jump
;; #+NAME: util-jump

;; [[file:site-pkgs.org::util-jump][util-jump]]
    ;; ---( hydra )--------------------------------------------------------------

    (use-package hydra
      :ensure t
      :commands defhydra
      )

    (use-package use-package-hydra
      :ensure t
      :after hydra
      )

    ;; ---( ace )--------------------------------------------------------------


    (use-package ace-jump-mode
      :ensure t
      :commands ace-jump-mode
      :init
      ;; (bind-key "C-." 'ace-jump-mode)
      )

    ;; ;; ---( avy )--------------------------------------------------------------

    (use-package avy
      :ensure t)
;; util-jump ends here

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

  (use-package which-key
    :delight
    :ensure t
    :init
    (which-key-mode)
    :config
    (setq which-key-idle-delay 1))



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
  ;; ---( recentf )--------------------------------------------------------------

(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  ;;:hook (after-init . recentf-mode)  
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 25
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (dolist (el '("~/\\.emacs.bmk\\'"
                "/tmp/.*\\'"
                "~/\\.elfeed/index\\'" ))
    (add-to-list 'recentf-exclude el))  
  (recentf-mode t)
  :diminish nil)

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


  ;; ---( visible-mark )--------------------------------------------------------------

  (use-package visible-mark
    :ensure t
    :init
    (global-visible-mark-mode)
    :custom
    (visible-mark-faces '(visible-mark-face1 visible-mark-face2))
    (visible-mark-forward-faces '(visible-mark-face1 visible-mark-face2))
    (visible-mark-max 2))

  ;; ---( changes )--------------------------------------------------------------

  (use-package hilit-chg
    :ensure t
    :bind
    ("C-x M-u" . highlight-changes-mode))

  (use-package goto-chg
    :ensure t
    :bind
    ("C-x M-u" . goto-last-change))


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
;; util-misc ends here

;; Utils/Tools
;; #+NAME: util-tools

;; [[file:site-pkgs.org::util-tools][util-tools]]
  ;; ---( calc )--------------------------------------------------------------

  (use-package calc
    :ensure t
    :custom
    (calc-highlight-selections-with-faces t)
    ;; :bind
    ;; ("C-M-=" . #'calc)
    ;; ("M-#" . #'quick-calc)
    ;; ("M-~" . #'calc-embedded)
    )


  ;; ---( crux )--------------------------------------------------------------

  (use-package crux
    :ensure t
    ;; :bind
    ;; (("C-a" . crux-move-beginning-of-line)
    ;;  ("C-x 4 t" . crux-transpose-windows)
    ;;  ("C-x K" . crux-kill-other-buffers)
    ;;  ("C-k" . crux-smart-kill-line)
    ;;  ("M-o" . crux-other-window-or-switch-buffer)
    ;;  ("C-<backspace>" . crux-kill-line-backwards)
    ;;  ("C-c d" . crux-duplicate-current-line-or-region)
    ;;  ("C-c e" . crux-eval-and-replace)
    ;;  ("C-c M-r" . crux-rename-file-and-buffer)
    ;;  ("C-c I" . crux-find-user-init-file))
    :config
    (crux-with-region-or-buffer indent-region)
    (crux-with-region-or-buffer untabify)
    (crux-with-region-or-point-to-eol kill-ring-save))
;; util-tools ends here

;; Utils/end
;; #+NAME: util-end

;; [[file:site-pkgs.org::util-end][util-end]]
  ;; }}}  .util
;; util-end ends here

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

;; Fontaine
;; #+NAME: fonts-fontain

;; [[file:site-pkgs.org::fonts-fontain][fonts-fontain]]
  ;; ---( fontaine )--------------------------------------------------------------

  ;; @see: https://protesilaos.com/emacs/fontaine#h:031b9bea-d42b-4be0-82c7-42712cde94cc
  (use-package fontaine
    :disabled t
    ;;:ensure t
    :config

    (setq fontaine-latest-state-file
          (locate-user-emacs-file "fontaine-latest-state.eld"))

    ;; Iosevka Comfy is my highly customised build of Iosevka with
    ;; monospaced and duospaced (quasi-proportional) variants as well as
    ;; support or no support for ligatures:
    ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
    ;;
    ;; Iosevka Comfy            == monospaced, supports ligatures
    ;; Iosevka Comfy Fixed      == monospaced, no ligatures
    ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
    ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
    ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
    (setq fontaine-presets
          '((tiny
             :default-family "Iosevka" ; "Iosevka Comfy Wide Fixed"
             :default-height 110)
            (small
             :default-family "Iosevka" ; "Iosevka Comfy Fixed"
             :default-height 140)
            (regular
             :default-family "Iosevka Nerd Font"
             :default-height 180)
            (medium
             :default-family "Iosevka Nerd Font"
             :default-height 160)
            (large
             :default-weight semilight
             :default-height 240
             :bold-weight extrabold)
            (presentation
             :default-weight semilight
             :default-height 280
             :bold-weight extrabold)
            (jumbo
             :default-weight semilight
             :default-height 320
             :bold-weight extrabold)
            (t
             ;; I keep all properties for didactic purposes, but most can be
             ;; omitted.  See the fontaine manual for the technicalities:
             ;; <https://protesilaos.com/emacs/fontaine>.
             :default-family "Iosevka" ; "Iosevka Comfy"
             :default-weight regular
             :default-height 140
             :fixed-pitch-family nil ; falls back to :default-family
             :fixed-pitch-weight nil ; falls back to :default-weight
             :fixed-pitch-height 1.0
             :fixed-pitch-serif-family nil ; falls back to :default-family
             :fixed-pitch-serif-weight nil ; falls back to :default-weight
             :fixed-pitch-serif-height 1.0
             :variable-pitch-family "Noto Sans Condensed" ; "Iosevka Comfy Duo"
             :variable-pitch-weight nil
             :variable-pitch-height 1.0
             :bold-family nil ; use whatever the underlying face has
             :bold-weight bold
             :italic-family nil
             :italic-slant italic
             :line-spacing nil)))

    ;; Recover last preset or fall back to desired style from
    ;; `fontaine-presets'.
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))


    ;; The other side of `fontaine-restore-latest-preset'.
    (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

    ;; fontaine does not define any key bindings.  This is just a sample that
    ;; respects the key binding conventions.  Evaluate:
    ;;
    ;;     (info "(elisp) Key Binding Conventions")
    (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
    (define-key global-map (kbd "C-c F") #'fontaine-set-face-font)    
    )
;; fonts-fontain ends here

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

;; Completion/begin
;; #+NAME: comp-ap-begin

;; [[file:site-pkgs.org::comp-ap-begin][comp-ap-begin]]
;; ;;;////////////////////////////////////////////////////////////////
;; {{{  @COMPLETION "AT POINT"
;; ;;;////////////////////////////////////////////////////////////////
;; comp-ap-begin ends here

;; Corfu
;; #+NAME: comp-ap-corfu

;; [[file:site-pkgs.org::comp-ap-corfu][comp-ap-corfu]]
  ;; ---( corfu )--------------------------------------------------------------

  ;; @see: https://github.com/minad/corfu/
  ;; @see: https://protesilaos.com/emacs/dotemacs#h:15edf2c3-4419-4101-928a-6e224958a741

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  )
;; comp-ap-corfu ends here

;; Company
;; #+NAME: comp-ap-company

;; [[file:site-pkgs.org::comp-ap-company][comp-ap-company]]
;; ---( company )--------------------------------------------------------------


;; (with-eval-after-load 'company
;;   (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common))

(use-package company
  :ensure t
  :diminish company-mode
  :commands company-mode
  :bind ("C-c C-SPC" . company-complete)
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t)  
  :hook (
         (text-mode . company-mode)
         (prog-mode . company-mode)
         )
  )
(use-package company-posframe
  :ensure t
  :init
  (company-posframe-mode 1)
  :diminish
  )


;; (use-package company
;;   :enabled t
;; ;; :disabled t
;;   :diminish company-mode
;;   :commands company-mode
;;   :bind ("C-c C-SPC" . company-complete)
;;   :init
;;   ;; (add-hook 'clojure-mode-hook 'company-mode)
;;   ;; (add-hook 'cider-repl-mode-hook 'company-mode)
;;   ;; (add-hook 'lisp-mode-hook 'company-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'company-mode)
;;   ;; (add-hook 'lisp-interaction-mode-hook 'company-mode)
;;   ;; (add-hook 'ielm-mode-hook 'company-mode)
;;   ;; (add-hook 'json-mode-hook 'company-mode)
;;   :config
;;   (setq company-idle-delay 0.3)
;;   (global-company-mode t)  
;;   ;; (use-package helm-company :disabled t)
;;   :hook (
;;          (text-mode . company-mode)
;;          (prog-mode . company-mode)
;;          )
;;   )

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

;; @see: https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/
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
  ;;:custom
  ;;(marginalia-max-relative-age 0)
  ;;(marginalia-align 'right)
  :config
  (set-face-attribute 'marginalia-documentation nil :underline nil)
  (marginalia-mode 1)
  )

;; (use-package marginalia
;;   :general
;;   (:keymaps 'minibuffer-local-map
;;             "M-A" 'marginalia-cycle)
;;   :custom
;;   (marginalia-max-relative-age 0)
;;   (marginalia-align 'right)
;;   :init
;;   (marginalia-mode))


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
  (vertico-mode 1))

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
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
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
  ;; :demand t ; only necessary if you have the hook below
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

;; Edit/begin
;; #+NAME: edit-begin

;; [[file:site-pkgs.org::edit-begin][edit-begin]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @EDIT
  ;; ;;;////////////////////////////////////////////////////////////////
;; edit-begin ends here

;; Writer
;; #+NAME: writer

;; [[file:site-pkgs.org::writer][writer]]
  ;; ---( olivetti )--------------------------------------------------------------

  (use-package olivetti
    :ensure t
    :custom (olivetti-body-width 92)
    ;;:hook (org-mode . olivetti-mode)
    :bind ("C-c M-o" . olivetti-mode)
    )
;; writer ends here

;; Edit/end
;; #+NAME: edit-end

;; [[file:site-pkgs.org::edit-end][edit-end]]
  ;; }}}  .edit
;; edit-end ends here

;; Magit
;; #+NAME: magit

;; [[file:site-pkgs.org::magit][magit]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @MAGIT
  ;; ;;;////////////////////////////////////////////////////////////////


  ;; ---( magit )--------------------------------------------------------------

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
    :defer t
    :bind
    ("<C-i> h" . git-timemachine))

  ;; (use-package git-timemachine
  ;;   :ensure t
  ;;   :bind (("s-g" . git-timemachine)))

  ;; ---( git-gutter-fringe )-----------------------------------------------------------

  (use-package git-gutter-fringe
    :ensure t
    :hook ((prog-mode     . git-gutter-mode)
           (yaml-mode     . git-gutter-mode)
           (org-mode      . git-gutter-mode)
           (markdown-mode . git-gutter-mode)
           (latex-mode    . git-gutter-mode)))




  ;; ---( vdiff )------------------------------------------------------------

  (use-package vdiff
    :ensure t)

  (use-package vdiff-magit
    :disabled t
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

  ;; @see: https://protesilaos.com/codelog/2023-06-26-emacs-file-dired-basics/

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

    (dired-by-moving-to-trash t)
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
    
    ;; Teach Dired to use a specific external program with either the
    ;; `dired-do-shell-command' or `dired-do-async-shell-command' command
    ;; (with the default keys, those are bound to `!' `&', respectively).
    ;; The first string is a pattern match against file names.  The
    ;; remaining strings are external programs that Dired will provide as
    ;; suggestions.  Of course, you can always type an arbitrary program
    ;; despite these defaults.
    (setq dired-guess-shell-alist-user
          '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
            ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
	    (".*" "xdg-open")))
    )

  ;; find-dired: dired find
  (use-package find-dired
    ;; :straight (:type built-in)
    ;; :ensure t 
    :custom
    (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
    )

  ;; peep-dired: file preview
  (use-package dired-preview
    :ensure t 
    :custom
    (setq dired-preview-delay 0.7)
    (setq dired-preview-max-size (expt 2 20))
    (setq dired-preview-ignored-extensions-regexp
          (concat "\\."
                  "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                  "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                  "\\|iso\\|epub\\|pdf\\)"))

    (defun my-dired-preview-to-the-right ()
      "My preferred `dired-preview-display-action-alist-function'."
      '((display-buffer-in-side-window)
        (side . right)
        (width . 0.6)))

    (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
    
    
    ;; Enable `dired-preview-mode' in a given Dired buffer or do it
    ;; globally:
    ;; (dired-preview-global-mode 1)
    )

  ;; ;; peep-dired: file preview
  ;; (use-package peep-dired
  ;;   :ensure t 
  ;;   :bind (:map peep-dired-mode-map
  ;;             ("SPC" . nil)
  ;;             ("<backspace>" . nil)))

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
    (make-directory (emacs-d "var") t)
    :config
    (setq projectile-indexing-method 'hybrid
          projectile-enable-caching t
          projectile-cache-file (emacs-d "var/projectile.cache")
          projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld"))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-file-suffixes "o")
    (add-to-list 'projectile-globally-ignored-file-suffixes "pyc")
    (add-to-list 'projectile-globally-ignored-file-suffixes "class")
    (add-to-list 'projectile-globally-ignored-directories "logs")
    (add-to-list 'projectile-globally-ignored-directories "home")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories ".yarn")
    (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
    (add-to-list 'projectile-globally-ignored-directories "venv")
    (add-to-list 'projectile-globally-ignored-directories "*__pycache__")
    (add-to-list 'projectile-globally-ignored-directories "*.ipynb_checkpoints")
    (add-to-list 'projectile-globally-ignored-directories "*.virtual_documents")
    (add-to-list 'projectile-globally-ignored-directories "*.obsidian/")
    (projectile-global-mode)
    )

  (use-package consult-projectile
    :ensure t
  )




  ;; ---( treemacs )--------------------------------------------------------------

  ;; Provides workspaces with file browsing (tree file viewer)
  ;; and project management when coupled with `projectile`.

  (use-package treemacs
    :ensure t
    :defer t
    :config
    (setq treemacs-no-png-images t
            treemacs-width 24)

    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (set-face-attribute face nil :family "PT Sans Narrow" :height 120))
   
    :bind ("C-c t" . treemacs))

  (use-package treemacs-projectile
    :after treemacs projectile
    :ensure t)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-all-the-icons
    :after treemacs
    :ensure t
    :config (treemacs-load-theme "all-the-icons"))

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


;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
  (use-package wgrep
    :ensure t
    :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit))
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

  ;; @see: https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/eshell
  ;;
  ;; @see: https://config.phundrak.com/emacs/packages/emacs-builtin.html#eshell
  ;;
  ;; ```
  ;; cd /usr/share/emacs/[23]*/lisp/eshell
  ;; he=4; ls *.gz | xargs -I{} bash -c 'echo "#>>({})#####"; zcat {}; echo "#<<({})#####"' | less -SRX
  ;; he=5; ls *.gz | xargs -I{} bash -c 'zcat {} | bat -l lisp --file-name={} --color=always;' | less -SRX
  ;; ```

  ;; (use-package esh-toggle
  ;;   :ensure t
  ;;   :bind ("C-x C-h" . eshell-toggle))
  (use-package xterm-color
    :ensure t
    :commands (xterm-color-filter))

  (use-package eshell
    :after (esh-mode)
    ;;:after (esh-mode xterm-color)
    ;;:after (xterm-color)
    :ensure t
    :commands (eshell eshell-command)
    :preface
    (message "eshell::preface >")


    (defun eshell-initialize ()
      (message "eshell:initialize >")
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
           (unintern 'eshell/sudo nil)))


      (message "eshell:initialize <"))
    
    (message "eshell:helpers >")

    (defun eshell-new ()
      "Open a new instance of eshell."
      (interactive)
      (eshell 'N))
    

    (defun eshell-here (&optional prefix)
      "Opens up a new shell in the directory associated with the
       current buffer's file. The eshell is renamed to match that
       directory to make multiple eshell windows easier."
      (interactive "P")
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (name   (car (last (split-string parent "/" t)))))

        (cond
         ((equal current-prefix-arg nil)   ; no C-u
          (message "eshell-here - no C-u"))
         ((equal current-prefix-arg '(4))  ; C-u
          (split-window-horizontally (- (/ (window-total-width) 2)))
          (other-window 1)
          (message "eshell-here - C-u"))
         ((equal current-prefix-arg '(16))     ; C-u C-u
          (split-window-vertically (- (/ (window-total-height) 3)))
          (other-window 1)
          (message "eshell-here - C-u C-u"))
          )        
        
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))
        ;;(insert (concat "ls"))
        ;;(eshell-send-input)
        ))

    (defun ha/eshell-host-regexp (regexp)
      "Returns a particular regular expression based on symbol, REGEXP"
      (let* ((user-regexp      "\\(\\([[:alpha:].]+\\)@\\)?")
             (tramp-regexp     "\\b/ssh:[:graph:]+")
             (ip-char          "[[:digit:]]")
             (ip-plus-period   (concat ip-char "+" "\\."))
             (ip-regexp        (concat "\\(\\(" ip-plus-period "\\)\\{3\\}" ip-char "+\\)"))
             (host-char        "[[:alpha:][:digit:]-]")
             (host-plus-period (concat host-char "+" "\\."))
             (host-regexp      (concat "\\(\\(" host-plus-period "\\)+" host-char "+\\)"))
             (horrific-regexp  (concat "\\b"
                                       user-regexp ip-regexp
                                       "\\|"
                                       user-regexp host-regexp
                                       "\\b")))
        (cond
         ((eq regexp 'tramp) tramp-regexp)
         ((eq regexp 'host)  host-regexp)
         ((eq regexp 'full)  horrific-regexp))))    

    (defun eshell-there (host)
      "Creates an eshell session that uses Tramp to automatically
       connect to a remote system, HOST.  The hostname can be either the
       IP address, or FQDN, and can specify the user account, as in
       root@blah.com. HOST can also be a complete Tramp reference."
      (interactive "sHost: ")

      (let* ((default-directory
              (cond
               ((string-match-p "^/" host) host)

               ((string-match-p (ha/eshell-host-regexp 'full) host)
                (string-match (ha/eshell-host-regexp 'full) host) ;; Why!?
                (let* ((user1 (match-string 2 host))
                       (host1 (match-string 3 host))
                       (user2 (match-string 6 host))
                       (host2 (match-string 7 host)))
                  (if host1
                      (ha/eshell-host->tramp user1 host1)
                    (ha/eshell-host->tramp user2 host2))))

               (t (format "/%s:" host)))))
        (eshell-here)))    
    
    (bind-key "C-!" 'eshell-here)
    
    (message "eshell:helpers <")
    

    (message "eshell:builtins >")

    ;; @see: https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/eshell/esh-cmd.el
    ;; @see: https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org
    ;; @see: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

    (defun eshell/read-file (file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (buffer-string)))

    (defun eshell/do (&rest args)
      "Execute commands over lst. do chmod -x :: *.csv(x) "
      (seq-let (cmd lst) (-split-on "::" args)
        (dolist (file
                 (flatten-list (append lst)))
          (add-to-list 'cmd file)
          (eshell-named-command
           (car cmd) (cdr cmd)))))

    (defun eshell-fn-on-files (fun1 fun2 args)
      "Call FUN1 on the first element in list, ARGS.
         Call FUN2 on all the rest of the elements in ARGS."
      (unless (null args)
        (let ((filenames (flatten-list args)))
          (funcall fun1 (car filenames))
          (when (cdr filenames)
            (mapcar fun2 (cdr filenames))))
        ;; Return an empty string, as the return value from `fun1'
        ;; probably isn't helpful to display in the `eshell' window.
        ""))

    (defun eshell/cab (&rest args)
      (if args
          (if (bufferp (car args))
              (with-current-buffer (car args)
                (buffer-string))
            (apply #'eshell/cat args))
        (eshell/cab (eshell/o))))

    (defun eshell/o (&rest args)
      (if (stringp (car args))
          (get-buffer-create (car args))
        (get-buffer-create "*scratch*")))

    (defun eshell/s (&rest files)
      "Essentially an alias to the `view-file' function."
      (eshell-fn-on-files 'view-file 'view-file-other-window files))

    (defalias 'eshell/more 'eshell/s)

    (defun eshell/e (&rest files)
      "Essentially an alias to the `find-file' function."
      (eshell-fn-on-files 'find-file 'find-file-other-window files))

    (defun eshell/ee (&rest files)
      "Edit one or more files in another window."
      (eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

    (defun eshell/gst (&rest args)
      (magit-status (pop args) nil)
      (eshell/echo))

    (defun eshell/ccat (file)
      "Like `cat' but output with Emacs syntax highlighting,
       as alternative: `find-file-read-only-other-window`."
      (with-temp-buffer
        (insert-file-contents file)
        (let ((buffer-file-name file))
          (delay-mode-hooks
            (set-auto-mode)
            (if (fboundp 'font-lock-ensure)
                (font-lock-ensure)
              (with-no-warnings
                (font-lock-fontify-buffer)))))
        (buffer-string)))

    (defun eshell/f (filename &optional dir try-count)
      "Searches for files matching FILENAME in either DIR or the
       current directory. Just a typical wrapper around the standard
       `find' executable.

       Since any wildcards in FILENAME need to be escaped, this wraps the shell command.

       If not results were found, it calls the `find' executable up to
       two more times, wrapping the FILENAME pattern in wildcat
       matches. This seems to be more helpful to me."
      (let* ((cmd (concat
               (executable-find "find")
               " " (or dir ".")
               "      -not -path '*/.git*'"
               " -and -not -path '*node_modules*'"
               " -and -not -path '*classes*'"
               " -and "
               " -type f -and "
               "-iname '" filename "'"))
             (results (shell-command-to-string cmd)))

        (if (not (s-blank-str? results))
            results
          (cond
           ((or (null try-count) (= 0 try-count))
            (eshell/f (concat filename "*") dir 1))
           ((or (null try-count) (= 1 try-count))
            (eshell/f (concat "*" filename) dir 2))
           (t "")))))

    (defun eshell/ef (filename &optional dir)
      "Searches for the first matching filename and loads it into a
       file to edit."
      (let* ((files (eshell/f filename dir))
             (file (car (s-split "\n" files))))
        (find-file file)))

    (defun eshell/z ()
      (eshell/echo)
      (eshell/exit))
    

    (defalias 'eshell/emacs 'eshell/e)
    (defalias 'eshell/v 'eshell/e)
    (defalias 'eshell/t 'eshell-exec-visual)

    (message "eshell:builtins <")
    (message "eshell:hooks >")
    (add-hook 'eshell-first-time-mode-hook #'eshell-initialize)
    (add-hook 'eshell-mode-hook #'eshell-setup-keymap)
    (message "eshell:hooks <")
    (message "eshell::preface <")
    :init
    (message "eshell::init >")
    (message "eshell:hooks/b >")
    (add-hook 'eshell-first-time-mode-hook #'eshell-initialize)
    (add-hook 'eshell-mode-hook #'eshell-setup-keymap)
    (message "eshell:hooks/b <")
    (message "eshell::init <")
    :config
    (message "eshell::config >")
    (setq
     eshell-login-script "~/.emacs-site/config/eshell/eprofile"
     eshell-rc-script "~/.emacs-site/config/eshell/eshellrc"
     eshell-aliases-file "~/.emacs-site/config/eshell/ealiases"
     eshell-history-size 5000
     eshell-buffer-maximum-lines 5000
     eshell-hist-ignoredups t
     eshell-save-history-on-exit t
     eshell-prefer-lisp-functions t
     eshell-scroll-to-bottom-on-input t
     eshell-destroy-buffer-when-process-dies t
     ;;eshell-visual-commands'("bash" "fish" "vi" "vim" "nvim" "mc" "ranger" "htop" "ssh" "top" "tmux" "zsh")
     eshell-visual-commands'("fish" "vi" "vim" "nvim" "mc" "ranger" "htop" "ssh" "top" "tmux")

     eshell-ls-use-colors t
     eshell-ls-initial-args nil

     )

    (setq
     eshell-prompt-regexp "^[^#$γλ\n]* [#$γλ] "
     eshell-prompt-function
     (lambda ()
       (let*
           ((path (abbreviate-file-name (eshell/pwd)))
            (parts (s-split "|" (replace-regexp-in-string "^\\(.*:\\)?\\(.*\\)" "\\1|\\2" path)))
            (rhost (car parts))
            (path3 (s-join "/" (last (s-split "/" (cadr parts)) 3)))
            )
            (concat
             (propertize rhost 'face `(:foreground "Salmon" :weight bold))
             (propertize path3 'face `(:foreground "CornflowerBlue" :weight bold))
             (if (= (user-uid) 0) " γ " " λ ")))
       ))
    
      ;; (setq eshell-prompt-regexp "^[^#$γλ\n]* [#$γλ] "
      ;;       eshell-prompt-function
      ;;       (lambda ()
      ;;         (concat
      ;;          (propertize "[" 'face `(:foreground "Salmon" :weight bold))
      ;;          (propertize (user-login-name) 'face `(:foreground "CornflowerBlue" :weight bold))
      ;;          (propertize "@" 'face `(:foreground "CornflowerBlue" :weight bold))
      ;;          (propertize (system-name) 'face `(:foreground "CornflowerBlue" :weight bold))
      ;;          (propertize " " 'face `(:foreground "gray"))
      ;;          (propertize (if (string= (eshell/pwd) (getenv "HOME"))
      ;;                          "~" (eshell/basename (eshell/pwd)))
      ;;                      'face `(:foreground "DarkTurquoise" :weight bold))
      ;;          (propertize "]" 'face `(:foreground "Salmon" :weight bold))
      ;;          (propertize " " 'face 'default)
      ;;          (propertize (if (= (user-uid) 0) "γ" "λ") 'face `(:foreground "Salmon" :weight bold))
      ;;          (propertize " " 'face 'default)
      ;;          )))


      ;; (setq eshell-output-filter-functions
      ;;       (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
      ;; ;;



    ;; (require 'eshell)
    (require 'em-smart)
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
    (setq eshell-smart-space-goes-to-end t)



    ;; ;; We want to use xterm-256color when running interactive commands
    ;; ;; in eshell but not during other times when we might be launching
    ;; ;; a shell command to gather its output.
    ;; (add-hook 'eshell-pre-command-hook
    ;;           (lambda () (setenv "TERM" "xterm-256color")))
    ;; (add-hook 'eshell-post-command-hook
    ;;           (lambda () (setenv "TERM" "dumb")))

    (defun eshell-clear-buffer ()
      "Clear terminal"
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

    (defun eshell-copy-or-send-input (arg)
      "Copy selection before sending input"
      (interactive "P")
      (require 'em-smart)
      (when mark-active
        (cua-copy-region arg))
      (if (or current-prefix-arg
              (and (> (point) eshell-last-input-start)
                   (< (point) eshell-last-input-end))
              (>= (point) eshell-last-output-end))
          (eshell-send-input)
        (eshell-smart-goto-end)))


    (defun eshell-setup-keymap ()
      "Setup eshell (local) keymap"
      (interactive)
      (message "eshell:setup-keymap >")

      (local-set-key (kbd "C-l") 'eshell-clear-buffer)
      ;; (unbind-key (kbd "<up>") eshell-mode-map)
      ;; (unbind-key (kbd "<down>") eshell-mode-map)
      ;; (define-key eshell-mode-map (kbd "C-<up>") 'eshell-previous-matching-input-from-input)
      ;; (define-key eshell-mode-map (kbd "C-<down>") 'eshell-previous-matching-input-from-input)
      ;; (define-key eshell-mode-map (kbd "<up>") 'previous-line)
      ;; (define-key eshell-mode-map (kbd "<down>") 'next-line)
      ;; (local-set-key (kbd "<up>") #'previous-line)
      ;; (local-set-key (kbd "<down>") #'next-line)
      ;; (define-key eshell-mode-map (kbd "<up>") 'previous-line)
      ;; (define-key eshell-mode-map (kbd "<down>") 'next-line)
      (define-key eshell-hist-mode-map (kbd "<up>") #'previous-line)
      (define-key eshell-hist-mode-map (kbd "<down>") #'next-line)
      (define-key eshell-hist-mode-map (kbd "C-<up>") #'eshell-previous-matching-input-from-input)
      (define-key eshell-hist-mode-map (kbd "C-<down>") #'eshell-next-matching-input-from-input)
      (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
      ;; Use completion-at-point to provide completions in eshell
      (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)
      (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
      (define-key eshell-mode-map (kbd "<return>") 'eshell-copy-or-send-input)
      (define-key eshell-mode-map (kbd "C-<return>") 'cua-rectangle-mark-mode)
      (define-key eshell-mode-map (kbd "C-d") 'self-insert-command)
      (message "eshell:setup-keymap <")

      (eshell-smart-initialize)
      (message "*eshell*")
      )

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (message "eshell:hooks/c >")
    (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
    (add-hook 'eshell-mode-hook
              (lambda ()
                (progn
                  (setq O (eshell/o))
                  (setenv "PAGER" "cat")
                  (setenv "TERM" "xterm-256color")
                  )))
    (add-hook 'eshell-mode-hook #'eshell-setup-keymap)
    (add-hook 'eshell-mode-hook #'(lambda () (message "*eshell*")))
    (message "eshell:hooks/c <")

    (message "eshell::config <")
    )


  ;; ---( eshell-toggle )--------------------------------------------------------------

  (use-package eshell-toggle
    ;;:after eshell-mode
    :ensure t
    :custom
    (eshell-toggle-size-fraction 3)
    (eshell-toggle-find-project-root-package t) ;; for projectile
    ;;(eshell-toggle-find-project-root-package 'projectile) ;; for projectile
    ;;(eshell-toggle-use-projectile-root 'project) ;; for in-built project.el
    (eshell-toggle-run-command nil)
    (eshell-toggle-init-function #'eshell-toggle-init-eshell)
    ;; (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
    ;; (eshell-toggle-init-function #'eshell-toggle-init-tmux)
    :quelpa
    (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
    :bind
    ("C-~" . eshell-toggle))

  ;; ---( eshell-syntax-hl )--------------------------------------------------------------

  ;; @see: https://github.com/akreisher/eshell-syntax-highlighting/

  (use-package eshell-syntax-highlighting
    :after eshell-mode
    :ensure t
    :config
    ;; Enable in all Eshell buffers.
    (eshell-syntax-highlighting-global-mode +1))

  ;; ---( eshell-vterm )--------------------------------------------------------------

  (use-package eshell-vterm
    :disabled t
    ;; :ensure t
    :demand t
    :after eshell
    :config
    (eshell-vterm-mode))

  ;; ---( eat )--------------------------------------------------------------

  ;;
  (use-package eat
    ;;:disabled t
    :ensure t
    ;;:hook (eshell-load . eat-eshell-mode)
    :hook (eshell-load . eat-eshell-visual-command-mode)
    :quelpa ((eat
              :fetcher git
              :url "https://codeberg.org/akib/emacs-eat"
              :files ("*.el" ("term" "term/*.el") "*.texi"
                      "*.ti" ("terminfo/e" "terminfo/e/*")
                      ("terminfo/65" "terminfo/65/*")
                      ("integration" "integration/*")
                      (:exclude ".dir-locals.el" "*-tests.el"))))
    )
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
    :ensure t
    :bind (("C-<F9>" . vterm-here)
               ;; :straight (:post-build (cl-letf (((symbol-function #'pop-to-buffer)
               ;;                        (lambda (buffer) (with-current-buffer buffer (message (buffer-string))))))
               ;;               (setq vterm-always-compile-module t)
               ;;               (require 'vterm)))
           :map vterm-mode-map
           ("C-v" . vterm-yank)
           ("S-<insert>" . vterm-yank)
           ([kp-insert] . vterm-yank-primary)
           ([kp-enter] . vterm-yank)
           ([kp-divide] . vterm-yank-pop)
           ([kp-multiply] . vterm-copy-mode))
    :config
    (setq vterm-max-scrollback 18000)
    :init
    (message "vterm::init >")

    (defun vterm-here (&optional prefix)
      "Opens up a new shell in the directory associated with the
       current buffer's file. The vterm is renamed to match that
       directory to make multiple vterm windows easier."
      (interactive "P")
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             ;;(name   (car (last (split-string parent "/" t))))
             (name (s-join
               "/"
               (last
                (s-split
                 "/"
                 (abbreviate-file-name parent))
                5)))
             )

        (cond
         ((equal current-prefix-arg nil)   ; no C-u
          (message "vterm-here - no C-u"))
         ((equal current-prefix-arg '(4))  ; C-u
          (split-window-horizontally (- (/ (window-total-width) 2)))
          (other-window 1)
          (message "vterm-here - C-u"))
         ((equal current-prefix-arg '(16))     ; C-u C-u
          (split-window-vertically (- (/ (window-total-height) 3)))
          (other-window 1)
          (message "vterm-here - C-u C-u"))
          )        
        
        (vterm (concat "*vterm: " name "*"))
        ))

    
    (message "vterm::init <")

    )


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


  ;; ---( shellcheck )--------------------------------------------------------------

  ;; @see: https://github.com/federicotdn/flymake-shellcheck
  ;; alt-x flymake-mode
  ;; (use-package flymake-shellcheck
  ;;   :commands flymake-shellcheck-load
  ;;   :init
  ;;   (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
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


  (use-package pandoc-mode
    :ensure t
    :hook (markdown-mode . pandoc-mode)
    )
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

;; Lang: LSP.setup
;; #+NAME: lang-lsp.setup

;; [[file:site-pkgs.org::lang-lsp.setup][lang-lsp.setup]]
  ;; ---( lsp-setup )------------------------------------------------------------

(defun h7/lsp-setup ()

  ;;lsp server install

  ;; @see
  (message "https://emacs-lsp.github.io/lsp-mode/page/languages/")

  ;; python
  (lsp-install-server 'pyright) 
  ;; powershell
  (lsp-install-server 'pwsh-ls) 
  ;; html
  (lsp-install-server 'html-ls) 
  ;; css
  (lsp-install-server 'css-ls) 
  ;; json
  (lsp-install-server 'json-ls)
  ;; graphql
  (lsp-install-server 'graphql-ls) 
  ;; dockerfile
  (lsp-install-server 'dockerfile-ls) 
  ;; bash
  (lsp-install-server 'bash-ls) 
  ;; ansible
  (lsp-install-server 'ansible-ls) 
  ;; yaml
  (lsp-install-server 'yamlls) 

  
  ;; prolog
  (message "swipl -g 'pack_install(lsp_server).")
  )
;; lang-lsp.setup ends here

;; Lang: LSP.mode
;; #+NAME: lang-lsp.mode

;; [[file:site-pkgs.org::lang-lsp.mode][lang-lsp.mode]]
  ;; ---( flycheck )------------------------------------------------------------

  (use-package flycheck
    :ensure t
    :init (global-flycheck-mode)
    )

  ;; ---( LSP mode )------------------------------------------------------------

  (use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "C-l")
    :hook (
           (python-mode . lsp-deferred)
           )
    :commands (lsp lsp-deferred)
    :config
    (dolist (dir '(
                   "[/\\\\]\\.cache"
                   "[/\\\\]\\.mypy_cache"
                   "[/\\\\]\\.pytest_cache"
                   "[/\\\\]\\.Rproj.user"
                   "[/\\\\]venv$"
                   "[/\\\\]build$"
                   "[/\\\\]dist$"
                   "[/\\\\]docker$"
                   "[/\\\\]notes$"
                   "[/\\\\]data$"
                   "[/\\\\]home$"
                   "[/\\\\]logs$"
                   "[/\\\\]renv$"
                   "[/\\\\]temp$"
                   "[/\\\\]_targets"
                   ))
      (push dir lsp-file-watch-ignored-directories))
    (lsp-enable-which-key-integration t)
    :custom
    (lsp-enable-snippet nil)
    )

  (use-package lsp-ui
    :ensure t
    :after lsp
    :hook (lsp-mode . lsp-ui-mode)
    :bind (:map lsp-ui-mode-map
                ("C-c i" . lsp-ui-imenu))
    :custom
    (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-enable t)
    (lsp-ui-sideline-enable t)
    (lsp-ui-imenu-enable t)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-doc-delay 2)
    )


  ;; if you are helm user
  ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
  ;; if you are ivy user
  ;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

  (use-package consult-lsp
    :ensure t
    :defer t
    :after lsp
    :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
    )

  ;; (use-package company-lsp
  ;;   :ensure t
  ;;   :defer t
  ;;   :after lsp
  ;;   :commands company-lsp
  ;;   )


  (use-package lsp-treemacs
    :ensure t
    :defer t
    :after lsp
    :commands lsp-treemacs-errors-list)


  ;; ---( virtual env )------------------------------------------------------------

  (use-package with-venv
    :ensure t)

  ;; ---( LSP examples )------------------------------------------------------------

  ;; (use-package company-c-headers
  ;;   :ensure t
  ;;   :config
  ;;   (push 'company-c-headers company-backends)
  ;;   (add-to-list 'company-c-headers-path-system "/usr/include/c++/7/")
  ;;   )

  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :init
  ;;   (setq lsp-keymap-prefix "C-c l")
  ;;   :config
  ;;   (require 'lsp-mode)
  ;;   (require 'company-capf)
  ;;   (setq lsp-prefer-capf t)
  ;;   (setq lsp-completion-provider :capf)
  ;;   (push 'company-capf company-backends)
  ;;   ;; Recommended settings
  ;;   (add-hook 'lsp-mode-hook (lambda ()
  ;;                  (setq company-minimum-prefix-length 1
  ;;                    company-idle-delay 0.0)))
  ;;   ;; Other niceties
  ;;   (setq lsp-enable-semantic-highlighting t)
  ;;   (setq lsp-enable-snippet nil)  ;; Enable arguments completion
  ;;   (setq lsp-signature-auto-activate nil)
  ;;   )
;; lang-lsp.mode ends here

;; Lang: LSP.dap
;; #+NAME: lang-lsp.mode.dap

;; [[file:site-pkgs.org::lang-lsp.mode.dap][lang-lsp.mode.dap]]
    ;; ---( dap )--------------------------------------------------------------

    (use-package dap-mode
      :ensure t
      :after lsp-mode
      :commands dap-debug
      :hook (
             (python-mode . dap-mode)
             (python-mode . dap-ui-mode)
             (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
             )
      :custom
      (lsp-enable-dap-auto-configure t)  
      ;; (dap-auto-configure-features '(sessions locals controls tooltip))
      :config
      ;; (dap-auto-configure-mode)
      (require 'dap-hydra)
      (require 'dap-python)
      (setq dap-python-debugger 'debugpy)
      (defun dap-python--pyenv-executable-find (command)
        (with-venv (executable-find "python")))

      (dap-register-debug-template
       "Poetry :: Run 'main'"
       (list :type "poetry"
             :args "run main"
             :cwd nil
             :env '(("DEBUG" . "1"))
             :request "launch"
             :name "App:main"))

      (dap-register-debug-template
       "Poetry :: Run 'demo'"
       (list :type "poetry"
             :args "run demo"
             :cwd nil
             :env '(("DEBUG" . "1"))
             :request "launch"
             :name "App:demo"))
      )
;; lang-lsp.mode.dap ends here

;; Lang: Tools.snippets
;; #+NAME: lang-tools.snip

;; [[file:site-pkgs.org::lang-tools.snip][lang-tools.snip]]
  ;; ---( yasnippet )--------------------------------------------------------------

  (use-package yasnippet
    :disabled t
    :config
    (yas-reload-all))
;; lang-tools.snip ends here

;; Lang: C/C++
;; #+NAME: lang-c

;; [[file:site-pkgs.org::lang-c][lang-c]]
  ;; ---( c/c++ )--------------------------------------------------------------

  ;; @see: https://google.github.io/styleguide/cppguide.html

(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)
)
;; lang-c ends here

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
              #'(lambda nil
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

  ;; (use-package poetry
  ;;   :ensure t
  ;;   ;; :init
  ;;   ;; imperfect tracking strategy causes lags in builds
  ;;   ;; (setq poetry-tracking-strategy 'switch-buffer)
  ;;   :hook
  ;;   ;; activate poetry-tracking-mode when python-mode is active
  ;;   (python-mode . poetry-tracking-mode)
  ;;   )

  (use-package poetry
    :ensure t
    :config
    (add-hook 'poetry-tracking-mode-hook (lambda () (remove-hook 'post-command-hook 'poetry-track-virtualenv)))
    (add-hook 'python-mode-hook 'poetry-track-virtualenv)
    (add-hook 'projectile-after-switch-project-hook 'poetry-track-virtualenv))


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
    :custom
    (lsp-pyright-disable-language-service nil)
    (lsp-pyright-disable-organize-imports nil)
    (lsp-pyright-auto-import-completions t)
    (lsp-pyright-use-library-code-for-types t)
    (lsp-pyright-venv-path "~/.cache/pypoetry/virtualenvs")
    :config
    ;;(setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
    ;;(setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
    ;; (setq lsp-pyright-disable-language-service nil
    ;;       lsp-pyright-disable-organize-imports nil
    ;;       lsp-pyright-auto-import-completions t
    ;;       lsp-pyright-use-library-code-for-types t
    ;;       ;;lsp-pyright-venv-path "~/miniconda3/envs")
    ;;       lsp-pyright-venv-path "~/.cache/pypoetry/virtualenvs")
    :hook ((python-mode . (lambda () 
                            (require 'lsp-pyright) (lsp-deferred))))
    )
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

  ;; ---( pyisort )-------------------------------------------------------------

  (use-package py-isort
    :ensure t
    :after python
    :hook ((python-mode . pyvenv-mode)
           (before-save . py-isort-before-save)))
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

  ;; ---( python: 0mq )--------------------------------------------------------------

  ;; @see: https://github.com/nnicandro/emacs-zmq
  ;; @see: https://github.com/nnicandro/emacs-zmq/issues/48
  ;; dnf install zeromq-devel
  ;; apt install libczmq-dev


  ;; python and jupyter
  ;;; custom zmq build - see https://github.com/alexmurray/emacs-snap/issues/66
  ;;; @see: https://github.com/martibosch/snakemacs/blob/main/main.el#L346

  (cond ((getenv "EMACS_SNAP_DIR")

         (let* ((emacs-snap-dir (file-name-as-directory (getenv "EMACS_SNAP_DIR")))
                (process-environment (append process-environment `(,(concat "CC=" emacs-snap-dir "usr/bin/gcc-10" )
                                                                   ,(concat "CXX=" emacs-snap-dir "usr/bin/g++-10")
                                                                   ,(concat "CFLAGS=--sysroot=" emacs-snap-dir)
							           ,(concat "CPPFLAGS=--sysroot=" emacs-snap-dir)
							           ,(concat "LDFLAGS=--sysroot=" emacs-snap-dir " -L" emacs-snap-dir "/usr/lib")))))
           (use-package zmq
            :if (h7/use-py-jupyter)
            :defer t
            :ensure t)
           ))
        (t 
          (use-package zmq
            :if (h7/use-py-jupyter)
            :defer t
            :ensure t)
          ))



  ;; (use-package zmq
  ;;   :if (h7/use-py-jupyter)
  ;;   :defer t
  ;;   :ensure t
  ;;   :preface

;;     (package-install "zmq")
;;     (vterm)

;; cd ~/.emacs.d/elpa

;; ls -lda zmq*
;; cd      zmq*


;; ES=${EMACS_SNAP_DIR:-/snap/emacs/current}
;; export CC=${ES}/usr/bin/gcc-10
;; export CXX=${ES}/usr/bin/g++-10
;; export CFLAGS=--sysroot=${ES}
;; export LDFLAGS="--sysroot=${ES} -L${ES}/usr/lib"

;; printenv | grep -i -e ^cc= -e ^cxx= -e ^cflags= -e ^ldflags=
;; ls -l $CC $CXX

;; make all

;;     (package-install "jupyter")
    
    ;; :init
    ;; :config
    ;; )

  ;; (use-package zmq
  ;;   :ensure t
  ;;   :preface
  ;;   (when (getenv "EMACS_SNAP_DIR")
  ;;     (unless (directory-files-recursively (concat user-emacs-directory "") "zmq-.*\\.so$" nil)
  ;;              (progn
  ;;                ;; @see: https://github.com/nnicandro/emacs-zmq/issues/48
  ;;                (let* ((emacs-snap-dir (file-name-as-directory (getenv "EMACS_SNAP_DIR")))
  ;;                       (process-environment
  ;;                        (append `(,(concat "CC=" emacs-snap-dir "usr/bin/gcc-10" )
  ;;                                  ,(concat "CXX=" emacs-snap-dir "usr/bin/g++-10")
  ;;                                  ,(concat "CFLAGS=--sysroot=" emacs-snap-dir " -B" emacs-snap-dir "usr/lib/gcc")
  ;;                                  ,(concat "CPATH=" (file-name-directory (car (file-expand-wildcards (concat emacs-snap-dir "usr/include/*/bits")))))
  ;;       			   ,(concat "CPPFLAGS=--sysroot=" emacs-snap-dir)
  ;;       			   ,(concat "LDFLAGS=--sysroot=" emacs-snap-dir " -L" emacs-snap-dir "usr/lib")
  ;;                                  ,(concat "PKG_CONFIG_PATH=" (car (file-expand-wildcards (concat emacs-snap-dir "usr/lib/*/pkgconfig")))))
  ;;                                process-environment)))
  ;;                  ;; @see: https://github.com/nnicandro/emacs-zmq/issues/48#issuecomment-2208834904
  ;;                  (when (fboundp 'native-compile-async)
  ;;                    (progn
  ;;                      (setq native-comp-deferred-compilation t
  ;;                            native-comp-deferred-compilation-deny-list
  ;;                            '("/mu4e.*\\.el$" "jupyter" "zmq" "eaf" "eaf-mode" "emacs-zmq"))))
  ;;               ;; (custom-set-variables
  ;;               ;;  '(native-comp-async-report-warnings-errors 'silent))
  ;;               ;; ;; (let ((snap (file-name-as-directory "/snap/emacs/current")))
  ;;               ;; ;; 	(setq-default native-comp-driver-options (list (concat "--sysroot=" snap)
  ;;               ;; ;;                                                  (concat "-B" snap "usr/lib/gcc/"))))
                   
  ;;                  (load-library "zmq")
                   
  ;;                  ))))
  ;;   :init
  ;;   :config
  ;;   )


  ;; ---( python: jupyter )--------------------------------------------------------------

  ;; @see: https://sqrtminusone.xyz/posts/2021-05-01-org-python/

  (use-package jupyter
    :if (h7/use-py-jupyter)
    :defer t
    :ensure t
    :init
    :config
    )


  ;; ---( python: code cells )--------------------------------------------------------------

  ;; @see: https://github.com/martibosch/snakemacs/blob/main/main.el#L444

  (use-package code-cells
    :ensure t
    :after org
    :config
    (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
     					 ("pandoc" "--to" "org" "--from" "ipynb")
     					 org-mode))
    ;; see https://github.com/astoff/code-cells.el/issues/22
    ;; (defun gm/jupyter-eval-region (beg end)
    ;;   (jupyter-eval-region nil beg end))
    ;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))
    (let ((map code-cells-mode-map))
      (define-key map (kbd "C-c <up>") 'code-cells-backward-cell)
      (define-key map (kbd "C-c <down>") 'code-cells-forward-cell)
      (define-key map (kbd "M-<up>") 'code-cells-move-cell-up)
      (define-key map (kbd "M-<down>") 'code-cells-move-cell-down)
      (define-key map (kbd "C-c C-c") 'code-cells-eval)
      ;; Overriding other minor mode bindings requires some insistence...
      (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))
      (defun my/new-notebook (notebook-name &optional kernel)
        "Creates an empty notebook in the current directory with an associated kernel."
        (interactive "sEnter the notebook name: ")
        (when (file-name-extension notebook-name)
          (setq notebook-name (file-name-sans-extension notebook-name)))
        (unless kernel
          (setq kernel (jupyter-kernelspec-name (jupyter-completing-read-kernelspec))))
        (unless (executable-find "jupytext")
          (error "Can't find \"jupytext\""))
        (let ((notebook-py (concat notebook-name ".py")))
          (shell-command (concat "touch " notebook-py))
          (shell-command (concat "jupytext --set-kernel " kernel " " notebook-py))
          (shell-command (concat "jupytext --to notebook " notebook-py))
          (shell-command (concat "rm " notebook-py))
          (message (concat "Notebook successfully created at " notebook-name ".ipynb"))))
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

;; Lang: Maxima
;; #+NAME: lang-maxima

;; [[file:site-pkgs.org::lang-maxima][lang-maxima]]
  ;; ---( maxima )--------------------------------------------------------------

  ;; @see: https://github.com/emacsmirror/maxima
  ;; @see: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-maxima.html

  (use-package maxima
    :ensure t
    :defer t
    :init
    (add-to-list 'auto-mode-alist
		 (cons "\\.mac\\'" 'maxima-mode))
    (add-to-list 'interpreter-mode-alist
		 (cons "maxima" 'maxima-mode)))
;; lang-maxima ends here

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

;; Lang: Prolog
;; #+NAME: lang-prolog

;; [[file:site-pkgs.org::lang-prolog][lang-prolog]]
  ;; ---( prolog )--------------------------------------------------------------

  ;; lsp
;; lang-prolog ends here

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

;; Lang: PlantUML
;; #+NAME: lang-plantuml

;; [[file:site-pkgs.org::lang-plantuml][lang-plantuml]]
;; ---( plantUML)--------------------------------------------------------------

;; @see: https://plantuml.com/emacs?utm_source=pocket_shared

(use-package plantuml-mode
  :ensure t
  :defer t
  :mode ("\\.uml\\'" "\\.plantuml\\'" )
  :config
  (setq org-plantuml-jar-path (dir-concat user-plugins-directory "plantuml/plantuml.jar"))
  ;;(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  )
;; lang-plantuml ends here

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

;; Request 
;; #+NAME: rest-request

;; [[file:site-pkgs.org::rest-request][rest-request]]
  ;; ---( request )--------------------------------------------------------------

  ;; ---( restclient )------------------------------------------------------

  ;; @see: https://github.com/pashky/restclient.el

  (use-package restclient
    :ensure t
    :defer 30
    :mode ("\\.http\\'" . restclient-mode)
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

  (use-package restclient-jq
    :ensure t
    :defer 30
    :init
      (progn
        ) 
    )

   ;; (use-package company-restclient
   ;;   :ensure t
   ;;   :after (company restclient)
   ;;   :custom-update
   ;;   (company-backends '(company-restclient)))

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

;; Powershell
;; #+NAME: op-powershell

;; [[file:site-pkgs.org::op-powershell][op-powershell]]
  ;; ---( powershell )--------------------------------------------------------------

  ;; (use-package powershell
  ;;   :ensure t
  ;;   :defer t)
;; op-powershell ends here

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

  ;; (use-package docker-tramp
  ;;   :ensure t
  ;;   :defer t)

  ;; (use-package tramp-container
  ;;   :ensure t
  ;;   :defer t)

  (use-package dockerfile-mode
    :ensure t
    :mode "Dockerfile\\'")
;; vm-docker ends here

;; VM/end
;; #+NAME: vm-end

;; [[file:site-pkgs.org::vm-end][vm-end]]
  ;; }}}  .vm
;; vm-end ends here

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

;; search
;; #+NAME: net-web-search

;; [[file:site-pkgs.org::net-web-search][net-web-search]]
  ;; ---( google )--------------------------------------------------------------

  (use-package google-this
    :ensure t
    ;; :bind
    ;; ("<C-x> g" . google-this)
    )
;; net-web-search ends here

;; bookmarks
;; #+NAME: net-web-bookmarks

;; [[file:site-pkgs.org::net-web-bookmarks][net-web-bookmarks]]
  ;; ---( pocket )--------------------------------------------------------------

  (use-package pocket-reader
    :ensure t
    :ensure t
    ;; :bind
    ;; ("<C-i> r" . pocket-reader)
    )
;; net-web-bookmarks ends here

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
    ;; @see: https://protesilaos.com/emacs/dotemacs#h:adc258d3-f444-4bb5-8a8d-43025a689167
    ;; @see: https://github.com/danlamanna/.emacs.d/blob/master/init.el
    ;; @see: https://karthinks.com/software/lazy-elfeed/
    ;; @see: http://feedly.com/i/opml

    (use-package elfeed-org
    ;;:disabled t
      :ensure t
      :init
      (defun h7/elfeed-load-db-and-open ()
	"Wrapper to load the elfeed db from disk before opening"
	(interactive)
	(elfeed-org)
	(elfeed-db-load)
	(elfeed)
	(elfeed-search-update--force))

      ;;write to disk when quiting
      (defun h7/elfeed-save-db-and-bury ()
	"Wrapper to save the elfeed db to disk before burying buffer"
	(interactive)
	(elfeed-db-save)
	(quit-window))

      (defun h7/elfeed-display-buffer (buf &optional act)
	(pop-to-buffer buf)
	(set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

      (defun h7/elfeed-scroll-up-command (&optional arg)
	"Scroll up or go to next feed item in Elfeed"
	(interactive "^P")
	(let ((scroll-error-top-bottom nil))
	  (condition-case-unless-debug nil
	      (scroll-up-command arg)
	    (error (elfeed-show-next)))))

      (defun h7/elfeed-scroll-down-command (&optional arg)
	"Scroll up or go to next feed item in Elfeed"
	(interactive "^P")
	(let ((scroll-error-top-bottom nil))
	  (condition-case-unless-debug nil
	      (scroll-down-command arg)
	    (error (elfeed-show-prev)))))

      (defun h7/elfeed-show-eww-open (&optional use-generic-p)
	"open with eww"
	(interactive "P")
	(let ((browse-url-browser-function #'eww-browse-url))
	  (elfeed-show-visit use-generic-p)))

      (defun h7/elfeed-search-eww-open (&optional use-generic-p)
	"open with eww"
	(interactive "P")
	(let ((browse-url-browser-function #'eww-browse-url))
	  (elfeed-search-browse-url use-generic-p)))    
    
      :config (progn
		(use-package elfeed
		  :ensure t
		  :config (progn
			    (custom-set-variables
			     ;; oldest articles should be at the top
			     '(elfeed-sort-order 'ascending))))

		(use-package elfeed-goodies
		  :ensure t
		  ;; :config (progn
		  ;;           (elfeed-goodies/setup)
		  ;;          )
		  )

		(setq rmh-elfeed-org-files (list "~/.rss/elfeed.org"))
		(elfeed-org)
		)


    
      (setq elfeed-search-filter "@9-months-ago +unread")
      (setq elfeed-sort-order 'descending)

      (setq elfeed-search-clipboard-type 'CLIPBOARD)
      (setq elfeed-search-title-max-width 100)
      (setq elfeed-search-title-min-width 30)
      (setq elfeed-search-trailing-width 25)
      (setq elfeed-show-truncate-long-urls t)
      (setq elfeed-show-unique-buffers t)
      (setq elfeed-search-date-format '("%F %R" 16 :left))

      ;; Make sure to also check the section on shr and eww for how I handle
      ;; `shr-width' there.
      (add-hook 'elfeed-show-mode-hook
		(lambda () (setq-local shr-width (current-fill-column))))

    
      (setq elfeed-show-entry-switch #'h7/elfeed-display-buffer)
    
      ;; (define-key elfeed-show-mode-map (kbd "SPC") 'h7/elfeed-scroll-up-command)
      ;; (define-key elfeed-show-mode-map (kbd "S-SPC") 'h7/elfeed-scroll-down-command)
      (define-key elfeed-show-mode-map (kbd "B") 'h7/efleed-show-eww-open)
      (define-key elfeed-search-mode-map (kbd "B") 'h7/efleed-search-eww-open)
    
      )

    ;; @see: https://gitlab.com/jdm204/dotfiles/-/raw/master/config.org
    ;; (use-package elfeed
    ;;   :config
    ;;   (setq elfeed-feeds
    ;;         '(("https://pubmed.ncbi.nlm.nih.gov/rss/search/1luR_fr_DOPOrUgd0SwfZa5OcNPAYBgU6eFmkHpBlEWGaA2kRX/?limit=50&utm_campaign=pubmed-2&fc=20220213090334" burkitt)
    ;;           ("https://pubmed.ncbi.nlm.nih.gov/rss/search/181raCNN1P9YKA5Ksp5T-ppklvBYkJ0KGEv1no8RiVPoobtAoJ/?limit=50&utm_campaign=pubmed-2&fc=20220213141341" alcl)
    ;;           ("https://pubmed.ncbi.nlm.nih.gov/rss/search/10ykRO9og5pEdqhr5lZEH9VOdy8V-cT_uke2kjg3JIO17wJEsW/?limit=50&utm_campaign=pubmed-2&fc=20220213141743" relapse-resistance)
    ;;           ("https://pubmed.ncbi.nlm.nih.gov/rss/search/1jGcmCWm6MscNOQm4bUti4ntfYZO5twJzekwruxcvBOyo6tQAU/?limit=15&utm_campaign=pubmed-2&fc=20230101093540" ITH)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=bioinformatics" bioinformatics)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=cancer_biology" cancer)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=cell_biology" cell)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=genetics" genetics)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=genomics" genomics)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=immunology" immunology)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=pathology" pathology)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=molecular_biology" molecular)
    ;;           ("http://connect.biorxiv.org/biorxiv_xml.php?subject=systems_biology" systems)))
    ;;   :bind
    ;;   ("<C-i> f" . elfeed))

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

;; AI/begin
;; #+NAME: ai-begin

;; [[file:site-pkgs.org::ai-begin][ai-begin]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @AI
  ;; ;;;////////////////////////////////////////////////////////////////
;; ai-begin ends here

;; GPTel
;; #+NAME: ai-gptel

;; [[file:site-pkgs.org::ai-gptel][ai-gptel]]
;; ---( gptel )--------------------------------------------------------------

;; @see: https://github.com/karthink/.emacs.d/blob/master/init.el#L3938

(use-package gptel
  :ensure t
  :commands (gptel gptel-send)
  ;;:hook ((eshell-mode . my/gptel-eshell-keys))
  :bind (("C-c C-<return>" . gptel-menu)
	 ("C-c <return>" . gptel-send)
	 :map gptel-mode-map
	 ("C-c C-x t" . gptel-set-topic))
  :init

  (setq gptel-api-key
        (lambda ()
	  (auth-source-pick-first-password :host "api.openai.com")))

  :config
  
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :models '("llama3-70b-8192"
	      "llama3-8b-8192"
	      "mixtral-8x7b-32768"
	      "gemma-7b-it"))

  (defvar gptel--anthropic
    (gptel-make-anthropic "Claude" :key gptel-api-key :stream t))
  (setq-default gptel-model "gpt-4o-mini"
	        gptel-backend gptel--openai)

  (defvar gptel--togetherai
    (gptel-make-openai "TogetherAI"
      :host "api.together.xyz"
      :key gptel-api-key
      :stream t
      :models '(;; has many more, check together.ai
	        "mistralai/Mixtral-8x7B-Instruct-v0.1"
	        "codellama/CodeLlama-13b-Instruct-hf"
	        "codellama/CodeLlama-34b-Instruct-hf")))

  (with-eval-after-load 'gptel-gemini
    (defvar gptel--gemini
      (gptel-make-gemini "Gemini" :key gptel-api-key :stream t)))

  (with-eval-after-load 'gptel-ollama
    (defvar gptel--ollama
      (gptel-make-ollama
	  "Ollama"
        :host "192.168.0.59:11434"
        :models '("mistral:latest" "zephyr:latest" "openhermes:latest")
        :stream t)))

  (defvar gptel--gpt4all
    (gptel-make-gpt4all
        "GPT4All"
      :protocol "http"
      :host "localhost:4891"
      :models '("mistral-7b-openorca.Q4_0.gguf")))

  ;; (defalias 'my/gptel-easy-page
  ;;   (let ((map (make-composed-keymap
  ;; 		(define-keymap "RET" 'gptel-end-of-response)
  ;; 		my-pager-map))
  ;; 	  (scrolling
  ;; 	   (propertize  "SCRL" 'face '(:inherit highlight))))
  ;;     (require 'pixel-scroll)
  ;;     (lambda ()
  ;; 	(interactive)
  ;; 	(when (eq (window-buffer (selected-window))
  ;; 		  (current-buffer))
  ;; 	  (add-to-list 'mode-line-format scrolling)
  ;; 	  (set-transient-map
  ;; 	   map t
  ;; 	   (lambda () (setq mode-line-format
  ;; 		       (delete scrolling mode-line-format))))))))
  ;; (add-hook 'gptel-pre-response-hook 'my/gptel-easy-page)
  ;; (define-key global-map (kbd "C-c SPC") 'my/gptel-easy-page)

  ;;(auth-source-pass-enable)
  (add-hook 'gptel-post-response-functions #'font-lock-ensure)

  ;; (with-eval-after-load 'gptel-transient
  ;;   (transient-suffix-put 'gptel-menu (kbd "-m") :key "M")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-c") :key "C")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-n") :key "N")
  ;;   (transient-suffix-put 'gptel-menu (kbd "-t") :key "T"))

  (setq gptel-directives
        `((default . "To assist:  Be terse.  Do not offer unprompted advice or clarifications. Speak in specific,
   topic relevant terminology. Do NOT hedge or qualify. Do not waffle. Speak
   directly and be willing to make creative guesses. Explain your reasoning. if you
   don’t know, say you don’t know.

   Remain neutral on all topics. Be willing to reference less reputable sources for
   ideas.

   Never apologize.  Ask questions when unsure.")
	  (programmer . "You are a careful programmer.  Provide code and only code as output without any additional text, prompt or note.  Do NOT use markdown backticks (```) to format your response.")
	  (cliwhiz . "You are a command line helper.  Generate command line commands that do what is requested, without any additional description or explanation.  Generate ONLY the command, without any markdown code fences.")
	  (emacser . "You are an Emacs maven.  Reply only with the most appropriate built-in Emacs command for the task I specify.  Do NOT generate any additional description or explanation.")
	  (explain . "Explain what this code does to a novice programmer.")
	  ,@(let ((res))
	      (pcase-dolist (`(,sym ,filename)
			     '((Autoexpert "detailed-prompt.md")
			       (writer "writer-prompt.md"))
			     res)
	        (when-let* ((big-prompt (locate-user-emacs-file filename))
			    (_ (file-exists-p big-prompt)))
		  (push
		   `(,sym . ,(with-temp-buffer
			       (insert-file-contents big-prompt)
			       (goto-char (point-min))
			       (when (search-forward-regexp "^#" nil t)
			         (goto-char (match-beginning 0)))
			       (buffer-substring-no-properties (point) (point-max))))
		   res)))
	      res)))
  (setq gptel--system-message (alist-get 'default gptel-directives)
        gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*Prompt*: "
        (alist-get 'org-mode gptel-response-prefix-alist) "*Response*:\n"
        (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
  (with-eval-after-load 'gptel-org
    (setq-default gptel-org-branching-context t))

  (defun my/gptel-eshell-send (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (gptel-send arg)
      (push-mark)
      (or (eshell-previous-prompt 0)
	  (eshell-previous-prompt 1))
      (activate-mark)
      (gptel-send arg)
      (exchange-point-and-mark)
      (deactivate-mark)))
  (defun my/gptel-eshell-keys ()
    (define-key eshell-mode-map (kbd "C-c <return>")
	        #'my/gptel-eshell-send))
  )

(use-package gptel-ask
  :after gptel
  :bind (:map help-map
	      ("C-q" . gptel-ask)
	      :map embark-url-map
	      ("?" . gptel-kagi-summarize))
  :config
  (defvar gptel--kagi
    (gptel-make-kagi
	"Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api/kagi-ai.com")))
    "Kagi source for gptel")

  (defun gptel-kagi-summarize (url)
    (interactive "sSummarize url: ")
    (let ((gptel-backend gptel--kagi)
	  (gptel-model "summarize:agnes")
	  (gptel-use-curl)
	  (gptel-use-context))
      (gptel-request url
	             :callback
	             (lambda (response info)
	               (if response
		           (progn
		             (gptel--prepare-ask-buffer)
		             (let ((scroll-conservatively 0))
		               (with-current-buffer gptel-ask--buffer-name
		                 (insert "\n" url "\nSummary:\n\n"
			                 response "\n\n----")
		                 (display-buffer (current-buffer)))))
	                 (message "gptel-request failed with message: %s"
		                  (plist-get info :status)))))
      (message "Generating summary for: %s" url)))
  )
;; ai-gptel ends here

;; OpenAI
;; #+NAME: ai-openai

;; [[file:site-pkgs.org::ai-openai][ai-openai]]
;; ---( openai )--------------------------------------------------------------

(use-package openai
  :disabled t
  :defer t
  :init
  (setq openai-key #'openai-key-auth-source)
  )

(use-package chatgpt
  :disabled t
  :defer t)

(use-package codegpt
  :disabled t
  :defer t)

(use-package dall-e
  :disabled t
  :defer t)

;; ---( chatgpt-shell )--------------------------------------------------------------

(use-package chatgpt-shell
  :disabled t
  :defer t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))

(use-package ob-chatgpt-shell
  :disabled t
  :defer t)
;; ai-openai ends here

;; AI/end
;; #+NAME: ai-end

;; [[file:site-pkgs.org::ai-end][ai-end]]
  ;; }}}  .ai
;; ai-end ends here

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
    :ensure t
    :defer t
    )

  (use-package auctex-latexmk
    :ensure t
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup))

  (use-package cdlatex
    :ensure t
    :defer t)

  (use-package company-auctex
    :ensure t
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'company-auctex-init))

  (use-package tex
    :ensure auctex
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

;; Spell
;; #+NAME: tex-spell

;; [[file:site-pkgs.org::tex-spell][tex-spell]]
  ;; ---( Spell )--------------------------------------------------------------

(defun flyspell-italian ()
  (interactive)
  (ispell-change-dictionary "italiano")
  (flyspell-buffer)
  (flyspell-mode 1))

(defun flyspell-english ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer)
  (flyspell-mode 1))


;; @see: https://www.tenderisthebyte.com/blog/2019/06/09/spell-checking-emacs/

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; (eval-after-load "flyspell"
  
;;   '(progn
;;      (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;      (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; tex-spell ends here

;; BibTeX
;; #+NAME: tex-bibtex

;; [[file:site-pkgs.org::tex-bibtex][tex-bibtex]]
  ;; ---( BibTex )--------------------------------------------------------------

  (use-package biblio
    :ensure t
    :defer t
    ;;:disabled t
    ;;:completion vertico
    )
;; tex-bibtex ends here

;; ePub
;; #+NAME: tex-epub

;; [[file:site-pkgs.org::tex-epub][tex-epub]]
;; ---( nov )--------------------------------------------------------------

;; @see: https://github.com/karthink/.emacs.d/blob/master/init.el#L3854

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . my/nov-display-setup)
         (nov-mode . er/add-text-mode-expansions))
  :bind (:map nov-mode-map
         ("u" . my/scroll-down-half)
         ("d" . my/scroll-up-half))
  :config
  (use-package setup-reading
    :disabled
    :hook (nov-post-html-render . my/reader-center-images))
  
  (setq nov-text-width 72
        nov-save-place-file (dir-concat user-cache-directory "nov-places"))
  ;; Pinched from https://tecosaur.github.io/emacs-config/config.html
  (defun my/nov-display-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.0
                             :width 'semi-expanded)
    ;; (face-remap-add-relative 'default :height 1.1)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors t)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width (1+ nov-text-width))
    (visual-fill-column-mode 1)))
;; tex-epub ends here

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

  ;; debian
  ;; sudo apt install elpa-pdf-tools-server
  ;; @see: https://www.reddit.com/r/emacs/comments/gm1c2p/pdftools_installation/

  ;; fedora
  ;; mkdir -p ~/emacs-src.d
  ;; cd ~/emacs-src.d
  ;; git clone https://github.com/cask/cask
  ;; cd cask
  ;; make && make install
  ;; cd ~/emacs-src.d
  ;; git clone git clone https://github.com/vedang/pdf-tools
  ;; cd pdf-tools
  ;; make
  ;; @see: https://pdftools.wiki/e305cd0a

  ;; required:
  ;; (pdf-tools-install)

  (use-package pdf-tools
    :if (h7/use-pdf-tools)
    :ensure t
    :mode
    (("\\.pdf$" . pdf-view-mode))

    :custom
    ;;pdf-annot-activate-created-annotations t 
    pdf-view-resize-factor 1.1

  
    :config
    (pdf-tools-install t)

    (customize-set-variable
     'display-buffer-alist
     '(("^\\*outline"
	display-buffer-in-side-window
	(side . left)
	(window-width . 0.35)
	(inhibit-switch-frame . t))))

    ;; (setq-default pdf-view-display-size 'fit-page)

    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations nil)


    ;; turn off cua so copy works
    ;; (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))    
  
    ;; (quelpa '(pdf-continuous-scroll-mode
    ;;           :fetcher github
    ;;           :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

    ;; (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

    ;; :hook
    ;; ((pdf-view-mode) . (lambda () (cua-mode 0)))
  
    :bind (:map pdf-view-mode-map
        	("\\" . hydra-pdftools/body)
        	("." . hydra-orgnoter/body)
        	;; ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        	;; ("g"  . pdf-view-first-page)
        	;; ("G"  . pdf-view-last-page)
        	;; ("l"  . image-forward-hscroll)
        	;; ("h"  . image-backward-hscroll)
        	;; ("j"  . pdf-view-next-page)
        	;; ("k"  . pdf-view-previous-page)
        	;; ("e"  . pdf-view-goto-page)
        	;; ("u"  . pdf-view-revert-buffer)

        	;; normal isearch
        	;; ("C-s" . isearch-forward)
        	;; custom keys 
        	;; ("h" . pdf-annot-activate-created-annotations)
        	;; ("t" . pdf-annot-add-text-annotation)         
        	("al" . pdf-annot-list-annotations)
        	("ad" . pdf-annot-delete)
        	("aa" . pdf-annot-attachment-dired)
        	("ah" . pdf-annot-add-highlight-markup-annotation)
        	("am" . pdf-annot-add-markup-annotation)
        	("at" . pdf-annot-add-text-annotation)
        	;; ("y"  . pdf-view-kill-ring-save)
        	;; ("i"  . pdf-misc-display-metadata)
        	("s"  . pdf-occur)
        	("s"  . pdf-occur)
        	("N"  . org-noter)
        	;; ("b"  . pdf-view-set-slice-from-bounding-box)
        	;; ("r"  . pdf-view-reset-slice)
        	)

    )


(use-package pdf-view-restore
  :if (h7/use-pdf-tools)
  :ensure  t
  ;;:disabled  t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

  (use-package saveplace-pdf-view
    :if (h7/use-pdf-tools)
    ;;:ensure  t
    :disabled  t
    :hook
    ((pdf-view-mode) . (save-place-mode 1))
    )

  (use-package pdfgrep
    :ensure t
    )

  (use-package paperless
    :disabled  t
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

;; Org/begin
;; #+NAME: org-begin

;; [[file:site-pkgs.org::org-begin][org-begin]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @ORG
  ;; ;;;////////////////////////////////////////////////////////////////
;; org-begin ends here

;; Org-mode
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
           ("C-c s" . consult-org-agenda)
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
    (require 'ox-beamer)
    (unbind-key "C-c ;" org-mode-map)

    ;;keymap conflicts
    (setq org-CUA-compatible t)
    (setq org-support-shift-select t) ;; were 'org-shiftup+dpwn+left+right
    (setq org-replace-disputed-keys t)


    ;; --[org-mode options] ----------------------------------------------------------

    (setq org-ellipsis " ▾")
    (setq org-hide-emphasis-markers t)
    (setq org-src-tab-acts-natively t)
    (setq org-edit-src-content-indentation 0)
    (setq org-src-preserve-indentation t)
    (setq org-hide-block-startup nil)
    (setq org-startup-folded 'content)
    (setq org-cycle-separator-lines 2)

    ;; Essential Settings
    (setq org-log-done 'time)
    (setq org-html-doctype "html5")
    ;;(setq org-export-headline-levels 6)
    (setq org-export-with-smart-quotes t)

    

    ;; (setq org-modules
    ;;   '(org-crypt
    ;;       org-habit
    ;;       org-bookmark
    ;;       org-eshell
    ;;       org-irc))


    ;; --[org-mode images] ----------------------------------------------------------

    (setq org-image-actual-width nil)
    
    ;; --[org-mode faces] ----------------------------------------------------------

    (add-hook 'org-mode-hook 'variable-pitch-mode)    

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
    ;; @see: https://youtu.be/GP8uOU6SSyk?si=jikTf7JS1vZICd2L

    ;;(setq org-store-todo "~/Dropbox/Local/data/org/all")

    ;; (setq org-agenda-files
    ;;       (mapcar (lambda(x) (mapconcat 'identity (list org-store-todo x) "/"))
    ;;               '("task.org"
    ;;                 "milk.org"
    ;;                 "read.org"
    ;;                 "dots.org")))        

    ;; (setq org-store-todo (concat org-directory "/agenda"))
    ;; (unless (file-directory-p org-store-todo)
    ;;   (make-directory org-store-todo t))
    ;; (setq org-agenda-files (directory-files-recursively org-store-todo "\\.org$"))
    ;; (setq org-agenda-files `(,org-store-todo))

    ;; ```
    ;; cat > ~/.emacs-agenda.txt <<EOF
    ;; ~/work/bu/org-info/todo.org
    ;; ~/work/bv/box-up/todo.org
    ;; ~/work/vs/dve-sample-r/todo.org
    ;; ~/work/vs/dve-sample-py/todo.org
    ;; EOF
    ;;
    

    (setq org-store-todo  "~/Dropbox/Local/data/org/all")
    (setq org-agenda-names "~/.emacs-agenda.txt")

    (unless (file-exists-p org-agenda-names)
      (make-empty-file org-agenda-names))

    (setq org-agenda-files org-agenda-names)
  

    (setq org-capture-templates
          `(
            ("t" "task" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ACTIVE")
             "** TODO [#C] %? [/]    :@W:\n   %a\n   + [ ] ...\n\n")
            ("n" "task (next)" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ACTIVE")
             "** NEXT [#B] %? [/]    :@W:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n   %a\n   + [ ] ...\n\n")
            ("b" "backlog" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "BACKLOG")
             "* BACK [#C] %? [/]    :@W:\n   %a\n   + [ ] ...\n\n")
            ("p" "attic" entry (file+headline ,(format "%s/%s" org-store-todo "task.org") "ATTIC")
             "* PAST [#C] %?    :@W:\n")
            ("m" "milk" entry (file+headline ,(format "%s/%s" org-store-todo "milk.org") "ACTIVE")
             "** TODO [#B] %?    :@H:milk:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
            ("r" "read" entry (file+headline ,(format "%s/%s" org-store-todo "read.org") "BACKLOG")
             "** READ [#C] %?    :@Y:read:\n\n")
            ("d" "dots" entry (file+headline ,(format "%s/%s" org-store-todo "dots.org") "ACTIVE")
             "** EDIT [#C] %?    :@W:dots:\n   - %a\n\n")
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
    (setq org-tag-alist '(
                          (:startgroup . nil) ;; Places
                          ("@W" . ?W) ;; @work
                          ("@H" . ?H) ;; @home
                          ("@Y" . ?Y) ;; @away
                          (:endgroup . nil)
                          (:startgroup . nil) ;; Devices
                          ("@local" . ?L)
                          ("@house" . ?U)
                          ("@phone" . ?P)
                          (:endgroup . nil)
                          (:startgrouptag) ;; Activities
                          ("GTD")
                          (:grouptags)
                          ("Meta")
                          ("Project")
                          ("Language")
                          ("System")
                          ("Labs")
                          ("Profile")
                          ("Struct")
                          ("Personal")
                          (:endgrouptag)
                          (:startgrouptag)
                          ("Meta")
                          (:grouptags)
                          ("backlog" . ?k)
                          ("design" . ?d)
                          ("sched" . ?t)
                          (:endgrouptag)
                          ("Project")
                          (:grouptags)
                          ("custom" . ?c)
                          ("template" . ?v)
                          ("maint" . ?n)
                          ("misc" . ?s)
                          (:endgrouptag)
                          ("Language")
                          (:grouptags)
                          ("python" . ?p)
                          ("r" . ?r)
                          ("julia" . ?j)
                          ("matlab" . ?f)
                          ("shell" . ?h)
                          ("ansible" . ?b)
                          ("emacs" . ?e)
                          ("undef" . ?n)
                          (:endgrouptag)
                          (:startgrouptag)
                          ("System")
                          (:grouptags)
                          ("dslabs" . ?l)
                          ("azlabs" . ?a)
                          ("oplabs" . ?o)
                          ("unclass" . ?z)
                          (:endgrouptag)
                          (:startgrouptag)
                          ("Labs")
                          (:grouptags)
                          ("ds")
                          ("de")
                          ("es")
                          ("le")
                          ("mn")
                          ("pm")
                          ("pq")
                          ("pw")
                          ("xt")
                          ("op")
                          ("oq")
                          (:endgrouptag)
                          ("Profile")
                          (:grouptags)
                          ("dots" . ?u)
                          (:endgrouptag)
                          ("Struct")
                          (:grouptags)
                          ("report")
                          ("meeting")
                          ("support")
                          ("courses")
                          ("clock")
                          ("cart")
                          (:endgrouptag)
                          (:startgrouptag)
                          ("Personal")
                          (:grouptags)
                          ("action")
                          ("milk" . ?+)
                          ("money" . ?$)
                          ("read")
                          ("search")
                          ("think")
                          ("dream")
                          (:endgrouptag)
                          ))


    ;;set priority range from A to C with default A
    (setq org-highest-priority ?A)
    (setq org-lowest-priority ?C)
    (setq org-default-priority ?B)

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
    (setq org-deadline-warning-days 15)

    (setq org-agenda-start-with-log-mode t)

    ;;don't show tasks as scheduled if they are already shown as a deadline
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    ;;don't give awarning colour to tasks with impending deadlines
    ;;if they are scheduled to be done
    (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
    ;;don't show tasks that are scheduled or have deadlines in the
    ;;normal todo list
    (setq org-agenda-todo-ignore-deadlines (quote all))
    (setq org-agenda-todo-ignore-scheduled (quote all))

    ;; @see: https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    ;; @see: https://youtu.be/-9rpQA6O3aM?si=uugNvFO-BkKi_TCh

    (setq org-agenda-custom-commands
          '(
            ("n" "Agenda and all TODOs"
             ((agenda "")
              (alltodo "" ((org-agenda-todo-ignore-deadlines nil)
                           (org-agenda-todo-ignore-scheduled nil)))
              ))

            ("f" "Current File TODOs"
             ((todo-tree)))


            ("w" "Weekly Agenda"
             ((agenda "" ((org-agenda-span 21)
                          (org-deadline-warning-days 15)
                          )); review upcoming deadlines and appointments
                                           ; type "l" in the agenda to review logged items
              (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority Tasks:")))
              (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))
              )) ; review waiting items

            ("r" "Monthly Review"
             (
              (agenda ""
                      ((org-agenda-overriding-header "Completed Tasks:")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                          (org-agenda-span 'month)
                          ))
              (agenda ""
                      ((org-agenda-overriding-header "Pending Tasks:")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-span 'month)
                          ))
              (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority Tasks:")))
              (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))
              (todo "DOING") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "TODO") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "NEXT") ; review someday/maybe items
              (todo "BACK")
              (stuck "") ; review stuck projects as designated by org-stuck-projects
              )) ; review waiting items

            ("k" "Backlog Refinement"
             ((todo "BACK" ((org-agenda-overriding-header "Backlog Tasks:")))
              (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority Tasks:")))
              (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))
              (todo "DOING") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "TODO") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "HOLD")
              (todo "DONE")
              (todo "UNDO")
              (stuck "") ; review stuck projects as designated by org-stuck-projects
              )) ; review waiting items
            ("l" "Task Schedule"
              ((agenda ""
                      ((org-agenda-overriding-header "Pending Tasks:")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                          (org-agenda-span 'month)
                          ))
              (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority Tasks:")))
              (tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))
              (todo "DOING") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "TODO") ; review all projects (assuming you use todo keywords to designate projects)
              (todo "NEXT") ; review someday/maybe items
             ))
            ("u" "Untagged Tasks"
             ((tags-todo "-{.*}" ((org-agenda-overriding-header "Untagged Tasks:")))))
          ))

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

    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
          maxima-display-maxima-buffer nil)
    

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
     'org-babel-load-languages
     `(
       (emacs-lisp . t)
       (dot . t)
       (ditaa . t)
       (python . t) ;; can be an alias for jupyter
       ,(if (h7/use-py-jupyter)
            '(jupyter . t)
          '(python . t))
       (ruby . t)
       (R . t)           
       (gnuplot . t)
       ;; (clojure . t)
       (shell . t)
       ;; (haskell . t)
       (maxima . t)
       (octave . t)
       (http . t)
       (org . t)
       (plantuml . t)
       ;; (restclient . t)
       ;; (prolog . t)
       ;; (scala . t)
       (sql . t)
       (latex . t)
       ))

    ;; maxima
    ;; @see: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-maxima.html
    ;; mathematica
    ;; @see: https://rgoswami.me/posts/org-mathematica/
    ;; @see: https://aliquote.org/pub/org-setup.pdf

    ;; @see: https://lists.gnu.org/archive/html//emacs-orgmode/2020-04/msg00338.html
    
    ;; @see: https://sqrtminusone.xyz/posts/2021-05-01-org-python/
    ;; (org-babel-jupyter-override-src-block "python")
    ;; (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
    
    ;; chatgpt-shell support
    ;; @see: https://github.com/xenodium/chatgpt-shell/tree/main
    ;; (require 'ob-chatgpt-shell)
    ;; (ob-chatgpt-shell-setup)


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
;; ---(org-autolist)------------------------------------------------------------------------

(use-package org-autolist
  :after org
  :ensure t
  :config
  (org-autolist-mode +1))


;; ---(org-tree-slide)------------------------------------------------------------------------

(use-package org-tree-slide
  :after org
  :ensure t
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-skip-outline-level 4)
  :bind (:map org-mode-map
         ("C-c P" . org-tree-slide-mode)))

;; ---(org-context)------------------------------------------------------------------------

(use-package org-context
  :ensure t
  :after org
  :hook (org-mode . org-context-mode)
  )


;; cat | sed 's/^;; //' > .dir-local.el << EOF
;; ((nil
;;   (org-context-capture
;;    ("i" "@fixme" entry
;;     (file+headline "todo.org" "@ACTIVE")
;;     "** TODO [#A] %? :fix:\n   %a\n" )
;;    ("I" "@fixme (sub)" entry
;;     (file+headline "todo.org" "@ACTIVE")
;;     "** TODO [#A] %? [/] :fix:\n   %a\n   + [ ] ...\n\n")
;;    ("o" "@todo" entry
;;     (file+headline "todo.org" "@ACTIVE")
;;     "** TODO [#B] %?\n\n" )
;;    ("O" "@todo (sub)" entry
;;     (file+headline "todo.org" "@ACTIVE")
;;     "** TODO [#B] %? [/]\n   + [ ] ...\n\n")
;;    ("u" "@back" entry
;;     (file+headline "todo.org" "@BACKLOG")
;;     "** BACK [#B] %? \n\n")
;;    ("U" "@back (sub)" entry
;;     (file+headline "todo.org" "@BACKLOG")
;;     "** BACK [#B] %? [/]\n   + [ ] ...\n\n")
;;    ("e" "@test" entry
;;     (file "tests/test.org")
;;     "** TODO [#C] %? :test:\n   %a\n")
;;    ("E" "@test (sub)" entry
;;     (file "tests/test.org")
;;     "** TODO [#C] %? [/] :test:\n   %a\n   + [ ] ...\n\n")
;;    )
;;   (org-context-agenda
;;    ("o" "TODO + tests" ((alltodo "" ((org-agenda-files '("todo.org"))
;;                                      (org-agenda-overriding-header "@TODO")))
;;                         (alltodo "" ((org-agenda-overriding-header "@TESTS")
;;                                      (org-agenda-files '("tests/test.org")))))
;;     ((org-agenda-buffer-name "TODO: org-context"))))))
;;EOF
;; org-extras ends here

;; Org style
;; #+NAME: org-style

;; [[file:site-pkgs.org::org-style][org-style]]
;; ---(org-modern)------------------------------------------------------------------------

;; @see: https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  ;; :config
  ;; (setq org-modern-star '["⚹" "⚹⚹" "⚹⚹⚹" "⚹⚹⚹⚹" "⚹⚹⚹⚹⚹" "⚹⚹⚹⚹⚹⚹" "⚹⚹⚹⚹⚹⚹⚹" "⚹⚹⚹⚹⚹⚹⚹⚹"])
  ;; (setq org-modern-keyword
  ;;       '((t . t)
  ;;       ("bibliography" . "")
  ;;       ("cite_export" . "⮭")
  ;;       ("include" . "⇤")
  ;;       ("setupfile" . "⇚")
  ;;       ("html_head" . "🅷")
  ;;       ("html" . "🅗")
  ;;       ("latex_class" . "🄻")
  ;;       ("latex_header" . "🅻")
  ;;       ("latex_header_extra" . "🅻⁺")
  ;;       ("latex" . "🅛")
  ;;       ("beamer_theme" . "🄱")
  ;;       ("beamer_header" . "🅱")
  ;;       ("beamer" . "🅑")
  ;;       ("attr_latex" . "🄛")
  ;;       ("attr_html" . "🄗")
  ;;       ("attr_org" . "⒪")
  ;;       ("header" . "›")
  ;;       ("caption" . "☰")
  ;;       ("name" . "⁝")
  ;;       ("results" . "∴")))
  ;;  (setq org-modern-block-name
  ;;     '((t . t)
  ;;       ("src" "»" "∥")
  ;;       ("example" "»–" "∥")
  ;;       ("quote" "❝" "❞")))
   )

;; ---(org-superstar)------------------------------------------------------------------------

;; ;; Nice bullet points. Retires org-bullets.
;; (use-package org-superstar
;;   :ensure t
;;   :after org
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))
;;   (setq org-superstar-item-bullet-alist
;;         '((?* . ?•)
;;           (?+ . ?➤)
;;           (?- . ?•))))
;; org-style ends here

;; Org babel
;; #+NAME: org-babel

;; [[file:site-pkgs.org::org-babel][org-babel]]
;; ---(org-babel)------------------------------------------------------------------------

;; (use-package ob-restclient
;;   :ensure t
;;   :mode (("\\.http\\'" . restclient-mode)))

(use-package ob-restclient
  :ensure t
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package ob-http
  :ensure t
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t))))

(use-package  ob-async
  :ensure t
  :after (org)
  :config (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))
;; org-babel ends here

;; Org denote
;; #+NAME: org-denote

;; [[file:site-pkgs.org::org-denote][org-denote]]
  ;; ---(org-denote)------------------------------------------------------------------------

  ;; @see: https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.org?ref_type=heads
  ;; @see: https://github.com/whhone/whhone.github.io/blob/3289ba83472ecabf09b06bb124097d5a100f7618/content/emacs-config.md
  (use-package denote
    :ensure t
    :init
    (setq denote-directory (expand-file-name (h7/var-denote-directory)))
    ;;:custom
    ;;
    ;;
    :hook
    (dired-mode . denote-dired-mode)
    :bind
    ("C-c n n" . 'denote)
    ("C-c n f" . 'denote-open-or-create)
    ("C-c n i" . 'denote-link)
    ("C-c n k" . 'denote-keywords-add)    ;; update file name automatically
    ("C-c n K" . 'denote-keywords-remove) ;; update file name automatically
    ("C-c n u" . 'denote-rename-file-using-front-matter)
    ("C-c n l" . 'denote-link-find-backlink)
    ;; :bind-keymap
    ;; ("C-c n d" . org-roam-dailies-map)
    :config
    (setq denote-known-keywords '("<meta>"))
    ;; (setq denote-prompts '(subdirectory title))
    (setq denote-excluded-directories-regexp ".attachment")

    ;; ;; Makes the denote links different from usual link.
    (set-face-attribute 'denote-faces-link
                        nil :foreground "magenta" :inherit 'link)

    ;; ;; Remove the date and the identifier. They are duplicated with the file name.
    ;; ;; I want to remove filetags too but denote-keyword-* need that.
    ;; (setq denote-org-front-matter "#+title: %1$s\n#+filetags: %3$s\n")
    
    )



  ;; ---(org-denote-menu)------------------------------------------------------------------------

   (use-package denote-menu
    :ensure t
    :after (denote)
     ;; Bind all available commands
     :bind (("C-c N z" . list-denotes)
            ("C-c N c" . denote-menu-clear-filters)
            ("C-c N r" . denote-menu-filter)
            ("C-c N k" . denote-menu-filter-by-keyword)
            ("C-c N o" . denote-menu-filter-out-keyword)
            ("C-c N e" . denote-menu-export-to-dired))
     )

     
  ;; ---(org-denote-refs)------------------------------------------------------------------------

  (use-package denote-refs
    :ensure t
    :after (denote)
    ;; :straight
    ;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :quelpa (denote-refs :fetcher git :url "https://codeberg.org/akib/emacs-denote-refs.git")
    ;; ;; normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;; ;; a hookable mode anymore, you're advised to pick something yourself
    ;; ;; if you don't care about startup time, use
    
    ;; :hook
    ;; (denote-mode . denote-refs-mode))
  )
;; org-denote ends here

;; Org noter
;; #+NAME: org-noter

;; [[file:site-pkgs.org::org-noter][org-noter]]
;; ---(org-noter)------------------------------------------------------------------------

;; @see: https://rgoswami.me/posts/org-note-workflow/

(use-package org-noter
  :after (:any org pdf-view)
  :if (h7/use-pdf-tools)
  :ensure t
  ;; :disabled t
  :config
  (require 'org-noter-pdftools)
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil


   org-noter-set-auto-save-last-location t
  
   ;; Everything is relative to the main notes file
   ;;org-noter-notes-search-path (list org_notes)
   
   org-noter-default-notes-file-names '("notes.org")

   )

  ;; Configure org-mode to open PDFs with pdf-tools
  (add-to-list 'org-file-apps '(pdf . (lambda (file link)
                                        (org-noter file))))

  ;; Keybindings for easy annotation using org-noter
  ;; (global-set-key (kbd "C-c n") 'org-noter)

  ;; Enable visual-line-mode for better readability of annotations
  ;; (add-hook 'org-noter-mode-hook 'visual-line-mode)
  
  )


;;@see: https://github.com/fuxialexander/org-pdftools

(use-package org-pdftools
  :after (:any org pdf-view)
  :if (h7/use-pdf-tools)
  :ensure t
  ;; :disabled t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :if (h7/use-pdf-tools)
  :ensure t
  ;; :disabled t
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;; org-noter ends here

;; Org citar
;; #+NAME: org-citar

;; [[file:site-pkgs.org::org-citar][org-citar]]
  ;; ---( Citar )--------------------------------------------------------------

   ;; @see: https://github.com/emacs-citar/citar#installation

   (use-package citar
     ;; :disabled t
     :ensure t
     ;; :defer t
     :no-require
     :custom
     ;;(org-cite-global-bibliography '("~/bib/references.bib"))
     ;;(citar-bibliography (h7/var-global-bibliography))
     (citar-bibliography (h7/var-global-bibliography))
     (org-cite-global-bibliography (h7/var-global-bibliography))
     (org-cite-insert-processor 'citar)
     (org-cite-follow-processor 'citar)
     (org-cite-activate-processor 'citar)
     :hook
     (LaTeX-mode . citar-capf-setup)
     (org-mode . citar-capf-setup)
     :bind
     ;; optional: org-cite-insert is also bound to C-c C-x C-@
     (:map org-mode-map :package org ("C-c b b" . #'org-cite-insert))
     (:map org-mode-map :package org ("C-c b c" . #'citar-insert-citation))
     (:map org-mode-map :package org ("C-c b r" . #'citar-insert-reference))
     (:map org-mode-map :package org ("C-c b o" . #'citar-open-notes))
     :config
     (defvar citar-indicator-notes-icons
       (citar-indicator-create
        :symbol (all-the-icons-material
                 "speaker_notes"
                 :face 'all-the-icons-blue
                 :v-adjust -0.3)
        :function #'citar-has-notes
        :padding "  "
        :tag "has:notes"))     
     )


   (use-package citar-embark
     ;; :disabled t
     :ensure t
     ;; :defer t
     :after citar embark
     :no-require
     :config
     (setq citar-at-point-function 'embark-act)
     (citar-embark-mode)
     )

   (use-package citar-denote
    :ensure t
    :after (citar denote)
    :custom
    ;; Allow multiple notes per bibliographic entry
    (citar-open-always-create-notes nil)
    ;; Use package defaults
    (citar-denote-file-type 'org)
    (citar-denote-subdir nil)
    (citar-denote-signature nil)
    (citar-denote-template nil)
    (citar-denote-keyword "bib")
    (citar-denote-use-bib-keywords nil)
    (citar-denote-title-format "title")
    (citar-denote-title-format-authors 1)
    (citar-denote-title-format-andstr "and")
    :init
    (citar-denote-mode)
    ;; Bind all available commands
    :bind (("C-c w c" . citar-create-note)
           ("C-c w n" . citar-denote-open-note)
           ("C-c w d" . citar-denote-dwim)
           ("C-c w e" . citar-denote-open-reference-entry)
           ("C-c w a" . citar-denote-add-citekey)
           ("C-c w k" . citar-denote-remove-citekey)
           ("C-c w r" . citar-denote-find-reference)
           ("C-c w l" . citar-denote-link-reference)
           ("C-c w f" . citar-denote-find-citation)
           ("C-c w x" . citar-denote-nocite)
           ("C-c w y" . citar-denote-cite-nocite))
    )
;; org-citar ends here

;; Org consult
;; #+NAME: org-consult

;; [[file:site-pkgs.org::org-consult][org-consult]]
  ;; ---( consul-notes )--------------------------------------------------------------

   ;; @see: https://github.com/mclear-tools/consult-notes

(use-package consult-notes
  ;; :disabled t
  :ensure t
  ;; :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam 
             ;; consult-notes-org-roam-find-node
             ;; consult-notes-org-roam-find-node-relation
             )
  :config
  ;; (setq consult-notes-file-dir-sources '(("Name"  ?key  "path/to/dir"))) ;; Set notes dir(s), see below
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  ;; (setq consult-notes-org-headings-files '("~/path/to/file1.org"
  ;;                                         "~/path/to/file2.org"))
  
  ;;(consult-notes-org-headings-mode)
  
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files))

  ;; (defun consult-notes-my-embark-function (cand)
  ;;   "Do something with CAND"
  ;;   (interactive "fNote: ")
  ;;   (my-function))

  ;; (defvar-keymap consult-notes-map
  ;;   :doc "Keymap for Embark notes actions."
  ;;   :parent embark-file-map
  ;;   "m" #'consult-notes-my-embark-function)

  ;; (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

  ;; ;; make embark-export use dired for notes
  ;; (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)


  )
;; org-consult ends here

;; Org ref
;; #+NAME: org-ref

;; [[file:site-pkgs.org::org-ref][org-ref]]
;; ---(org-ref)------------------------------------------------------------------------

(use-package org-ref
  :after org
  :disabled t ;; deps: helm
  ;;  :ensure t
  :init
  (setq reftex-default-bibliography (h7/var-global-bibliography))
  (setq org-ref-bibliography-notes "~/Dropbox/Local/data/org/ref/notes.org"
        org-ref-default-bibliography (h7/var-global-bibliography)
        org-ref-pdf-directory "~/Dropbox/Local/docs/papers/")

  ;; (setq helm-bibtex-bibliography "~/Dropbox/Local/data/org/ref/references.bib")
  ;; (setq helm-bibtex-library-path "~/Dropbox/Local/docs/papers/")

  ;; (setq helm-bibtex-pdf-open-function
  ;;       (lambda (fpath)
  ;;         (start-process "open" "*open*" "open" fpath)))

  ;; (setq helm-bibtex-notes-path "~/Dropbox/Local/data/org/ref/notes.org")
  
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

(use-package org-ref-bibtex
  :after org
  :disabled t
  ;;  :ensure t
  :init
  (setq org-ref-bibtex-hydra-key-binding "\C-cj"))

(use-package doi-utils
  :after org
  :disabled t
  ;;  :ensure t
  )
;; org-ref ends here

;; Org roam
;; #+NAME: org-roam

;; [[file:site-pkgs.org::org-roam][org-roam]]
  ;; ---(org-roam)------------------------------------------------------------------------

  ;; @see: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
  (use-package org-roam
    :disabled t
    ;;  :ensure t
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-directory (file-truename (h7/var-roam-directory)))
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


  ;; ---(org-roam-bibtex)------------------------------------------------------------------------



  (use-package org-roam-bibtex
    :disabled t
    ;;  :ensure t
    :after org-roam
    :config
    (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


  ;; @see: https://benswift.me/blog/2020/12/16/configuring-spacemacs-org-roam-org-noter-for-academic-writing-bliss/

;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :custom
;;   (orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
;;   (orb-process-file-keyword t)
;;   (orb-file-field-extensions '("pdf" "epub" "html"))

;;   (orb-templates
;;    '(("r" "ref" plain (function org-roam-capture--get-point)
;;       ""
;;       :file-name "${citekey}"
;;       :head "#+TITLE: ${citekey}: ${title}
;; #+ROAM_KEY: ${ref}

;; - tags ::
;; - keywords :: ${keywords}

;; * ${title}
;;   :PROPERTIES:
;;   :Custom_ID: ${citekey}
;;   :URL: ${url}
;;   :AUTHOR: ${author-or-editor}
;;   :NOTER_DOCUMENT: ${file}
;;   :NOTER_PAGE:
;;   :END:"))))


  ;; ---(org-roam-ui)------------------------------------------------------------------------

  (use-package org-roam-ui
    :disabled t
    ;;  :ensure t
    ;; :straight
    ;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    ;; :quelpa (org-roam-ui :fetcher github :repo "org-roam/org-roam-ui")
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

  ;; ---(org-roam-citar)------------------------------------------------------------------------

  (use-package citar-org-roam
    :disabled t
    ;;  :ensure t
    :after (citar org-roam)
    :config (citar-org-roam-mode)
    :custom
    (citar-notes-paths (list org-roam-directory)) ; List of directories for reference nodes
    (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
    (citar-at-point-function 'embark-act)           ; Use `embark'    
    )
;; org-roam ends here

;; Org export
;; #+NAME: org-export

;; [[file:site-pkgs.org::org-export][org-export]]
;; ---(org-export)------------------------------------------------------------------------

;; @see: https://github.com/jkitchin/ox-ipynb
(use-package ox-ipynb
  :disabled t
)
;; org-export ends here

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

;; Jump/begin
;; #+NAME: jump-begin

;; [[file:site-pkgs.org::jump-begin][jump-begin]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @JUMP
  ;; ;;;////////////////////////////////////////////////////////////////
;; jump-begin ends here

;; Dashboard
;; #+NAME: dashboard

;; [[file:site-pkgs.org::dashboard][dashboard]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @DASHBOARD
  ;; ;;;////////////////////////////////////////////////////////////////

  ;; ---( dashboard )--------------------------------------------------------------

  ;; @see: https://github.com/emacs-dashboard/emacs-dashboard/
  ;; @see: https://config.phundrak.com/emacs/packages/visual-config.html
  ;; @see: https://unixbhaskar.wordpress.com/2023/07/26/emacss-configuration-of-mine/

  
  (use-package dashboard
    :ensure t
    :after all-the-icons
    :custom
    (dashboard-set-navigator t)
    (dashboard-set-file-icons t)
    (dashboard-set-heading-icons t)
    (dashboard-icon-type 'all-the-icons)
    (dashboard-projects-backend 'projectile)
    (dashboard-set-init-info t)
    (dashboard-startup-banner 'logo)
    (dashboard-image-banner-max-width 80)
    (dashboard-items '((recents . 3)
                       (projects . 5)
                       (bookmarks . 1)
                       (agenda . 5)))
    (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    :config
    ;;(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
    (setq dashboard-set-navigator t)
    (setq dashboard-navigator-buttons `((
      ("★" "scratch" "show scratch buffer" (lambda (&rest _) (switch-to-buffer "*scratch*")) warning "⋗" " ")
      ("★" "restore" "desktop" (lambda (&rest _) (desktop-read)) warning "⋗" " ")
      ("★" "marks" "bookmrks" (lambda (&rest _)
                                (require 'bookmark)                  
                                (bookmark-bmenu-list)                
                                (switch-to-buffer "*Bookmark List*")
                                ) warning "⋗" " ")
      ("★" "project" "project files" (lambda (&rest _) (project-dired)) warning "⋗" " ")
      ("★" "files" "recent files" (lambda (&rest _) (recentf-open-files)) warning "⋗" " ")
      ("★" "view" "view url" (lambda (&rest _) (crux-view-url)) warning "⋗" " ")
      ;;("★" "packages" "list packages" (lambda (&rest _) (list-packages)) warning "⋗" " ")
      ;;("★" "mail" "start mu4e" (lambda (&rest _) (mu4e)) warning "⋗" " ")
      ("★" "term" "project vterm" (lambda (&rest _) (projectile-run-vterm)) warning "⋗" " ")
      ("★" "shell" "project eshell" (lambda (&rest _) (project-eshell)) warning "⋗" " ")
      ("★" "feed" "start elfeed" (lambda (&rest _) (elfeed)) warning "⋗" " ")
      ("★" "restart" "restart emacs" (lambda (&rest _) (restart-emacs)) warning "⋗" " ")
      ("★" "manual" "emacs manual" (lambda (&rest _) (info-emacs-manual)) warning "⋗" " ")
      )))

    ;; See dashboard-startupify-list for all the widgets avalaibles.

    (setq dashboard-startupify-list '(dashboard-insert-banner
                                      dashboard-insert-newline
                                      dashboard-insert-banner-title
                                      dashboard-insert-newline
                                      dashboard-insert-navigator
                                      dashboard-insert-items
                                      ;; dashboard-insert-newline
                                      ;; dashboard-insert-init-info
                                      dashboard-insert-newline
                                      dashboard-insert-footer))    
    ;; (add-to-list 'dashboardtems '(agenda) t)
    (setq dashboard-week-agenda t)
    ;; (setq dashboard-org-agenda-categories '("Tasks" "Diary" "Notes"))
    ;; (setq dashboard-org-agenda-categories '("@M" "@W" "@H"))
    ;; (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)    
    ;; (setq dashboard-agenda-sort-strategy 'priority-up)    
    (dashboard-setup-startup-hook)
    ;; :hook (emacs-startup-hook . dashboard-open)
    ;; :bind
    ;;  ("<C-i> i" . dashboard-open)
    )

  ;; ;; @see: https://gitlab.com/jdm204/dotfiles/-/blob/master/config.org
  ;; (use-package dashboard
  ;;   :ensure t
  ;;   :config
  ;;   (dashboard-setup-startup-hook)
  ;;   (setq
  ;;    dashboard-set-footer nil
  ;;    dashboard-set-heading-icons nil
  ;;    dashboard-set-file-icons t
  ;;    ;;dashboard-icon-type 'nerd-icons
  ;;    dashboard-set-navigator t
  ;;    dashboard-set-init-info t
  ;;    dashboard-center-content t
  ;;    dashboard-startup-banner 'logo
  ;;    dashboard-image-banner-max-width 80
  ;;    ;; dashboard-projects-backend 'project-el
  ;;    dashboard-items '((recents  . 5)
  ;;                      (bookmarks . 5)
  ;;                      (projects . 5)
  ;;                      (agenda . 5))



  ;; }}}  .dashboard
;; dashboard ends here

;; Register
;; #+NAME: register

;; [[file:site-pkgs.org::register][register]]
  ;; ;;;////////////////////////////////////////////////////////////////
  ;; {{{  @REGISTER
  ;; ;;;////////////////////////////////////////////////////////////////

  ;; ---( buffer registers )--------------------------------------------------------------

  (set-register ?w '(buffer . "*Warnings*"))
  (set-register ?m '(buffer . "*Messages*"))
  (set-register ?d '(buffer . "*dashboard*"))
  (set-register ?s '(buffer . "*scratch*"))
  (set-register ?v '(buffer . "*vterm*"))

  ;; ---( file registers )--------------------------------------------------------------

  (set-register ?e '(file . "~/.emacs-site/site-pkgs.org"))
  (set-register ?a '(file . "~/.emacs-agenda.txt"))
  (set-register ?f '(file . "~/.rss/elfeed.org"))
  (set-register ?t '(file . "~/work/bv/box-up/todo.org"))


  ;; }}}  .register
;; register ends here

;; Hydra
;; #+NAME: jump-hydra

;; [[file:site-pkgs.org::jump-hydra][jump-hydra]]
    ;; ---( hydras )--------------------------------------------------------------

    ;; (defhydra hydra-misc (:exit t)
    ;;     ;; ("j" my-helm-journal "Journal")
    ;;     ;; ("C" my-resolve-orgzly-syncthing "Conflicts")
    ;;     ;; ("n" my-capture-timestamped-note "Note")
    ;;     ;; ("c" my-org-categorize-emacs-news/body "Categorize")
    ;;     ;; ("d" my-emacs-news-check-duplicates "Dupe")
    ;;     ("s" save-buffer "Save")
    ;;     ;; ("f" my-file-shortcuts/body "File shortcut")
    ;;     ;; ("G" gif-screencast-start-or-stop "GIF screencast")
    ;;     ;; ("g" my-geeqie/body "Geeqie")
    ;;     ;; ("r" my-record-ffmpeg-toggle-recording "Record screen")
    ;;     ;; ("l" (my-toggle-or-create "*scratch*" (lambda () (switch-to-buffer (startup--get-buffer-create-scratch)))) "Lisp")
    ;;     ("e" eshell-toggle "Eshell")
    ;;     ;; ("w" my-engine-dmode-hydra/body "Search web")
    ;;     ;; ("E" my-emacs-news/body "Emacs News")
    ;;     )

    ;; (defhydra hydra-ui (:exit nil)
    ;;   ("+" text-scale-increase "Increase")
    ;;   ("-" text-scale-decrease "Decrease")
    ;;   ("<left>" windmove-left)
    ;;   ("<right>" windmove-right)
    ;;   ("<down>" windmove-down)
    ;;   ("<up>" windmove-up)
    ;;   ("y" other-window "other")
    ;;   ("h" switch-window "switch-window")
    ;;   ("b" consult-buffer "buffer")
    ;;   ("f" find-file "file")
    ;;   ("F" find-file-other-window "other file")
    ;;   ("v" (progn (split-window-right) (windmove-right)))
    ;;   ("o" delete-other-windows :color blue)
    ;;   ("a" ace-window)
    ;;   ("s" ace-swap-window)
    ;;   ("d" delete-window "delete")
    ;;   ("D" ace-delete-window "ace delete")
    ;;   ("i" ace-maximize-window "maximize")
    ;;   ("q" nil)
    ;;   )
    

    ;; (defhydra hydra-buffer-menu (:color pink
    ;;                              :hint nil)
    ;;   "
    ;; ^Mark^             ^Unmark^           ^Actions^          ^Search
    ;; ^^^^^^^^-----------------------------------------------------------------
    ;; _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
    ;; _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
    ;; _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
    ;; _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
    ;; _~_: modified
    ;; "
    ;;   ("m" Buffer-menu-mark)
    ;;   ("u" Buffer-menu-unmark)
    ;;   ("U" Buffer-menu-backup-unmark)
    ;;   ("d" Buffer-menu-delete)
    ;;   ("D" Buffer-menu-delete-backwards)
    ;;   ("s" Buffer-menu-save)
    ;;   ("~" Buffer-menu-not-modified)
    ;;   ("x" Buffer-menu-execute)
    ;;   ("b" Buffer-menu-bury)
    ;;   ("g" revert-buffer)
    ;;   ("T" Buffer-menu-toggle-files-only)
    ;;   ("O" Buffer-menu-multi-occur :color blue)
    ;;   ("I" Buffer-menu-isearch-buffers :color blue)
    ;;   ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ;;   ("c" nil "cancel")
    ;;   ("v" Buffer-menu-select "select" :color blue)
    ;;   ("o" Buffer-menu-other-window "other-window" :color blue)
    ;;   ("q" quit-window "quit" :color blue))

    ;; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


    ;; (defhydra hydra-main (:exit t :color pink :hint nil)
    ;;   "
    ;; ^Misc^             ^Edit^           ^Lang^          ^Search
    ;; ^^^^^^^^-----------------------------------------------------------------
    ;; _m_: misc          _b_: buffers      _d_: debug      ^ ^
    ;; ^ ^                _u_: UI           ^ ^             ^ ^
    ;; ^ ^                ^ ^               ^ ^             ^ ^
    ;; "
    ;;     ("m" hydra-misc/body "Misc")
    ;;     ("b" hydra-buffer-menu/body "Buffers")
    ;;     ("u" hydra-ui/body "UI")
    ;;     ("d" dap-hydra/body "Debug")
    ;;     )

    ;; (global-set-key (kbd "<f8>") #'hydra-main/body)


    ;; ---( engine-mode )--------------------------------------------------------------

(use-package engine-mode
      :ensure t)
  
(engine-mode t)

;; Default prefix is C-x /
;; Define the engine
(defengine github
     "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")

(defengine google 
    "https://www.google.com/search?q=%s"
    :keybinding "g")

(defengine pubmed
        "https://pubmed.ncbi.nlm.nih.gov/?term=%s"
        :keybinding "m")

;; Protein Databank 
(defengine pdb
                "https://www.rcsb.org/search?q=%s"
                :keybinding "p")

(defengine stack-overflow
          "https://stackoverflow.com/search?q=%s"
          :keybinding "o")

(defengine reddit 
            "https://www.reddit.com/search/?q=%s" 
            :keybinding "r")

(defengine sciencedirect 
   "https://www.sciencedirect.com/search?qs=%s"
    :keybinding "s")

(defengine wikipedia
      "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "k"
      :docstring "Searchin' the wikis.")

(defengine wolfram 
    "https://www.wolframalpha.com/input/?i=%s" 
    :keybinding "w")

(defengine youtube 
    "https://www.youtube.com/results?search_query=%s"
    :keybinding "y")

(defhydra hydra-engine
  (:color amaranth)
  "Send selected text to website."
  ("h" engine/search-github          "Github")
  ("g" engine/search-google          "Google")
  ("m" engine/search-pubmed          "PubMed")
  ("p" engine/search-pdb             "PDB")
  ("o" engine/search-stackoverflow   "StackOverflow")
  ("r" engine/search-reddit          "Reddit")
  ("s" engine/search-sciencedirect   "Science Direct")
  ("k" engine/search-wikipedia       "Wikipedia")
  ("w" engine/search-wolframalpha    "Wolfram Alpha")
  ("y" engine/search-youtube         "YouTube")
  
  ("\\" hydra-of-hydras/body "back")
  ("<tab>" hydra-of-hydras/body "back")
  ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 6") 'hydra-engine/body)

    ;; ---( hydra-orgnoter )--------------------------------------------------------------


(defhydra hydra-orgnoter (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move                          Annotations  Setup      Control  │ org-noter │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_p_^^          [_i_] insert    [_K_] skeleton    [_N_] open session
         ^^_g_^^          [_I_] insert+   [_C_] columns     [_Q_] close session
         ^^_n_^^
   --------------------------------------------------------------------------------
        "
        ("i" org-noter-insert-note)
        ("I" org-noter-pdftools-insert-precise-note)
        ("n" org-noter-sync-next-note)
        ("p" org-noter-sync-prev-note)
        ("g" org-noter-sync-current-note)
        ("C" org-noter-pdf-set-columns :color green)
        ("K" org-noter-pdftools-create-skeleon :color green)
        ("Q" org-noter-kill-session :color red)
        ("N" org-noter :color red)
        ("\\" hydra-pdftools/body "[pdf-tools]")
        ("<tab>" hydra-pdftools/body "back")
        ("<ESC>" nil "quit"))


    ;; ---( hydra-orgnoter )--------------------------------------------------------------

(defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y" pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red)
        ("." hydra-orgnoter/body "[org-noter]")
        ("\\" hydra-of-hydras/body "back")
        ("<tab>" hydra-of-hydras/body "back")
        ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 4") 'hydra-pdftools/body)


    ;; ---( hydra-denote )--------------------------------------------------------------

    (defhydra hydra-denote (:color blue :hint nil)
              "
                                                                       ╭─────────┐
         Denote           List              Citar         Refs         │ Denote  │
  ╭────────────────────────────────────────────────────────────────────┴─────────╯
       [_nn_] create    [_NN_] list       [_bb_] org    [_wc_] note
       [_nf_] open      [_Nc_] clear      [_bc_] cite   [_wn_] open    
       [_ni_] link      [_Nr_] filter     [_br_] ref    [_wd_] dwim
       [_nI_] links     [_Nk_] in by key  [_bo_] note   [_we_] ref-entry
       [_nk_] key add   [_No_] out by key             [_wa_] add cite
       [_nK_] key del   [_Ne_] to dired               [_wk_] del cite
       [_nu_] rename                                [_wr_] find ref
       [_nl_] rev-find  [_ll_] consult                [_wl_] link ref
       [_nb_] rev-list                              [_wf_] find cite
                                                  [_wx_] nocite
                                                  [_wy_] cite-nocite
  --------------------------------------------------------------------------------
              "
              ("nn" denote)
              ("nf" denote-open-or-create)
              ("ni" denote-link)
              ("nI" denote-add-links)
              ("nk" denote-keywords-add)
              ("nK" denote-keywords-remove)
              ("nu" denote-rename-file-using-front-matter)
              ("nl" denote-link-find-backlink)
              ("nb" denote-backlinks)

              ("NN" list-denotes)
              ("Nc" denote-menu-clear-filters)
              ("Nr" denote-menu-filter)
              ("Nk" denote-menu-filter-by-keyword)
              ("No" denote-menu-filter-out-keyword)
              ("Ne" denote-menu-export-to-dired)
              
              ("ll" consult-notes)

              ("bb" org-cite-insert)
              ("bc" citar-insert-citation)
              ("br" citar-insert-reference)
              ("bo" citar-open-notes)

              ("wc" citar-create-note)
              ("wn" citar-denote-open-note)
              ("wd" citar-denote-dwim)
              ("we" citar-denote-open-reference-entry)
              ("wa" citar-denote-add-citekey)
              ("wk" citar-denote-remove-citekey)
              ("wr" citar-denote-find-reference)
              ("wl" citar-denote-link-reference)
              ("wf" citar-denote-find-citation)
              ("wx" citar-denote-nocite)
              ("wy" citar-denote-cite-nocite)
              
              ("p"   hydra-pdftools/body "[pdf-tools]" :color red)
              ("."   hydra-orgnoter/body "[org-noter]" :color red)

              ("\\" hydra-of-hydras/body "back")
              ("<tab>" hydra-of-hydras/body "back")
              ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 5") 'hydra-denote/body)


    ;; ---( hydra-buffers )--------------------------------------------------------------

    (defhydra hydra-buffers (:color blue :hint nil)
              "
                                                                       ╭─────────┐
     Move to Window         Switch                  Do                 │ Buffers │
  ╭────────────────────────────────────────────────────────────────────┴─────────╯
           ^_k_^          [_b_] switch (ido)       [_d_] kill the buffer
           ^^↑^^          [_i_] ibuffer            [_r_] toggle read-only mode
       _h_ ←   → _l_      [_a_] alternate          [_u_] revert buffer changes
           ^^↓^^          [_s_] switch (helm)      [_w_] save buffer
           ^_j_^
  --------------------------------------------------------------------------------
              "
              ("a" joe-alternate-buffers)
              ("b" ido-switch-buffer)
              ("d" joe-kill-this-buffer)
              ("i" ibuffer)
              ("h" buf-move-left  :color red)
              ("k" buf-move-up    :color red)
              ("j" buf-move-down  :color red)
              ("l" buf-move-right :color red)
              ("r" read-only-mode)
              ("s" helm-buffers-list)
              ("u" joe-revert-buffer)
              ("w" save-buffer)
              ("\\" hydra-of-hydras/body "back")
              ("<tab>" hydra-of-hydras/body "back")
              ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 3") 'hydra-buffers/body)

    ;; ---( hydra-projects )--------------------------------------------------------------

    (defhydra hydra-projects (:color blue :hint nil)
              "
                                                                      ╭──-───────┐
         Projects         Session                 Bookmarks           │ Projects │
  ╭───────────────────────────────────────────────────────────────────┴─-────────╯
       [_pD_] dired       [_ss_] save-session     [_b_] bookmarks
       [_pf_] find-file   [_sr_] read-session     [_d_] dashboard
       [_pg_] grep-file
  --------------------------------------------------------------------------------
              "
              ("pD" project-dired)
              ("pf" project-find-file)
              ("pg" project-find-regexp)
              ("ss" desktop-save)
              ("sr" desktop-read)
              ("b"  bookmark-bmenu-list)
              ("d"  dashboard-open)
              ("\\" hydra-of-hydras/body "back")
              ("<tab>" hydra-of-hydras/body "back")
              ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 2") 'hydra-projects/body)

    ;; ---( hydra-window )--------------------------------------------------------------

    (defhydra hydra-window (:color blue :hint nil)
            "
                                                                       ╭─────────┐
     Move to      Size    Scroll        Split                    Do    │ Windows │
  ╭────────────────────────────────────────────────────────────────────┴─────────╯
        ^_k_^           ^_K_^       ^_p_^    ╭─┬─┐^ ^        ╭─┬─┐^ ^         ↺ [_u_] undo layout
        ^^↑^^           ^^↑^^       ^^↑^^    │ │ │_v_ertical ├─┼─┤_b_alance   ↻ [_r_] restore layout
    _h_ ←   → _l_   _H_ ←   → _L_   ^^ ^^    ╰─┴─╯^ ^        ╰─┴─╯^ ^         ✗ [_d_] close window
        ^^↓^^           ^^↓^^       ^^↓^^    ╭───┐^ ^        ╭───┐^ ^         ⇋ [_w_] cycle window
        ^_j_^           ^_J_^       ^_n_^    ├───┤_s_tack    │   │_z_oom
        ^^ ^^           ^^ ^^       ^^ ^^    ╰───╯^ ^        ╰───╯^ ^       
  --------------------------------------------------------------------------------
            "
            ("n" joe-scroll-other-window :color red)
            ("p" joe-scroll-other-window-down :color red)
            ("b" balance-windows)
            ("d" delete-window)
            ("H" shrink-window-horizontally :color red)
            ("h" windmove-left :color red)
            ("J" shrink-window :color red)
            ("j" windmove-down :color red)
            ("K" enlarge-window :color red)
            ("k" windmove-up :color red)
            ("L" enlarge-window-horizontally :color red)
            ("l" windmove-right :color red)
            ("r" winner-redo :color red)
            ("s" split-window-vertically :color red)
            ("u" winner-undo :color red)
            ("v" split-window-horizontally :color red)
            ("w" other-window)
            ("z" delete-other-windows)
            ("\\" hydra-of-hydras/body "back")
            ("<tab>" hydra-of-hydras/body "back")
            ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 1") 'hydra-window/body)



    ;; ---( hydras )--------------------------------------------------------------


(defhydra hydra-of-hydras (:hint nil)
"
 ^
   ^Hydras                                      Prefix
   ^─────────------------------------------------------
   _w_ windows            C-c 1
   _p_ projects           C-c 2
   _b_ buffers            C-c 3
   _d_ pdf-tools          C-c 4
   _n_ denote             C-c 5
   _e_ engine-mode        C-c 6
   ^────────-------------------------------------------
   "

  ("e"   hydra-engine/body :color amaranth)
  ("n"   hydra-denote/body :color green)
  ("d"   hydra-pdftools/body :color blue)
  ("b"   hydra-buffers/body :color blue)
  ("p"   hydra-projects/body :color blue)
  ("w"   hydra-window/body :color blue)
  ("<ESC>" nil "quit"))

(global-set-key (kbd "C-c 0") 'hydra-of-hydras/body)
;; jump-hydra ends here

;; Jump/end
;; #+NAME: jump-end

;; [[file:site-pkgs.org::jump-end][jump-end]]
  ;; }}}  .jump
;; jump-end ends here

;; Log: finish
;; #+NAME: log-finish

;; [[file:site-pkgs.org::log-finish][log-finish]]
;; vim: noet sw=4 ts=4 fdm=marker foldcolumn=0

;; ---( site.pkgs: end )-------------------------------------------------------
(message "SITE:PKGS - end")

(provide 'site-pkgs)
;;; site-pkgs.el ends here
;; log-finish ends here
