(let* ((my-lisp-dir "~/.emacs.d/")
        (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;(if (boundp 'window-system)
;;    (set-frame-size (selected-frame) 132 60))

;;(setq frame-title-format "%b")
;;(column-number-mode t)
;;(setq inhibit-splash-screen t)

;;(menu-bar-mode 1)

;;(when (boundp 'tool-bar-mode)
;;  (tool-bar-mode -1))
;;(when (boundp 'scroll-bar-mode)
;;  (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

(transient-mark-mode t)
(delete-selection-mode 1)

(savehist-mode t)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-q") 'delete-other-windows)
(global-set-key (kbd "<M-left>") 'previous-buffer)
(global-set-key (kbd "<M-right>") 'next-buffer)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<f11>") 'slime)

;; Reenable # on UK Mac Keyboard inside Emacs
;;(fset 'insertPound "#")
;;(global-set-key (kbd "M-3") 'insertPound)

(when (boundp 'w32-initialized)
  (require 'setup-cygwin))

(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab) whitespace-line-column 132)

(require 'windmove)
(setq w32-lwindow-modifier 'super
      w32-rwindow-modifier 'super)
;;(global-set-key (kbd "<M-up>")  'windmove-up)
;;(global-set-key (kbd "<M-down>")  'windmove-down)

(require 'swbuff-y nil t)
(iswitchb-mode t)

(cua-mode)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(eval-after-load "tex" 
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
    )
  )

(add-hook 'LaTeX-mode-hook (lambda ()
  (push 
    '("Latexmk" "latexmk %s" TeX-run-TeX nil t
      :help "Run Latexmk on file")
    TeX-command-list)))

;;(setenv "PATH" (concat "/usr/local/bin:/usr/texbin:" (getenv "PATH")))

;; (eval-after-load "slime"
;;    '(progn
;;       (slime-setup '(slime-fancy slime-company))
;;       (setq   slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;; 	      slime-complete-symbol*-fancy t
;; 	      slime-fuzzy-completion-in-place t
;; 	      slime-net-coding-system 'utf-8-unix)
;;       (define-key slime-mode-map (kbd "s-SPC") 'slime-complete-symbol)))

(setq company-backends '(company-elisp 
			 (company-gtags company-etags company-dabbrev-code
					company-keywords)
			 company-files company-dabbrev))

(require 'company)
(setq company-idle-delay nil)
(global-company-mode t)
(global-set-key "\t" 'company-indent-or-complete)

(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key company-active-map "\e" 'company-abort)

(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-active-map "\t" 'company-select-next)

(defun company-indent-or-complete ()
  (interactive)
  (setq company-require-match nil)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(defun slime-repl-paredit-history-fix ()
  (defun slime-repl-history-pattern (&optional use-current-input)
    "Return the regexp for the navigation commands."
    (cond ((slime-repl-history-search-in-progress-p)
	   slime-repl-history-pattern)
	  (use-current-input
	   (assert (<= slime-repl-input-start-mark (point)))
	   (let ((str (slime-repl-current-input t)))
	     (cond ((string-match "^[ \n]*$" str) nil)
		   (t (concat "^" (regexp-quote (buffer-substring
						 slime-repl-input-start-mark
						 (if (> (point) slime-repl-input-start-mark)
						     (point)
						   point-max))))))))
	  (t nil))))

(defun slime-repl-paredit-curly-fix ()
  (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
    (define-key slime-repl-mode-map "{" 'paredit-open-curly)
    (define-key slime-repl-mode-map "}" 'paredit-close-curly)))

(add-hook 'slime-repl-mode-hook
           (lambda ()
 	    (define-key slime-repl-mode-map (kbd "TAB") 'company-indent-or-complete)
 	    (define-key slime-repl-mode-map (kbd "s-SPC") 'slime-complete-symbol)
 	    (slime-repl-paredit-history-fix)
 	    (slime-repl-paredit-curly-fix)))

(require 'clojure-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(defun slime-fuzzy-fix ()
  (defun slime-fuzzy-insert-completion-choice (completion max-length)
    (destructuring-bind (symbol-name score chunks classification-string) completion
      (let ((start (point))
	    (end))
	(insert symbol-name)
	(setq end (point))
	(dolist (chunk chunks)
	  (put-text-property (+ start (first chunk)) 
			     (+ start (first chunk) 
				(length (second chunk)))
			     'face 'bold))
	(put-text-property start (point) 'mouse-face 'highlight)
	(dotimes (i (- max-length (- end start)))
	  (insert " "))
	(insert (format " %s %-8.2f"
			classification-string
			(string-to-number score)))
	(insert "\n")
	(put-text-property start (point) 'completion completion)))))

(add-hook 'slime-mode-hook 'slime-fuzzy-fix)
(add-hook 'slime-repl-mode-hook 'slime-fuzzy-fix)

(autoload 'paredit-mode "paredit-22-beta/paredit.el" "Minor mode for pseudo-structurally editing" t)
(defun enable-paredit () (paredit-mode t))
;;(add-hook 'clojure-mode-hook 'enable-paredit)
;;(add-hook 'slime-mode-hook 'enable-paredit)
;;(add-hook 'slime-repl-mode-hook 'enable-paredit)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
 
 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(require 'hl-sexp)

 (add-hook 'clojure-mode-hook    'hl-sexp-mode)
 (add-hook 'slime-repl-mode-hook    'hl-sexp-mode)
 (add-hook 'emacs-lisp-mode-hook    'hl-sexp-mode)

(add-hook 'clojure-mode-hook    'highlight-parentheses-mode)
(add-hook 'slime-repl-mode-hook    'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook    'highlight-parentheses-mode)
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

(show-paren-mode t)
(setq show-paren-delay 50)

(require 'color-theme)

(color-theme-initialize)

;; (if (boundp 'w32-initialized)
(color-theme-deep-blue2)
;;  (progn (require 'zenburn)
;;         (zenburn)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(color-theme-load-all-themes nil)
 '(column-number-mode t)
 '(company-backends (quote (company-elisp (company-gtags company-etags company-dabbrev-code company-keywords) company-oddmuse company-files company-dabbrev)))
 '(completion-show-help nil)
 '(emacsw32-max-frames nil)
 '(icomplete-mode t)
 '(swbuff-y-mode t)
 '(w32shell-cygwin-bin "c:\\cygwin\\bin")
 '(word-wrap t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#102e4e" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "outline" :family "Monaco"))))
 '(hl-sexp-face ((((class color) (background dark)) (:background "#102048"))))
 '(show-paren-match ((((class color)) (:foreground "white" :background nil :weight ultra-bold))))
 '(show-paren-mismatch ((((class color)) (:background "red" :weight ultra-bold)))))

