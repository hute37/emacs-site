#+TITLE: Site Packages Configuration
#+PROPERTY: header-args :results silent

* Initialization & Core

;;; #PNX(ENH-01): Cleaned up dead/unloaded legacy files (site-load.el, site-init.el, site-attic.org, site-config.org, site-custom.el, site-local.el)

(use-package bind-key :ensure t)
(use-package diminish :ensure t)

* Completion "at-point"

;;; #PNX(OBS-01): Removed obsolete company, company-posframe, and company backend packages
;;; #PNX(DUP-01): Resolved dual completion stack by standardizing entirely on Corfu and Cape

(use-package corfu
:ensure t
:custom
(corfu-auto t)
(corfu-auto-delay 0.2)
(corfu-auto-prefix 2)
(corfu-cycle t)
(corfu-preselect 'prompt)
(corfu-quit-no-match 'separator)
(corfu-preview-current nil)
:hook (after-init . global-corfu-mode)
:bind
(:map corfu-map
("C-SPC" . corfu-complete)
("TAB"   . corfu-complete)
(\[tab\]   . corfu-complete)
("RET"   . corfu-insert)
("C-n"   . corfu-next)
("C-p"   . corfu-previous)
("C-g"   . corfu-quit)))

(use-package cape
:ensure t
:init
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-elisp-block))

* Project & Navigation

;;; #PNX(OBS-03): Removed obsolete etags and gtags use-package blocks in favor of project.el and xref

(use-package project
:ensure nil
:bind ("C-x p" . project-prefix-map))

* Lang :: Python / LSP

;;; #PNX(OBS-02): Removed global flycheck mode in favor of native flymake and lsp diagnostics
;;; #PNX(DUP-02): Enforced strict Python LSP/Eglot buffer-local coexistence (lsp-mode for standard scripts, eglot for notebooks)
;;; #PNX(DUP-03): Fixed malformed parentheses and logical condition in Python notebook hook

(use-package lsp-mode
:ensure t
:hook ((python-mode . lsp-deferred)
(python-ts-mode . lsp-deferred))
:commands lsp)

(defun h7/python-use-eglot ()
"Force Eglot for Python notebook buffers."
(when (bound-and-true-p lsp-mode)
(lsp-disconnect))
(eglot-ensure))

(add-hook 'python-ts-mode-hook
(lambda ()
(when (and buffer-file-name
(string-match-p "/notebooks/" buffer-file-name))
(remove-hook 'python-mode-hook #'lsp-deferred t)
(remove-hook 'python-ts-mode-hook #'lsp-deferred t)
(h7/python-use-eglot))))

(add-hook 'python-mode-hook
(lambda ()
(when (and buffer-file-name
(string-match-p "/notebooks/" buffer-file-name))
(remove-hook 'python-mode-hook #'lsp-deferred t)
(remove-hook 'python-ts-mode-hook #'lsp-deferred t)
(h7/python-use-eglot))))