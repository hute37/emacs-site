;;; $Id: .emacs,v 1.6 2005-12-05 03:06:24 hute37 Exp $

;;;  
;;; $Revision: 1.6 $
;;;  
;;; $Log: .emacs,v $
;;; Revision 1.6  2005-12-05 03:06:24  hute37
;;; ntemacs update
;;;
;;; Revision 1.4  2005/11/30 23:18:58  hute37
;;; upd
;;;
;;; Revision 1.3  2005/11/27 19:07:17  hute37
;;; kkv
;;;
;;; Revision 1.2  2005/11/27 19:05:39  hute37
;;; new configured dot
;;;
;;;
;;;

;; '(Info-additional-directory-list (quote ("D:\\usr\\share\\info")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs-start")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highligh-search nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(auto-compression-mode t nil (jka-compr))
 '(blink-matching-paren-on-screen t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox")
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cperl-clobber-lisp-bindings t)
 '(cperl-electric-keywords t)
 '(cperl-electric-lbrace-space t)
 '(cperl-electric-linefeed t)
 '(cperl-electric-parens t)
 '(cperl-font-lock t)
 '(cperl-hairy t)
 '(cperl-lazy-help-time 3)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "English")
 '(custom-enabled-themes (quote (tango-dark)))
 '(cvs-allow-dir-commit t)
 '(default-input-method "latin-1-prefix")
 '(display-buffer-reuse-frames t)
 '(ecb-options-version "2.32")
 '(elfeed-sort-order (quote ascending))
 '(eudc-default-return-attributes (quote all))
 '(eudc-protocol (quote ldap))
 '(eudc-server "localhost")
 '(explicit-shell-file-name "/bin/bash")
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil nil (hl-line))
 '(grep-command "egrep -n -e ")
 '(hc-ctrl-backslash-completes-a-la-mode t)
 '(hc-ctrl-x-c-is-completion t)
 '(home-end-enable nil)
 '(inhibit-startup-screen 1)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(keypad-numlock-setup (quote numeric) nil (keypad))
 '(keypad-numlock-shifted-setup (quote prefix) nil (keypad))
 '(keypad-setup (quote cursor) nil (keypad))
 '(keypad-shifted-setup (quote S-cursor) nil (keypad))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-use-insert-directory-program nil)
 '(mode-compile-expert-p t)
 '(mouse-wheel-follow-mouse t)
 '(package-selected-packages
   (quote
    (ansible ansible-doc ansible-vault hydra twittering-mode elfeed-goodies elfeed-org ob-http org-plus-contrib restclient helm evil edit-server regex-tool dockerfile-mode docker flycheck-haskell ghc haskell-mode ensime puppet-mode pipenv ein elpy ess yaml-mode req-package projectile powerline pdf-tools markdown-mode magit jumpc diminish company ack ace-jump-mode)))
 '(perl-dbg-flags "-c -w -MB::Lint")
 '(ps-line-number t)
 '(ps-paper-type (quote a4))
 '(ps-printer-name nil)
 '(recentf-mode t nil (recentf))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(standard-indent 4)
 '(tab-stop-list (quote (4 16 24 32 40 48 56 64 72 80 88 96 104 112 120)))
 '(tag-reuse-buffers nil t)
 '(tag-reuse-window t t)
 '(time-stamp-time-zone "MET")
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(twgrep-change-readonly-file t t)
 '(vc-cvs-stay-local nil)
 '(wgrep-auto-save-buffer nil t)
 '(wgrep-enable-key "e" t)
 '(woman-imenu t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;(color-theme-z-gnome2)
