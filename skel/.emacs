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
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auto-compression-mode t nil (jka-compr))
 '(blink-matching-paren-on-screen t)
 '(browse-url-browser-function 'browse-url-generic)
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
 '(custom-enabled-themes '(ayu-dark))
 '(custom-safe-themes
   '("21055a064d6d673f666baaed35a69519841134829982cbbb76960575f43424db" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "c0a0c2f40c110b5b212eb4f2dad6ac9cac07eb70380631151fa75556b0100063" "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1" "4eb69f17b4fa0cd74f4ff497bb6075d939e8d8bf4321ce8b81d13974000baac1" "660376e0336bb04fae2dcf73ab6a1fe946ccea82b25f6800d51977e3a16de1b9" "13880fa28757754bc40c85b05689c801ddaa877f2fe65abf1779f37776281ef1" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "9685cefcb4efd32520b899a34925c476e7920725c8d1f660e7336f37d6d95764" "4641f2added941818ca5a618aa38206d6dd6c2fa553137e2d0e1c82073b8674a" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "62ac2452579ac2a291c2f558017d17344e8f0913a52ce7878bb01ac6548fbfe0" "0f1cd0a67646b62f9c8421993e09424d69562bae3e8a69b9d23e199a69c168ce" "a1966abd6ce763bdb037b913890d796813807bcb63183f072ea272d323c45c20" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "e7b7d1e49adc2b0533b4fe57617c358ecbca80f39d05a30b825b998fa86bc372" "e01db763cd9daa56f75df8ebd057f84017ae8b5f351ec90c96c928ad50f3eb25" "25f81851315ee76bd43cb551767861d24d2450d07e8e3ca412d09adbe28f5f98" default))
 '(cvs-allow-dir-commit t)
 '(default-input-method "latin-1-prefix")
 '(display-buffer-reuse-frames t)
 '(ecb-options-version "2.32")
 '(elfeed-sort-order 'ascending)
 '(eudc-default-return-attributes 'all)
 '(eudc-protocol 'ldap)
 '(eudc-server "localhost")
 '(explicit-shell-file-name "/bin/bash")
 '(fci-rule-color "dark green")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode nil nil (hl-line))
 '(grep-command "egrep -n -e ")
 '(hc-ctrl-backslash-completes-a-la-mode t)
 '(hc-ctrl-x-c-is-completion t)
 '(highlight-tail-colors '(("#aecf90" . 0) ("#c0efff" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#aaeeee")))
 '(home-end-enable nil)
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-header)
 '(inhibit-startup-screen 1)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-height 20)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(keypad-numlock-setup 'numeric nil (keypad))
 '(keypad-numlock-shifted-setup 'prefix nil (keypad))
 '(keypad-setup 'cursor nil (keypad))
 '(keypad-shifted-setup 'S-cursor nil (keypad))
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
 '(ps-paper-type 'a4)
 '(ps-printer-name nil)
 '(recentf-mode t nil (recentf))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(standard-indent 4)
 '(tab-stop-list '(4 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
 '(tag-reuse-buffers nil t)
 '(tag-reuse-window t t)
 '(time-stamp-time-zone "MET")
 '(tool-bar-mode nil)
 '(track-eol t)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(twgrep-change-readonly-file t t)
 '(vc-cvs-stay-local nil)
 '(wgrep-auto-save-buffer nil t)
 '(wgrep-enable-key "e" t)
 '(woman-imenu t)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#29aeff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#72a4ff" "#f78fe7" "#4ae8fc" "#ffffff"])
 '(z-use-helm nil))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Source Code Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 135 :width normal)))))

(custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Source Sans Pro" :foundry "ADBE" :slant normal :weight semi-bold :height 135 :width normal))))
   '(fixed-pitch ((t ( :family "JetBrains Mono Medium")))))

;;(color-theme-z-gnome2)
