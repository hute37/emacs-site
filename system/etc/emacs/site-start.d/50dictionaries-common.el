;; File: startup.el.in
;; Description: Emacsen startup for dictionaries-common in Debian
;; Authors: Rafael Laboissière <rafael@debian.org>
;;          Agustin Martin     <agmartin@debian.org>
;; Created on: Fri Oct 22 09:48:21 CEST 1999

(if (member debian-emacs-flavor '(emacs19
                                  emacs-snapshot))
    (message "Skipping dictionaries-common setup for %s" debian-emacs-flavor)

  (debian-pkg-add-load-path-item
   (concat "/usr/share/"
	   (symbol-name debian-emacs-flavor)
	   "/site-lisp/dictionaries-common"))
  
  (autoload 'flyspell-word "flyspell" nil t)
  (autoload 'flyspell-mode "flyspell" nil t)
  (autoload 'flyspell-prog-mode "flyspell" nil t)
  
  ;; Load the emacsen cache file, containing calls to
  ;; debian-ispell-add-dictionary-entry for each installed dictionary.
  ;; Since this might result in a call to debian-ispell do this only if 
  ;; it exists, that is, if package is not removed
  
  (if (not (file-exists-p "/usr/share/emacs/site-lisp/dictionaries-common/debian-ispell.el"))
      (message "Info: Package dictionaries-common removed but not purged.")
    (load "debian-ispell" t)
    (load "/var/cache/dictionaries-common/emacsen-ispell-dicts.el" t))
  )

;;; Previous code for loading ispell.el and refreshing spell-checking
;;; pulldown menus has been removed from this file since it should no
;;; longer be needed.

;;Local Variables:
;;mode: lisp
;;End:

