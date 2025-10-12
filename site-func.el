;;; site-func.el --- utility functions definition in ~/.emacs config -*- lexical-binding: t; -*-

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

;; h7 prefix

;;; Code:

;; ---( site.func: begin )-------------------------------------------------------
(message "SITE:FUNC - begin")

;; ---( ... )--------------------------------------------------------------


;;;////////////////////////////////////////////////////////////////
;;;  @FUNCTIONS
;;;////////////////////////////////////////////////////////////////
(message "SITE:FUNCIONS")

;; ---( util )---------------------------------------------------------

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))


;;;////////////////////////////////////////////////////////////////
;;;  @MACRO
;;;////////////////////////////////////////////////////////////////
(message "SITE:MACRO")


;; ---( input macros )---------------------------------------------------------

(defun scroll-up-one ( )
  "up-one."
  (interactive)
  (scroll-up 1))
(defun scroll-down-one ( )
  "down-one."
  (interactive)
  (scroll-down 1))


;; ---( frames )---------------------------------------------------------

;;(require 'frame-cmds)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; ---( wrap )---------------------------------------------------------

;; Toggles between line wrapping in the current buffer.
(defun toggle-line-wrapping ()
  "Toggle between line wrapping in the current buffer."
  (interactive)
  (if (eq truncate-lines nil)
      (progn
        (setq truncate-lines t)
        (redraw-display)
        (message "Setting truncate-lines to t"))
    (setq truncate-lines nil)
    (redraw-display)
    (message "Setting truncate-lines to nil"))
  )


(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun xah-newline-to-space () ;; xah-space-to-newline-2022-06-08
  "Replace spaces to a newline in current block or selection.

If `universal-argument' is called first,
ask user to enter a character to replace.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version: 2017-08-19 2021-11-28"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward "\n+" nil t)
          (replace-match " " ))))))


;; ---( kbd-macro )-----------------------------------------------------

(defun start-or-end-kbd-macro ()
  ;; A doc string.  This is optional.
  "Start defining a keyboard macro, or stop if we're already defining."
  ;; IMPORTANT: Any function bound to a key MUST have an interactive spec,
  ;; usually just the following line:
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))


;; ---( ascii )---------------------------------------------------------

(defun self-insert-backquote ( )
  "insert backquote `."
  (interactive)
  (insert-char ?` 1))

(defun self-insert-tilde ( )
  "insert tilde ~."
  (interactive)
  (insert-char ?~ 1))

;; ---( format )---------------------------------------------------------

;; Untabifies entire buffer.
(defun untabify-buffer ()
  "Untabifies entire buffer."
  (interactive)
  (point-to-register 1)
  (goto-char (point-min))
  (untabify (point-min) (point-max))
  (register-to-point 1)
  )

;; Tabifies entire buffer.
(defun tabify-buffer ()
  "Tabifies entire buffer."
  (interactive)
  (point-to-register 1)
  (goto-char (point-min))
  (tabify (point-min) (point-max))
  (register-to-point 1)
  )

(defun remove-trailing-ctl-M ()
  "Propose to remove trailing ^M from a file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (not (string-match ".gz$" (buffer-file-name)))
             (search-forward-regexp "
$" nil t))
                                        ;: a ^M is found
        (if (or (= (preceding-char) ?\^J)
                (= (following-char) ?\^J) )
            (if (y-or-n-p (format "Remove trailing ^M from %s? "
                                  (buffer-file-name)))
                (progn (goto-char (point-min))
                       (perform-replace "
" "" nil nil nil)
                       (pop-mark)
                       (save-buffer))
              (message "No transformation."))))))


;; ---( format )---------------------------------------------------------


;; @see: https://github.com/seth/my-emacs-dot-d/blob/master/emacs-init.org

(defun google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))


;; ---( site.func: end )-------------------------------------------------------
(message "SITE:FUNC - end")

(provide 'site-func)
;;; site-func.el ends here
