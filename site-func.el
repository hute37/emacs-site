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
  "Toggles between line wrapping in the current buffer."
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
