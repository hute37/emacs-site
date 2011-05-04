;; sml-mosml.el: Modifies sml-mode & inferior-sml-mode for Moscow ML.
;; Based on sml-poly-ml.el

;; To use this library just put

;;(autoload 'sml-mosml "sml-mosml" "Set up and run Moscow ML." t)

;; in your .emacs file. If you only ever use Moscow ML then you might as
;; well put something like

;;(setq sml-mode-hook
;;      '(lambda() "SML mode defaults to Moscow ML"
;;	 (define-key  sml-mode-map "\C-cm" 'sml-mosml)))

;; for your sml-mode-hook. The command prompts for the program name
;; and the database to use, if any. 


(require 'sml-proc)

(defvar sml-mosml-error-regexp
  "^File \"\\(.+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)\\-.+:"
  "Default regexp matching Moscow ML error messages.")

;;; The reg-expression used when looking for errors. Moscow ML errors:
;;;
;;; File "/home/sestoft/sml/fejl0.sml", line 1, characters 4-10:
;;; ! 6 + "ghsd";
;;; !     ^^^^^^
;;; ! Type clash: expression of type
;;; !   string
;;; ! cannot be made to have type
;;; !   int

(defun sml-mosml-error-parser (pt) 
 "This function parses a Moscow ML error message into a 3 element list.
  (file start-line start-col)"
 (save-excursion
   (goto-char pt)
   (re-search-forward sml-mosml-error-regexp)      
   (list (buffer-substring (match-beginning 1) ; file
			   (match-end 1))
	 (string-to-int (buffer-substring      ; start line
			 (match-beginning 2)
			 (match-end 2)))
	 (string-to-int (buffer-substring      ; start col
			 (match-beginning 3)
			 (match-end 3))))))

(defun sml-mosml ()
   "Set up and run Moscow ML.
Note: defaults set here will be clobbered if you setq them in the
{inferior-}sml-mode-hook.

 sml-program-name  <option>
 sml-default-arg   <option>
 sml-use-command   \"use \\\"%s\\\"\"
 sml-cd-command    \"FileSys.chDir \\\"%s\\\"\"
 sml-prompt-regexp \"^- \"
 sml-error-regexp  sml-mosml-error-regexp
 sml-error-parser  'sml-mosml-error-parser"

   (interactive)
   (let ((cmd (read-string "Command name: " "mosml"))
	 (args (read-string "Arguments or options? (may be none): " "")))
     (setq sml-program-name  cmd
	   sml-default-arg   args
	   sml-use-command   "use \"%s\""
	   sml-cd-command    "load \"FileSys\"; FileSys.chDir \"%s\""
	   sml-prompt-regexp "^- "
	   sml-error-regexp  sml-mosml-error-regexp
	   sml-error-parser  'sml-mosml-error-parser)
     (sml-run cmd sml-default-arg)))

(setq sml-program-name  "mosml"
      sml-default-arg   ""
      sml-use-command   "use \"%s\""
      sml-cd-command    "load \"FileSys\"; FileSys.chDir \"%s\""
      sml-prompt-regexp "^- "
      sml-error-regexp  sml-mosml-error-regexp
      sml-error-parser  'sml-mosml-error-parser)

;; sml-mosml.el ended just there
