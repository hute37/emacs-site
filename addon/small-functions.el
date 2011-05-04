;; small-functions.el --- Small function definitions that are usefull.
;;
;;  Steve Kemp <skx@tardis.ed.ac.uk>
;;  http://www.tardis.ed.ac.uk/~skx/
;;
;;  These function definitions are the partly written myself
;; and partly borrowed/stolen/copied from other people - credit
;; is given where known.
;;
;;  Feel free to borrow/copy/steal code found in this file..
;;
;; Tue Aug 17 15:51:03 1999
;;
;;;;

;;
;;  Make sure I don't accidentally kill emacs.
;; This will force emacs to ask me if I'm sure that I want to quit.
;; Bind the function to C-c C-x
(global-set-key "\C-x\C-c" '(lambda () 
			      (interactive)
			      (if (y-or-n-p-with-timeout "Do you want to exit " 4 nil)
				  (save-buffers-kill-emacs))))

;; Stop Emacs from asking for "y-e-s", when a "y" will do.
(fset 'yes-or-no-p 'y-or-n-p)

(defun my-indent-file  (file)
  "Indent a file using my indentation style.
Also untabifys the file."
  (interactive "fIndent File: ")
  (let ((buf (find-file file)))
    (my-indent-buffer buf)
    (save-buffer)
  ))

(defun my-indent-buffer  (buffer)
  "Indent a buffer using my indentation style.
Also untabifys the buffer."
  (interactive "bIndent Buffer: ")
  (my-indent-region (point-min) (point-max)))

(defun my-indent-current-buffer  ()
  "Indent current buffer using my indentation style.
Also untabifys the buffer."
  (interactive)
  (my-indent-buffer (current-buffer)))

(defun my-indent-region  (start end)
  "Indent a region using my indentation style.
Also untabifys the region."
  (interactive "r")
  (indent-region start end nil)
  (untabify start end))


;; The following little lump of lisp will ensure the first assignment operators
;; on each of the lines line up. This is part of our local formatting style
;; 'cos it looks nice ;-)
;; The style of the lisp however, is atrocious. All the problems come from ==,
;; which looks too much like 'op='.
;; Paul Hudson
(defun align-equals (start end)
 "Make the first assignment operator on each line line up vertically"
 (interactive "*r")
 (save-excursion
   (let ((indent 0))
     (narrow-to-region start end)
     (beginning-of-buffer)
     (while (not (eobp))
       (if (find-assignment)
	   (progn
	     (exchange-point-and-mark)
	     (setq indent (max indent (current-column)))
	     (delete-horizontal-space)
	     (insert " ")))
       (forward-line 1))
     (beginning-of-buffer)
     (while (not (eobp))
       (if (find-assignment)
	   (indent-to-column (1+ (- indent  (- (mark) (point))))))
       (forward-line 1)))
   (widen)))


;;
;; Find an assignment statement
;;
(defun find-assignment ()
  (if (re-search-forward
	     "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
	     (save-excursion (end-of-line) (point)) t)
      (progn
	(goto-char (match-beginning 0))
	(if (looking-at ".==")
	    nil
	  (if (looking-at "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
	      (set-mark (match-end 0))
	    (forward-char 1)
	    (set-mark (1+ (point))))
	  (delete-horizontal-space)
	  t))
    nil))


;;
;; Insert a time stamp at the point
;;
(defun insert-time-stamp ()
  "Insert current date and time."
  (interactive "*")
  (insert (current-time-string)))

;;
;;  A simple function to move the point the the previous window, when
;; there are multiple windows on screen.  Simpler to use that
;; "other-window"
;;
(defun other-window-backward (&optional n)
  "Select Nth previous window"
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;;
;; Key bindings for next window, and previous window.
;; taken straight from the Glickenstein.
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

;;
;; An equivilent of Apropos, but it acts up lisp variables.
;;
(defun variable-apropos (string)
  "Like apropos, but lists only symbols that are names of user
modifiable variables.  Argument REGEXP is a regular expression.
   Returns a list of symbols, and documentation found"
  (interactive "sVariable apropos (regexp): ")
  (let ((message
         (let ((standard-output (get-buffer-create "*Help*")))
           (print-help-return-message 'identity))))
    (if (apropos string  'user-variable-p)
        (and message (message message)))))
(define-key help-map "\C-v" 'variable-apropos)


;;
;; Run a shell command on a region, and paste the results of the command
;; over that region.
;;
(defun my-shell-command-on-region nil
  "Replace region with ``shell-command-on-region''.

By default, this will make mark active if it is not and then prompt
you for a shell command to run and replaces region with the results.
This is handy for doing things like getting external program locations
in scripts and running grep and whatnot on a region."
  (interactive)
  (save-excursion
    (if (equal mark-active nil)
        (push-mark nil nil -1))
    ; Next couple lines stolen from simple.el
    (setq string
          (read-from-minibuffer "Shell command on region: " nil nil nil
                                'shell-command-history))
    (shell-command-on-region (region-beginning) (region-end) string -1)
    ; Get rid of final newline cause I normally did by hand anyway.
    (delete-char -1)))


(defun reindent-files (filelist)
  "Allow files to be reindented.."
  (while filelist
      (reindent-file (car filelist))
      (setq filelist (cdr filelist))))

(defun reindent-file (file)
  "This will reindent a file"
  (interactive)
  (save-excursion
    (find-file file)
    (indent-region (point-min) (point-max) nil)
    ;; uncomment these two lines after testing with a few files
    ;;(save-buffer)
    ;;(kill-buffer nil)
    ))

;;(reindent-files (list "first.c" "second.c" "some/path/third.c"))

(defun ^m-buffer ()
  "Remove all ^M's from the buffer."
  (interactive)
  (^m-region (point-min) (point-max)))

(defalias '^M '^m-buffer)
(defalias '^M '^m-buffer)

(defun ^m-region (min max)
  "Remove all ^M's from the region."
  (interactive "r")
  (save-excursion
    (goto-char max)
    (while (re-search-backward "\C-m$" min t)
      (delete-char 1))))





;; From: terra@diku.dk (Morten Welinder)
;; Newsgroups: gnu.emacs.help
;; Subject: Re: How do you get *scratch buffer after lost ?
;; Date: 7 May 1997 23:18:51 GMT
;; Organization: Department of Computer Science, U of Copenhagen
;;
;; sramani@imtn.dsccc.com (Shubha Ramani) writes:
;;
;; >If one accidently kills the scratch buffer, how do you >regain it
;; ? Is there a command to bring it back ?
;;
;; You can always do "C-x C-b *scratch* RET" but keeping the following
;; piece of code around in your ~/.emacs file will make *scratch*
;; harder to delete in the first place -- it magically reappears when
;; you kill it.

;; Morten
;;

;;; Make the *scratch* buffer behave like "The thing your aunt gave you,
;;; which you don't know what is."
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  "Kill the current (*scratch*) buffer, then create a new one.
 This is called from a hook, kill-buffer-query-functions, and its
 purpose is to prevent the *scratch* buffer from being killed."
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

;; Ditto for the messags buffer
(save-excursion
  (set-buffer (get-buffer-create "*Messages*"))
  (fundamental-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-messages-buffer))

(defun kill-messages-buffer ()
  "Kill the current (*Messages*) buffer, then create a new one.
 This is called from a hook, kill-buffer-query-functions, and its
 purpose is to prevent the *Messages* buffer from being killed."
  (remove-hook 'kill-buffer-query-functions 'kill-messages-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *messages* buffer
  (set-buffer (get-buffer-create "*Messages*"))
  (fundamental-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-messages-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)


(defun remove-blank-lines ()
  "Delete blank lines from the current buffer."
  (interactive "*")
  (while (re-search-forward "^$")
    (kill-line)))

(defun add-full-stop ()
  "Terminate each line with a full stop."
  (interactive "*")
  (while (re-search-forward "$")
    (insert ".")
    (forward-char )))


(defun get-ip-address ()
  "Show the IP address of the current machine."
  (interactive)
  (save-excursion
    (ipconfig);; autoloaded from net-utils.el
    (unwind-protect
	(progn
	  ;; We are now in buffer "*Ipconfig*".
	  ;; wait for the ipconfig process to finish.
	  (while (let ((p (get-process "Ipconfig")))
		   (and p (process-status p)))
	    (sit-for 1))
	  (beginning-of-buffer)
	  (if (save-match-data ;; Don't mess up my caller's match data.
		(re-search-forward "^[ \t]*IP Address[. ]*:[ \t]*" nil t))
	      (buffer-substring (point) (progn (end-of-line) (point)))
	    (error "Can't find IP address")
	    )
	  )
      (kill-buffer "*Ipconfig*")
      )
    )
  )


(defun strip-html ()
  "Remove HTML tags from the current buffer, 
   (this will affect the whole buffer regardless of the restrictions in effect)."
  (interactive "*")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "<[^<]*>" (point-max) t)
	(replace-match "\\1"))
      (goto-char (point-min))
      (replace-string "&copy;" "(c)")
      (goto-char (point-min))
      (replace-string "&amp;" "&")
      (goto-char (point-min))
      (replace-string "&lt;" "<")
      (goto-char (point-min))
      (replace-string "&gt;" ">")
      (goto-char (point-min)))))



;;; Set the % key to goto matched parenthesis.
;;; Posted to the NTEmacs mailing list by
;;; Chris McMahan
(show-paren-mode t)
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	  ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	  (t (self-insert-command (or arg 1)))))


(defun delete-header-cruft (P)
  "Delete lines which appear to be RFC-822 cruft, mail or news.
With prefix arg, start from point; otherwise do whole buffer."
  (interactive "P")
  (or P (goto-char (point-min)))
  (while (re-search-forward
          (concat "^\\("
                  "Xref\\|Path\\|Newsgroups\\|Followup-To\\|"
                  "Lines\\|Message-ID\\|Reply-To\\|NNTP-Posting-Host\\|"
                  "Received\\|X-Mailer\\|MIME-Version\\|References\\|"
                  "Content-Type\\|Content-Transfer-Encoding\\|Status\\|"
                  "In-Reply-To\\|X-Newsreader\\|"
                  "\\): .*\n")
          nil t)
    (replace-match "")))


(defsubst PMIN ()
  "Go to `point-min'."
  (goto-char (point-min)))

(defsubst PMAX ()
  "Go to `point-max'."
  (goto-char (point-max)))


;;; ----------------------------------------------------------------------
;;;
(defun bin-string-to-int (8bit-string)
  "Convert 8BIT-STRING  string to integer."
  (let* ((list  '(128 64 32 16 8 4 2 1))
	 (i   0)
	 (int 0)
         )
    (while (< i 8)
      (if (not (string= "0" (substring 8bit-string i (1+ i))))
	  (setq int (+ int (nth i list) )))
      (incf  i)
      )
    int
    ))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-bin-string (n &optional length)
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i    0)
	 (len  (or length 8))
	 (s    (make-string len ?0))
	 )
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i))
      )
    s
    ))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-hex-string (n &optional separator pad)
  "Convert integer N to hex string. SEPARATOR between hunks is \"\".
PAD says to padd (bit hex string with leading zeroes."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
	       (setq x (format "%x" (logand x 255)))
	       (if (= 1 (length x)) (concat "0" x) x)))
   (list (ash n -24) (ash n -16) (ash n -8) n)
   separator))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-oct-string (n &optional separator)
  "Convert integer N into Octal. SEPARATOR between hunks is \"\"."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
	       (setq x (format "%o" (logand x 511)))
	       (if (= 1 (length x)) (concat "00" x)
		 (if (= 2 (length x)) (concat "0" x) x))))
   (list (ash n -27) (ash n -18) (ash n -9) n)
   separator))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun radix (str base)
  "Convert STR according to BASE."
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz")
        (case-fold-search t)
        (n 0)
        i)
    (mapcar '(lambda (c)
               (setq i (string-match (make-string 1 c) chars))
               (if (>= (or i 65536) base)
                   (error "%c illegal in base %d" c base))
               (setq n (+ (* n base) i)))
            (append str nil))
    n))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun bin-to-int (str)
  "Convert STR into binary."
  (radix str 2))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun oct-to-int (str)
  "Convert STR into octal."
  (radix str 8))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun hex-to-int (str)
  "Convert STR into hex."
  (if (string-match "\\`0x" str) (setq str (substring str 2)))
  (radix str 16))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-net (float)
  "Decode packed FLOAT 32 bit IP addresses."
  (format "%d.%d.%d.%d"
          (truncate (% float 256))
          (truncate (% (/ float 256.0) 256))
          (truncate (% (/ float (* 256.0 256.0)) 256))
          (truncate (% (/ float (* 256.0 256.0 256.0)) 256))
          ))

;;; ----------------------------------------------------------------------
;;;
(defsubst str-left (str count)
  "Use STR and read COUNT chars from left.
If the COUNT exeeds string length or is zero, whole string is returned."
  (if (> count 0)
      (substring str 0 (min (length str) count))
    str))

;;; ----------------------------------------------------------------------
;;;  - You can do this with negative argument to substring, but if you exceed
;;;    the string len, substring will barf and quit with error.
;;;  - This one will never call 'error'.
;;;
(defsubst str-right (str count)
  "Use STR and read COUNT chars from right.
If the COUNT exeeds string length or is zero, whole string is returned."
  (let* ((pos (- (length str)  count))
	 )
    (if (> pos 0)
	(substring str (- 0 count))
      str
      )))

;;; ----------------------------------------------------------------------
;;; - This old version is equivalent to the new one. The NEW one
;;;   was needed because Emacs 19.30+ didn't allow integer in
;;;   'concat function any more.
;;; - This is interesting macro, but ... Hmm, I think it is
;;;   too slow to be used regularly. Use with care in places where
;;;   time is not critical.
;;;
;;old (defmacro strcat (var-sym &rest body)
;;old"Shorthand to (setq VAR-SYM (concat VAR-SYM ...))"
;;old   (` (setq (, var-sym) (concat (or (, var-sym) "") (,@ body)))))

;;; #todo: Remove strcat

(defmacro strcat (var &rest body)
  "Like C strcat. Put results to VAR using BODY forms.
Integers and variables passed in BODY to VAR
Example call:  (strcat var \"hello \" \"there \" 1234 \" \" 55)"
  (` (setq (, var)
	   (concat
	    (or (, var) "")
	    (mapconcat
	     (function
	      (lambda (x)
		(cond
		 ((stringp x) x)
		 ((integerp x) (int-to-string x))
		 (t   (eval x))          ;; it's variable
		 )))
	     (quote (, body))
	     ""
	     )))))

(defsubst line-wrap-p ()
  "Check if line wraps. ie. line is longer that current window."
  (> (line-length) (nth 2 (window-edges))))

;;; ----------------------------------------------------------------------
;;; - Ever struggled with peeking the lists..?
;;; - I have, and printing the contents of auto-mode-alist into
;;;   the buffer is very easy with this.
;;; - Should be default emacs function.
;;;
(defun list-print (list)
  "Insert content of LIST into current point."
  (interactive "XLisp symbol, list name: ")
  (mapcar
   (function
    (lambda (x) (insert (2str x) "\n")))
   list))

;;; ----------------------------------------------------------------------
;;; 1990, Sebastian Kremer, Institute for Theoretical Physics, West Germany
;;; BITNET: ab027@dk0rrzk0.bitnet
;;;
(defsubst list-to-string (list &optional separator)
  "Convert LIST into string. Optional SEPARATOR defaults to \" \".

Input:

  LIST       '(\"str\" \"str\" ...)
  separator  ' '

Return:
  str"
  (mapconcat
   (function identity)			;returns "as is"
   list
   (or separator " ")
   ))


(defun shell-command-to-string (command)
  "Returns shell COMMAND's ouput as string. Tinylibm."
  (with-temp-buffer
    (shell-execute command (current-buffer))
    (buffer-string)))

;;; ----------------------------------------------------------------------
;;; Easier to use than lowlevel `call-process'
;;;
(defsubst shell-execute (command &optional buffer)
  "Executes shell COMMAND and optionally output to BUFFER.

References:

  `shell-file-name'	variable
  `shell-exec-nok-p'	function

Return:

  0	error
  nbr	ok"
  (call-process
   shell-file-name
   nil
   buffer
   nil
   shell-command-switch ;; -c
   command
   ))

;;; ----------------------------------------------------------------------
;;;
(defsubst file-name-dos (file)
  "Convert FILE slashes to dos format."
  (subst-char file ?/ ?\\))

;;; ----------------------------------------------------------------------
;;;
(defsubst file-name-unix (file)
  "Convert FILE slashes to unix format."
  (subst-char file ?\\ ?/))


(defun pop-to-buffer-or-window (buffer)
  "Like `pop-to-buffer' BUFFER, but find any visible window."
  (let* (win
         )
    (setq win (get-buffer-window buffer t))
    (if (null win)
	(pop-to-buffer buffer)
      (raise-frame (window-frame win))
      (select-frame (window-frame win))
      (select-window win)
      )))


;;
;; Grep for the symbol under the cursor.  Works only for C / C++ / H / RC
;; files
;;
(defun grep-curdir-symbol-at-point ()
  "Grep current directory for symbol at cursor."
  (interactive)
  (grep (concat "grep -n -e " (current-word)  " *.c *.cpp *.h *.rc NUL"
)))



(defun w32-restore-frame ()
  "Restore a minimized frame"
   (interactive)
   (w32-send-sys-command 61728))
 
(defun w32-maximize-frame ()
  "Maximize the current frame"
   (interactive)
   (w32-send-sys-command 61488))

(defun show-functions-in-buffer ()
  "Show the functions defined in the current buffer"
  (interactive)
  (let ((current (current-buffer))
	(buffer (get-buffer-create "*Function Definitions*")))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "(defun \\([^(|^ ]*\\)" nil t)
	(progn
	  (set-buffer buffer)
	  (insert (format "Found : %s\n" (match-string 1)))
	  (set-buffer current)
	))
      )
    (pop-to-buffer buffer)))
    

(defun point-at-bol ()
  "Return the index of the character at the start of the line.
  This is a built in function in Xemacs, but not Emacs."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (point)))

(defun pbug ()
  "Check parenthesis bugs or similar horrors.

Even with Emacs advanced programming facilities, checking mismatching
parenthesis or missing quote (so called \"pbug\") is no less annoying than
pointer chasing in C.

This function divides the buffer into regions and tries evaluating them one
by one.  It stops at the first region where it fails to evaluate because of
pbug or any other errors. It sets point and mark (and highlights if
`transient-mark-mode' is on) on the failing region and center its first
line.  \"^def\" is used to define regions.  You may also `eval-region'
right after pbug is done to let lisp parse pinpoint the bug.

No more \"End of file during parsing\" horrors!"
  (interactive)
  (let ((point (point))
	(region-regex "^(def..")
	defs beg end)
    (goto-char (point-min))
    (setq defs (loop while (search-forward-regexp region-regex nil t)
		     collect (point-at-bol)))
    ;; so it evals last definition
    (nconc defs (list (point-max)))
    (setq beg (point-min))
    (while defs
      (goto-char beg)
      (setq end (pop defs))
      ;; to be cool, uncomment these to see pbug doing step by step
      ;; (message "checking pbug from %s to %s..." beg end)
      ;; (sit-for 1)
      (when (eq (condition-case nil
		    (eval-region beg (1- end))
		  (error 'pbug-error))
		'pbug-error)
	(push-mark end 'nomsg 'activate)
	(goto-char beg)
	(recenter)
	(error "a pbug found from %s to %s" beg end))
      (setq beg end))
    (goto-char point)
    (message "no pbug found")))


(defun debug-on-error ()
  "Toggle variable `debug-on-error'."
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error=%s" debug-on-error)
  )

(provide 'small-functions)