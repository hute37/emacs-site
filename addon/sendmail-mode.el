;;; sendmail-mode.el --- major mode for editing Sendmail configuration files

;; Keywords:	languages, faces
;; Author:	Jonathan Marten  <jonathan.marten@uk.sun.com> or
;; Last edit:	16-Oct-99        <jjm@keelhaul.demon.co.uk>

;; This file is an add-on for XEmacs or GNU Emacs (not tested with the latter).
;;
;; It is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; It is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Sendmail configuration files are usually pretty unintelligible, so
;; some help with syntax colouring and comment indentation is always
;; welcome.  This mode attempts to do that.
;;
;; The fontification rules and the list of keywords were derived from
;; the O'Reilly book and the online documentation included with the
;; latest versions of sendmail.  They may be neither complete nor 100%
;; accurate; since the configuration file syntax is not particularly
;; rigourous or consistent a totally accurate fontification might not
;; be possible.  However, it has been tested on as many configuration
;; files as I could find, from a number of systems and of various
;; vintages.  Additional keywords or examples of incorrect fontification
;; or comment indentation would be most welcome.
;;
;; Syntactic fontification is done, but only for quoted strings.
;;
;; No comment syntax is defined, because Emacs' built-in indentation and
;; fontification cannot handle the varying rules for placement and
;; validity of comments, especially with the differences between pre-V8
;; and later sendmail.  Comments are fontified using the font-lock
;; regexps, and comment indentation is handled specially using
;; `sendmail-indent-for-comment'.  This is because the syntax of a comment,
;; or even whether one is allowed at all, varies depending on what sort of
;; line the comment is on and the version of the configuration file.  The
;; only particularly complex case is for an "R" line, where the comment
;; has no delimiter but is separated from the RHS by at least one tab.
;; We have to take care that there is at least that one tab here, even
;; in the case where the indent is past the comment column and normally
;; only a single space would be inserted.
;;
;; All brackets ("[]", "<>", "()" and "{}") are normally considered
;; worthy of matching.  However, "<>" sometimes leads to anomalous
;; indications because "$>" does not have a matching open "<".  This
;; should be fixed in paren.el by not considering a character to be
;; a paren if the character preceding it has char-quote syntax
;; ("/" in syntax table).  In this mode, "$" has such syntax.
;;
;; There is no indentation in the configuration file, so (contrary to the
;; usual Emacs convention), the TAB key is defined to self-insert.
;;
;; Sendmail is very sensitive to the format of the configuration file, and
;; small and sometimes invisible errors can cause serious problems.  This
;; mode attempts to help as far as is possible:
;;
;; 1.  Additional spaces before or after tabs, and lines containing only
;;     whitespace, are highlighted.  This can help to avoid unintended
;;     addition of whitespace tokens and continuation lines.
;;
;; 2.  Pre-V8 sendmail requires a final newline in the configuration file,
;;     otherwise the last line of the file is silently ignored.  This is
;;     enforced by ensuring that `require-final-newline' is forced to
;;     "add always" if it is set to "never" by default.  If the setting is
;;     "ask" then it stays.

;;; Change Log:
;;
;; Version 1.0, June 1999	First public release
;;
;; Version 1.01, October 1999	Eliminated a redundant 'progn'!
;;				Properly define mode hook variable

;;; To do:
;;
;; 1.  Detect the version of the configuration file and set the V8 option
;;     automatically.
;;
;; 2.  How about a command to take the configuration file and run address
;;     tests, sendmail debugging, etc. in a subprocess?
;;
;; 3.  The font-lock regexps could possibly be optimised some more.
;;
;; 4.  Is it a good approach to use `indent-for-comment' and then fix up the
;;     results afterwards?  It may be better to do all the work here.


;;; Code:

;; Requires
(require 'regexp-opt)


;; Custom
(defgroup sendmail-mode nil
  "Sendmail mode customizations"
  :group 'tools
  :prefix "sendmail-")

(defcustom sendmail-allow-v8-comments t
  "*If non-nil, allow comments on all command lines (as with V8 sendmail).
If nil, comments are only allowed on R lines (without #), standalone lines,
and on S and P command lines (with #)."
  :type 'boolean
  :group 'sendmail-mode)

(defcustom sendmail-mode-hook nil
  "*List of hook functions run by `sendmail-mode' (see `run-hooks')."
  :type 'hook
  :group 'sendmail-mode)


;; Other variables
(defvar sendmail-mode-commands "CDFHKMOPRSTV"
  "Sendmail configuration file commands which may start a line.")

(defvar sendmail-mode-v8-comment-commands sendmail-mode-commands
  "Sendmail configuration file commands which may have a comment in V8 sendmail.")

(defvar sendmail-mode-non-v8-comment-commands "PS"
  "Sendmail configuration file commands which may have a comment in non-V8 sendmail.")

(defvar sendmail-mode-map nil
  "Keymap used in Sendmail mode buffers")

(defvar sendmail-mode-syntax-table nil
  "Syntax table used in Sendmail mode buffers")

(defvar sendmail-mode-comment-start "# "
  "String used to insert a new Sendmail mode comment (except on an R line).")

(defvar sendmail-mode-comment-start-skip "#[ \t]+"
  "Regexp to match a Sendmail mode comment and everything up to its body (except
on an R line).")

(defvar sendmail-mode-r-comment-start-skip "^\\(R[^\t\n]+\t+[^\t\n]+\t+\\)"
  "Regexp to match a Sendmail mode comment and everything up to its body
on an R line.")


;; Faces
(defface sendmail-space-face                            ; copied from make-mode.el
  '((((class color))
     (:background "hotpink"))
    (((background light))
      (:background "black"))
    (((background dark))
      (:background "white")))
  "Face to use for highlighting misplaced spaces and blank lines in Sendmail mode."
  :group 'sendmail-mode)


;; Font lock
(defconst sendmail-font-lock-keywords
  (purecopy
   (list
    '("^[ \t]+$" 0 sendmail-space-face)                 ; white lines can cause trouble
    '("\\( +\\)\t" 1 sendmail-space-face)               ; as can spaces around tabs
    '("\t\\( +\\)" 1 sendmail-space-face)               ;

    (list (eval-when-compile                            ; keywords
            (concat
             "\\W\\("
             (regexp-opt '("error" "bodytype" "envid" "opMode" 
                           "dequote" "host" "dbm" "btree" "hash" "nis"
                           "strict" "mime" "pass8"
                           "check_relay" "check_mail" "check_rcpt" "check_compat" 
                           "client_addr" "client_name" "client_port" 
                           "USAGE" "NOUSER" "NOHOST" "UNAVAILABLE" "SOFTWARE" 
                           "TEMPFAIL" "PROTOCOL" "CONFIG" 
                           "local" "prog"
                           "none" "add-to" "add-apparently-to" "add-to-undisclosed" 
                           "add-bcc"
                           "public" "needmailhelo" "needexpnhelo" "noexpn" 
                           "needvrfyhelo" "novrfy" "restrictmailq" "restrictqrun" 
                           "noreceipts" "goaway" "authwarnings" 
                           "debug" "aaonly" "usevc" "primary" "igntc" "recurse" 
                           "defnames" "stayopen" "dnsrch" "HasWildcardMX"))
             "\\|"
             (mapconcat 'identity (list "[Ff]alse" "[Tt]rue"
                                        (regexp-quote "[IPC]")
                                        (regexp-quote "[TCP]")
                                        (regexp-quote "[LPC]")
                                        (regexp-quote "*file*")
                                        (regexp-quote "*include*")) "\\|")
             "\\)\\W"))
          1 'font-lock-keyword-face nil)

    (list (concat "^[" sendmail-mode-commands "]")      ; line commands
      0 'font-lock-keyword-face)

    '("^[TV]\\(.+\\)$"                                  ; strings (user/version)
      1 'font-lock-string-face)

    '("^H\\(\\?.\\?\\)?\\(.+\\):"                       ; headers
      2 'font-lock-function-name-face)

    '("^[SM]\\(.*?\\)\\>"                               ; definition (ruleset or mailer)
      1 'font-lock-function-name-face)

    '("^[OP]\\W*\\(.+\\)="                              ; definition (P and new-style O)
      (1 'font-lock-function-name-face)
      ("=\\(.+\\)$" nil nil (1 'font-lock-string-face)))

    '("^[CDF]{\\(.+?\\)}"                               ; definition (with braces)
      (1 'font-lock-function-name-face)
      ("\\s-+\\(.+\\)$" nil nil (1 'font-lock-string-face)))

    '("^[CDFO]\\([^{ ]\\)"                              ; definition (CDF and old-style O)
      (1 'font-lock-function-name-face)
      (".+$" nil nil (0 'font-lock-string-face)))

    '("^K\\(\\w+\\)\\W+\\(\\w+\\)"                      ; database
      (1 'font-lock-function-name-face)
      (2 'font-lock-keyword-face t))

    '("\\$[=?&~]?\\(?:{\\w+}\\|\\S-\\)"                 ; variables (char or word)
      0 'font-lock-variable-name-face t)

    '("\\$[#>]\\(?:\\w.*?\\|\\s-+\\w+?\\)\\>"           ; variables ($# and $>)
      0 'font-lock-reference-face t)

    '("\\(^\\|[^$]\\)\\(#.*\\)$"                        ; comments (line or appended)
      2 'font-lock-comment-face t)

    (list (concat sendmail-mode-r-comment-start-skip    ; comments (after 2nd tab)
                  "\\(.*\\)$")
          2 'font-lock-comment-face t)))
  "Expressions to highlight in Sendmail mode buffers.")

(put 'sendmail-mode 'font-lock-defaults '(sendmail-font-lock-keywords nil nil))


;; Syntax table
(unless sendmail-mode-syntax-table
  (setq sendmail-mode-syntax-table (copy-syntax-table nil))
  (modify-syntax-entry ?\   " "     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\t  " "     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\f  " "     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\r  " "     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\n  " "     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\_  "w"     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\-  "w"     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\(  "()"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\)  ")("    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\[  "(]"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\]  ")["    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\<  "(>"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\>  ")<"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\{  "(}"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\}  "){"    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\"  "\""    sendmail-mode-syntax-table)
  (modify-syntax-entry ?\#  "."     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\$  "/"     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\\  "/"     sendmail-mode-syntax-table)
  (modify-syntax-entry ?\,  "."     sendmail-mode-syntax-table))


;; Keymap
(unless sendmail-mode-map
  (setq sendmail-mode-map (make-sparse-keymap))
  (define-key sendmail-mode-map [(meta \;)] 'sendmail-indent-for-comment)
  (define-key sendmail-mode-map [(tab)] 'self-insert-command))


;;;###autoload
(defun sendmail-mode ()
  "Major mode for editing Sendmail configuration files.

\\{sendmail-mode-map}

\\[sendmail-mode] runs the hook `sendmail-mode-hook', if that is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sendmail-mode-map)
  (set-syntax-table sendmail-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start nil)
  (make-local-variable 'comment-end)
  (setq comment-end nil)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip nil)
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (and (eq require-final-newline nil)
       (make-variable-buffer-local 'require-final-newline)
       (setq require-final-newline t))
  (setq mode-name "Sendmail")
  (setq major-mode 'sendmail-mode)
  (run-hooks 'sendmail-mode-hook))


(defun sendmail-indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment.
A comment may or may not be permitted, depending on context and the setting
of `sendmail-allow-v8-comments'."
  (interactive "*")
  (let (empty comment this)
    (save-excursion
      (beginning-of-line)
      (setq empty (looking-at "\\s-*$"))
      (setq comment (looking-at "\\s-*#"))
      (setq this (char-after)))

    (cond
     ((or empty comment)                                ; empty or comment line
      (beginning-of-line)
      (delete-horizontal-space)
      (unless comment (insert sendmail-mode-comment-start)))

     ((eq this ?R)                                      ; rule line
      (let ((comment-start-skip sendmail-mode-r-comment-start-skip)
            (comment-start "")
            (indent-tabs-mode t))
        (indent-for-comment)
        (skip-chars-forward "\t")
        (if (> (current-column) comment-column)         ; past normal comment indent
            (if (not (eq (char-before) ?\ ))
                (if (not (looking-at "[\t\n]")) (insert "\t"))
              (delete-backward-char 1)                  ; must have a tab there
              (insert "\t")))))

     ((string-match (regexp-quote (char-to-string this)) ; command line
                    (if sendmail-allow-v8-comments sendmail-mode-v8-comment-commands
                      sendmail-mode-non-v8-comment-commands))
      (let ((comment-start sendmail-mode-comment-start)
            (comment-start-skip sendmail-mode-comment-start-skip))
        (indent-for-comment)))

     (t                                                 ; anything else
      (error "Can't have a comment here")))))


;; Provides
(provide 'sendmail-mode)

;;; sendmail-mode.el ends here
