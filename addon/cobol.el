;;; Cobol mode for GNU Emacs (version 1.01, Jun 21, 1988)
;;; Copyright (c) 1987 Free Software Foundation, Inc.
;;; Written by Robert A Sutterfield (bob@cis.ohio-state.edu) and
;;;  Paul W. Placeway (paul@tut.cis.ohio-state.edu), as changes to fortran.el
;;; Bugs to bug-cobol-mode@cis.ohio-state.edu

;;;    [0) the left column is column 1]
;;;  +  1) newline should indent to the same column as the start of
;;;        the previous line
;;;  +  2) tabs at 8 and every four thereafter (12, 16, 20, etc.)
;;;  +  3) tabs should be expanded to spaces on input
;;;  +  (3a) no tabs should appear in the buffer
;;;  no 4) right margin bell at 72 (hard to do)
;;;  +  5) (optional) flash matching parentheses
;;;  +  6) no auto-fill (WHY -- PWP) (not by default)
;;;  *  7) auto startup on .cob files
;;;	   To do this, the expression ("\\.cob$" . cobol-mode) must be
;;;	   added to loaddefs.el in the gnu-emacs lisp directory, and
;;;	   loaddefs must be re-byte-code-compiled.
;;;	   Also, an autoload must be set up for cobol-mode in loaddefs.el;
;;;	   see the loaddefs.el file in this directory.
;;;  +  8) auto indent to that of the last line (more magic than that...)
;;;  +  9) delete on a blank line should go back to LAST tab stop
;;;  + 10) C-c C-c moves cursor to ARG (or prompted) column, adding
;;;        spaces to get there if needed
;;;    11) C-c C-l does (goto-line)
;;;
;;; COBOL mode adapted from:
;;;; Fortran mode for GNU Emacs  (beta test version 1.21, Oct. 1, 1985)
;;;; Copyright (c) 1986 Free Software Foundation, Inc.
;;;; Written by Michael D. Prange (mit-eddie!mit-erl!prange).
;;;; Author acknowledges help from Stephen Gildea <mit-erl!gildea>

;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Bugs to bug-cobol-mode@cis.ohio-state.edu.

(require 'easymenu)
(require 'ffap)

(defgroup cobol nil
  "Cobol mode for Emacs"
  :link '(custom-manual "(emacs)Cobol")
  :group 'languages)


(defvar cobol-ffap-lowercase t
  "*Convert Copy Name to lowercase before searching." ) 

(defvar cobol-ffap-path
  ;; Need smarter defaults here!  Suggestions welcome.
  (ffap-list-env "EMACS_COPY_PATH" ".") ;; better than nothing
  "*Search Path for COPY, set by `$EMACS_COPY_PATH' env variable.") 

(defun cobol-ffap-mode (name)
  "*Search Copy on `cobol-ffap-path' path. See ffap library for details."
  (ffap-locate-file 
   (if cobol-ffap-lowercase
       (downcase name)
     name) t cobol-ffap-path))


(defcustom cobol-indent-increment 3
  "*Amount of indentation to add to a line when it can be indented.")

(defcustom cobol-do-indent 3
  "*Extra indentation applied to `perform' blocks.")

(defcustom cobol-if-indent 3
  "*Extra indentation applied to `if' blocks.")

(defcustom cobol-continuation-indent 6
  "*Extra indentation applied to `continuation' lines.")

(defcustom cobol-pic-column 50
  "*The column that PIC clauses should be aligned to.")

(defcustom cobol-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed produces fixed comment indentation to comment-column,
and 'relative indents to current cobol indentation plus comment-column.")

(defcustom cobol-comment-line-column 6
  "*Indentation for text in comment lines.")

(defvar comment-line-start nil
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")

(defcustom cobol-minimum-statement-indent 7 	;;; this puts it in column 8
  "*Minimum indentation for cobol statements.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defcustom cobol-comment-indent-char " "
  "*Character to be inserted for Cobol comment indentation.
Normally a space.")

(defcustom cobol-line-number-indent 1
  "*Maximum indentation for Cobol line numbers.
6 means right-justify them within their six-column field.")

(defvar cobol-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible do-loop ends.")

(defcustom cobol-continuation-char ?-
  "*Character which is inserted in column 7 by \\[cobol-split-line]
to begin a continuation line.  Normally ?-")

(defcustom cobol-comment-region "      ** "
  "*String inserted by \\[cobol-comment-region] at start of each line in region.")

(defcustom cobol-electric-line-number t
  "*Non-nil causes line number digits to be moved to the correct column as typed.")

(defcustom cobol-startup-message nil
  "*Non-nil displays a startup message when cobol-mode is first called.")

(defcustom cobol-column-ruler
  (concat "0    00  1         2         3         4         5         6         7  2\n"
	  "1.../67..0..../....0..../....0..../....0..../....0..../....0..../....0..\n")
  "*String displayed above current line by \\[cobol-column-ruler].")

(defconst cobol-mode-version "1.02")

(defvar cobol-mode-syntax-table nil
  "Syntax table in use in cobol-mode buffers.")

(if cobol-mode-syntax-table
    ()
  (setq cobol-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" cobol-mode-syntax-table)
  (modify-syntax-entry ?+ "." cobol-mode-syntax-table)
  (modify-syntax-entry ?- "." cobol-mode-syntax-table)
  (modify-syntax-entry ?* "." cobol-mode-syntax-table)
  (modify-syntax-entry ?/ "." cobol-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" cobol-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" cobol-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" cobol-mode-syntax-table)
  (modify-syntax-entry ?. "w" cobol-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cobol-mode-syntax-table))



;;;-------------
;;;  functions
;;;-------------

(defun cobol-xemacs ()
  (or (string-match "Lucid"  emacs-version)
      (string-match "XEmacs" emacs-version)))


;; ------------------------------------------------------------------------------------
;; ---(Syntax Highlight: Begin)--------------------------------------------------------
;; ------------------------------------------------------------------------------------

(defconst cobol-font-lock-keywords-1 nil
  "Subdued level highlighting for Cobol mode.")

(defconst cobol-font-lock-keywords-2 nil
  "Medium level highlighting for Cobol mode.")

(defconst cobol-font-lock-keywords-3 nil
  "Gaudy level highlighting for Cobol mode.")

(defun cobol-fontify-string (limit)
  (let ((match (match-string 1)))
    (cond ((string= "'" match)
	   (re-search-forward "\\([^'\n]*'?\\)" limit))
	  ((string= "\"" match)
	   (re-search-forward "\\([^\"\n]*\"?\\)" limit)))))

(let (
		(comment-chars 
		 (concat 
		  "*"
		  ))		; `d' for `debugging' comments
      (cobol-type-types
		 (concat 
		  "comp\\|pic\\(?:ture\\)?\\|value"
		  ))
;		(eval-when-compile
;         (regexp-opt 
;			 '("pic" "picture" 
;				"comp" 
;				"value" ))))
 
      (cobol-keywords
		 (concat 
		  "a\\(?:ccept\\|dd\\)\\|b\\(?:lank\\|y\\)\\|c\\(?:ase\\|"
		  "lose\\|o\\(?:mp\\(?:ute\\)?\\|ntinue\\|py\\)\\|ycle\\)"
		  "\\|di\\(?:splay\\|vi\\(?:de\\|sion\\)\\)\\|e\\(?:lse\\"
		  "(?:if\\)?\\|nd\\(?:do\\|if\\)?\\|valuate\\|xit\\(?:\\)"
		  "?\\)\\|f\\(?:ormat\\(?:\\)?\\|rom\\)\\|g\\(?:iving\\|o"
		  "\\(?:to\\)?\\)\\|i\\(?:n\\(?:clude\\|itialize\\|quire\\|"
		  "to\\)\\|[fs]\\)\\|l\\(?:abel\\|ine\\)\\|m\\(?:ove\\|ultiply"
		  "\\)\\|o\\(?:ccurs\\|mitted\\|pen\\)\\|p\\(?:erform\\|rint"
		  "\\)\\|r\\(?:e\\(?:ad\\|cord\\|defines\\|mainder\\|turn\\)"
		  "\\|ounded\\)\\|s\\(?:e\\(?:ction\\|lect\\)\\|paces?\\|t\\"
		  "(?:andard\\|op\\)\\|ubtract\\)\\|t\\(?:h\\(?:en\\|ru\\)\\"
		  "|o\\)\\|until\\|va\\(?:lue\\|rying\\)\\|w\\(?:h\\(?:en\\|"
		  "ile\\)\\|rite\\)\\|zero\\(?:es\\)?"
		  ))

;       (eval-when-compile
;         (regexp-opt 
;			 '("continue" "format" "end" "enddo" "if" "then"
;				"else" "endif" "elseif" "while" "inquire" "stop"
;				"return" "include" "open" "close" "read" "write"
;				"format" "print" "select" "case" "cycle" "exit"
;				"to" "from" "by" "go" "goto" "into" "is"
;				"label" "record" "standard" "omitted"
;				"spaces" "zeroes" "zero" "space" "blank"
;				"redefines" "comp" "value" "occurs"
;				"move" "initialize"
;				"copy" "division" "section" "exit"
;				"evaluate" "when"
;				"display" "accept" "line"
;				"perform" "thru" "varying" "until"
;				"add" "subtract" "multiply" "divide" 
;				"giving" "remainder" "rounded"
;				"compute" ))))
      (cobol-logicals
		 (concat 
		  "\\(and\\|not\\|or\\)"
		  ))
;		(eval-when-compile
;         (regexp-opt 
;			 '("and" "or" "not"))))

      (cobol-constants
		 (concat 
		  "[0-9]*"
		  ))
      (cobol-operators
		 (concat 
		  "\\(+\\|-\\|*\\|/\\|\>\\|\<\\|=\\|or\\)"
		  ))
;		(eval-when-compile
;         (regexp-opt 
;			 '("\+" "\-" 
;				 ))))
		)

;       (eval-when-compile
;         (regexp-opt 
;			 '("and" "or" "not" "lt" "le" "eq" "ge" "gt" "ne"
;				"true" "false")))))

  (setq cobol-font-lock-keywords-1
		  (list
			;;
			;; Fontify syntactically (assuming strings cannot be quoted
			;; or span lines).
			(cons (concat "^......[" comment-chars "].*") 'font-lock-comment-face)
			'("\\(\\s\"\\)"					 ; single- or double-quoted string
			  (1 font-lock-string-face)
			  (cobol-fontify-string nil nil (1 font-lock-string-face)))
			;;
			;; Program, subroutine and function declarations, plus calls.
			(list (concat "\\(^......[^" comment-chars "]\\|"
							  "^[\t][^" comment-chars "]\\)"
							  "\\([a-z][a-z0-9\\-]+\\)")
					'(1 font-lock-keyword-face)
					'(2 font-lock-function-name-face nil t))))

  (setq cobol-font-lock-keywords-2
		  (append cobol-font-lock-keywords-1
					 (list
					  ;;
					  ;; Fontify all type specifiers (must be first; see below).
					  (cons (concat "\\<\\(" cobol-type-types "\\)\\>")
							  '(1 font-lock-type-face))
					  ;;
					  ;; Fontify all builtin keywords (except logical, do
					  ;; and goto; see below).
					  (cons (concat "\\<\\(" cobol-keywords "\\)\\>")
							  '(1 font-lock-keyword-face))
					  ;;
					  ;; Fontify all builtin operators.
					  (cons (concat "\\<\\(" cobol-logicals "\\)\\>")
							  '(1 font-lock-keyword-face)))))

  (setq cobol-font-lock-keywords-3
		  (append cobol-font-lock-keywords-2
					 (list
					  ;;
					  ;; Fontify all builtin keywords (except logical, do
					  ;; and goto; see below).
					  (cons (concat "\\<\\(" cobol-operators "\\)\\>")
							  '(1 font-lock-keyword-face nil t))
					  ;;
					  ;; Fontify all builtin operators.
					  (cons (concat "\\<\\(" cobol-constants "\\)\\>")
							  '(1 font-lock-constant-face nil t))))))


(defvar cobol-font-lock-keywords cobol-font-lock-keywords-2
  "Default expressions to highlight in Cobol mode.")


;; XEmacs change
(put 'cobol-mode 'font-lock-defaults 
	  '(
		 (
		  cobol-font-lock-keywords-3
;		  cobol-font-lock-keywords
;		  cobol-font-lock-keywords-1
;		  cobol-font-lock-keywords-2
;		  cobol-font-lock-keywords-3
;;		  cobol-font-lock-keywords-3
		  )
		 nil t 
		 (
		  (?/ . "$/")
		  (?\; . "w" ) 
		  (?+ . "." ) 
		  (?- . "." ) 
		  (?* . "." ) 
		  (?/ . "." )   
		  (?\' . "\"" )
		  (?\" . "\"" )
		  (?\\ . "/" ) 
		  (?. . "w" ) 
		  (?\n . ">" ) 
		  )	 
		 ))			 
;		  ("_$" . "w")



(defvar cobol-mode-map () 
  "Keymap used in cobol mode.")

(if cobol-mode-map
    ()
  (setq cobol-mode-map (make-sparse-keymap)) ; this SHOULD be a real keymap
  (define-key cobol-mode-map ";" 'cobol-abbrev-start)
  (define-key cobol-mode-map "\C-c;" 'cobol-comment-region)
  (define-key cobol-mode-map [(meta control prior)] 'beginning-of-cobol-subprogram)
  (define-key cobol-mode-map [(meta control next)] 'end-of-cobol-subprogram)
  (define-key cobol-mode-map "\e\C-a" 'beginning-of-cobol-subprogram)
  (define-key cobol-mode-map "\e\C-e" 'end-of-cobol-subprogram)
  (define-key cobol-mode-map "\e;" 'cobol-indent-comment)
;; (define-key cobol-mode-map "\e\C-h" 'mark-cobol-subprogram)
  (define-key cobol-mode-map "\e\n" 'cobol-split-line)
  (define-key cobol-mode-map "\e\C-q" 'cobol-indent-subprogram)
  (define-key cobol-mode-map "\C-c\C-w" 'cobol-window-create)
  (define-key cobol-mode-map "\C-c\C-r" 'cobol-column-ruler)
  (define-key cobol-mode-map "\C-c\C-p" 'cobol-previous-statement)
  (define-key cobol-mode-map "\C-c\C-n" 'cobol-next-statement)
  (define-key cobol-mode-map "\C-c\C-c" 'cobol-goto-column)
  (define-key cobol-mode-map "\C-cc" 'cobol-goto-column) ; avoid confusion
  (define-key cobol-mode-map "\C-c\C-l" 'goto-line) ; for Sam
  (define-key cobol-mode-map "\C-cl" 'goto-line) ; avoid confusion
;;(define-key cobol-mode-map "\t" 'cobol-indent-line)
  (define-key cobol-mode-map [(meta control tab)] 'cobol-indent-line)
  (define-key cobol-mode-map "\C-m" 'newline-and-indent) ; magic RET key
  (let ((n ?\ ))
    (while (< n 127)
      (define-key cobol-mode-map (char-to-string n) 'cobol-self-insert)
      (setq n (1+ n))))
;  (define-key cobol-mode-map "\177" 'cobol-back-delete) ; magic DEL key too
;  (define-key cobol-mode-map "0" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "1" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "2" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "3" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "4" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "5" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "6" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "7" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "8" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "9" 'cobol-electric-line-number)

  )


;; Menus - using easymenu.el
(defvar cobol-mode-menu)  
(defvar cobol-mode-debug-menu)

(defvar cobol-mode-menu-def
     '("Cobol"
        ["Toggle Auto-fill" cobol-auto-fill-mode :style toggle
         :selected (eq auto-fill-function 'cobol-do-auto-fill)]
        ["Toggle abbrev-mode" abbrev-mode :style toggle :selected abbrev-mode]
        "--"
       ["Comment-out Region"
        (cobol-comment-region (region-beginning) (region-end) nil)]
       ["Uncomment-out region"
        (cobol-comment-region (region-beginning) (region-end) t)]
       ["Indent Region" 
        (indent-region (region-beginning) (region-end) 4)]
       ["Indent Subprogram" cobol-indent-subprogram t]
       "--"
       ["Beginning of Subprogram" beginning-of-cobol-subprogram t]
       ["End of Subprogram" end-of-cobol-subprogram t]
       ("Mark"
        ["Subprogram" mark-cobol-subprogram t]
        ["IF Block" cobol-mark-if t]
        ["DO Block" cobol-mark-do t])
       ["Narrow to Subprogram" cobol-narrow-to-subprogram t]
       ["Widen" widen t]
       "--"
       ["Temporary column ruler" cobol-column-ruler t]
       ["72-column window" cobol-window-create t]
;       ["Full Width Window"
;        (enlarge-window-horizontally (- (frame-width) (window-width)))
;        (< (window-width) (frame-width))]
       ["Momentary 72-column window" cobol-window-create-momentarily t]
       "--"
       ["Break Line at Point" cobol-split-line t]
       ["Join Line" cobol-join-line t]
       ["Fill Statement/Comment" fill-paragraph  t]
		 ))




(if (or (featurep 'easymenu) (load "easymenu" t))
    (progn
      (easy-menu-define cobol-mode-menu cobol-mode-map 
			"COBOL editing menu" 
			cobol-mode-menu-def)
      ))



(defvar cobol-mode-abbrev-table nil)
(if cobol-mode-abbrev-table
    ()
  (define-abbrev-table 'cobol-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
    (define-abbrev cobol-mode-abbrev-table  ";b"   "byte" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ch"  "character" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev cobol-mode-abbrev-table  ";c"   "continue" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cm"  "common" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cx"  "complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";di"  "dimension" nil)
    (define-abbrev cobol-mode-abbrev-table  ";do"  "double" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dc"  "double complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dp"  "double precision" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev cobol-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev cobol-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev cobol-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev cobol-mode-abbrev-table  ";eq"  "equivalence" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ex"  "external" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ey"  "entry" nil)
    (define-abbrev cobol-mode-abbrev-table  ";f"   "format" nil)
    (define-abbrev cobol-mode-abbrev-table  ";fu"  "function" nil)
    (define-abbrev cobol-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev cobol-mode-abbrev-table  ";im"  "implicit" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ib"  "implicit byte" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ic"  "implicit complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ich" "implicit character" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ii"  "implicit integer" nil)
    (define-abbrev cobol-mode-abbrev-table  ";il"  "implicit logical" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ir"  "implicit real" nil)
    (define-abbrev cobol-mode-abbrev-table  ";inc" "include" nil)
    (define-abbrev cobol-mode-abbrev-table  ";in"  "integer" nil)
    (define-abbrev cobol-mode-abbrev-table  ";intr" "intrinsic" nil)
    (define-abbrev cobol-mode-abbrev-table  ";l"   "logical" nil)
    (define-abbrev cobol-mode-abbrev-table  ";op"  "open" nil)
    (define-abbrev cobol-mode-abbrev-table  ";pa"  "parameter" nil)
    (define-abbrev cobol-mode-abbrev-table  ";pr"  "program" nil)
    (define-abbrev cobol-mode-abbrev-table  ";p"   "print" nil)
    (define-abbrev cobol-mode-abbrev-table  ";re"  "real" nil)
    (define-abbrev cobol-mode-abbrev-table  ";r"   "read" nil)
    (define-abbrev cobol-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev cobol-mode-abbrev-table  ";rw"  "rewind" nil)
    (define-abbrev cobol-mode-abbrev-table  ";s"   "stop" nil)
    (define-abbrev cobol-mode-abbrev-table  ";su"  "subroutine" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ty"  "type" nil)
    (define-abbrev cobol-mode-abbrev-table  ";w"   "write" nil)))



;; This is actually the expression for C++ mode, but it's used for C too.
(defvar cobol-label-re
      (concat
		 "\\("
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Label = only one cobol name
       "\\([ \t]+\\)"	                  ; mandatory whitespaces
       "\\(division\\|section\\)"         ; division, section
		 "\\|"
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Label = only one cobol name
       "[.]"                              ; Point Terminated
		 "\\)"
		 ))


;; This is actually the expression for C++ mode, but it's used for C too.
(defvar cobol-imenu-generic-expression
  (`
   (("Sections"
     (,
      (concat
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Label = only one cobol name
       "\\([ \t]+\\)"	                  ; mandatory whitespaces
       "\\(division\\|section\\)"         ; division, section
       )) 3)
    ("Copy"
     (, (concat
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\([ \t]*\\)"	                  ; Col 8-xx optional whitespaces
       "\\(\\bcopy\\b\\)"	          ; keyword Copy
       "\\([ \t]+\\)"	                  ; mandatory whitespaces
       "\\([^ \t.\n]+\\)"	          ; copy name (how to treat ""  ?)
	 )) 6)
    ("Records"
     (,
      (concat
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\(\\b01\\b\\)"	                  ; Col 8 Level 01
       "\\([ \t]+\\)"	                  ; mandatory whitespaces
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Variable = one cobol name
       )) 5)
    ("Variables"
     (,
      (concat
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\(\\b77\\b\\)"	                  ; Col 8 Level 77
       "\\([ \t]+\\)"	                  ; mandatory whitespaces
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Variable = one cobol name
       )) 5)
    ("Labels"
     (,
      (concat
       "^"				  ; beginning of line is required
       "\\(......\\)"                     ; Col 1-6 comment (how to treat tabs ?)
       "\\([^*]\\)"	                  ; Col 7 ^comment
       "\\([a-zA-Z][a-zA-Z0-9-]*\\)"	  ; Col 8 Label = only one cobol name
       "[.]"                              ; Point Terminated
       )) 3)
    ))
  "Imenu generic expression for Cobol mode.  See `imenu-generic-expression'.")

;;;###autoload
(defun cobol-mode ()
  "Major mode for editing Cobol code.
Tab indents the current cobol line correctly. 

Type `;?' or `;\\[help-command]' to display a list of built-in abbrevs for 
Cobol keywords.

Key definitions:
\\{cobol-mode-map}

Variables controlling indentation style and extra features:

 `comment-start'
    Should allways be nil in Cobol mode.  Cobol has no in-line comments.
 `cobol-do-indent'
    Extra indentation within do blocks.  (default 4)
 `cobol-if-indent'
    Extra indentation within if blocks.  (default 4)
 `cobol-continuation-indent'
    Extra indentation appled to continuation statements.  (default 6)
 `cobol-indent-increment'
    Amount of indentation to add to a line when it can be indented (default 4)
 `cobol-comment-line-column'
    Amount of indentation for text within full-line comments. (default 6)
 `cobol-comment-indent-style'
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at column cobol-comment-line-column
    relative  means indent at cobol-comment-line-column beyond the
 	      indentation for a line of code.
    Default value is fixed.
 `cobol-comment-indent-char'
    Character to be inserted instead of space for full-line comment
    indentation.  (default SPC)
 `cobol-minimum-statement-indent'
    Minimum indentation for cobol statements. (default 8)
 `cobol-line-number-indent'
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 `cobol-check-all-num-for-matching-do'
    Non-nil causes all numbered lines to be treated as possible 'continue'
    statements.  (default nil)
 `cobol-continuation-char'
    character to be inserted in column 5 of a continuation line.
    (default is ?-)
 `cobol-comment-region'
    String inserted by \\[cobol-comment-region] at start of each line in 
    region.  (default \"      ** \")
 `cobol-electric-line-number'
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 `cobol-startup-message'
    Set to nil to inhibit message first time cobol-mode is used.

Turning on Cobol mode calls the value of the variable cobol-mode-hook 
with no args, if that value is non-nil.
"
  (interactive)
  (kill-all-local-variables)
  (if cobol-startup-message
      (message "Emacs Cobol mode ver. %s.  Mail bugs to bug-cobol-mode@cis.ohio-state.edu" 
					cobol-mode-version))
  (setq cobol-startup-message nil)
 
  (setq local-abbrev-table cobol-mode-abbrev-table)
  (set-syntax-table cobol-mode-syntax-table)

 ;; Font Lock mode support.
  (if (cobol-xemacs) nil ; XEmacs uses properties 
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
          '((cobol-font-lock-keywords
             cobol-font-lock-keywords-1 
				 cobol-font-lock-keywords-2
				 cobol-font-lock-keywords-3)
            t t
				((?/ . "$/") ("_$" . "w"))
            beginning-of-line
            (font-lock-syntactic-keywords . cobol-font-lock-keywords)))

    ;; Set up support for find-file.el.
    (make-variable-buffer-local 'ff-other-file-alist)
    (make-variable-buffer-local 'ff-search-directories)
    (setq ff-other-file-alist   'cobol-other-file-alist
          ff-search-directories 'cobol-search-directories
          ff-pre-load-hooks     'ff-which-function-are-we-in
          ff-post-load-hooks    'ff-set-point-accordingly
          ff-file-created-hooks 'cobol-make-body))



  (make-local-variable 'font-lock-defaults)

;  (setq font-lock-defaults '((cobol-font-lock-keywords
;			      cobol-font-lock-keywords-1
;			      cobol-font-lock-keywords-2
;			      cobol-font-lock-keywords-3)
;			     t t ((?/ . "$/") ("_$" . "w"))))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cobol-indent-line)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'cobol-comment-hook)

  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip "^ *\\*") ; The only way to do a comment is a * in column 7

  (make-local-variable 'comment-line-start)
  (setq comment-line-start "** ")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "![ \t]*")

  (make-local-variable 'comment-start)
  (setq comment-start nil)		; COBOL has no in-line comments

  (make-local-variable 'comment-column)
  (setq comment-column cobol-comment-line-column)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'write-file-hooks)
  (setq write-file-hooks (cons 'cobol-no-tabs-hook write-file-hooks))

  (make-local-variable 'find-file-hooks)
  (setq find-file-hooks (cons 'cobol-no-tabs-hook find-file-hooks))

  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)

  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (make-local-variable 'fill-column)
  (setq fill-column 70)

  (when (featurep 'easymenu)
    (easy-menu-add cobol-mode-menu cobol-mode-map))

  (setq mode-name "Cobol")
  (setq major-mode 'cobol-mode)

  (use-local-map cobol-mode-map)


  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression cobol-imenu-generic-expression)
  (setq imenu-case-fold-search t)  ; Ignore Case in imenu-search

  (run-hooks 'cobol-mode-hook)

  ) ;; --- cobol-mode


(defun cobol-comment-hook ()
  cobol-comment-line-column)		; ALLWAYS comment in the comment column

(defun cobol-self-insert (arg)
  "Do a self-insert-command, and check for the right margin, ringing
the bell if it is reached."
  (interactive "*p")
  (let ((column (current-column)))
    (self-insert-command arg)
    (if (and (< column fill-column)
	     (>= (current-column)
		 fill-column))
	(beep 't))))

(defun cobol-goto-column (arg)
  "Goto column ARG, counting from column 1, adding spaces to
 the end of the line if needed"
  (interactive "NGoto column: ")
  (if (> arg 0)
      (progn
	(end-of-line)
	(if (> (current-column) (- arg 1))
	    (progn
	      (beginning-of-line)
	      (forward-char (- arg 1)))
	  (insert-char ?  (- arg (current-column) 1))))))
    
(defun cobol-back-delete (arg &optional killp)
  "Slightly magic version of backward-delete-char-untabify"
  (interactive "*p\nP")
  (let (atws (column (current-column)))
    (insert-char ?\n 1)
    (forward-char -1)
    (beginning-of-line)
    (if (looking-at "[ \t]*$")
	(progn
	  (if (= (% (+ column 1) cobol-indent-increment) 0)
	      (setq column (max cobol-minimum-statement-indent
				(- column cobol-indent-increment)))
	    (setq column (max cobol-minimum-statement-indent
			      (* (/ column cobol-indent-increment)
				 cobol-indent-increment))))
	  (delete-horizontal-space)
	  (insert-char (if (stringp cobol-comment-indent-char)
			   (aref cobol-comment-indent-char 0)
			 cobol-comment-indent-char)
		       column))
      (progn
	(end-of-line)
	(backward-delete-char-untabify arg killp)))
    (end-of-line)
    (delete-char 1)))

(defun cobol-no-tabs-hook ()
  "Hook for write file that removes all tabs from the buffer.
This function must return nil so that the file will actually be written."
  (save-excursion
    ; the following code is stolen from tabify.el...
    (goto-char (point-min))
    (while (search-forward "\t" nil t)        ; faster than re-search
      (let ((start (point))
	    (column (current-column))
	    (indent-tabs-mode nil))
	(skip-chars-backward "\t")
	(delete-region start (point))
	(indent-to column))))
  nil)				; just in case to make sure file is written

(defun cobol-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (delete-horizontal-regexp " \t\\*") ; kill the old comment stuff
	 (indent-to (cobol-comment-hook))
	 (insert comment-line-start))
	;; No existing comment.
	;; Insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (indent-to (cobol-comment-hook))
	 (insert comment-line-start)
	 )))

;;	 (insert-char (if (stringp cobol-comment-indent-char)
;;			  (aref cobol-comment-indent-char 0)
;;			  cobol-comment-indent-char)
;;		      (- (calculate-cobol-indent) (current-column))))))

(defun cobol-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts cobol-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert cobol-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert cobol-comment-region)))
      (let ((com (regexp-quote cobol-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))


(defun cobol-abbrev-start ()
  "Typing ;\\[help-command] or ;? lists all the Cobol abbrevs. 
Any other key combination is executed normally."
  (interactive)
  ;; XEmacs change
  (let (e c)
    (insert last-command-char)
    (setq e (next-command-event)
	  c (event-to-character e))
    ;; insert char if not equal to `?'
    (if (or (= c ??) (eq c help-char))
	(cobol-abbrev-help)
      (setq unread-command-events (list e)))))


(defun cobol-abbrev-help ()
  "List the currently defined abbrevs in Cobol mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (cobol-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun cobol-prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (insert-abbrev-table-description 'cobol-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun cobol-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of cobol-column-ruler.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display 
   cobol-column-ruler (save-excursion (beginning-of-line) (point))
   nil "Type SPC or any command to erase ruler."))

(defun cobol-window-create ()
  "Makes the window 72 columns wide."
  (interactive)
  (let ((window-min-width 2))
    (split-window-horizontally 73))
  (other-window 1)
  (switch-to-buffer " cobol-window-extra" t)
  (select-window (previous-window)))

(defun cobol-window-create-momentarily (&optional arg)
  "Momentarily makes the window 72 columns wide.
Optional ARG non-nil and non-unity disables the momentary feature.
See also `cobol-window-create'."
  (interactive "p")
  (if (or (not arg)
	  (= arg 1))
      (save-window-excursion
	(if (not (equal (cobol-window-create) 'error))
	    (progn (message "Type SPC to continue editing.")
		   ;; XEmacs change
		   (let ((char (next-command-event)))
		     (or (equal (event-to-character char) ? )
			 (setq unread-command-events (list char)))))))
    (cobol-window-create)))

(defun cobol-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert ?\n comment-line-start ?\  )
      (insert ?\n cobol-continuation-char))
  (cobol-indent-line))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun cobol-electric-line-number (arg)
  "Self insert, but if part of a Cobol line number indent it automatically.
Auto-indent does not happen if a numeric arg is used."
  (interactive "P")
  (if (or arg (not cobol-electric-line-number))
      (self-insert-command arg)
    (if (or (save-excursion (re-search-backward "[^ \t0-9]"
						(save-excursion
						  (beginning-of-line)
						  (point))
						t)) ;not a line number
	    (looking-at "[0-9]"))		;within a line number
	(insert last-command-char)
      (skip-chars-backward " \t")
      (insert last-command-char)
      (cobol-indent-line))))

(defun beginning-of-cobol-subprogram ()
  "Moves point to the beginning of the current cobol subprogram."
  (interactive)
  (let ((case-fold-search t))
;    (if (looking-at "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")
;		  (forward-line 1)))
    (beginning-of-line nil)
    (re-search-backward cobol-label-re nil 'move)
    (beginning-of-line nil)
    (re-search-forward "[^ \t]" nil 'move)
	 (recenter 4)
	 ))

(defun end-of-cobol-subprogram ()
  "Moves point to the end of the current cobol subprogram."
  (interactive)
  (let ((case-fold-search t))
    (end-of-line nil)
    (re-search-forward cobol-label-re nil 'move)
    (beginning-of-line nil)
    (re-search-forward "[^ \t]" nil 'move)
	 (recenter 4)
	 ))

(defun mark-cobol-subprogram ()
  "Put mark at end of cobol subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-cobol-subprogram)
  (push-mark (point))
  (beginning-of-cobol-subprogram))
  
(defun cobol-previous-statement ()
  "Moves point to beginning of the previous cobol statement.
Returns 'first-statement if that statement is the first
non-comment Cobol statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (looking-at
	   (concat "      " (regexp-quote (char-to-string
					   cobol-continuation-char)))))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
;;		(or (looking-at comment-line-start-skip))
		(looking-at "[ \t]*$")))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test	
	   (cobol-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun cobol-next-statement ()
  "Moves point to beginning of the next cobol statement.
 Returns 'last-statement if that statement is the last
 non-comment Cobol statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement (= (forward-line 1) 0))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
		    )))
    (if (not not-last-statement)
 	'last-statement)))

(defun cobol-blink-matching-if ()
  ;; From a Cobol ENDIF statement, blink the matching IF statement.
  (let ((top-of-window (window-start)) matching-if
	(endif-point (point)) message)
    (if (save-excursion (beginning-of-line)
			(skip-chars-forward " \t0-9")
			(looking-at "END-IF\\b"))
	(progn
          (if (not (setq matching-if (cobol-beginning-if)))
              (setq message "No matching if.")
            (if (< matching-if top-of-window)
                (save-excursion
                  (goto-char matching-if)
                  (beginning-of-line)
                  (setq message
                        (concat "Matches "
                                (buffer-substring
                                 (point) (progn (end-of-line) (point))))))))
	  (if message
	      (message "%s" message)
	    (goto-char matching-if)
	    (sit-for 1)
	    (goto-char endif-point))))))

(defun cobol-mark-if ()
  "Put mark at end of Cobol IF-ENDIF construct, point at beginning.
The marks are pushed."
  (interactive)
  (let (endif-point if-point)
    (if (setq endif-point (cobol-end-if))
        (if (not (setq if-point (cobol-beginning-if)))
            (message "No matching if.")
          ;; Set mark, move point.
          (goto-char endif-point)
          (push-mark)
          (goto-char if-point)))))

(defun cobol-end-if ()
  ;; Search forwards for first unmatched ENDIF.  Return point or nil.
  (if (save-excursion (beginning-of-line)
                      (skip-chars-forward " \t0-9")
                      (looking-at "END-IF\\b"))
      ;; Sitting on one.
      (match-beginning 0)
    ;; Search for one.  The point has been already been moved to first
    ;; letter on line but this should not cause troubles.
    (save-excursion
      (let ((count 1))
        (while (and (not (= count 0))
                    (not (eq (cobol-next-statement) 'last-statement))
                    ;; Keep local to subprogram.
                    (not (looking-at
                          "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")))

          (skip-chars-forward " \t0-9")
          (cond ((looking-at "end[ \t]*if\\b")
                 (setq count (- count 1)))

                ((looking-at "IF[ \t]")
                 (save-excursion
                   (if (or
                        (looking-at ".*[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
                        (let (then-test) ; Multi-line if-then.
                          (while
                              (and (= (forward-line 1) 0)
                                   ;; Search forward for then.
                                   (or (looking-at "     [^ 0\n]")
                                       (looking-at "\t[1-9]"))
                                   (not
                                    (setq then-test
                                          (looking-at
                                           ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
                          then-test))
                       (setq count (+ count 1)))))))

        (and (= count 0)
             ;; All pairs accounted for.
             (point))))))

(defun cobol-beginning-if ()
  ;; Search backwards for first unmatched IF-THEN.  Return point or nil.
  (if (save-excursion
        ;; May be sitting on multi-line if-then statement, first move to
        ;; beginning of current statement.  Note: `cobol-previous-statement'
        ;; moves to previous statement *unless* current statement is first
        ;; one.  Only move forward if not first-statement.
        (if (not (eq (cobol-previous-statement) 'first-statement))
            (cobol-next-statement))
        (skip-chars-forward " \t0-9")
        (and
         (looking-at "IF[ \t]")
         (save-match-data
           (or (looking-at ".*[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
               ;; Multi-line if-then.
               (let (then-test)
                 (while
                     (and (= (forward-line 1) 0)
                          ;; Search forward for then.
                          (or (looking-at "     [^ 0\n]")
                              (looking-at "\t[1-9]"))
                          (not
                           (setq then-test
                                 (looking-at
                                  ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
                 then-test)))))
      ;; Sitting on one.
      (match-beginning 0)
    ;; Search for one.
    (save-excursion
      (let ((count 1))
        (while (and (not (= count 0))
                    (not (eq (cobol-previous-statement) 'first-statement))
                    ;; Keep local to subprogram.
                    (not (looking-at
                          "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")))

          (skip-chars-forward " \t0-9")
          (cond ((looking-at "if[ \t]*(")
                 (save-excursion
                   (if (or
                        (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
                        (let (then-test) ; Multi-line if-then.
                          (while
                              (and (= (forward-line 1) 0)
                                   ;; Search forward for then.
                                   (or (looking-at "     [^ 0\n]")
                                       (looking-at "\t[1-9]"))
                                   (not
                                    (setq then-test
                                          (looking-at
                                           ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
                          then-test))
                       (setq count (- count 1)))))
                ((looking-at "end[ \t]*if\\b")
                 (setq count (+ count 1)))))

        (and (= count 0)
             ;; All pairs accounted for.
             (point))))))

(defun cobol-indent-line ()
  "Indents current cobol line based on its contents and on previous lines."
  (interactive)
  (if (or (eq last-command 'cobol-indent-line) ; if we just did a tab
	  (let (atws)
	    (insert-char ?\n 1)
	    (forward-char -1)
	    (beginning-of-line)
	    (setq atws (looking-at "[ \t]*$"))
	    (end-of-line)
	    (delete-char 1)
	    (not atws)))
      (insert-char (if (stringp cobol-comment-indent-char)
		       (aref cobol-comment-indent-char 0)
		     cobol-comment-indent-char)
		   (- cobol-indent-increment
		      (% (+ (current-column) 1) cobol-indent-increment)))
	
    (let ((do-another-tab nil)
	  (cfi (calculate-cobol-indent))
	  (cur-col (current-column))) ; we did NOT just do a tab
      (save-excursion
	(beginning-of-line)
	(if (not (= cfi (current-indentation)))
	    (cobol-indent-to-column cfi)
	  ; else the line is indented correctly; check for a comment
	  (beginning-of-line)
	  (if (re-search-forward comment-start-skip
				 (save-excursion (end-of-line) (point)) 'move)
	      (cobol-indent-comment)
	    ; else not looking at a comment; make another tab
	    (if (= cur-col cfi)
		(setq do-another-tab 't)))))
      (if do-another-tab
	  (insert-char (if (stringp cobol-comment-indent-char)
			   (aref cobol-comment-indent-char 0)
			 cobol-comment-indent-char)
		       (- cobol-indent-increment
			  (% (+ (current-column) 1)
			     cobol-indent-increment))))
      ;; Never leave point in left margin.
      (if (< (current-column) cfi)
	  (move-to-column cfi)))))

(defun cobol-indent-subprogram ()
  "Properly indents the Cobol subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-cobol-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-cobol-indent ()
  "Calculates the cobol indent column based on previous lines."
  (let (icol first-statement (special-col nil) (case-fold-search t))
    (save-excursion
      (setq first-statement (cobol-previous-statement))
      (if first-statement
	  (setq icol cobol-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol cobol-minimum-statement-indent)
	    (setq icol (cobol-current-line-indentation)))
	  (if (looking-at "[ \t]*\\*")	; if looking a at comment
	      (setq special-col 't))
	  (skip-chars-forward " \t0-9")
	  (cond ((looking-at "if[ \t]*(")
		 (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
			 (let (then-test)	;multi-line if-then
			   (while (and (= (forward-line 1) 0) ;search forward for then
				       (looking-at "     [^ 0]")
				       (not (setq then-test (looking-at ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
			   then-test))
		     (setq icol (+ icol cobol-if-indent))))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol cobol-if-indent)))
		((looking-at "do\\b")
		 (setq icol (+ icol cobol-do-indent)))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))	; blank lines do nothing
	    ((looking-at comment-line-start-skip) ; junk for comments
	     (setq icol cobol-comment-line-column)
	     (setq special-col t))
	    ((looking-at (concat "      "
				 (regexp-quote (char-to-string cobol-continuation-char))))
	     (setq icol cobol-continuation-indent)
	     (setq special-col t))
	    (first-statement)		;if first in the file, don't do anything
	    ((and cobol-check-all-num-for-matching-do
		  (looking-at "[ \t]*[0-9]+")
		  (cobol-check-for-matching-do))
	     (setq icol (- icol cobol-do-indent)))
	    (t
	     (skip-chars-forward " \t")	; skip to first real stuff
	     (cond
	      ;;; The following are for special names that MUST
	      ;;; start in area A (column 8-11)
	      ((looking-at "[a-z]+ +division") ; divisions in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "[a-z]+ +section") ; sections in area A
	       (setq icol cobol-minimum-statement-indent))
	      ;; this SHOULD get paragraph names
	      ((looking-at "[a-z]+\\.") ; paragraphs
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "fd ")	; fd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "sd ")	; sd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "rd ")	; rd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "cd ")	; cd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "01 ")	; 01 level numbers in A too
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "77 ")	; and 77 level numbers
	       (setq icol cobol-minimum-statement-indent))

	      ;;; the following are for end-of-block detection
	      ((looking-at "end-if\\b")
	       (setq icol (- icol cobol-if-indent)))
	      ((looking-at "else\\b")
	       (setq icol (- icol cobol-if-indent)))
	      ((and (looking-at "continue\\b")
		    (cobol-check-for-matching-do))
	       (setq icol (- icol cobol-do-indent)))
	      ((looking-at "end[ \t]*do\\b")
	       (setq icol (- icol cobol-do-indent)))
	      ((and (looking-at "end\\b[ \t]*[^ \t=(a-z]")
		    (not (= icol cobol-minimum-statement-indent)))
	       (message "Warning: `end' not in column %d.  Probably an unclosed block." cobol-minimum-statement-indent))
	      (t			; in the case of normal lines
	       nil)
	       ))))
    (if special-col
	icol
      (max cobol-minimum-statement-indent icol))))

(defun cobol-current-line-indentation ()
  "Indentation of current line, ignoring Cobol line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (current-indentation))
;  (save-excursion
;    (beginning-of-line)
;    (cond ((looking-at comment-line-start-skip)
;	   (goto-char (match-end 0))
;	   (skip-chars-forward
;	     (if (stringp cobol-comment-indent-char)
;		 cobol-comment-indent-char
;	         (char-to-string cobol-comment-indent-char))))
;	  ((looking-at "     [^ 0\n]")
;	   (goto-char (match-end 0)))
;	  (t
;	   ;; Move past line number.
;	   (move-to-column 5)))
;    ;; Move past whitespace.
;    (skip-chars-forward " \t")
;    (current-column)))

(defun cobol-indent-to-column (col)
  "Indents current line with spaces to column COL.
notes: 1) A minus sign character in column 6 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If cobol-continuation-char is the first non-whitespace character,
          this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if cobol-comment-indent-style
	    (let ((char (if (stringp cobol-comment-indent-char)
			    (aref cobol-comment-indent-char 0)
			    cobol-comment-indent-char)))
	      (delete-horizontal-space)
	      (insert-char char cobol-comment-line-column)))

;;      (if (looking-at "     [^ 0\n]")
;;	  (forward-char 8)
;;	(delete-horizontal-space)
;;	;; Put line number in columns 0-4
;;	;; or put continuation character in column 5.
;;	(cond ((eobp))
;;	      ((= (following-char) cobol-continuation-char)
;;	       (indent-to 5)
;;	       (forward-char 1))
;;	      ((looking-at "[0-9]+")
;;	       (let ((extra-space (- 5 (- (match-end 0) (point)))))
;;		 (if (< extra-space 0)
;;		     (message "Warning: line number exceeds 5-digit limit.")
;;		   (indent-to (min cobol-line-number-indent extra-space))))
;;	       (skip-chars-forward "0-9"))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
;;      (if (re-search-forward comment-start-skip
;;			     (save-excursion (end-of-line) (point)) t)
;;	  (progn (goto-char (match-beginning 0))
;;		 (if (not (= (current-column) (cobol-comment-hook)))
;;		     (progn (delete-horizontal-space)
;;			    (indent-to (cobol-comment-hook))))))
      )))

(defun cobol-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indente.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) cobol-line-number-indent)
	 (or (= (current-column) cobol-line-number-indent)
	     (progn (skip-chars-forward "0-9")
		    (= (current-column) 5))))))

(defun cobol-check-for-matching-do ()
  "When called from a numbered statement, returns t
 if matching 'do' is found, and nil otherwise."
  (let (charnum
	(case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]*[0-9]+")
	  (progn
	    (skip-chars-forward " \t")
	    (skip-chars-forward "0") ;skip past leading zeros
	    (setq charnum (buffer-substring (point)
					    (progn (skip-chars-forward "0-9")
						   (point))))
	    (beginning-of-line)
	    (and (re-search-backward
		  (concat "\\(^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]\\)\\|\\(^[ \t0-9]*do[ \t]*0*"
			  charnum "\\b\\)\\|\\(^[ \t]*0*" charnum "\\b\\)")
		  nil t)
		 (looking-at (concat "^[ \t0-9]*do[ \t]*0*" charnum))))))))


;;ErrrErrrFrom: simon@gnu (Simon Marshall)
;; Find the next ! not in a string.
(defun cobol-match-!-comment (limit)
  nil)

(provide 'cobol)


(setq ffap-alist                   ; add something to `ffap-alist'
 (cons
  '( cobol-mode . cobol-ffap-mode )
  ffap-alist))





;;; cobol.el ends here
