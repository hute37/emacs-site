;; brief.el -- Brief-Emulation for Emacs
;; Copyright (C) 2001  Mirko Link

;; Author: Mirko Link <mirkolink@mirkolinkonline.de>
;; Keywords: emulations brief
;; $Revision: 1.1 $
;; $Date: 2005-11-26 01:15:01 $

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111, USA.

;; Revision: 1.01
;; - Kommentar entfernt
;; - truncate-lines immer eingeschaltet (per find-file-hook)
;; - beim öffnen einer Datei wird der Dateiname im Minibuffer angezeigt
;; - die Brief-Tastenbindung C-x wird im Emacs-Lisp-mode deaktiviert
;; - die Brief-Tastenbindung C-c wird nur noch im cs31-mode aktiviert

;; Dieser Brief-mode entstand auf Basis des Brief-Makros keyboard.h von Dave Nanian und Michael Strickman.

;; Brief-Tastenkürzel:
;; 	Tastenanschlag		Befehl

;; -  	Alt-a			Ausschließ. Markierung
;; +	Alt-b			Pufferliste
;; +	Ctrl-b			Zeile an Fensterende
;; +	Alt-C			Spaltenmarkierung
;; +	Ctrl-C			Zeile in Fenster zentrieren
;; +	Alt-D			Zeile löschen
;; +	Ctrl-D			Puffer nach unten schieben
;; +	Alt-E			Datei editieren
;; +	Alt-F			Dateinamen anzeigen
;; +	Alt-G			Nach Zeile
;; +	Alt-H			Hilfe
;; +	Alt-I			Einfügemodus ein/aus
;; +	Alt-J			Nach Lesezeichen
;; +	Alt-K			Bis Zeilenende löschen
;; +	Alt-L			Zeilenmarkierung
;; +	F1			Fenster wechseln
;; -	F2			Fenster ändern
;; +	F3			Fenster erstellen
;; +	F4			Fenster löschen
;; -	Alt-F1			Rand ein/aus
;; +	Alt-F2			Zoomumschaltung
;; +	F5			Vorwärts suchen
;; +    F6                      Vorwärts wechseln
;; +    Shift-F5                Erneut suchen
;; +    Shift-F6                Eneut wechseln
;; +	Ctrl-F5			Groß-/Kleinschreibung berücksichtigen
;; +	Ctrl-F6			Regelausdruck ein/aus
;; +	Alt-F5			Rückwärts suchen
;; +	Alt-F6			Rückwärts wechseln
;; +	F7			Aufzeichnen
;; +	Alt-M			Markierung
;; +	Alt-N			Nächster Puffer
;; +	Ctrl-N			Nächster Fehler
;; +	Alt-O			Ausgabedatei wechseln
;; +	Alt-P			Block drucken
;; +	Ctrl-P			Fehlereinblendfenster
;; +	Alt-Q			Zitieren
;; +	Alt-R			Datei einlesen
;; +	Ctrl-R			Wiederholen
;; +	Alt-S			Vorwärts suchen
;; +	Alt-T			Vorwärts wechseln
;; +	Ctrl-T			Zeile an Fensteranfang
;; +	Alt-U			Rückgängig
;; +	Ctrl-U			Puffer nach oben
;; +	Alt-V			Versions-ID anzeigen
;; -	Alt-F7			Tastenmakro laden
;; -	Shift-F7		Aufzeichnungsunterbrechung
;; +	F8			Ablaufen
;; -	Alt-F8			Tastenmakro sichern
;; -	F9			Makrodatei laden
;; -	Shift-F9		Makrodatei löschen
;; +	F10			Befehl ausführen
;; -	Alt-F1			Rand ein/aus
;; +	Alt-F10			Puffer kompilieren
;; +	Pos1			Zeilenanfang
;; +	Pos1 Pos1		Fensteranfang
;; +	Pos1 Pos1 Pos1		Pufferanfang
;; +	Ende			Zeilenende
;; +	Ende Ende		Fensterende
;; +	Ende Ende Ende		Pufferende
;; +	Alt-W			Schreiben
;; +	Ctrl-W			Sicherung ein/aus
;; +	Alt-X			Verlassen
;; +	Ctrl-X			Schreiben und verlassen
;; +	Alt-Z			BRIEF	unterbrechen
;; +	Ctrl-Z			Zoomumschaltung
;; +	Alt-Minus		Vorheriger Puffer
;; +	Ctrl-Minus		Puffer löschen
;; +	Rückschrittaste		Rückschritt
;; +	Ctrl-Rückschritt	Vorh. Wort löschen
;; +	Eingabetaste		Eingabe
;; +	Ctrl-Eingabe		Neue Zeile
;; +	Escape			Abbrechen
;; +	Tab			Tabulator
;; +	Shift-Tab		Rücktabulator
;; +	Shift-Ende		Rechte Fensterseite
;; +	Ctrl-Pos1		Fensteranfang
;; +	Ctrl-Ende		Fensterende
;; +	Bild ab			Bild ab
;; +	Bild auf		Bild auf
;; +	Ctrl-Bild ab		Pufferende
;; +	Ctrl-Bild auf		Pufferanfang
;; +	Pfeil unten		Unten
;; +	Pfeil links		Links
;; +	Pfeil rechts		Rechts
;; +	Pfeil oben		Oben
;; +	Ctrl-Rechts		Nächstes Wort
;; +	Ctrl-Links		Vorh. Wort
;; -	Ctrl-Pause		Unterbrechung
;; +	Alt-1			Lesezeichen 1 einfügen
;; +	Alt-2			Lesezeichen 2 einfügen
;; +	Alt-3			Lesezeichen 3 einfügen
;; 	.			.
;; 	.			.
;; +	Alt-0			Lesezeichen 10 einfügen
;; +	Lösch			Löschen
;; +	Einfg			Einsetzen (aus Zwischenabl.)
;; +	Graue Taste -		Ausschneiden (in Zwischenabl.)
;; +	Graue Taste +		Kopieren (in Zwischenabl.)
;; +	Graue Taste *		Rückgängig

;; Code:

(require 'bs)
(require 'rect)
(require 'view)

(defgroup brief nil
  "Der Brief-Mode paßt den Emacs an das Tastenmodell des Brief-Editors an."
  :prefix "brief-"
  :group 'emulations)

(defcustom brief-clean-eof nil
  "Beim Speichern einer Datei werden Tabulatoren und Leerzeichen am
Zeilenende entfernt. Ist diese Variable ungleich nil, dann bleiben
alle Zeichen erhalten."
  :type 'boolean
  :group 'brief)

(defcustom brief-mode-modeline-string " *Brief*"
  "Der Inhalt dieser Variablen wird in der Statuszeile angezeigt,
wenn der brief-mode aktiv ist."
  :type 'string
  :group 'brief)

(defcustom brief-scroll-step 1
  "Der Wert wird bei aktivem Brief-mode auf die Variable `scroll-step' geschrieben."
  :type 'number
  :group 'brief)

;;interne Variablen
(defvar old-scroll-step 0)
(defvar old-brief-clean-eof brief-clean-eof)
(defvar brief-del-window-map (make-sparse-keymap) "Tastatur-Tabelle für das Löschen von Fenstern.")
(defvar brief-change-window-map (make-sparse-keymap) "Tastatur-Tabelle für das Wechseln von Fenstern.")
(defvar brief-create-edge-map (make-sparse-keymap) "Tastatur-Tabelle für das Erstellen von Fenstern.")
(defvar brief-copy-line nil "Es wurde eine Zeile in die Zwischenablage kopiert.")
(defvar ziel-fenster nil "Interne Variable zur Window-Navigation.")
(defvar line-mode-active nil "Der Brief-Zeilenmodus ist aktiv.")
(defvar rect-mode-active nil "Der Brief-Rechteckmodus ist aktiv.")
(defvar list-line-overlays nil "Liste der Overlays, welche die markierten Zeilen zeigen.")
(defvar list-rect-overlays nil "Liste der Overlays, welche das markierte Rechteck zeigen.")
(defvar transient-mark-mode-was-active nil "Zeigt an, daß der transient-mark-mode aktiv war.")
(defvar brief-mode-map (make-sparse-keymap) "Lokale Tastatur-Tabelle für den brief-mode.")
(defvar brief-last-last-command nil "Der Vergangenheitswert der Variablen `last-command'.")
(defvar brief-replace-oldstring () "Nach diesem String wird mit `brief-translate-again' gesucht.")
(defvar brief-replace-newstring () "Mit diesem String wird der mit `brief-translate-again' gesuchte String ersetzt.")
(defvar brief-replace-direction "v" "Gibt die Richtung der letzten Suchen-Ersetzen-Operation an.")
(defvar brief-search-string () "Nach diesem String wird mit `brief-search-again' gesucht.")
(defvar brief-search-direction "v" "Gibt die Richtung der letzten Suchen-Ersetzen-Operation an.")

;;;###autoload
(defcustom brief-mode nil
  "Zeigt den Status des brief-mode an:
nil - brief-mode ist inaktiv
 t  - brief-mode ist aktiv"
  :set (lambda (symbol value) (brief-mode (if value 1 0)))
  :initialize 'custom-initialize-default
  :require 'brief
  :version "20.7"
  :type 'boolean
  :group 'brief)

(defcustom brief-load-hook nil
  "Hooks, die nach dem Laden des brief-mode-Packetes aufgerufen werden."
  :type 'hook
  :group 'brief)

(defcustom brief-mode-hook nil
  "Hook, der durch die Funktion `brief-mode' gestartet wird."
  :type 'hook
  :group 'brief)

(defconst brief-version " $Revision: 1.1 $ "
  "Die Version des Brief-Emulators.")

(defconst brief-mode-help-address "mirkolink@mirkolinkonline.de"
  "Die email-Adresse des brief-mode-Authors.")

(defcustom brief-replace-regexp nil
  "Diese Variable entscheidet, ob bei den Suchen-Ersetzen-Operationen nach regulären Ausdrücken
gesucht (= t) wird oder nicht(= nil)"
  :type 'boolean
  :group 'brief)

;;   ***********************************************************************************
;;  ***                                                                               ***
;; **** Brief-Funktionen und Tastendefinitionen für die Arbeit mit Makros             ****
;;  ***                                                                               ***
;;   ***********************************************************************************
(define-key brief-mode-map [f7]        'brief-toggle-kbd-macro)
(define-key brief-mode-map [f8]        'call-last-kbd-macro)
;;(define-key brief-mode-map [(meta f8)] 'save-kbd-macro)
;; erst benennen
;; dann als lisp (?) abspeichern -> brief speichert Tastenfolgen: <taste>

(defun brief-toggle-kbd-macro ()
  "Startet oder beendet die Aufzeichnung eines Makros"
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

;;   ***********************************************************************************
;;  ***                                                                               ***
;; **** Brief-Funktionen und Tastendefinitionen für den help-mode                     ****
;;  ***                                                                               ***
;;   ***********************************************************************************
(define-key view-mode-map [escape]           'View-quit)
(define-key view-mode-map [(control home)]   (lambda ()
					       (interactive)
					       (move-to-window-line 0)))
(define-key view-mode-map [(control end)]    (lambda ()
					       (interactive)
					       (move-to-window-line -1)))
(define-key view-mode-map [(next)]           'scroll-up)
(define-key view-mode-map [(prior)]          'scroll-down)
(define-key view-mode-map [(control next)]   (lambda ()
					       (interactive)
					       (goto-char (point-min))))
(define-key view-mode-map [(control prior)]  (lambda ()
					       (interactive)
					       (goto-char (point-max))))
(define-key view-mode-map [(home)]           'brief-home)
(define-key view-mode-map [(end)]            'brief-end)
(define-key view-mode-map [(meta ?0)]        (lambda ()
					       (interactive)
					       (bookmark-set "10")))
(define-key view-mode-map [(meta ?1)]        (lambda ()
					       (interactive)
					       (bookmark-set "1")))
(define-key view-mode-map [(meta ?2)]        (lambda ()
					       (interactive)
					       (bookmark-set "2")))
(define-key view-mode-map [(meta ?3)]        (lambda ()
					       (interactive)
					       (bookmark-set "3")))
(define-key view-mode-map [(meta ?4)]        (lambda ()
					       (interactive)
					       (bookmark-set "4")))
(define-key view-mode-map [(meta ?5)]        (lambda ()
					       (interactive)
					       (bookmark-set "5")))
(define-key view-mode-map [(meta ?6)]        (lambda ()
					       (interactive)
					       (bookmark-set "6")))
(define-key view-mode-map [(meta ?7)]        (lambda ()
					       (interactive)
					       (bookmark-set "7")))
(define-key view-mode-map [(meta ?8)]        (lambda ()
					       (interactive)
					       (bookmark-set "8")))
(define-key view-mode-map [(meta ?9)]        (lambda ()
					       (interactive)
					       (bookmark-set "9")))
(define-key view-mode-map [(meta b)]         'brief-buf-list)
(define-key view-mode-map [(meta n)]         'brief-edit-next-buffer)
(define-key view-mode-map [(meta -)]         'brief-edit-previous-buffer)
(define-key view-mode-map [(control -)]      'brief-delete-curr-buffer)
(define-key view-mode-map [(meta e)]         'brief-edit-file)
(define-key view-mode-map [(f1)]             brief-change-window-map)
(define-key view-mode-map [(shift kp-up)]    'brief-change-window-0)
(define-key view-mode-map [(shift kp-right)] 'brief-change-window-1)
(define-key view-mode-map [(shift kp-down)]  'brief-change-window-2)
(define-key view-mode-map [(shift kp-left)]  'brief-change-window-3)
(define-key view-mode-map [(meta up)]        'brief-change-window-0)
(define-key view-mode-map [(meta right)]     'brief-change-window-1)
(define-key view-mode-map [(meta down)]      'brief-change-window-2)
(define-key view-mode-map [(meta left)]      'brief-change-window-3)
(define-key view-mode-map [(f3)]             brief-create-edge-map)
(define-key view-mode-map [(control t)]      'brief-to-top)
(define-key view-mode-map [(control b)]      'brief-to-bottom)
(define-key view-mode-map [(control c)]      'brief-center-line)
(define-key view-mode-map [(control u)]      'brief-screen-up)
(define-key view-mode-map [(control d)]      'brief-screen-down)
(define-key view-mode-map [(shift kp-home)]  'brief-left-side)
(define-key view-mode-map [(shift kp-end)]   'brief-right-side)
(define-key view-mode-map [(meta home)]      'brief-left-side)
(define-key view-mode-map [(meta end)]       'brief-right-side)
(define-key view-mode-map [(meta f2)]        'brief-zoom-window)
(define-key view-mode-map [(control z)]      'brief-zoom-window)
(define-key view-mode-map [(f4)]             brief-del-window-map)
(define-key view-mode-map [(control right)]  'brief-next-word)
(define-key view-mode-map [(control left)]   'brief-previous-word)
(define-key view-mode-map [(f5)]             'brief-search-fwd)
(define-key view-mode-map [(meta s)]         'brief-search-fwd)
(define-key view-mode-map [(meta f5)]        'brief-search-back)
(define-key view-mode-map [(shift f5)]       'brief-search-again)
(define-key view-mode-map [(f6)]             'brief-translate)
(define-key view-mode-map [(meta t)]         'brief-translate)
(define-key view-mode-map [(meta f6)]        'brief-translate-back)
(define-key view-mode-map [(shift f6)]       'brief-translate-again)
(define-key view-mode-map [(control f6)]     'brief-toggle-re)
(define-key view-mode-map [(control f5)]     'brief-change-case-fold-search)

;;   ***********************************************************************************
;;  ***                                                                               ***
;; **** Brief-Funktionen und Tastendefinitionen für den Minibuffer                    ****
;;  ***                                                                               ***
;;   ***********************************************************************************
(define-key minibuffer-local-map            [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map         [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
;;(define-key brief-mode-map                  [control pause] 'abort-recursive-edit)

;;   ***********************************************************************************
;;  ***                                                                               ***
;; **** Brief-Funktionen und Tastendefinitionen zur Cursornavigation und Markierungen ****
;;  ***                                                                               ***
;;   ***********************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [mouse-1]	           'brief-mouse-set-point)
(define-key brief-mode-map [double-mouse-1]	   'brief-mouse-set-point)
(define-key brief-mode-map [triple-mouse-1]	   'brief-mouse-set-point)
(define-key brief-mode-map [down-mouse-1]	   'brief-mouse-drag-region)
(define-key brief-mode-map [drag-mouse-1]	   'brief-mouse-set-region)
(define-key brief-mode-map [(meta l)]              'brief-switch-line-mark)
(define-key brief-mode-map [(meta c)]              'brief-switch-column-mark)
(define-key brief-mode-map [(meta d)]              'brief-delete-line)
(define-key brief-mode-map [(meta g)]              'goto-line)
(define-key brief-mode-map [(meta i)]              'overwrite-mode)
(define-key brief-mode-map [(meta j)]              'bookmark-jump)
(define-key brief-mode-map [(meta k)]              'kill-line)
(define-key brief-mode-map [(meta m)]              'brief-switch-horiz-mark)
(define-key brief-mode-map [(meta o)]              'write-file)
(define-key brief-mode-map [(meta u)]              'undo)
(define-key brief-mode-map [(meta v)]              'emacs-version)
(define-key brief-mode-map [(f10)]                 'execute-extended-command)
(define-key brief-mode-map [(meta x)]              'save-buffers-kill-emacs)
(define-key brief-mode-map [(meta z)]              'shell)
(define-key brief-mode-map [(shift end)]           'end-of-line)
(define-key brief-mode-map [(control home)]        (lambda ()
						     (interactive)
						     (move-to-window-line 0)))
(define-key brief-mode-map [(control end)]         (lambda ()
						     (interactive)
						     (move-to-window-line -1)))
(define-key brief-mode-map [(next)]                'scroll-up)
(define-key brief-mode-map [(prior)]               'scroll-down)
(define-key brief-mode-map [(control next)]        (lambda ()
						     (interactive)
						     (goto-char (point-min))))
(define-key brief-mode-map [(control prior)]       (lambda ()
						     (interactive)
						     (goto-char (point-max))))
(define-key brief-mode-map [(insert)]              'brief-yank-clipboard)
(define-key brief-mode-map [(kp-multiply)]         'undo)

(substitute-key-definition 'previous-line 'brief-up-row          brief-mode-map global-map)
(substitute-key-definition 'next-line     'brief-down-row        brief-mode-map global-map)
(substitute-key-definition 'forward-char  'brief-forward-column  brief-mode-map global-map)
(substitute-key-definition 'backward-char 'brief-backward-column brief-mode-map global-map)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-delete-line (lambda ()
			       (interactive)
			       (let ((old-whole-value kill-whole-line))
				 (setq kill-whole-line t)
				 (beginning-of-line)
				 (kill-line)
				 (setq kill-whole-line old-whole-value))))

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.

(defun brief-switch-line-mark (&optional off)
  "Diese Funktion setzt die Marke und wählt den Zeilen-Modus zum
   Kopieren, Ausschneiden und Einfügen vor."
  (interactive)
  (let ((position (point)))
    (if (or (and mark-active
       		 line-mode-active)
       	    off)
       	(progn
       	  (remove-hook 'post-command-hook 'show-line-hook)
       	  (if transient-mark-mode-was-active
       	      (transient-mark-mode t))
       	  (deactivate-mark)
       	  (remove-line-overlays)
       	  (setq line-mode-active nil))
      (if (and transient-mark-mode
	       (not rect-mode-active))
       	  (setq transient-mark-mode-was-active t)
       	(setq transient-mark-mode-was-active nil))
      (if rect-mode-active
	  (brief-switch-column-mark t))
      (transient-mark-mode nil)
      (set-mark-command ())
      (add-hook 'post-command-hook 'show-line-hook () t)
      (setq line-mode-active t))
    (goto-char position)))

(defun show-line-hook ()
  "Hook, der die selektierten Zeilen markiert."
  (remove-line-overlays)
  (let ((start (progn
		 (save-excursion
		   (if (< (region-beginning) (region-end))
		       (goto-char (region-beginning))
		     (goto-char (region-end)))
		   (beginning-of-line)
		   (point)))))
    (let ((end (progn
		 (save-excursion
		   (if (< (region-beginning) (region-end))
		       (goto-char (region-end))
		     (goto-char (region-beginning)))
		   (end-of-line)
		   (if (< (point) (point-max))
		       (forward-char 1))
		   (point)))))
      (let ((overlay-line (make-overlay start end)))
	(overlay-put overlay-line  'face 'region)
	(setq list-line-overlays (cons overlay-line
				       list-line-overlays))))))

(defun remove-line-overlays ()
  "Löscht alle markierten Bereiche im Zeilenmodus."
  (let ((i 0))
    (while (< i (length list-line-overlays))
      (delete-overlay (nth i list-line-overlays))
      (setq i (+ i 1))))
  (setq list-line-overlays nil))

(defun brief-switch-horiz-mark ()
  "Schaltet Markierung ein und setzt an die aktuelle Position die Startmarke. Die Markierung erfolgt zeilenorientiert."
  (interactive)
  (if (and mark-active
       	   (not line-mode-active)
       	   (not rect-mode-active))
      (deactivate-mark)
    (if rect-mode-active
       	(brief-switch-column-mark t))
    (if line-mode-active
       	(brief-switch-line-mark t))
    (set-mark-command ())))

(defun brief-switch-column-mark (&optional off)
  "Diese Funktion setzt die Marke und wählt den Rectangle-Modus zum
   Kopieren, Ausschneiden und Einfügen vor."
  (interactive)
  (let ((position (point)))
    (if (or (and mark-active
       		 rect-mode-active)
       	    off)
       	(progn
       	  (remove-hook 'post-command-hook 'show-rect-hook)
       	  (if transient-mark-mode-was-active
       	      (transient-mark-mode t))
	  (setq brief-clean-eof old-brief-clean-eof)
       	  (deactivate-mark)
       	  (remove-rect-overlays)
       	  (setq rect-mode-active nil))
      (if (and transient-mark-mode
	       (not rect-mode-active))
       	  (setq transient-mark-mode-was-active t)
       	(setq transient-mark-mode-was-active nil))
      (if line-mode-active
	  (brief-switch-line-mark t))
      (setq old-brief-clean-eof brief-clean-eof)
      (setq brief-clean-eof t)
      (transient-mark-mode nil)
      (set-mark-command ())
      (add-hook 'post-command-hook 'show-rect-hook () t)
      (setq rect-mode-active t))
    (goto-char position)))

(defun show-rect-hook ()
  "Hook, der das selektierte Rechteck markiert."
  (remove-rect-overlays)
  (let ((start (region-beginning))
       	(end (region-end)))
    (operate-on-rectangle 'show-rect start end t)))

(defun show-rect (start ignore ignore)
  "Hook, der das selektierte Rechteck in der entsprechenden Zeile markiert."
  (let ((overlay-rect (make-overlay start (point))))
    (overlay-put overlay-rect  'face 'region)
    (setq list-rect-overlays (cons overlay-rect
		 	       	   list-rect-overlays))))

(defun remove-rect-overlays ()
  "Löscht alle markierten Bereiche im Rechteckmodus."
  (let ((i 0))
    (while (< i (length list-rect-overlays))
      (delete-overlay (nth i list-rect-overlays))
      (setq i (+ i 1))))
  (setq list-rect-overlays nil))

(defun brief-mouse-set-point (event)
  (interactive "e")
  (if (and mark-active
    	   rect-mode-active)
      (brief-switch-column-mark t)
    (if mark-active
    	(brief-switch-horiz-mark)))
  (mouse-set-point event))

(defun brief-mouse-drag-region (event)
  (interactive "e")
  (if (and mark-active
    	   rect-mode-active)
      (brief-switch-column-mark t)
    (if mark-active
    	(brief-switch-horiz-mark)))
  (mouse-drag-region event))

(defun brief-mouse-set-region (event)
  (interactive "e")
  (if (and mark-active
    	   rect-mode-active)
      (brief-switch-column-mark t)
    (if mark-active
    	(brief-switch-horiz-mark)))
  (mouse-set-region event))

(defun brief-up-row (arg)
  "Bewegt den Cursor eine (bzw. arg) Zeilen nach unten und fügt Leerzeichen ein, wenn das Zeilenende
weiter links liegt."
  (interactive "p")
  (if (minibuffer-window-active-p (selected-window))
      (previous-history-element arg)
    (let ((old-buffer-read-only buffer-read-only)
	  deactivate-mark
          (old-buffer-modified-p (buffer-modified-p))
	  (current-column (current-column)))
      (if buffer-read-only (toggle-read-only))
      (save-excursion
	(beginning-of-line)
	(if (and (re-search-forward "\\([ \t][ \t]*$\\)\\|$" nil t)
		 (not brief-clean-eof))
	      (delete-region (match-beginning 0) (point))))
      (previous-line arg)
      (move-to-column-force current-column)
      (while (< (current-column) current-column)
	(insert-before-markers " "))
      (set-buffer-modified-p old-buffer-modified-p)
      (if buffer-read-only (toggle-read-only)))))

(defun brief-down-row (arg)
  "Bewegt den Cursor eine (bzw. arg) Zeilen nach unten und fügt Leerzeichen ein, wenn das Zeilenende
weiter links liegt."
  (interactive "p")
  (if (minibuffer-window-active-p (selected-window))
      (next-history-element arg)
    (let ((old-buffer-read-only buffer-read-only)
          (old-buffer-modified-p (buffer-modified-p))
	  deactivate-mark
	  (current-column (current-column)))
      (if buffer-read-only (toggle-read-only))
      (save-excursion
	(beginning-of-line)
	(if (and (re-search-backward "\\([ \t][ \t]*$\\)\\|$" nil t)
		 (not brief-clean-eof))
	    (delete-region (match-beginning 0) (point))))
      (forward-line arg)
      (move-to-column current-column)
      (while (< (current-column) current-column)
	(insert " "))
      (set-buffer-modified-p old-buffer-modified-p)
      (if buffer-read-only (toggle-read-only)))))

(defun brief-forward-column (arg)
  "Bewegt den Cursor eine (bzw. arg) Spalte(n) nach links und fügt ein Leerzeichen, wenn notwendig, hinzu."
  (interactive "p")
  (if (minibuffer-window-active-p (selected-window))
      (forward-char)
    (let ((old-buffer-read-only buffer-read-only)
	  deactivate-mark
          (old-buffer-modified-p (buffer-modified-p))
	  (i 0)
	  (current-column (current-column))
	  (max-column (progn
			(save-excursion
			  (end-of-line)
			  (current-column)))))
      (if buffer-read-only (toggle-read-only))
      (if (= current-column max-column)
	  (while (< i arg)
	    (insert " ")
	    (setq i (1+ i)))
	(forward-char 1))
      (set-buffer-modified-p old-buffer-modified-p)
      (if buffer-read-only (toggle-read-only)))))

(defun brief-backward-column (arg)
  "Bewegt den Cursor eine Spalte nach rechts und löscht ggf. Leerzeichen und Tabulatoren bis zum Zeilenende.
Die Leerzeichen und Tabulatoren bleiben erhalten, wenn die Variable `brief-clean-eof' nicht nil ist."
  (interactive "p")
  (backward-char arg)
  (save-excursion
    (if (and (re-search-forward "\\([ \t][ \t]*$\\)\\|$" nil t)
	     (not brief-clean-eof))
	(let ((old-buffer-read-only buffer-read-only)
	      deactivate-mark
	      (old-buffer-modified-p (buffer-modified-p)))
	  (if buffer-read-only (toggle-read-only))
	  (delete-region (match-beginning 0) (point))
	  (set-buffer-modified-p old-buffer-modified-p)
	  (if buffer-read-only (toggle-read-only))))))


;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** sonstige Brief-Funktionen und Tastendefinitionen                             ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map          [(delete)]            'delete-char)
(define-key brief-mode-map          [(control backspace)] 'brief-delete-previous-word)
(define-key brief-mode-map          [(meta backspace)]    'brief-delete-next-word)
(define-key brief-mode-map          [(meta w)]            'brief-write-buffer)
(define-key brief-mode-map          [(control x)]         'brief-write-and-exit)
(define-key brief-mode-map          [(control w)]         'brief-set-backup)
(define-key brief-mode-map          [(meta f)]            'brief-display-file-name)
(define-key brief-mode-map          [(control return)]    'brief-open-line)
(define-key brief-mode-map          [(shift tab)]         'brief-back-tab)
(define-key brief-mode-map          [(meta q)]            'brief-quote)
(define-key brief-mode-map          [(kp-add)]            'brief-copy)
(define-key brief-mode-map          [(kp-subtract)]       'brief-cut)
(define-key brief-mode-map          [(home)]              'brief-home)
(define-key brief-mode-map          [(end)]               'brief-end)
(define-key brief-mode-map          [(meta h)]            'brief-help)
(define-key brief-mode-map          [(control r)]         'brief-repeat)
(define-key brief-mode-map          [(control g)]         'brief-routines)
(define-key brief-mode-map          [(meta p)]            'print-region)
(define-key brief-mode-map          [(meta r)]            'insert-region)
;;(define-key brief-mode-map          [(backspace)]         'backward-delete-char-untabify)
;;(define-key brief-mode-map          [(return)]            'newline)
;;(define-key brief-mode-map          [(escape)]            'keyboard-escape-quit)
;;(define-key brief-mode-map          [(tab)]               'indent-for-tab-command)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.
;; load_keystroke_macro
;; delete_to_bol
;; key

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.

(defalias 'brief-write-and-exit       (lambda ()
					(interactive)
					(save-buffers-kill-emacs t)))

(defalias 'brief-delete-previous-word 'backward-kill-word)
(defalias 'brief-delete-next-word     'kill-word)
(defalias 'brief-display-file-name    (lambda ()
					(interactive)
					(message (buffer-file-name))))
(defalias 'brief-open-line            (lambda ()
					(interactive)
                                        (end-of-line)
					(insert "\n")))
(defalias 'brief-back-tab             (lambda ()
					(interactive)
					(move-to-left-margin)))
(defalias 'brief-quote                'quoted-insert)
(defalias 'brief-help                 'info)
(defalias 'brief-repeat               'repeat)
(defalias 'brief-routines             'imenu)

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.
(defun brief-home ()
  "Bewegt den Cursor an den Zeilen-, an den Seiten- und an das Dateianfang."
  (interactive nil)
  (cond
   ((and (eq last-command 'brief-home)
	 (eq brief-last-last-command 'brief-home))
    (goto-char (point-min)))
   ((eq last-command 'brief-home)
    (move-to-window-line 0))
   (t
    (beginning-of-line)))
  (setq brief-last-last-command last-command))

(defun brief-end ()
  "Bewegt den Cursor ans Zeilen-, ans Seiten- und ans Dateiende."
  (interactive nil)
  (let ((old-buffer-read-only buffer-read-only)
	deactivate-mark
	(old-buffer-modified-p (buffer-modified-p)))
    (if buffer-read-only (toggle-read-only))
    (save-excursion
      (beginning-of-line)
      (if (and (re-search-forward "\\([ \t][ \t]*$\\)\\|$" nil t)
	       (not brief-clean-eof))
	  (delete-region (match-beginning 0) (point))))
    (set-buffer-modified-p old-buffer-modified-p)
    (if buffer-read-only (toggle-read-only)))
  (cond
   ((and (eq last-command 'brief-end)
	 (eq brief-last-last-command 'brief-end))
    (goto-char (point-max)))
   ((eq last-command 'brief-end)
    (move-to-window-line -1)
    (end-of-line))
   (t
    (beginning-of-line)
    (end-of-line)))
  (setq brief-last-last-command last-command))

(defun brief-clean-spaces ()
  "Löscht alle Leerzeichen am Ende einer Zeile. Es sei denn, die Variable `brief-clean-eof' ist nicht nil"
  (interactive)
  (if (not brief-clean-eof)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "[ \t][ \t]*$" nil t)
	  (delete-region (match-beginning 0) (point))))))

(defun brief-write-buffer (arg)
  "Bevor der Buffer gespeichert wird, wird die Funktion `brief-clean-spaces'
aufgerufen."
  (interactive "p")
  (brief-clean-spaces)
  (save-buffer arg))

(defun brief-yank-clipboard ()
  "Fügt den Inhalt der Zwischenablage an der aktuellen Position ein"
  (interactive)
  (if (not brief-copy-line)
      (if rect-mode-active
       	  (yank-rectangle)
	(if line-mode-active
	    (beginning-of-line))
       	(clipboard-yank))
    (let ((spalte (current-column)))
      (beginning-of-line)
      (clipboard-yank)
      (move-to-column spalte))))

(defun brief-cut ()
  "Führt blockspezifisches Ausschneiden entsprechend Brief durch."
  (interactive)
  (if mark-active
      (progn
	(if rect-mode-active
	    (progn
	      (kill-rectangle (region-beginning) (region-end))
	      (brief-switch-column-mark)
	      (setq rect-mode-active t))
	  (if line-mode-active
	      (let ((start (progn
			     (save-excursion
			       (if (< (region-beginning) (region-end))
				   (goto-char (region-beginning))
				 (goto-char (region-end)))
			       (beginning-of-line)
			       (point)))))
		(let ((end (progn
			     (save-excursion
			       (if (< (region-beginning) (region-end))
				   (goto-char (region-end))
				 (goto-char (region-beginning)))
			       (end-of-line)
			       (if (< (point) (point-max))
			       	   (forward-char 1))
			       (point)))))
	    	  (kill-region start end)
		  (brief-switch-line-mark)
		  (setq line-mode-active t)))
	    (kill-region (region-beginning) (region-end))))
	(setq brief-copy-line nil))
    (save-excursion
      (let (beg)
	(setq brief-copy-line 1)
	(beginning-of-line)
	(setq beg (point))
	(end-of-line)
	(if (< (point) (point-max))
	    (forward-char 1))
	(kill-region beg (point))))))

(defun get-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun brief-copy ()
  "Führt blockspezifisches Kopieren entsprechend Brief durch."
  (interactive)
  (if mark-active
      (progn
       	(if rect-mode-active
       	    (let ((position (point))
       		  (col-beg (get-column (region-beginning)))
       		  (col-end (get-column (region-end)))
       		  (reg-beg (region-beginning))
       		  (reg-end (region-end)))
	      (if (< reg-beg reg-end)
		  (progn
		    (kill-rectangle (region-beginning) (region-end))
		    (goto-char reg-beg)
		    (if (< col-end col-beg)
			(move-to-column col-end)))
		(goto-char reg-end)
		(kill-rectangle (region-end) (region-beginning))
 		(if (< col-beg col-end)
 		    (move-to-column col-beg)))
	      (yank-rectangle)
	      (brief-switch-column-mark)
	      (goto-char position)
	      (setq rect-mode-active t))
	  (if line-mode-active
	      (let ((start (progn
			     (save-excursion
			       (if (< (region-beginning) (region-end))
				   (goto-char (region-beginning))
				 (goto-char (region-end)))
			       (beginning-of-line)
			       (point)))))
		(let ((end (progn
			     (save-excursion
			       (if (< (region-beginning) (region-end))
				   (goto-char (region-end))
				 (goto-char (region-beginning)))
			       (end-of-line)
			       (if (< (point) (point-max))
				   (forward-char 1))
			       (point)))))
		  (kill-ring-save start end)
		  (brief-switch-line-mark)
		  (setq line-mode-active t)))
	      (kill-ring-save (region-beginning) (region-end))))
	(setq brief-copy-line nil))
    (save-excursion
      (let (beg)
	(setq brief-copy-line 1)
	(beginning-of-line)
	(setq beg (point))
	(end-of-line)
	(if (< (point) (point-max))
	    (forward-char 1))
	(kill-ring-save beg (point))))))

(defun brief-set-backup ()
  (interactive)
  (setq make-backup-files (not make-backup-files))
  (if make-backup-files
      (message "Sicherungsdateien werden erstellt.")
    (message "Sicherungsdateien werden nicht erstellt.")))

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für das Lokalisieren von Fehlern     ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [(control n)] 'brief-next-error)
(define-key brief-mode-map [(control p)] 'brief-next-error-1)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-next-error 'next-error)
(defalias 'brief-next-error-1 'previous-error)


;; Brief-Funktionen, die im emacs nachgebildet werden müssen.

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für das Compilieren von Dateien      ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [(meta f10)] 'brief-compile-it)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.
;; warnings-only
;; bgd-compilation

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-compile-it 'compile)

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für Lesezeichen                      ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [(meta ?0)]      (lambda ()
					      (interactive)
					      (bookmark-set "10")))
(define-key brief-mode-map [(meta ?1)]      (lambda ()
					      (interactive)
					      (bookmark-set "1")))
(define-key brief-mode-map [(meta ?2)]      (lambda ()
					      (interactive)
					      (bookmark-set "2")))
(define-key brief-mode-map [(meta ?3)]      (lambda ()
					      (interactive)
					      (bookmark-set "3")))
(define-key brief-mode-map [(meta ?4)]      (lambda ()
					      (interactive)
					      (bookmark-set "4")))
(define-key brief-mode-map [(meta ?5)]      (lambda ()
					      (interactive)
					      (bookmark-set "5")))
(define-key brief-mode-map [(meta ?6)]      (lambda ()
					      (interactive)
					      (bookmark-set "6")))
(define-key brief-mode-map [(meta ?7)]      (lambda ()
					      (interactive)
					      (bookmark-set "7")))
(define-key brief-mode-map [(meta ?8)]      (lambda ()
					      (interactive)
					      (bookmark-set "8")))
(define-key brief-mode-map [(meta ?9)]      (lambda ()
					      (interactive)
					      (bookmark-set "9")))

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für das Manipulieren von Puffern     ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [(meta b)]    'brief-buf-list)
(define-key brief-mode-map [(meta n)]    'brief-edit-next-buffer)
(define-key brief-mode-map [(meta -)]    'brief-edit-previous-buffer)
(define-key brief-mode-map [(control -)] 'brief-delete-curr-buffer)
(define-key brief-mode-map [(meta e)]    'brief-edit-file)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-buf-list 'bs-show)
(defalias 'brief-edit-next-buffer 'bs-cycle-next)
(defalias 'brief-edit-previous-buffer 'bs-cycle-previous)
(defalias 'brief-edit-file 'find-file)
(defalias 'brief-delete-curr-buffer 'kill-buffer)

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für das Manipulieren von Fenstern    ****
;;;; ****                                                                              ****
;;;; **************************************************************************************
(defvar brief-zoom-window-activ () "Speichert die Window-Konfiguration, die bestand, bevor ein Fenster gezoomt wurde.")

;; Keymap-Definitionen:
(define-key brief-change-window-map [(up)]             'brief-change-window-0)
(define-key brief-change-window-map [(right)]          'brief-change-window-1)
(define-key brief-change-window-map [(down)]           'brief-change-window-2)
(define-key brief-change-window-map [(left)]           'brief-change-window-3)
(define-key brief-mode-map          [(f1)]             brief-change-window-map)
(define-key brief-mode-map          [(shift kp-up)]    'brief-change-window-0)
(define-key brief-mode-map          [(shift kp-right)] 'brief-change-window-1)
(define-key brief-mode-map          [(shift kp-down)]  'brief-change-window-2)
(define-key brief-mode-map          [(shift kp-left)]  'brief-change-window-3)
(define-key brief-mode-map          [(meta up)]        'brief-change-window-0)
(define-key brief-mode-map          [(meta right)]     'brief-change-window-1)
(define-key brief-mode-map          [(meta down)]      'brief-change-window-2)
(define-key brief-mode-map          [(meta left)]      'brief-change-window-3)
(define-key brief-create-edge-map   [(up)]             'brief-create-edge-0)
(define-key brief-create-edge-map   [(right)]          'brief-create-edge-1)
(define-key brief-create-edge-map   [(down)]           'brief-create-edge-2)
(define-key brief-create-edge-map   [(left)]           'brief-create-edge-3)
(define-key brief-mode-map          [(f3)]             brief-create-edge-map)
(define-key brief-mode-map          [(control t)]      'brief-to-top)
(define-key brief-mode-map          [(control b)]      'brief-to-bottom)
(define-key brief-mode-map          [(control c)]      'brief-center-line)
(define-key brief-mode-map          [(control u)]      'brief-screen-up)
(define-key brief-mode-map          [(control d)]      'brief-screen-down)
(define-key brief-mode-map          [(shift kp-home)]  'brief-left-side)
(define-key brief-mode-map          [(shift kp-end)]   'brief-right-side)
(define-key brief-mode-map          [(meta home)]      'brief-left-side)
(define-key brief-mode-map          [(meta end)]       'brief-right-side)
(define-key brief-mode-map          [(meta f2)]        'brief-zoom-window)
(define-key brief-mode-map          [(control z)]      'brief-zoom-window)
(define-key brief-del-window-map    [(up)]             'del-up-window)
(define-key brief-del-window-map    [(right)]          'del-right-window)
(define-key brief-del-window-map    [(down)]           'del-down-window)
(define-key brief-del-window-map    [(left)]           'del-left-window)
(define-key brief-mode-map          [(f4)]             brief-del-window-map)

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-create-edge-0 'split-window-vertically)
(defalias 'brief-create-edge-3 'split-window-horizontally)
(defalias 'brief-center-line   'recenter)
(defalias 'brief-left-side     'beginning-of-line)
(defalias 'brief-right-side    'end-of-line)

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.
(defun brief-zoom-window ()
  "Schaltet das Window-zoom ein bzw. aus."
  (interactive)
  (if (= (count-windows) 1)
      (if brief-zoom-window-activ
	  (progn
	    (set-window-configuration brief-zoom-window-activ)
	    (setq brief-zoom-window-activ ()))
	())
    (progn
      (setq brief-zoom-window-activ (current-window-configuration))
      (delete-other-windows))))

(defun brief-screen-up ()
  "Rollt den Bildschirm um eine Zeile nach oben, und laesst den Cursor in dieser Zeile (falls moeglich)."
  (interactive)
  (scroll-up 1))

(defun brief-screen-down ()
  "Rollt den Bildschirm um eine Zeile nach oben, und laesst den Cursor in dieser Zeile (falls moeglich)."
  (interactive)
  (scroll-down 1))

(defun brief-to-top ()
  "Bewegt die aktuelle Zeile an den Fensteranfang."
  (interactive)
  (recenter 0))

(defun brief-to-bottom ()
  "Bewegt die aktuelle Zeile an das Fensterende."
  (interactive)
  (recenter -1))

(defun brief-create-edge-1 ()
  "Teilt das aktuelle Fenster vertikal in zwei gleiche Haelften und setzt den Cursor in das rechte Fenster."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun brief-create-edge-2 ()
  "Teilt das aktuelle Fenster vertikal in zwei gleiche Haelften und setzt den Cursor in das untere Fenster."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun brief-change-window-0 ()
  "Springt in das Fenster über dem aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-oberes-ziel-fenster)
  (if ziel-fenster
      (select-window ziel-fenster)
    ())
  (message buffer-file-name))

(defun set-oberes-ziel-fenster (window)
  "Setzt die Variable für die Window-Navigation."
  (if (oberes-fenster window)
      (setq ziel-fenster window)
    ()))

(defun oberes-fenster (window)
  "Ermittelt ob sich das Fenster direckt über dem aktiven Fenster befindet."
  (let ((left1 (nth 0 (window-edges (selected-window))))
	(left2 (nth 0 (window-edges window)))
	(top1 (nth 1 (window-edges (selected-window))))
	(top2 (nth 1 (window-edges window)))
	(width1 (nth 2 (window-edges (selected-window))))
	(width2 (nth 2 (window-edges window)))
	(heigh1 (nth 3 (window-edges (selected-window))))
	(heigh2 (nth 3 (window-edges window))))
    (and (= heigh2 top1)
	 (<= left2 left1)
	 (< left1 width2))))

(defun brief-change-window-2 ()
  "Springt in das Fenster unter dem aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-unteres-ziel-fenster)
  (if ziel-fenster
      (select-window ziel-fenster)
    ())
  (message buffer-file-name))

(defun set-unteres-ziel-fenster (window)
  "Setzt die Variable für die Window-Navigation."
  (if (unteres-fenster window)
      (setq ziel-fenster window)
    ()))

(defun unteres-fenster (window)
  "Ermittelt ob sich das Fenster direckt unter dem aktiven Fenster befindet."
  (let ((left1 (nth 0 (window-edges (selected-window))))
	(left2 (nth 0 (window-edges window)))
	(top1 (nth 1 (window-edges (selected-window))))
	(top2 (nth 1 (window-edges window)))
	(width1 (nth 2 (window-edges (selected-window))))
	(width2 (nth 2 (window-edges window)))
	(heigh1 (nth 3 (window-edges (selected-window))))
	(heigh2 (nth 3 (window-edges window))))
    (and (= heigh1 top2)
	 (>= left1 left2)
	 (< left1 width2))))

(defun brief-change-window-1 ()
  "Springt in das Fenster rechts vom aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-rechtes-ziel-fenster)
  (if ziel-fenster
      (select-window ziel-fenster)
    ())
  (message buffer-file-name))

(defun set-rechtes-ziel-fenster (window)
  "Setzt die Variable für die Window-Navigation."
  (if (rechtes-fenster window)
      (setq ziel-fenster window)
    ()))

(defun rechtes-fenster (window)
  "Ermittelt ob sich das Fenster direckt rechts neben dem aktiven Fenster befindet."
  (let ((left1 (nth 0 (window-edges (selected-window))))
	(left2 (nth 0 (window-edges window)))
	(top1 (nth 1 (window-edges (selected-window))))
	(top2 (nth 1 (window-edges window)))
	(width1 (nth 2 (window-edges (selected-window))))
	(width2 (nth 2 (window-edges window)))
	(heigh1 (nth 3 (window-edges (selected-window))))
	(heigh2 (nth 3 (window-edges window))))
    (and (= width1 left2)
	 (>= top1 top2)
	 (< top1 heigh2))))

(defun brief-change-window-3 ()
  "Springt in das Fenster links vom aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-linkes-ziel-fenster)
  (if ziel-fenster
      (select-window ziel-fenster)
    ())
  (message buffer-file-name))

(defun set-linkes-ziel-fenster (window)
  "Setzt die Variable für die Window-Navigation."
  (if (linkes-fenster window)
      (setq ziel-fenster window)
    ()))

(defun linkes-fenster (window)
  "Ermittelt ob sich das Fenster direckt links neben dem aktiven Fenster befindet."
  (let ((left1 (nth 0 (window-edges (selected-window))))
	(left2 (nth 0 (window-edges window)))
	(top1 (nth 1 (window-edges (selected-window))))
	(top2 (nth 1 (window-edges window)))
	(width1 (nth 2 (window-edges (selected-window))))
	(width2 (nth 2 (window-edges window)))
	(heigh1 (nth 3 (window-edges (selected-window))))
	(heigh2 (nth 3 (window-edges window))))
    (and (= width2 left1)
	 (<= top2 top1)
	 (< top1 heigh2))))

(defun del-left-window ()
  "Löscht das Fenster linkss vom aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-linkes-ziel-fenster)
  (if ziel-fenster
      (delete-window ziel-fenster)
    ()))

(defun del-right-window ()
  "Löscht das Fenster rechts vom aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-rechtes-ziel-fenster)
  (if ziel-fenster
      (delete-window ziel-fenster)
    ()))

(defun del-up-window ()
  "Löscht das Fenster über dem aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-oberes-ziel-fenster)
  (if ziel-fenster
      (delete-window ziel-fenster)
    ()))

(defun del-down-window ()
  "Löscht das Fenster unter dem aktiven Fenster."
  (interactive)
  (setq ziel-fenster nil)
  (walk-windows 'set-unteres-ziel-fenster)
  (if ziel-fenster
      (delete-window ziel-fenster)
    ()))

;;;; **************************************************************************************
;;;; ****                                                                              ****
;;;; **** Brief-Funktionen und Tastendefinitionen für das Suchen und Wechseln im Emacs ****
;;;; ****                                                                              ****
;;;; **************************************************************************************

;; Keymap-Definitionen:
(define-key brief-mode-map [(control right)] 'brief-next-word)
(define-key brief-mode-map [(control left)]  'brief-previous-word)
(define-key brief-mode-map [(f5)]            'brief-search-fwd)
(define-key brief-mode-map [(meta s)]        'brief-search-fwd)
(define-key brief-mode-map [(meta f5)]       'brief-search-back)
(define-key brief-mode-map [(shift f5)]      'brief-search-again)
(define-key brief-mode-map [(f6)]            'brief-translate)
(define-key brief-mode-map [(meta t)]        'brief-translate)
(define-key brief-mode-map [(meta f6)]       'brief-translate-back)
(define-key brief-mode-map [(shift f6)]      'brief-translate-again)
(define-key brief-mode-map [(control f6)]    'brief-toggle-re)
(define-key brief-mode-map [(control f5)]    'brief-change-case-fold-search)

;; Brief-Funktionen, die im emacs noch nicht nachgebildet werden.
;; block_search
;; _marksrch

;; Alias-Namen für Brief-Funktionen, die im Emacs unter anderen Namen existent sind.
(defalias 'brief-next-word 'forward-word)
(defalias 'brief-previous-word 'backward-word)
(defalias 'brief-i-search 'isearch-forward)

;; Brief-Funktionen, die im emacs nachgebildet werden müssen.
(defun brief-change-case-fold-search ()
  "Schaltet bei der Suche zwischen \"Klein-/Großschreibung beachten ja/nein\" um."
  (interactive)
  (if case-fold-search
      (progn
	(setq case-fold-search ())
	(message "Groß-/Kleinschreibung wird beachtet."))
    (progn
      (setq case-fold-search t)
      (message "Groß-/Kleinschreibung wird ignoriert."))))

(defun brief-search-fwd ()
  "Der Benutzer muß eine Zeichenkette als Suchfolge eingeben. Diese wird in der Variablen `brief-search-string'
gespeichert. Wenn die Markierung aktiv ist, wird die Region als Vorgabe benutzt. Sonst wird die Variable
`brief-search-string' als Vorgabe verwendet. Danach wird sie im Quelltext vorwärts gesucht."
  (interactive)
  (let ((hook-bak post-command-hook))
    (if (not mark-active)
	(setq brief-search-string (read-from-minibuffer "suchen nach: " brief-search-string))
      (setq brief-search-string (read-from-minibuffer "suchen nach: "
						      (buffer-substring-no-properties
						       (region-beginning)
						       (region-end))))
      (deactivate-mark))
    (setq brief-search-direction "v")
    (brief-search-forward-run)
    (setq post-command-hook hook-bak)))

(defun brief-search-again ()
  "Sucht in der vorherigen Suchrichtung nach der vorherigen Suchfolge."
  (interactive)
  (if (string-equal "v" brief-search-direction)
      (brief-search-forward-run)
    (brief-search-backward-run)))

(defun brief-search-forward-run ()
  "Sucht vorwaerts nach der Zeichenkette, die in der Variablen `brief-search-string' gespesucheert ist."
  (if (< 0 (length brief-search-string))
      (search-forward brief-search-string)
    (signal 'error (list "Kein Suchstring vorhanden"))))

(defun brief-search-back ()
  "Der Benutzer muß eine Zeichenkette als Suchfolge eingeben. Diese wird in der Variablen `brief-search-string'
gespesucheert. Danach wird sie im Quelltext rückwärts gesucht. Wurde zuvor schon eine Suchfolge eingegeben,
so wird diese standardmäßig angezeigt."
  (interactive)
  (let ((hook-bak post-command-hook))
    (if (not mark-active)
	(setq brief-search-string (read-from-minibuffer "suchen nach: " brief-search-string))
      (setq brief-search-string (read-from-minibuffer "suchen nach: "
						      (buffer-substring-no-properties
						       (region-beginning)
						       (region-end))))
      (deactivate-mark))
    (setq brief-search-direction "r")
    (brief-search-backward-run)
    (setq post-command-hook hook-bak)))

(defun brief-search-backward-run ()
  "Sucht rueckwaerts nach der Zesucheenkette, die in der Variablen `brief-search-string' gespesucheert ist."
  (if (< 0 (length brief-search-string))
      (search-backward brief-search-string)
    (signal 'error (list "Kein Suchstring vorhanden"))))

(defun brief-toggle-re ()
  "Schaltet die regulären Ausdrücke beim Suchen und Ersetzen ein bzw. aus."
  (interactive)
  (setq brief-replace-regexp (not brief-replace-regexp))
  (if brief-replace-regexp
      (message "Regelausdrücke an.")
    (message "Regelausdrücke aus.")))

(defun brief-translate (from-string to-string &optional arg)
  "Diese Funktion wurde der replace.el entnommen und um den zweiten Parameter ergänzt sowie ins deutsche übersetzt.
Quelle: `query-replace'"
  (interactive (brief-query-replace-read-args "Folge" "Ersatz" nil))
  (setq brief-replace-direction "v")
  (brief-perform-replace from-string to-string nil t brief-replace-regexp arg))

(defun brief-translate-back (from-string to-string &optional arg)
  "Diese Funktion (`perform-replace') wurde der replace.el entnommen und um den zweiten Parameter ergänzt
sowie ins deutsche übersetzt. Quelle: `query-replace'"
  (interactive (brief-query-replace-read-args "Folge" "Ersatz" nil))
  (setq brief-replace-direction "r")
  (brief-perform-replace from-string to-string t t brief-replace-regexp arg))

(defun brief-translate-again ()
  "Diese Funktion wiederholt die letzte Suchen-Ersetzen-Operation"
  (interactive)
  (if (string= brief-replace-direction "v")
      (brief-translate brief-replace-oldstring brief-replace-newstring)
    (brief-translate-back brief-replace-oldstring brief-replace-newstring)))

(defun brief-query-replace-read-args (string1 string2 regexp-flag)
  "Diese Funktion (`query-replace-read-args') wurde der replace.el entnommen und um den zweiten Parameter ergänzt."
  (let (from to)
    (if query-replace-interactive
	(setq from (car (if regexp-flag regexp-search-ring search-ring)))
      (setq from (read-from-minibuffer (format "%s: " string1)
				       brief-replace-oldstring nil nil
				       query-replace-from-history-variable
				       nil t)))
    (setq brief-replace-oldstring from)
    (setq to (read-from-minibuffer (format "%s '%s' ---  %s : " string1 from string2)
				   brief-replace-newstring nil nil
				   query-replace-to-history-variable from t))
    (setq brief-replace-newstring to)
    (list from to current-prefix-arg)))

(defun brief-perform-replace (from-string replacements
					  backward
					  query-flag regexp-flag delimited-flag
					  &optional repeat-count map)
  "Diese Funktion (`perform-replace') wurde der replace.el entnommen und um den Parameter `backward' erweitert.
Ist dieser Parameter nil, so wird vorwärts gesucht, sonst rückwärts."
  (or map (setq map query-replace-map))
  (and query-flag minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let ((nocasify (not (and case-fold-search case-replace
			    (string-equal from-string
					  (downcase from-string)))))
	(case-fold-search (and case-fold-search
			       (string-equal from-string
					     (downcase from-string))))
	(literal (not regexp-flag))
	(search-function (if backward
			     (if regexp-flag 're-search-backward 'search-backward)
			   (if regexp-flag 're-search-forward 'search-forward)))
	(search-string from-string)
	(real-match-data nil)		; the match data for the current match
	(next-replacement nil)
	(replacement-index 0)
	(keep-going t)
	(stack nil)
	(next-rotate-count 0)
	(replace-count 0)
	(nonempty-match nil)

	;; If non-nil, it is marker saying where in the buffer to stop.
	(limit nil)

	;; Data for the next match.  If a cons, it has the same format as
	;; (match-data); otherwise it is t if a match is possible at point.
	(match-again t)

	(message
	 (if query-flag
	     (substitute-command-keys
	      "Query replacing %s with %s: (\\<query-replace-map>\\[help] for help) "))))

    ;; If region is active, in Transient Mark mode, operate on region.
    (if (and transient-mark-mode mark-active)
	(progn
	  (setq limit (copy-marker (region-end)))
	  (goto-char (region-beginning))
	  (deactivate-mark)))
    (if (stringp replacements)
	(setq next-replacement replacements)
      (or repeat-count (setq repeat-count 1)))
    (if delimited-flag
	(setq search-function 're-search-forward
	      search-string (concat "\\b"
				    (if regexp-flag from-string
				      (regexp-quote from-string))
				    "\\b")))
    (push-mark)
    (undo-boundary)
    (unwind-protect
	;; Loop finding occurrences that perhaps should be replaced.
	(while (and keep-going
		    (not (eobp))
		    ;; Use the next match if it is already known;
		    ;; otherwise, search for a match after moving forward
		    ;; one char if progress is required.
		    (setq real-match-data
			  (if (consp match-again)
			      (progn (goto-char (nth 1 match-again))
				     match-again)
			    (and (or match-again
				     (progn
				       (forward-char 1)
				       (not (eobp))))
				 (funcall search-function search-string limit t)
				 ;; For speed, use only integers and
				 ;; reuse the list used last time.
				 (match-data t real-match-data)))))

	  ;; Record whether the match is nonempty, to avoid an infinite loop
	  ;; repeatedly matching the same empty string.
	  (setq nonempty-match
		(/= (nth 0 real-match-data) (nth 1 real-match-data)))

	  ;; If the match is empty, record that the next one can't be adjacent.
	  ;; Otherwise, if matching a regular expression, do the next
	  ;; match now, since the replacement for this match may
	  ;; affect whether the next match is adjacent to this one.
	  (setq match-again
		(and nonempty-match
		     (or (not regexp-flag)
			 (and (looking-at search-string)
			      (match-data)))))

	  ;; If time for a change, advance to next replacement string.
	  (if (and (listp replacements)
		   (= next-rotate-count replace-count))
	      (progn
		(setq next-rotate-count
		      (+ next-rotate-count repeat-count))
		(setq next-replacement (nth replacement-index replacements))
		(setq replacement-index (% (1+ replacement-index) (length replacements)))))
	  (if (not query-flag)
	      (progn
		(set-match-data real-match-data)
		(replace-match next-replacement nocasify literal)
		(setq replace-count (1+ replace-count)))
	    (undo-boundary)
	    (let (done replaced key def)
	      ;; Loop reading commands until one of them sets done,
	      ;; which means it has finished handling this occurrence.
	      (while (not done)
		(set-match-data real-match-data)
		(replace-highlight (match-beginning 0) (match-end 0))
		;; Bind message-log-max so we don't fill up the message log
		;; with a bunch of identical messages.
		(let ((message-log-max nil))
		  (message message from-string next-replacement))
		(setq key (read-event))
		;; Necessary in case something happens during read-event
		;; that clobbers the match data.
		(set-match-data real-match-data)
		(setq key (vector key))
		(setq def (lookup-key map key))
		;; Restore the match data while we process the command.
		(cond ((eq def 'help)
		       (with-output-to-temp-buffer "*Help*"
			 (princ
			  (concat "Query replacing "
				  (if regexp-flag "regexp " "")
				  from-string " with "
				  next-replacement ".\n\n"
				  (substitute-command-keys
				   query-replace-help)))
			 (save-excursion
			   (set-buffer standard-output)
			   (help-mode))))
		      ((eq def 'exit)
		       (setq keep-going nil)
		       (setq done t))
		      ((eq def 'backup)
		       (if stack
			   (let ((elt (car stack)))
			     (goto-char (car elt))
			     (setq replaced (eq t (cdr elt)))
			     (or replaced
				 (set-match-data (cdr elt)))
			     (setq stack (cdr stack)))
			 (message "No previous match")
			 (ding 'no-terminate)
			 (sit-for 1)))
		      ((eq def 'act)
		       (or replaced
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (setq replace-count (1+ replace-count))))
		       (setq done t replaced t))
		      ((eq def 'act-and-exit)
		       (or replaced
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (setq replace-count (1+ replace-count))))
		       (setq keep-going nil)
		       (setq done t replaced t))
		      ((eq def 'act-and-show)
		       (if (not replaced)
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (setq replace-count (1+ replace-count))
			     (setq replaced t))))
		      ((eq def 'automatic)
		       (or replaced
			   (progn
			     (replace-match next-replacement nocasify literal)
			     (setq replace-count (1+ replace-count))))
		       (setq done t query-flag nil replaced t))
		      ((eq def 'skip)
		       (setq done t))
		      ((eq def 'recenter)
		       (recenter nil))
		      ((eq def 'edit)
		       (let ((opos (point-marker)))
			 (goto-char (match-beginning 0))
			 (save-excursion
			   (funcall search-function search-string limit t)
			   (setq real-match-data (match-data)))
			 (save-excursion (recursive-edit))
			 (goto-char opos))
		       (set-match-data real-match-data)
		       ;; Before we make the replacement,
		       ;; decide whether the search string
		       ;; can match again just after this match.
		       (if (and regexp-flag nonempty-match)
			   (setq match-again (and (looking-at search-string)
						  (match-data)))))
		      ((eq def 'delete-and-edit)
		       (delete-region (match-beginning 0) (match-end 0))
		       (set-match-data
			(prog1 (match-data)
			  (save-excursion (recursive-edit))))
		       (setq replaced t))
		      ;; Note: we do not need to treat `exit-prefix'
		      ;; specially here, since we reread
		      ;; any unrecognized character.
		      (t
		       (setq this-command 'mode-exited)
		       (setq keep-going nil)
		       (setq unread-command-events
			     (append (listify-key-sequence key)
				     unread-command-events))
		       (setq done t))))
	      ;; Record previous position for ^ when we move on.
	      ;; Change markers to numbers in the match data
	      ;; since lots of markers slow down editing.
	      (setq stack
		    (cons (cons (point)
				(or replaced (match-data t)))
			  stack)))))
      (replace-dehighlight))
    (or unread-command-events
	(message "Replaced %d occurrence%s"
		 replace-count
		 (if (= replace-count 1) "" "s")))
    (and keep-going stack)))

(defun brief-switch-keys-hook ()
  "Diese Funktion deaktiviert bestimmte Brief-Tasten, um nützliche Standard-Tastaturbindungen
zu erhalten. Außerdem schaltet diese Funktion die Option truncate-lines ein."
  (if (string= mode-name "Emacs-Lisp")
      (define-key brief-mode-map [(control x)] nil))
  (if (string= mode-name "cs31")
      ()
    (define-key brief-mode-map [(control c)] nil))
  (setq truncate-lines t))

(add-hook 'find-file-hooks 'brief-switch-keys-hook () t)

;;Nach dem öffnen der Datei wird der aktuelle Dateiname im Minibuffer angezeigt.
(add-hook 'find-file-hooks 'brief-display-file-name () t)

;;;###autoload
(defun brief-mode (&optional arg)
  "Toggle Brief emulation minor mode.
With ARG, turn Brief mode on if ARG is positive, off otherwise.
Folgende Tastenbindungen werden unterstützt:
Tastenanschlag		Befehl
Alt-b			Pufferliste
Ctrl-b			Zeile an Fensterende
Alt-C			Spaltenmarkierung
Ctrl-C			Zeile in Fenster zentrieren
Alt-D			Zeile löschen
Ctrl-D			Puffer nach unten schieben
Alt-E			Datei editieren
Alt-F			Dateinamen anzeigen
Alt-G			Nach Zeile
Alt-H			Hilfe
Alt-I			Einfügemodus ein/aus
Alt-J			Nach Lesezeichen
Alt-K			Bis Zeilenende löschen
Alt-L			Zeilenmarkierung
F1			Fenster wechseln
F3			Fenster erstellen
F4			Fenster löschen
Alt-F2			Zoomumschaltung
F5			Vorwärts suchen
F6                      Vorwärts wechseln
Shift-F5                Erneut suchen
Shift-F6                Eneut wechseln
Ctrl-F5			Groß-/Kleinschreibung berücksichtigen
Ctrl-F6			Regelausdruck ein/aus
Alt-F5			Rückwärts suchen
Alt-F6			Rückwärts wechseln
F7			Aufzeichnen beginnen/beenden
Alt-M			Markierung
Alt-N			Nächster Puffer
Ctrl-N			Nächster Fehler
Alt-O			Ausgabedatei wechseln
Alt-P			Block drucken
Ctrl-P			Fehlereinblendfenster
Alt-Q			Zitieren
Alt-R			Datei einlesen
Ctrl-R			Wiederholen
Alt-S			Vorwärts suchen
Alt-T			Vorwärts wechseln
Ctrl-T			Zeile an Fensteranfang
Alt-U			Rückgängig
Ctrl-U			Puffer nach oben
Alt-V			Versions-ID anzeigen
F8			Ablaufen
F10			Befehl ausführen
Alt-F10			Puffer kompilieren
Pos1			Zeilenanfang
Pos1 Pos1		Fensteranfang
Pos1 Pos1 Pos1		Pufferanfang
Ende			Zeilenende
Ende Ende		Fensterende
Ende Ende Ende		Pufferende
Alt-W			Schreiben
Ctrl-W			Sicherung ein/aus
Alt-X			Verlassen
Ctrl-X			Schreiben und verlassen
Alt-Z			BRIEF	unterbrechen
Ctrl-Z			Zoomumschaltung
Alt-Minus		Vorheriger Puffer
Ctrl-Minus		Puffer löschen
Rückschrittaste		Rückschritt
Ctrl-Rückschritt	Vorh. Wort löschen
Eingabetaste		Eingabe
Ctrl-Eingabe		Neue Zeile
Escape			Abbrechen
Tab			Tabulator
Shift-Tab		Rücktabulator
Shift-Ende		Rechte Fensterseite
Ctrl-Pos1		Fensteranfang
Ctrl-Ende		Fensterende
Bild ab			Bild ab
Bild auf		Bild auf
Ctrl-Bild ab		Pufferende
Ctrl-Bild auf		Pufferanfang
Pfeil unten		Unten
Pfeil links		Links
Pfeil rechts		Rechts
Pfeil oben		Oben
Ctrl-Rechts		Nächstes Wort
Ctrl-Links		Vorh. Wort
Alt-1			Lesezeichen 1 einfügen
Alt-2			Lesezeichen 2 einfügen
Alt-3			Lesezeichen 3 einfügen
.			.
.			.
Alt-0			Lesezeichen 10 einfügen
Lösch			Löschen
Einfg			Einsetzen (aus Zwischenabl.)
Graue Taste -		Ausschneiden (in Zwischenabl.)
Graue Taste +		Kopieren (in Zwischenabl.)
Graue Taste *		Rückgängig"
  (interactive "P")
  (setq brief-mode (if (null arg)
		       (not brief-mode)
		     (> (prefix-numeric-value arg) 0)))
  (if (not brief-mode)
      (setq scroll-step old-scroll-step)
    (setq old-scroll-step scroll-step)
    (setq scroll-step brief-scroll-step))
  (when brief-mode
    ;; Force transient-mark-mode, so that the marking routines work as
    ;; expected.  If the user turns off transient mark mode, most
    ;; things will still work fine except the brief-(copy|kill)
    ;; functions won't work quite as nicely when regions are marked
    ;; differently and could really confuse people.  Caveat emptor.
    (if (fboundp 'transient-mark-mode)
	  (transient-mark-mode t))
    (setq truncate-lines t)
    (run-hooks 'brief-mode-hook)))

(if (fboundp 'add-minor-mode)
    (add-minor-mode 'brief-mode 'brief-mode-modeline-string
		    brief-mode-map nil 'brief-mode)
  (or (assq 'brief-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(brief-mode brief-mode-modeline-string) minor-mode-alist)))
  (or (assq 'brief-mode minor-mode-map-alist)
       (setq minor-mode-map-alist (cons (cons 'brief-mode brief-mode-map)
				       minor-mode-map-alist))))

(run-hooks 'brief-load-hook)
(provide 'brief)
