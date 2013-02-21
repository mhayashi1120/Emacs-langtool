;;; langtool.el --- Grammer check utility using LanguageTool

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: docs
;; URL: http://github.com/mhayashi1120/Emacs-langtool/raw/master/langtool.el
;; Emacs: GNU Emacs 22 or later
;; Version: 1.2.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Install LanguageTool (and java)
;; http://www.languagetool.org/

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'langtool)
;;     (setq langtool-language-tool-jar "/path/to/LanguageTool.jar")

;; This setting is optional
;;
;;     (global-set-key "\C-x4w" 'langtool-check)
;;     (global-set-key "\C-x4W" 'langtool-check-done)
;;     (global-set-key "\C-x4l" 'langtool-switch-default-language)
;;     (global-set-key "\C-x44" 'langtool-show-message-at-point)
;;     (global-set-key "\C-x4c" 'langtool-correct-buffer)
;;
;; Currently GNU java version not works.
;;     (setq langtool-java-bin "/path/to/java")
;;
;; If you want to specify your mother tongue.
;;     (setq langtool-mother-tongue "en")

;;; Usage:

;; * To check current buffer and show warnings.
;;
;;  M-x langtool-check

;; * To correct marker follow LanguageTool suggestions.
;;
;;  M-x langtool-correct-buffer

;; * Goto warning point and
;;
;;  M-x langtool-show-message-at-point

;; * To finish checking. All marker is removed.
;;
;;  M-x langtool-check-done

;;; TODO:
;; * process coding system (test on Windows)
;; * check only docstring (emacs-lisp-mode)
;;    or using (derived-mode-p 'prog-mode) and only string and comment
;; * I don't know well about java. But GNU libgcj version not works..
;; * java encoding <-> elisp encoding

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'compile)

(defgroup langtool nil
  "Customize langtool"
  :prefix "langtool-"
  :group 'applications)

(defvar current-prefix-arg)
(defvar unread-command-events)
(defvar locale-language-names)

(defface langtool-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'langtool)

(defcustom langtool-java-bin "java"
  "*Executing java command."
  :group 'langtool
  :type 'file)

(defcustom langtool-language-tool-jar nil
  "*LanguageTool jar file."
  :group 'langtool
  :type 'file)

(defcustom langtool-default-language nil
  "*Language name pass to LanguageTool."
  :group 'langtool
  :type 'string)

(defcustom langtool-mother-tongue nil
  "*Your mothertongue Language name pass to LanguageTool."
  :group 'langtool
  :type 'string)

(defcustom langtool-disabled-rules nil
  "*Disabled rules pass to LanguageTool.
String that separated by comma or list of string.
"
  :group 'langtool
  :type '(choice
          (list string)
          string))

(defvar langtool-local-disabled-rules nil)
(make-variable-buffer-local 'langtool-local-disabled-rules)

(defvar langtool-temp-file nil)
(make-variable-buffer-local 'langtool-temp-file)

(defconst langtool-output-regexp
  (concat
   "^[0-9]+\\.) Line \\([0-9]+\\), column \\([0-9]+\\), Rule ID: \\(.*\\)\n"
   "Message: \\(.*\\)\n"
   "\\(?:Suggestion: \\(.*\\)\n\\)?"
   ;; As long as i can read
   ;; src/dev/de/danielnaber/languagetool/dev/wikipedia/OutputDumpHandler.java
   "\\(\\(?:.*\\)\n\\(?:[ ^]+\\)\\)\n"
    "\n?"                               ; last result have no new-line
   ))

(defvar langtool-buffer-process nil)
(make-variable-buffer-local 'langtool-buffer-process)

(defvar langtool-mode-line-message nil)
(make-variable-buffer-local 'langtool-mode-line-message)
(put 'langtool-mode-line-message 'risky-local-variable t)

(defvar langtool-error-buffer-name " *LanguageTool Errors* ")

(if (fboundp 'region-active-p)
    (defalias 'langtool-region-active-p 'region-active-p)
  (defun langtool-region-active-p ()
    (and transient-mark-mode mark-active)))

(defun langtool-goto-next-error ()
  "Obsoleted function. Should use `langtool-correct-buffer'.
Goto next error."
  (interactive)
  (let ((overlays (langtool-overlays-region (point) (point-max))))
    (langtool-goto-error
     overlays
     (lambda (ov) (< (point) (overlay-start ov))))))

(defun langtool-goto-previous-error ()
  "Obsoleted function. Should use `langtool-correct-buffer'.
Goto previous error."
  (interactive)
  (let ((overlays (langtool-overlays-region (point-min) (point))))
    (langtool-goto-error
     (reverse overlays)
     (lambda (ov) (< (overlay-end ov) (point))))))

(defun langtool-show-message-at-point ()
  "Show error details at point"
  (interactive)
  (let ((msgs (langtool-current-error-messages)))
    (if (null msgs)
        (message "No errors")
      (let ((buf (get-buffer-create langtool-error-buffer-name)))
        (with-current-buffer buf
          (erase-buffer)
          (mapc
           (lambda (msg) (insert msg "\n"))
           msgs))
        (save-window-excursion
          (display-buffer buf)
          (let* ((echo-keystrokes)
                 (event (read-event)))
            (setq unread-command-events (list event))))))))

(defun langtool-check-done ()
  "Finish LanguageTool process and cleanup existing colorized texts."
  (interactive)
  (when langtool-buffer-process
    (delete-process langtool-buffer-process))
  (kill-local-variable 'langtool-buffer-process)
  (kill-local-variable 'langtool-mode-line-message)
  (kill-local-variable 'langtool-local-disabled-rules)
  (langtool-clear-buffer-overlays)
  (message "Cleaned up LanguageTool."))

;;;###autoload
(defalias 'langtool-check 'langtool-check-buffer)

;;;###autoload
(defun langtool-check-buffer (&optional lang)
  "Check context current buffer and light up errors.
Optional \\[universal-argument] read LANG name.

You can change the `langtool-default-language' to apply all session.
Restrict to selection when region is activated.
"
  (interactive
   (when current-prefix-arg
     (list (langtool-read-lang-name))))
  (langtool-check-command)
  ;; probablly ok...
  (when (listp mode-line-process)
    (add-to-list 'mode-line-process '(t langtool-mode-line-message)))
  (let* ((file (buffer-file-name))
         (region-p (langtool-region-active-p))
         (begin (and region-p (region-beginning)))
         (finish (and region-p (region-end))))
    (unless langtool-temp-file
      (setq langtool-temp-file (make-temp-file "langtool-")))
    (when (or (null file) (buffer-modified-p) region-p)
      (save-restriction
        (widen)
        (let ((coding-system-for-write buffer-file-coding-system))
          (write-region begin finish langtool-temp-file nil 'no-msg))
        (setq file langtool-temp-file)))
    (langtool-clear-buffer-overlays)
    ;;TODO
    (when region-p
      (deactivate-mark))
    (let ((command langtool-java-bin)
          args)
      (setq args (list "-jar" (expand-file-name langtool-language-tool-jar)
                       "-c" (langtool-java-coding-system buffer-file-coding-system)
                       "-l" (or lang langtool-default-language)
                       "-d" (langtool-disabled-rules)))
      (when langtool-mother-tongue
        (setq args (append args (list "-m" langtool-mother-tongue))))
      (setq args (append args (list file)))
      (langtool--debug "Command" "%s" args)
      (let* ((buffer (langtool-process-create-buffer))
             (proc (apply 'start-process "LanguageTool" buffer command args)))
        (set-process-filter proc 'langtool-process-filter)
        (set-process-sentinel proc 'langtool-process-sentinel)
        (process-put proc 'langtool-source-buffer (current-buffer))
        (process-put proc 'langtool-region-begin begin)
        (process-put proc 'langtool-region-finish finish)
        (setq langtool-buffer-process proc)
        (setq langtool-mode-line-message
              (list " LanguageTool"
                    (propertize ":run" 'face compilation-info-face)))))))

;;;###autoload
(defun langtool-switch-default-language (lang)
  "Switch `langtool-read-lang-name' to LANG"
  (interactive (list (langtool-read-lang-name)))
  (setq langtool-default-language lang)
  (message "Now default language is `%s'" lang))

(defun langtool-correct-buffer ()
  "Execute interactive correction after `langtool-check'"
  (interactive)
  (let ((ovs (langtool-overlays-region (point-min) (point-max))))
    (if (null ovs)
        (message "No error found. %s"
                 (substitute-command-keys
                  (concat
                   "Type \\[langtool-check-done] to finish checking "
                   "or type \\[langtool-check] to re-check buffer")))
      (barf-if-buffer-read-only)
      (langtool--correction ovs))))

(defvar langtool--debug nil)
(defun langtool-toggle-debug ()
  "Toggle LanguageTool debugging."
  (interactive)
  (setq langtool--debug (not langtool--debug))
  (if langtool--debug
      (message "LanguageTool debug ON.")
    (message "LanguageTool debug off.")))

(defun langtool--debug (key fmt &rest args)
  (when langtool--debug
    (let ((buf (get-buffer-create "*LanguageTool Debug*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "---------- [" key "] ----------\n")
        (insert (apply 'format fmt args) "\n")))))

(defun langtool-goto-error (overlays predicate)
  (catch 'done
    (mapc
     (lambda (ov)
       (when (funcall predicate ov)
         (goto-char (overlay-start ov))
         (throw 'done t)))
     overlays)
    nil))

(defun langtool-read-lang-name ()
  (completing-read "Lang: "
                   (or (mapcar 'list (langtool-available-languages))
                       locale-language-names)))

(defun langtool-create-overlay (tuple)
  (let ((line (nth 0 tuple))
        (col (nth 1 tuple))
        (len (nth 2 tuple))
        (sugs (nth 3 tuple))
        (msg (nth 4 tuple))
        (message (nth 5 tuple))
        (rule-id (nth 6 tuple))
        (context (nth 7 tuple)))
    (goto-char (point-min))
    (forward-line (1- line))
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      ;;  1. sketchy move to column that is indicated by LanguageTool.
      ;;  2. fuzzy match to reported sentence which indicated by ^^^ like string.
      (move-to-column col)
      (destructuring-bind (start . end)
          (langtool--fuzzy-search context len)
        (let ((ov (make-overlay start end)))
          (overlay-put ov 'langtool-simple-message msg)
          (overlay-put ov 'langtool-message message)
          (overlay-put ov 'langtool-suggestions sugs)
          (overlay-put ov 'langtool-rule-id rule-id)
          (overlay-put ov 'priority 1)
          (overlay-put ov 'face 'langtool-errline))))))

;;FIXME
;;http://sourceforge.net/tracker/?func=detail&aid=3054895&group_id=110216&atid=655717
(defun langtool--fuzzy-search (context-regexp length)
  (let* ((regexp (concat ".*?" context-regexp))
         (default (cons (point) (+ (point) length))))
    (or (and (null regexp)
             (cons (point) (+ (point) length)))
        (and (looking-at regexp)
             (cons (match-beginning 1) (match-end 1)))
        (let ((beg (min (line-beginning-position) (- (point) 20))))
          (loop while (and (not (bobp))
                           (<= beg (point)))
                ;; backward just sentence length to search sentence after point
                do (condition-case nil
                       (backward-char length)
                     (beginning-of-buffer nil))
                if (looking-at regexp)
                return (cons (match-beginning 1) (match-end 1))))
        default)))

(defun langtool-current-error-messages ()
  (remove nil
          (mapcar
           (lambda (ov)
             (overlay-get ov 'langtool-message))
           (overlays-at (point)))))

(defun langtool-clear-buffer-overlays ()
  (mapc
   (lambda (ov)
     (delete-overlay ov))
   (langtool-overlays-region (point-min) (point-max))))

(defun langtool-overlays-region (start end)
  (sort
   (remove
    nil
    (mapcar
     (lambda (ov)
       (when (overlay-get ov 'langtool-message)
         ov))
     (overlays-in start end)))
   (lambda (ov1 ov2)
     (< (overlay-start ov1) (overlay-start ov2)))))

(defun langtool-check-command ()
  (when (or (null langtool-java-bin)
            (not (executable-find langtool-java-bin)))
    (error "java command is not found"))
  (when (or (null langtool-language-tool-jar)
            (not (file-readable-p langtool-language-tool-jar)))
    (error "langtool jar file is not found"))
  (when langtool-buffer-process
    (error "Another process is running")))

(defun langtool-disabled-rules ()
  (let ((custom langtool-disabled-rules)
        (locals langtool-local-disabled-rules))
    (cond
     ((stringp custom)
      (mapconcat 'identity
                 (cons custom locals)
                 ","))
     (t
      (mapconcat 'identity
                 (append custom locals)
                 ",")))))

(defun langtool-process-create-buffer ()
  (generate-new-buffer " *LanguageTool* "))

(defun langtool-process-filter (proc event)
  (langtool--debug "Filter" "%s" event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)
    (let ((min (or (process-get proc 'langtool-process-done)
                   (point-min)))
          (buffer (process-get proc 'langtool-source-buffer))
          (begin (process-get proc 'langtool-region-begin))
          (finish (process-get proc 'langtool-region-finish))
          n-tuple)
      (goto-char min)
      (while (re-search-forward langtool-output-regexp nil t)
        (let* ((line (string-to-number (match-string 1)))
               (column (1- (string-to-number (match-string 2))))
               (rule-id (match-string 3))
               (suggest (match-string 5))
               (msg1 (match-string 4))
               ;; rest of line. Point the raw message.
               (msg2 (match-string 6))
               (message
                (concat "Rule ID: " rule-id "\n"
                        msg1 "\n\n"
                        msg2))
               (suggestions (and suggest (split-string suggest "; ")))
               (context (langtool--pointed-context-regexp msg2))
               (len (langtool--pointed-length msg2)))
          (setq n-tuple (cons
                         (list line column len suggestions
                               msg1 message rule-id context)
                         n-tuple))))
      (process-put proc 'langtool-process-done (point))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (when (and begin finish)
                (narrow-to-region begin finish))
              (mapc
               (lambda (tuple)
                 (langtool-create-overlay tuple))
               (nreverse n-tuple)))))))))

;;FIXME sometimes LanguageTool says wrong column.
(defun langtool--pointed-context-regexp (message)
  (when (string-match "\\(.*\\)\n\\( *\\)\\(\\^+\\)" message)
    (let* ((msg1 (match-string 1 message))
           (pre (length (match-string 2 message)))
           (len (length (match-string 3 message)))
           (end (+ pre len))
           (sentence (substring msg1 pre end))
           (regexp (cond
                    ((string-match "^[[:space:]]+$" sentence)
                     ;; invalid sentence only have whitespace,
                     ;; search with around sentence.
                     (concat
                      "\\("
                      (let* ((count (length sentence))
                             (spaces (format "[[:space:]\n]\\{%d\\}" count)))
                        spaces)
                      "\\)"
                      ;; considered truncated spaces that is caused by
                      ;; `langtool--sentence-to-fuzzy'
                      "[[:space:]]*?"
                      ;; to match the correct block
                      ;; suffix of invalid spaces.
                      (langtool--sentence-to-fuzzy
                       (let ((from (min end (length msg1))))
                         ;;TODO magic number.
                         (substring msg1 from (min (length msg1) (+ from 20)))))))
                    (t
                     (concat "\\("
                             (langtool--sentence-to-fuzzy sentence)
                             "\\)")))))
      regexp)))

(defun langtool--sentence-to-fuzzy (sentence)
  (mapconcat 'regexp-quote
             ;; this sentence is reported by LanguageTool
             (split-string sentence " +")
             ;; LanguageTool interpreted newline as space.
             "[[:space:]\n]+?"))

(defun langtool--pointed-length (message)
  (or
   (and (string-match "\n\\( *\\)\\(\\^+\\)" message)
        (length (match-string 2 message)))
   ;; never through here, but if return nil from this function make stop everything.
   1))

(defun langtool-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((source (process-get proc 'langtool-source-buffer))
          (code (process-exit-status proc))
          dead marks msg face)
      (when (/= code 0)
        (setq face compilation-error-face))
      (cond
       ((buffer-live-p source)
        (with-current-buffer source
          (setq marks (langtool-overlays-region (point-min) (point-max)))
          (setq face (if marks compilation-info-face compilation-warning-face))
          (setq langtool-buffer-process nil)
          (setq langtool-mode-line-message
                (list " LanguageTool"
                      (propertize ":exit" 'face face)))))
       (t (setq dead t)))
      (cond
       (dead)
       ((/= code 0)
        (message "LanguageTool finished with code %d" code))
       (marks
        (message "%s"
                 (substitute-command-keys
                  "Type \\[langtool-correct-buffer] to correct buffer.")))
       (t
        (message "LanguageTool successfully finished with no error."))))
    (let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun langtool-available-languages ()
  (when (stringp langtool-language-tool-jar)
    (let ((dir (expand-file-name "rules" (file-name-directory langtool-language-tool-jar))))
      (when (file-directory-p dir)
        (remove nil
                (mapcar
                 (lambda (f)
                   (when (file-directory-p f)
                     (file-name-nondirectory f)))
                 (directory-files dir t "^[^.].$")))))))

;;FIXME
;; http://java.sun.com/j2se/1.5.0/ja/docs/ja/guide/intl/encoding.doc.html
(defun langtool-java-coding-system (coding-system)
  (let* ((cs (coding-system-base coding-system))
         (csname (symbol-name cs))
         (aliases (langtool-coding-system-aliases cs))
         (names (mapcar 'symbol-name aliases))
         tmp)
    (cond
     ((string-match "utf-8" csname)
      "utf8")
     ((or (string-match "euc.*jp" csname)
          (string-match "japanese-iso-.*8bit" csname))
      "eucjp")
     ((string-match "shift.jis" csname)
      "sjis")
     ((string-match "iso.*2022.*jp" csname)
      "iso2022jp")
     ((setq tmp
            (loop for x in names
                  if (string-match "iso-8859-\\([0-9]+\\)" x)
                  return x))
      (concat "ISO8859_" (match-string 1 tmp)))
     ((memq cs '(us-ascii raw-text undecided no-conversion))
      "ascii")
     ((memq cs '(cyrillic-koi8))
      "koi8-r")
     ((setq tmp
            (loop for x in names
                  if (string-match "^windows-[0-9]+$" x)
                  return x))
      tmp)
     (t
      ;; default guessed as ascii
      "ascii"))))

(defun langtool-coding-system-aliases (coding-system)
  (if (fboundp 'coding-system-aliases)
      ;; deceive elint
      (funcall 'coding-system-aliases coding-system)
    (coding-system-get coding-system 'alias-coding-systems)))

(defun langtool--correction (overlays)
  (let ((conf (current-window-configuration)))
    (unwind-protect
        (let ((next (car overlays)))
          (while (setq next (langtool--correction-loop next overlays))))
      (langtool--expire-buffer-overlays)
      (set-window-configuration conf)
      (kill-buffer (langtool--correction-buffer)))))

(defun langtool--correction-loop (ov overlays)
  (let* ((suggests (overlay-get ov 'langtool-suggestions))
         (msg (overlay-get ov 'langtool-simple-message))
         (alist (langtool--correction-popup msg suggests)))
    (catch 'next
      (while (progn
               (goto-char (overlay-start ov))
               (let (message-log-max)
                 (message (concat "C-h or ? for more options; "
                                  "SPC to leave unchanged, "
                                  "Digit to replace word")))
               (let* ((echo-keystrokes) ; suppress echoing
                      (c (downcase (read-char)))
                      (pair (assq c alist)))
                 (cond
                  (pair
                   (let ((sug (nth 1 pair)))
                     ;;TODO when region contains newline.
                     ;; -> insert newline after suggestion.
                     (delete-region (overlay-start ov) (overlay-end ov))
                     (insert sug)
                     (langtool--erase-overlay ov))
                   nil)
                  ((memq c '(?q))
                   (keyboard-quit))
                  ((memq c '(?c))
                   (langtool--erase-overlay ov)
                   nil)
                  ((memq c '(?e))
                   (message (substitute-command-keys
                             "Type \\[exit-recursive-edit] to finish the edit."))
                   (recursive-edit)
                   ;; stay current cursor and wait next user command.
                   (throw 'next ov))
                  ((memq c '(?i))
                   (let ((rule (overlay-get ov 'langtool-rule-id)))
                     (unless (member rule langtool-local-disabled-rules)
                       (setq langtool-local-disabled-rules
                             (cons rule langtool-local-disabled-rules)))
                     (langtool--ignore-rule rule overlays))
                   nil)
                  ((memq c '(?\C-h ?\?))
                   (langtool--correction-help)
                   t)
                  ((memq c '(?\d))
                   (throw 'next (langtool--prev-overlay ov overlays)))
                  ((memq c '(?\s)) nil)
                  (t (ding) t)))))
      ;; next item
      (langtool--next-overlay ov overlays))))

(defun langtool--expire-buffer-overlays ()
  (mapc
   (lambda (o)
     (unless (overlay-get o 'face)
       (delete-overlay o)))
   (langtool-overlays-region (point-min) (point-max))))

(defun langtool--ignore-rule (rule overlays)
  (loop for ov in overlays
        do (let ((r (overlay-get ov 'langtool-rule-id)))
             (when (equal r rule)
               (langtool--erase-overlay ov)))))

(defun langtool--erase-overlay (ov)
  (overlay-put ov 'face nil))

(defun langtool--next-overlay (current overlays)
  (loop for o in (cdr (memq current overlays))
        if (overlay-get o 'face)
        return o))

(defun langtool--prev-overlay (current overlays)
  (loop for o in (cdr (memq current (reverse overlays)))
        if (overlay-get o 'face)
        return o))

(defvar langtool--correction-keys
  [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9])

(defface langtool-correction-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "red1" :foreground "yellow" :bold t)))
  "Face used to visualize correction."
  :group 'langtool)

(defun langtool--correction-popup (msg suggests)
  (let ((buf (langtool--correction-buffer)))
    (delete-other-windows)
    (let ((win (split-window)))
      (set-window-buffer win buf))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert msg "\n\n")
        (loop for s in suggests
              for c across langtool--correction-keys
              do (progn
                   (insert "(" c ") ")
                   (let ((start (point)))
                     (insert s)
                     ;; colorize suggestion.
                     ;; suggestion may contains whitespace.
                     (let ((ov (make-overlay start (point))))
                       (overlay-put ov 'face 'langtool-correction-face)))
                   (insert "\n"))
              collect (list c s))))))

(defun langtool--correction-help ()
  (let ((help-1 "[q/Q]uit correction; [c/C]lear the colorized text; ")
        (help-2 "[i/I]gnore the rule over current session.")
        (help-3 "[e/E]dit the buffer manually")
        (help-4 "SPC skip; DEL move backward;")
        )
    (save-window-excursion
      (unwind-protect
          (let ((resize-mini-windows 'grow-only))
            (select-window (minibuffer-window))
            (erase-buffer)
            (message nil)
            ;;(set-minibuffer-window (selected-window))
            (enlarge-window 2)
            (insert (concat help-1 "\n" help-2 "\n" help-3 "\n" help-4))
            (sit-for 5))
        (erase-buffer)))))

(defun langtool--correction-buffer ()
  (get-buffer-create "*Langtool Correction*"))

;; initialize custom variables guessed from environment.
(let ((env (or (getenv "LANG")
               (getenv "LC_ALL")))
      lang mt)
  (and env
       (string-match "^\\(..\\)_" env)
       (setq lang (downcase (match-string 1 env)))
       (member lang (langtool-available-languages))
       (setq mt lang))
  (unless langtool-mother-tongue
    (setq langtool-mother-tongue mt))
  (unless langtool-default-language
    (setq langtool-default-language (or mt "en"))))

(provide 'langtool)

;;; langtool.el ends here
