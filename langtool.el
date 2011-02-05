;;; langtool --- Grammer check utility using LanguageTool

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: grammer checker java
;; URL: http://github.com/mhayashi1120/Emacs-langtool/raw/master/langtool.el
;; URL: http://www.emacswiki.org/emacs/download/langtool.el
;; Emacs: GNU Emacs 22 or later

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
;;     (global-set-key "\C-x4w" 'langtool-check-buffer)
;;     (global-set-key "\C-x4W" 'langtool-check-done)
;;     (global-set-key "\C-x4n" 'langtool-goto-next-error)
;;     (global-set-key "\C-x4p" 'langtool-goto-previous-error)
;;     (global-set-key "\C-x44" 'langtool-show-message-at-point)
;;
;; Currently GNU java version not works.
;;     (setq langtool-java-bin "/path/to/java")

;;; Usage:

;; * To check current buffer and show warnings.
;;
;;  M-x langtool-check-buffer
;;
;; * Goto warning point and
;;
;;  M-x langtool-show-message-at-point

;; * To finish checking.
;;
;;  M-x langtool-check-done

;;; TODO:
;; * check only docstring (emacs-lisp-mode)
;;    or using (derived-mode-p 'prog-mode) and only string and comment
;; * I don't know well about java. But GNU libgcj version not works..

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'flymake)

(defgroup langtool nil
  "Customize langtool"
  :group 'applications)

(defvar current-prefix-arg)
(defvar unread-command-events)
(defvar locale-language-names)

(defcustom langtool-java-bin "java"
  "*Executing java command."
  :group 'langtool
  :type 'file)

(defcustom langtool-language-tool-jar nil
  "*LanguageTool jar file."
  :group 'langtool
  :type 'file)

(defcustom langtool-default-language "en"
  "*Language name pass to LanguageTool."
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

(defvar langtool-temp-file nil)
(make-variable-buffer-local 'langtool-temp-file)

(defconst langtool-output-regexp 
  (concat
   "^[0-9]+\\.) Line \\([0-9]+\\), column \\([0-9]+\\), Rule ID: \\(.*\\)\n"
   "Message: \\(.*\\)\n"
   "Suggestion: \\(\\(?:.*\\)\n\\(?:.*\\)\n\\(?:.*\\)\\)\n"
    "\n?"
   ))

(defvar langtool-buffer-process nil)
(make-variable-buffer-local 'langtool-buffer-process)

(defvar langtool-mode-line-process 
  '(langtool-buffer-process " LanguageTool running..."))

(defun langtool-goto-next-error ()
  "Goto next error."
  (interactive)
  (let ((overlays (langtool-overlays-region (point) (point-max))))
    (langtool-goto-error 
     overlays
     (lambda (ov) (< (point) (overlay-start ov))))))

(defun langtool-goto-previous-error ()
  "Goto previous error."
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
  "Finish LanguageTool process and cleanup existing overlays."
  (interactive)
  (when langtool-buffer-process
    (delete-process langtool-buffer-process))
  (langtool-clear-buffer-overlays)
  (message "Cleaned up LanguageTool."))

(defun langtool-check-buffer (&optional lang)
  "Check context current buffer.
Optional \\[universal-argument] read LANG name."
  (interactive
   (when current-prefix-arg
     (list (langtool-read-lang-name))))
  (langtool-check-command)
  (add-to-list 'mode-line-process langtool-mode-line-process)
  (let ((file (buffer-file-name)))
    (unless langtool-temp-file
      (setq langtool-temp-file (make-temp-file "langtool-")))
    (when (or (null file) (buffer-modified-p))
      (save-restriction
        (widen)
        (let ((coding-system-for-write buffer-file-coding-system))
          (write-region (point-min) (point-max) langtool-temp-file nil 'no-msg))
        (setq file langtool-temp-file)))
    (langtool-clear-buffer-overlays)
    (let ((command langtool-java-bin)
          args)
      (setq args (list "-jar" (expand-file-name langtool-language-tool-jar)
                       "-c" (langtool-java-coding-system buffer-file-coding-system)
                       "-l" (or lang langtool-default-language)
                       "-d" (langtool-disabled-rules)
                       file))
      (let* ((buffer (langtool-process-create-buffer))
             (proc (apply 'start-process "LanguageTool" buffer command args)))
        (set-process-filter proc 'langtool-process-filter)
        (set-process-sentinel proc 'langtool-process-sentinel)
        (process-put proc 'langtool-source-buffer (current-buffer))
        (setq langtool-buffer-process proc)))))

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
  (completing-read "Lang: " locale-language-names))

(defun langtool-create-overlay (line column message)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (forward-line (1- line))
          (let ((start (line-beginning-position))
                (end (line-end-position)))
            (move-to-column column)
            (backward-word)
            ;;FIXME LanguageTool column sometimes wrong!
            ;; restrict to current line
            (setq start (min end (max start (point))))
            (forward-word 2)
            (setq end (min end (point)))
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'langtool-message message)
              (overlay-put ov 'priority 1)
              (overlay-put ov 'face 'flymake-errline))))
      ;;TODO ignore?
      (end-of-buffer nil))))

(defvar langtool-error-buffer-name " *LanguageTool Errors* ")
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
  (cond
   ((stringp langtool-disabled-rules)
    langtool-disabled-rules)
   ((consp langtool-disabled-rules)
    (mapconcat 'identity langtool-disabled-rules ","))
   (t
    "")))

(defun langtool-process-create-buffer ()
  (generate-new-buffer " *LanguageTool* "))

(defun langtool-process-filter (proc event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert event)
    (let ((min (or (process-get proc 'langtool-process-done)
                   (point-min)))
          (buffer (process-get proc 'langtool-source-buffer))
          messages)
      (goto-char min)
      (while (re-search-forward langtool-output-regexp nil t)
        (let ((line (string-to-number (match-string 1)))
              (column (string-to-number (match-string 2)))
              (message
               (concat (match-string 3) "\n" 
                       (match-string 4) (match-string 5))))
          (setq messages (cons
                          (list line column message)
                          messages))))
      (process-put proc 'langtool-process-done (point))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (mapc
           (lambda (msg)
             (let ((line (nth 0 msg))
                   (col (nth 1 msg))
                   (message (nth 2 msg)))
               (langtool-create-overlay line col message)))
           messages))))))

(defun langtool-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((source (process-get proc 'langtool-source-buffer)))
      (when (buffer-live-p source)
        (with-current-buffer source
          (setq langtool-buffer-process nil))))
    (unless (= (process-exit-status proc) 0)
      (message "LanguageTool finished with code %d" 
               (process-exit-status proc)))
    (let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;TODO
(defun langtool-java-coding-system (coding-system)
  (let ((cs (coding-system-base coding-system)))
    (case cs
      (utf-8 "utf-8")
      (euc-jp "euc-jp")
      (shift_jis "sjis")
      (iso-2022-7bit "iso2022jp")
      (t "ascii"))))

(provide 'langtool)

;;; langtool.el ends here
