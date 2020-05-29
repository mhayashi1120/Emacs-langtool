langtool.el
===========

## Install:

Install LanguageTool version 3.0 or later (and java)
https://languagetool.org/

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

```
(require 'langtool)
```

## Settings (required):

langtool.el have 3 types of client.

1. Command line

 This setting should be set, if you use rest of clients, to get full of
 completion support. And you should be set the variables before load
 this library.

```
(setq langtool-language-tool-jar "/path/to/languagetool-commandline.jar")
(require 'langtool)
```

Alternatively, you can set the classpath where LanguageTool's jars reside
(e.g. ArchLinux):

```
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(require 'langtool)
```


You can set a script that hold java setting (e.g. Gentoo):

```
(setq langtool-bin "/path/to/your/langtool")
(require 'langtool)
```

2. HTTP server & client

 You can use HTTP server implementation. This is very fast after listen server,
 but has security risk if there are multiple user on a same host.

```
(setq langtool-language-tool-server-jar "/path/to/languagetool-server.jar")
```

You can change HTTP server port number like following.

```
(setq langtool-server-user-arguments '("-p" "8082"))
```

3. HTTP client

If you have running HTTP LanguageTool server instance on any machine:

```
(setq langtool-http-server-host "localhost"
      langtool-http-server-port 8082)
```

Now testing although, that running instance is working under HTTPSServer or via
general ssl support (e.g. nginx) following may be working. Again, this is now
testing, so please open issue when the ssl/tls connection is not working.

```
(setq langtool-http-server-stream-type 'tls)
```

## Optional settings

* Key binding if you desired.

```
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)
```

* Default language is detected by LanguageTool automatically.
  Please set `langtool-default-language` if you need specific language.

```
(setq langtool-default-language "en-US")
```

  Otherwise, invoke `M-x langtool-check` with `C-u` (universal-argument)

* Currently GNU java version is not working.
  Please change the variable to your favorite java executable.

```
(setq langtool-java-bin "/path/to/java")
```

* Maybe your LanguageTool have launcher. (e.g. Gentoo)
  You need to set `langtool-bin`.
  See https://github.com/mhayashi1120/Emacs-langtool/issues/24

```
(setq langtool-bin "/usr/bin/languagetool")
```

* Maybe you want to specify your mother tongue.

```
(setq langtool-mother-tongue "en")
```

* To customize LanguageTool commandline arguments.

```
(setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
```

  You can also make the variable to buffer local like following:

```
(add-hook '**SOME**-mode-hook
          (lambda () (set (make-local-variable 'langtool-java-user-arguments)
                         '("-Dfile.encoding=UTF-8"))))
```

  NOTE: Although there is no good example, `langtool-user-arguments` is
  a similar custom variable.

## Usage:

* To check current buffer and show warnings.

```
M-x langtool-check
```

  Check with different language. You can complete supported language
  with C-i/TAB

```
C-u M-x langtool-check
```

* To correct marker follow LanguageTool suggestions.

```
M-x langtool-correct-buffer
```

* Go to warning point you can see a report from LanguageTool.
  Otherwise:

```
M-x langtool-show-message-at-point
```

* Show LanguageTool report automatically by `popup`
  This idea come from:
  https://laclefyoshi.hatenablog.com/entry/20150912/langtool_popup

```
(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))
```

```
(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)
```

* To finish checking. All langtool marker is removed.

```
M-x langtool-check-done
```

* To filter errors to display per major mode

For `org-mode`,
```
(eval-after-load 'org-mode
  '(progn
     (setq langtool-generic-check-predicate
           '(lambda (start end)
              ;; set up for `org-mode'
              (let* ((begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
                     (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
                     (case-fold-search t)
                     (ignored-font-faces '(org-verbatim
                                           org-block-begin-line
                                           org-meta-line
                                           org-tag
                                           org-link
                                           org-level-1
                                           org-document-info))
                     (rlt t)
                     ff
                     th
                     b e)
                (save-excursion
                  (goto-char start)

                  ;; get current font face
                  (setq ff (get-text-property start 'face))
                  (if (listp ff) (setq ff (car ff)))

                  ;; ignore certain errors by set rlt to nil
                  (cond
                   ((memq ff ignored-font-faces)
                    ;; check current font face
                    (setq rlt nil))
                   ((string-match "^ *- $" (buffer-substring (line-beginning-position) (+ start 2)))
                    ;; dash character of " - list item 1"
                    (setq rlt nil))
                   ((and (setq th (thing-at-point 'evil-WORD))
                         (or (string-match "^=[^=]*=[,.]?$" th)
                             (string-match "^\\[\\[" th)))
                    ;; embedded cde like =w3m= or org-link [[http://google.com][google]] or [[www.google.com]]
                    ;; langtool could finish checking before major mode prepare font face for all texts
                    (setq rlt nil))
                   (t
                    ;; inside source block?
                    (setq b (re-search-backward begin-regexp nil t))
                    (if b (setq e (re-search-forward end-regexp nil t)))
                    (if (and b e (< start e)) (setq rlt nil)))))
                ;; (if rlt (message "start=%s end=%s ff=%s" start end ff))
                rlt)))))
```

Only check text inside comment or string when coding,
```
(eval-after-load 'prog-mode
  '(progn
     (unless (featurep 'flyspell) (require 'flyspell))
     (setq langtool-generic-check-predicate
           '(lambda (start end)
              (let* ((f (get-text-property start 'face)))
                (memq f flyspell-prog-text-faces))))))
```
