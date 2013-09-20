# langtool.el

Grammar check utility using LanguageTool

## install:

Install LanguageTool (and java)
http://www.languagetool.org/

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

    (require 'langtool)
    (setq langtool-language-tool-jar "/path/to/LanguageTool.jar")

This setting is optional

    (global-set-key "\C-x4w" 'langtool-check)
    (global-set-key "\C-x4W" 'langtool-check-done)
    (global-set-key "\C-x4l" 'langtool-switch-default-language)
    (global-set-key "\C-x44" 'langtool-show-message-at-point)
    (global-set-key "\C-x4c" 'langtool-correct-buffer)

Currently GNU java version not works.

    (setq langtool-java-bin "/path/to/java")

If you want to specify your mother tongue.

    (setq langtool-mother-tongue "en")

## Usage:

* To check current buffer and show warnings.

    M-x langtool-check

* To correct marker follow LanguageTool suggestions.

    M-x langtool-correct-buffer

* Goto warning point and

    M-x langtool-show-message-at-point

* To finish checking. All marker is removed.

    M-x langtool-check-done
