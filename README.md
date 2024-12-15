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

Or use Melpa (https://melpa.org/)

## NOTE (2023-01-25)

Confirmed working on following environment

### Java

`java --version`

> openjdk 17.0.4 2022-07-19  
> OpenJDK Runtime Environment (build 17.0.4+8-Debian-1deb11u1)  
> OpenJDK 64-Bit Server VM (build 17.0.4+8-Debian-1deb11u1, mixed mode, sharing)

### LanguageTool

Can be downloaded from [here](https://languagetool.org/download/)

`java -jar languagetool-commandline.jar --version`

> LanguageTool version 6.0 (2022-12-29 12:13:11 +0000, e44dbb0)

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

*Note:* you can use the free plan with the following settings:
```
(setq langtool-http-server-host "api.languagetoolplus.com"
      langtool-http-server-port 443
      langtool-http-server-stream-type 'tls)

```

Also, if you have a premium account, setup:
```
(setq langtool-http-username "<username>"
      ;; https://languagetool.org/editor/settings/access-tokens
      langtool-http-apiKey "<your-apiKey>")
```

## Optional settings

* Key binding if you desired.

```
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-interactive-correction)
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

* You can optionally use extension package `langtool-popup` in this repository.
  To show automatically popup the cursor.

* To finish checking. All langtool marker is removed.

```
M-x langtool-check-done
```

