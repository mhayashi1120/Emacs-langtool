# Java

## Debian(stable)

Build new JDK to try the newest LanguageTool.
https://wiki.debian.org/JavaPackage



# Test

## create .test-init.el
   To indicate `langtool-language-tool-jar`

## make check

Currently almost test is empty although.

## Manual tests

### Check is working

Try to check some buffer. (C-x 4 w)

  1. Command line
  2. Server <-> Client
  3. Client
    manually invoke following in shell:
    java -cp *langtool-language-tool-server-jar* org.languagetool.server.HTTPServer -p 8082
    
    set following:
    (setq langtool-http-server-host "localhost"
          langtool-http-server-port 8082)

### Interactive correction is working

Try to correct buffer (C-x 4 c)


# TODO

unsupport 3.x

more unit test ( for each 4.x )

check version dependent code. (old code is remaining)

