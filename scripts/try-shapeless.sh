#!/bin/sh
COURSIER_URL=https://raw.githubusercontent.com/alexarchambault/coursier/v1.0.0-M12/coursier
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && curl -s --output ~/.coursier/cr $COURSIER_URL && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  com.chuusai:shapeless_2.11:2.3.1 \
  com.lihaoyi:ammonite-repl_2.11.8:0.6.2 \
  \
)" java ammonite.repl.Main --predef 'import shapeless._'
