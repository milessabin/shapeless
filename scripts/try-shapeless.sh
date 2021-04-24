#!/bin/sh
COURSIER_URL=https://get-coursier.io/coursier
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.0:1.6.8 \
  com.chuusai:shapeless_2.13:2.3.3 \
  -- --predef-code 'import shapeless._' < /dev/tty
