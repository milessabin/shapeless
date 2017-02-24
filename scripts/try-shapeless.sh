#!/bin/sh
COURSIER_URL=https://raw.githubusercontent.com/alexarchambault/coursier/v1.0.0-M15/coursier
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P \
  com.lihaoyi:ammonite_2.11.8:0.8.2 \
  com.chuusai:shapeless_2.11:2.3.2 \
  -- --predef 'import shapeless._' < /dev/tty
