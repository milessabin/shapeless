#!/bin/bash

java \
-Xmx512M \
-XX:+UseCompressedOops \
-Xmx2G \
-XX:MaxPermSize=256M \
-jar `dirname $0`/sbt-launch.jar "$@"
