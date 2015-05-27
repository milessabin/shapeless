#!/bin/bash

if [[ -z $1 ]] ; then
  echo "No remote specified ... aborting"
else
  git push $1 master scala-2.10.x scalajs-2.11.x scalajs-2.10.x
fi
