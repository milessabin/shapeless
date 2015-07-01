#!/bin/bash

set -o errexit

if [ -n "$(git status --porcelain)" ]; then 
  echo "There are uncommitted changes ... aborting"
elif [[ -z $1 ]] ; then
  echo "No remote specified ... aborting"
else
  git fetch $1
  git checkout master
  git pull $1 master
  git checkout scala-2.10.x
  git pull $1 scala-2.10.x
  git checkout scalajs-2.11.x
  git pull $1 scalajs-2.11.x
  git checkout scalajs-2.10.x
  git pull $1 scalajs-2.10.x
  git checkout master
fi
