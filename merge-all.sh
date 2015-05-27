#!/bin/bash

set -o errexit

if [ -n "$(git status --porcelain)" ]; then 
  echo "There are uncommitted changes ... aborting"
else 
  git checkout scala-2.10.x
  git merge --no-edit master
  git checkout scalajs-2.11.x
  git merge --no-edit master
  git checkout scalajs-2.10.x
  git merge --no-edit scala-2.10.x
  git checkout master
fi
