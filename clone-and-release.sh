#!/bin/bash

LOCAL_REPO=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
RELEASE_DIR=$(mktemp -d --tmpdir shapeless-release.XXXXXXXXXX)

echo "Releasing in ${RELEASE_DIR}"

git clone ${LOCAL_REPO} ${RELEASE_DIR}
cd ${RELEASE_DIR}
git checkout scala-2.10.x
git checkout scalajs-2.11.x
git checkout scalajs-2.10.x
git checkout master
git checkout jdk6-canary
${RELEASE_DIR}/release.scalascript
cd ${LOCAL_REPO}
rm -Rf ${RELEASE_DIR}
