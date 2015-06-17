#!/bin/bash

LOCAL_REPO=$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)
RELEASE_DIR=$(mktemp -d --tmpdir shapeless-release.XXXXXXXXXX)

echo "Releasing in ${RELEASE_DIR}"

git clone ${LOCAL_REPO} ${RELEASE_DIR}
cd ${RELEASE_DIR}
${RELEASE_DIR}/release.scalascript
cd ${LOCAL_REPO}
rm -Rf ${RELEASE_DIR}
