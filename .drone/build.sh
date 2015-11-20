#!/bin/bash
set -e
APIKEY=$1
cd $DRONE_BUILD_DIR
printenv
rm RELEASE_NOTES.md ; echo "### 1.1.$DRONE_BUILD_NUMBER" > RELEASE_NOTES.md
#/build.sh BuildPackage
./build.sh PublishNuget nugetkey=${APIKEY}
