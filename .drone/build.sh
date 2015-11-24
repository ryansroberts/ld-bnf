#!/bin/bash
set -e
APIKEY=$1
printenv
rm RELEASE_NOTES.md ; echo "### 1.2.$BUILD_NUMBER" > RELEASE_NOTES.md
#/build.sh BuildPackage
./build.sh PublishNuget nugetkey=${APIKEY}
