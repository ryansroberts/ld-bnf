#!/bin/bash
set -e
printenv
rm RELEASE_NOTES.md ; echo "### 1.2.$2" > RELEASE_NOTES.md
./build.sh PublishNuget nugetkey=$1
