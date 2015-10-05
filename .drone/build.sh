#!/bin/bash
set -e
cd $DRONE_BUILD_DIR

./build.sh BuildPackage
./build.sh PublishNuget nugetkey=$$APIKEY
