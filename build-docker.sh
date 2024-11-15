#!/usr/bin/env sh

docker run --rm -v .:/build freepascal/fpc:trunk-full sh -c "cd /build; ./build.sh"
