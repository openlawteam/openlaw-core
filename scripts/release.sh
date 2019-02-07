#!/bin/sh
# Releases binary artifacts to bintray.
# TODO: Rationalize what "release trigger" should be in this world.
#
# Dependencies:
#   $BINTRAY_USER
#   $BINTRAY_PASS
#   $RELEASE_TRIGGER
if [ "$RELEASE_TRIGGER" = "true" ]; then
    sbt 'release with-defaults'
fi
