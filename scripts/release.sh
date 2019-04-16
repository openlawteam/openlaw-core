#!/bin/sh
# Releases binary artifacts to bintray.
# TODO: Rationalize what "release trigger" should be in this world.
#
# Dependencies:
#   $BINTRAY_USER
#   $BINTRAY_PASS
#   $RELEASE_TRIGGER
if [ -z "$BINTRAY_USER" ] || [ -z "$BINTRAY_PASS" ]; then
    echo "Missing BINTRAY_USER or BINTRAY_PASS environment variable! Exiting..."
    exit 1
fi

# Publish to Bintray. Will do nothing by default for safety unless overridden
# via RELEASE_TRIGGER=true.
if [ "$RELEASE_TRIGGER" = "true" ]; then
    sbt release
else
    echo "Not really releasing, set RELEASE_TRIGGER=true to go live."
fi
