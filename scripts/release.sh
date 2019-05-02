#!/bin/sh
# Releases binary artifacts to bintray.
#
# Dependencies:
#   $BINTRAY_USER
#   $BINTRAY_PASS
#   $RELEASE_TRIGGER

# The sbt-git plugin is going to pull info straight from git, but when building
# into container we leave the .git folder out to not invalidate layer caching,
# and mount it only for the release phase. However, the sbt-git plugin fails
# *silently* if it can't locate a git repository and just rolls back to a system
# timestamp as the version number. To avoid this, we want to manually verify the
# git directory is present first and fail if not.
#
# (We check relative to $PWD rather than this script path, since sbt will fail
#  in other unexpected ways if you aren't already at project root, so better to
#  just fail if we're not at project root.)
if [ ! -d "$PWD/.git" ]; then
    echo "ERROR: No git repo located for versioning, aborting..."
    exit 1
fi

# Check for BINTRAY variables used by sbt-bintray for authentication.
# (see https://github.com/sbt/sbt-bintray#credentials)
if [ -z "$BINTRAY_USER" ] || [ -z "$BINTRAY_PASS" ]; then
    echo "Missing BINTRAY_USER or BINTRAY_PASS environment variable! Exiting..."
    exit 1
fi

# Publish to Bintray. Will do nothing by default for safety unless overridden
# via RELEASE_TRIGGER=true.
if [ "$RELEASE_TRIGGER" = "true" ]; then
    version=$(sbt version)
    # release Scala & then ScalaJS to avoid issue where release-with-defaults only releases Scala lib
    # the next-version flag is to silence SBT's interactive release shell prompt - it doesn't actually alter the version
    # confirmed via running `sbt version` after release.
    sbt ';project openlawCore ;release release-version ${version} next-version ${version-SNAPSHOT} with-defaults'
    sbt ';project openlawCoreJS ;release release-version ${version} next-version ${version-SNAPSHOT} with-defaults'
else
    echo "Not really releasing, set RELEASE_TRIGGER=true to go live."
fi
