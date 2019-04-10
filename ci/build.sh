#!/bin/sh
# Builds the local image, suitable for running tests or doing a release. This
# script handles doing "layer cached build" in CI by storing the previous image
# between runs.

set -x # TODO: DEBUGGING so echo on for now 

# Allow passing a $REMOTE_IMAGE variable if the remote image tag will be
# different (e.g. pushing somewhere other than Docker Hub).
IMAGE="openlaw/core"
REMOTE_IMAGE=${REMOTE_IMAGE:-$IMAGE}

# Default is not to push the cache, to override define PUSH_CACHE=1 env var.
PUSH_CACHE=${PUSH_CACHE:-0}

# Get the current git branch name, this will be used to tag the buildcache such
# that multiple PRs can exist simultaneously without their caches potentially
# colliding.
#
# This will use the $BRANCH environment variable if already set, otherwise will
# fall back to trying to parse git locally. The latter may not work in many CI
# Docker containers (since they don't contain git) so you likely want to make
# sure this is passed in CI based on the particular host's methodology.
BRANCH=${BRANCH:-$(git rev-parse --abbrev-ref HEAD)}

# Define full tags for both current branch and master branch
MASTER_TAG="${REMOTE_IMAGE}:buildcache-master"
BRANCH_TAG="${REMOTE_IMAGE}:buildcache-${BRANCH}"

# pull previous cached image
docker pull "$MASTER_TAG" || true
docker pull "$BRANCH_TAG" || true

# build builder image using previous images as layer cache.
# use master branch cache as well in case of first build for a new branch.
#
# If you need additional build parameters/arguments, you can include them via
# the $BUILD_PARAMS environment variable.
docker build ${BUILD_PARAMS:+"$BUILD_PARAMS"} \
    --cache-from="$MASTER_TAG" \
    --cache-from="$BRANCH_TAG" \
    -t "$IMAGE" -t "$BRANCH_TAG" .

# push the buildcache somewhere for storage
# this assumes docker is already logged in.
if [ "$PUSH_CACHE" -eq "1" ]; then
    docker push "$BRANCH_TAG"
fi
