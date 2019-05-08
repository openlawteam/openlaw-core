#!/bin/sh
# Speeds up a "serverless" CI build, by manually storing a copy of the previous
# docker build and pulling it and taking advantage of `--cache-from` directive
# to use it as cache when there is no local cache present.
#
# In the (hopefully not-so-distant) future, Docker BuildKit should enable easier
# storage of a distributed cache system shared amongst serverless workers
# without this hack.
set -e

# Allow passing a $REMOTE_IMAGE variable if the remote image tag will be
# different (e.g. pushing somewhere other than Docker Hub).
IMAGE="openlaw/core"
REMOTE_IMAGE=${REMOTE_IMAGE:-$IMAGE}

# Default is not to push the cache to remote, to override define PUSH_CACHE=1
# env var. In most cases in CI we will want to be pushing the cache, but since
# it requires docker to be logged in prior, make sure it's an explicit request.
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

# Define full tags for both current branch and master branch. We want to be
# able to fall back to the last master branch build in the situation where
# this is the first build for a new branch/PR, so we don't start from scratch
# in that situation.
#
# Allow for an optional ID tag to isolate caches.
ID=${ID:-"ol"}
MASTER_TAG="${REMOTE_IMAGE}:buildcache-${ID}-master"
BRANCH_TAG="${REMOTE_IMAGE}:buildcache-${ID}-${BRANCH}"

# Pull previous cached image(s) from remote docker registry.
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
