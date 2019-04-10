set -x
IMAGE="889468772737.dkr.ecr.us-west-2.amazonaws.com/openlawteam/openlaw-core"

MASTER_TAG="${IMAGE}:buildcache-master"
BRANCH_TAG="${IMAGE}:buildcache-${GIT_BRANCH}"


# pull previous cached image
docker pull "$MASTER_TAG" || true
docker pull "$BRANCH_TAG" || true

# build builder image using previous images as layer cache.
# use master branch cache as well in case of first build for a new branch.
docker build \
    --network=host \
    --cache-from="$MASTER_TAG" \
    --cache-from="$BRANCH_TAG" \
    -t "$IMAGE" -t "$BRANCH_TAG" .

# push the buildcache somewhere for storage
# this assumes docker is already logged in.
if [ "$PUSH_CACHE" -eq "1" ]; then
    docker push "$BRANCH_TAG"
fi