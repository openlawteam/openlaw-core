FROM openlaw/scala-builder:node

# install plugins
COPY project ./project
RUN sbt update #( installing plugins... )

# install deps
COPY build.sbt .
RUN sbt update #( installing deps... )

# build core and tests
COPY shared ./shared
RUN sbt compile test:compile fastOptJS
# TODO(mroth): ^^ I'd like to get those SBT_OBTS out of this if they are not
# specific to this project. I suspect that may be something we want to roll
# into the JVM_OPTS of scala-builder itself or the environment? LMK!

# put a copy of local CI scripts directly in image, so we don't have to rely
# upon mounting it in CI environments (kind of a pain in CircleCI due to remote
# docker setup)
COPY scripts ./scripts
