FROM openlaw/scala-builder:node

# build dependencies
COPY project ./project
RUN sbt update

# build core and tests
COPY shared ./shared
COPY build.sbt .
RUN SBT_OPTS="-Xmx4G" sbt compile test:compile fastOptJS

# TODO(mroth): ^^ I'd like to get those SBT_OBTS out of this if they are not
# specific to this project. I suspect that may be something we want to roll
# into the JVM_OPTS of scala-builder itself or the environment? LMK!