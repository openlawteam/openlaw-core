FROM openlaw/scala-builder:node

# install plugins
COPY project ./project
# copy .sbtopts for project-specific JVM settings (we may want to use these across
# projects, or get rid of entirely - TBD)
COPY .sbtopts .
RUN sbt update #( installing plugins... )

# install deps
COPY build.sbt .
RUN sbt update #( installing deps... )

# build core and tests
COPY shared ./shared
RUN sbt compile test:compile fastOptJS

# put a copy of local CI scripts directly in image, so we don't have to rely
# upon mounting it in CI environments (kind of a pain in CircleCI due to remote
# docker setup)
COPY scripts ./scripts
