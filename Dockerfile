FROM logankoester/archlinux
RUN pacman -S jdk-openjdk sbt --noconfirm
RUN sbt test:compile
RUN sbt clean coverage test
RUN sbt coverageReport
RUN sbt coverageAggregate
RUN sbt codacyCoverage