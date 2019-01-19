FROM pritunl/archlinux
RUN pacman -S solidity jdk11-openjdk sbt npm --noconfirm
RUN sbt clean
# Figure out how to make the below a dry run to test it
# RUN sbt release
