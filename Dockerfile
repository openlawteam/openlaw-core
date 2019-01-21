FROM pritunl/archlinux
RUN pacman -S solidity jdk11-openjdk sbt npm --noconfirm
RUN sbt clean
RUN sbt dist
