FROM pritunl/archlinux
RUN pacman -S jdk-openjdk sbt --noconfirm
RUN sbt clean
RUN sbt dist
