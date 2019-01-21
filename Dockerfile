FROM logankoester/archlinux
RUN pacman -S jdk-openjdk sbt --noconfirm
RUN sbt test:compile