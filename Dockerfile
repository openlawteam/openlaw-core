FROM ubuntu:18.04 as base

ENV DEBIAN_FRONTEND=noninteractive TERM=xterm

RUN apt-get update
RUN apt-get install -y locales && locale-gen en_US.UTF-8 && dpkg-reconfigure locales
ENV LANGUAGE=en_US.UTF-8 LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

# Utilities
RUN apt-get install -y --no-install-recommends vim less net-tools inetutils-ping wget curl git telnet nmap socat dnsutils netcat tree htop unzip sudo software-properties-common jq psmisc iproute2 python ssh rsync gettext-base

# Java
RUN apt-get --yes install default-jdk-headless

FROM base as tools

RUN apt-get install -y build-essential --fix-missing

# SBT
RUN curl -L -o sbt.deb http://dl.bintray.com/sbt/debian/sbt-1.2.6.deb && \
    dpkg -i sbt.deb

# NodeJS (required for ScalaJS)
RUN wget -O - https://nodejs.org/dist/v10.10.0/node-v10.10.0-linux-x64.tar.gz | tar xz && \
    mv node* node
ENV PATH $PATH:/node/bin

# Cache
FROM tools as cache

COPY project /src/project
RUN cd /src && \
    sbt update

# build
COPY . /src/

FROM cache as build

WORKDIR /src
ENV SBT_OPTS="-Xmx4G"

RUN sbt compile

FROM build as test

RUN sbt coverage test
RUN sbt coverageReport
RUN sbt coverageAggregate

ARG CODACY_PROJECT_TOKEN
ENV CODACY_PROJECT_TOKEN=${CODACY_PROJECT_TOKEN}

RUN sbt codacyCoverage

FROM test as release

ARG BINTRAY_USER
ENV BINTRAY_USER=${BINTRAY_USER}

ARG BINTRAY_PASS
ENV BINTRAY_PASS=${BINTRAY_PASS}

ARG RELEASE
ENV RELEASE=${RELEASE}

RUN git branch --set-upstream-to=origin/master master
RUN if [ "$RELEASE" = "true" ]; then sbt 'release with-defaults'; fi
