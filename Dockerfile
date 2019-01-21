FROM ubuntu:18.04 as base

ENV DEBIAN_FRONTEND=noninteractive TERM=xterm
RUN echo "export > /etc/envvars" >> /root/.bashrc && \
    echo "export PS1='\[\e[1;31m\]\u@\h:\w\\$\[\e[0m\] '" | tee -a /root/.bashrc /etc/skel/.bashrc && \
    echo "alias tcurrent='tail /var/log/*/current -f'" | tee -a /root/.bashrc /etc/skel/.bashrc

RUN apt-get update
RUN apt-get install -y locales && locale-gen en_US.UTF-8 && dpkg-reconfigure locales
ENV LANGUAGE=en_US.UTF-8 LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8

# Runit
RUN apt-get install -y --no-install-recommends runit
CMD bash -c 'export > /etc/envvars && /usr/sbin/runsvdir-start'

# Utilities
RUN apt-get install -y --no-install-recommends vim less net-tools inetutils-ping wget curl git telnet nmap socat dnsutils netcat tree htop unzip sudo software-properties-common jq psmisc iproute2 python ssh rsync gettext-base

# Java
RUN apt-get --yes install default-jdk

FROM base as tools

RUN apt-get update
RUN apt-get install -y build-essential --fix-missing

# SBT
RUN curl -L -o sbt.deb http://dl.bintray.com/sbt/debian/sbt-1.2.6.deb && \
    dpkg -i sbt.deb

# Cache
FROM tools as cache
COPY project /src/project
RUN cd /src && \
    sbt update

# build
COPY . /src/

FROM build as test

RUN cd /src && \
    SBT_OPTS="-Xmx4G" sbt clean coverage test && sbt coverageReport && sbt coverageAggregate && sbt codacyCoverage
