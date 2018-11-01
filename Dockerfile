FROM ubuntu

# Install KEVM dependencies
RUN apt-get update && apt-get -y upgrade
RUN apt-get -y install build-essential
RUN apt-get -y install git make gcc maven openjdk-8-jdk flex pkg-config libmpfr-dev autoconf libtool pandoc zlib1g-dev
RUN apt-get -y install z3 libz3-dev
RUN apt-get -y install tmux

# Install latest node and npm
RUN apt-get -y install curl gnupg
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash
RUN apt-get -y install nodejs

# build source
WORKDIR klab
COPY . .
RUN make deps

# env setup
ENV KLAB_EVMS_PATH=/klab/evm-semantics
ENV TMPDIR=/tmp
RUN make link
