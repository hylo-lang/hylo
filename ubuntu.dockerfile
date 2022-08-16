FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/Los_Angeles

RUN apt update && \
    apt install -y llvm-11 && \
    apt install -y wget && \
    apt-get update && \
    apt-get install -y \
    libc++-dev \
    libc++abi-dev \
    binutils \
    git \
    gnupg2 \
    libc6-dev \
    libcurl4 \
    libedit2 \
    libgcc-9-dev \
    libpython2.7 \
    libsqlite3-0 \
    libstdc++-9-dev \
    libxml2 \
    libz3-dev \
    pkg-config \
    tzdata \
    uuid-dev \
    zlib1g-dev

RUN mv /usr/bin/llvm-config-11 /usr/bin/llvm-config

RUN wget -q https://download.swift.org/swift-5.6.1-release/ubuntu2004/swift-5.6.1-RELEASE/swift-5.6.1-RELEASE-ubuntu20.04.tar.gz && \
    tar xzf swift-5.6.1-RELEASE-ubuntu20.04.tar.gz && \
    rm -f swift-5.6.1-RELEASE-ubuntu20.04.tar.gz && \
    mv swift-5.6.1-RELEASE-ubuntu20.04 /usr/share/swift

ENV PATH="${PATH}:/usr/share/swift/usr/bin"

COPY . /src

WORKDIR /src

RUN swift package resolve

RUN swift .build/checkouts/LLVMSwift/utils/make-pkgconfig.swift

RUN swift build -v -c debug
