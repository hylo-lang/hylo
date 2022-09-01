FROM swiftlang/swift:nightly-jammy

RUN apt update && \
    apt install -y llvm-11

RUN mv /usr/bin/llvm-config-11 /usr/bin/llvm-config

RUN apt-get update && \
    apt-get install -y libc++-dev && \
    apt-get install -y libc++abi-dev

COPY . /src

WORKDIR /src

RUN swift package resolve

RUN swift .build/checkouts/LLVMSwift/utils/make-pkgconfig.swift

# RUN swift build -v -c debug