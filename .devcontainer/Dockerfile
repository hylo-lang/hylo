ARG SWIFT_VERSION=6.1
ARG CMAKE_VERSION=3.29.1

# Other ARG declarations must follow FROM
FROM swift:${SWIFT_VERSION}

ARG HYLO_LLVM_BUILD_TYPE=MinSizeRel
ARG HYLO_LLVM_BUILD_RELEASE=20240303-215025
ARG HYLO_LLVM_VERSION=17.0.6

ENV HYLO_LLVM_DOWNLOAD_URL="https://github.com/hylo-lang/llvm-build/releases/download"

# llvm-15 is installed to get llvm-cov, which is needed to generate our coverage reports.
# Note, apt-get is used for stable CLI interface instead of apt. Cleaning is done in the same
# layer as installations to save disk space.
RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg curl libzstd-dev libzstd1 lsb-release make \
       ninja-build tar wget zstd software-properties-common python3-pip llvm-15 cmake && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN cmake --version

# Get the LLVM build(s) for the host architecture
RUN <<EOT bash -ex -o pipefail

    file_prefix="llvm-\${HYLO_LLVM_VERSION}-\$(uname -m)-unknown-linux-gnu"
    url_prefix="\${HYLO_LLVM_DOWNLOAD_URL}/\${HYLO_LLVM_BUILD_RELEASE}/\$file_prefix"

    # Add 'Debug' to the loop below to install a debug build of LLVM
    # as well.  It is not there by default because it was causing us
    # to run out of disk space in GitHub CI.
    for build_type in MinSizeRel; do
        curl --no-progress-meter -L "\${url_prefix}-\${build_type}.tar.zst" | tar -x --zstd -C /opt
        ln -s /opt/\${file_prefix}-\${build_type} /opt/llvm-\${build_type}
    done

EOT

# Make and install the llvm.pc file.
ADD make-pkgconfig.sh /tmp
RUN <<EOT bash -ex -o pipefail

    export PATH="/opt/llvm-${HYLO_LLVM_BUILD_TYPE}/bin:\$PATH"
    /tmp/make-pkgconfig.sh /usr/local/lib/pkgconfig/llvm.pc > /dev/null
    rm /tmp/make-pkgconfig.sh

EOT
