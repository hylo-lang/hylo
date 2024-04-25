ARG SWIFT_VERSION=5.9
# Other ARG declarations must follow FROM
FROM swift:${SWIFT_VERSION}

ARG HYLO_LLVM_BUILD_TYPE=MinSizeRel
ARG HYLO_LLVM_BUILD_RELEASE=20240303-215025
ARG HYLO_LLVM_VERSION=17.0.6

ENV HYLO_LLVM_DOWNLOAD_URL="https://github.com/hylo-lang/llvm-build/releases/download"

RUN apt install -y gnupg
RUN apt update
RUN apt install -y curl libzstd-dev libzstd1 lsb-release make ninja-build tar wget zstd software-properties-common

#
# Get a recent cmake (https://askubuntu.com/a/865294)
#

# A-1. Uninstall the default version provided by Ubuntu's package manager and configuration
RUN if $(/usr/bin/which cmake) ; then apt purge --auto-remove cmake ; fi

# A-2. Prepare for installation
RUN apt clean all

# A-3. Obtain a copy of kitware's signing key.
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null

# A-4. Add kitware's repository to your sources list
RUN apt-add-repository "deb https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main"

# A-5. As an optional step, is recommended that we also install the kitware-archive-keyring package to ensure that Kitware's keyring stays up to date as they rotate their keys.
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1A127079A92F09ED
RUN apt update
RUN apt install -y kitware-archive-keyring
RUN rm /etc/apt/trusted.gpg.d/kitware.gpg

# A-6. Finally we can update and install the cmake package.
RUN apt update
RUN apt install -y cmake

# Despite the fact that we have installed the key with apt-key above,
# this presence of repository creates GPG signing errors for
# devcontainer feature installation such as common-utils (see
# devcontainer.json).  Now that we have cmake installed we can drop it.
RUN apt-add-repository --remove "deb https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main"

# Get the LLVM builds for the host architecture
RUN <<EOT bash -ex -o pipefail

    file_prefix="llvm-\${HYLO_LLVM_VERSION}-\$(uname -m)-unknown-linux-gnu"
    url_prefix="\${HYLO_LLVM_DOWNLOAD_URL}/\${HYLO_LLVM_BUILD_RELEASE}/\$file_prefix"

    for build_type in Debug MinSizeRel; do
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
