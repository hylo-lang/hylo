#!/usr/bin/env bash

CURRENT_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CURRENT_DIR}/../" ; pwd -P )
BUILD_DIR=${PROJECT_DIR}/.build/support
SDK_DIR="${1:-/usr/local/lib/val}"

# Exit when any command fails.
set -e
set -x

# Compile the support library.
cd ${PROJECT_DIR}
swift build -c release --target Support
ar -rv libval_support.a ${PROJECT_DIR}/.build/release/Support.build/src/io.cc.o

# Install the support library.
sudo mkdir -p ${SDK_DIR}/lib
sudo mv libval_support.a ${SDK_DIR}/lib/.
