#!/usr/bin/env bash

CURRENT_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CURRENT_DIR}/../" ; pwd -P )
BUILD_DIR=${PROJECT_DIR}/.build/support
TARGET_DIR="${1:-/usr/local/lib/}"

# Exit when any command fails.
set -e

# Compile the support library.
cd ${PROJECT_DIR}
swift build -c release --target Support
ar -rv ValSupport.a ${PROJECT_DIR}/.build/release/Support.build/src/io.cc.o

# Install the support library.
mv ValSupport.a ${TARGET_DIR}/.
