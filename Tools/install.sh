#!/usr/bin/env bash

CURRENT_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CURRENT_DIR}/../" ; pwd -P )
BUILD_DIR=${PROJECT_DIR}/.build/support
TARGET_DIR="${1:-/usr/local/lib/}"

# Exit when any command fails.
set -e

# Compile the support library.
mkdir -p ${BUILD_DIR}
clang++ -O2 -c -std=c++20 -o ${BUILD_DIR}/ValSupport.o ${PROJECT_DIR}/Sources/Support/src/io.cc
ar -rv ${BUILD_DIR}/ValSupport.a ${BUILD_DIR}/ValSupport.o

# Install the support library.
mv ${BUILD_DIR}/ValSupport.a ${TARGET_DIR}/.
