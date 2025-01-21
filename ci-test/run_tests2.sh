#!/bin/bash

set -ex

ls -lsa .ninja-build
ls -lsa .ninja-build/Sources
.ninja-build/Sources/hc --version
.ninja-build/Sources/hc --help

.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/AtomicOperations.hylo --verbose
.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/AtomicOperations.hylo --emit llvm
cat AtomicOperations.ll

echo "-------------------------"

.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/Break.hylo --emit llvm
cat Break.ll
