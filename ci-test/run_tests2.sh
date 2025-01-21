#!/bin/bash

set -x


.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/AtomicOperations.hylo --verbose --emit llvm
cat AtomicOperations.ll

echo "-------------------------"

.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/Break.hylo --verbose --emit llvm
cat Break.ll
