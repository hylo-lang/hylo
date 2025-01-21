#!/bin/bash

set -x


.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/Break.hylo --verbose

.ninja-build/Sources/hc Tests/EndToEndTests/TestCases/Break.hylo --verbose --emit llvm
cat StdLib.ll | grep -A 20 "UInt64AtomicRepresentation"
