#!/bin/bash

set -x

# printenv

/usr/bin/gcc --version
/usr/bin/clang++ --version
/opt/hostedtoolcache/swift-5.10.1-RELEASE-ubuntu2404/5.10.1/x86_64/usr/bin/clang++ --version

/usr/bin/gcc ci-test/atomics_test.c -o ci-test/atomics_test
ci-test/atomics_test

/usr/bin/clang++ ci-test/atomics_test.c -o ci-test/atomics_test
ci-test/atomics_test

/opt/hostedtoolcache/swift-5.10.1-RELEASE-ubuntu2404/5.10.1/x86_64/usr/bin/clang++ ci-test/atomics_test.c -o ci-test/atomics_test
ci-test/atomics_test

/usr/bin/clang++ ci-test/atomics_test1.ll
/usr/bin/clang++ ci-test/atomics_test2.ll
