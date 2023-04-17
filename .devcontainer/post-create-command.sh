#!/usr/bin/env bash

swift --version && swift package resolve
sudo .build/checkouts/Swifty-LLVM/Tools/make-pkgconfig.sh /usr/local/lib/pkgconfig/llvm.pc
sudo Tools/install.sh /usr/local/lib/
