This directory contains source files that are shared among targets via symlinking.

These files can't be centralized into a common library because the names they define need to shadow
those coming from other modules like Foundation and XCTest.
