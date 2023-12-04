// This dummy target is used only on Windows to prepare us to build with
// SPM_BUILD_TOOL_SUPPORT_NO_REENTRANT_BUILD=1 (see ../../SPMBuildToolSupport/README.md).  It allows
// us to build the executable targets used by build tool plugins by requesting a single target, and
// without creating a product for each such executable
// (https://github.com/apple/swift-package-manager/issues/7133)
