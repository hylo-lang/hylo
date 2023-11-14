#!/usr/bin/env bash

# Get the project directory from the current script
CUR_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CUR_DIR}/../" ; pwd -P )

# The targets that we are checking/formatting as parameters to `swift package plugin`
# Only top-level targets need to be present here
TARGETS_ARGS="--target CLI --target Driver --target FrontEnd --target Core --target IR --target CodeGenLLVM --target StandardLibrary --target Utils --target UtilsTests --target HyloTests --target DriverTests"


function usage {
    echo "Usage: $(basename $0) [-h] [-d] fix|lint|fix_lint"
    echo "  -h          Display help"
    echo "  -d          Use Debug swift-format builds (faster to compile swift-format)"
    echo "  fix         Run swift-format to correct formatting"
    echo "  lint        Run swift-format to check for formatting errors"
    echo "  fix_lint    Run swift-format to correct formatting and then check for formatting errors"
}

SWIFT_FORMAT_BUILD_MODE=release
while getopts ":hd" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        d)
            SWIFT_FORMAT_BUILD_MODE=debug
            ;;
        *)
            echo "Invalid parameters"
            usage
            exit 1
            ;;
    esac
done
shift "$(($OPTIND -1))"

SHOULD_LINT=0
SHOULD_FIX=0
MODE=$1
case $MODE in
    fix)
        SHOULD_FIX=1
        ;;
    lint)
        SHOULD_LINT=1
        ;;
    fix_lint)
        SHOULD_FIX=1
        SHOULD_LINT=1
        ;;
    *)
        usage
        exit 1
esac

# Check the swift-format executable.
# If we find the executable at the expected location, use it directly;
# otherwise, use it through `swift run`.
SWIFT_FORMAT="swift run -c ${SWIFT_FORMAT_BUILD_MODE} swift-format"
if [ -f "${PROJECT_DIR}/.build/${SWIFT_FORMAT_BUILD_MODE}/swift-format" ]; then
    SWIFT_FORMAT="${PROJECT_DIR}/.build/${SWIFT_FORMAT_BUILD_MODE}/swift-format"
fi

COMMON_ARGS="--configuration ${PROJECT_DIR}/.swift-format.json -r -p ${PROJECT_DIR}/Sources ${PROJECT_DIR}/Tests ${PROJECT_DIR}/*.swift"

[ $SHOULD_FIX -eq 1 ] && ${SWIFT_FORMAT} -i ${COMMON_ARGS} && echo "Formatted the source code."
[ $SHOULD_LINT -eq 1 ] && ${SWIFT_FORMAT} lint -s ${COMMON_ARGS} && echo "Linted the source code."
