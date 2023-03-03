#!/usr/bin/env bash

# Get the project directory from the current script
CUR_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CUR_DIR}/../" ; pwd -P )

# The targets that we are checking/formatting as parameters to `swift package plugin`
# Only top-level targets need to be present here
TARGETS_ARGS="--target CLI --target ValCommand --target FrontEnd --target Core --target IR --target CodeGenCXX --target ValModule --target Utils --target UtilsTests --target ValTests --target ValCommandTests"


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

# Check the swift-format executable
SWIFT_FORMAT="${PROJECT_DIR}/.build/${SWIFT_FORMAT_BUILD_MODE}/swift-format"
if [ ! -f ${SWIFT_FORMAT} ]; then
    echo "Building: ${SWIFT_FORMAT}"
    swift build -c ${SWIFT_FORMAT_BUILD_MODE} --target swift-format
fi
if [ ! -f ${SWIFT_FORMAT} ]; then
    echo "Cannot build ${SWIFT_FORMAT}"
    exit 1
fi

COMMON_ARGS="--configuration ${PROJECT_DIR}/.swift-format.json -r -p ${PROJECT_DIR}/Sources ${PROJECT_DIR}/Tests ${PROJECT_DIR}/*.swift"
${SWIFT_FORMAT} -i ${COMMON_ARGS}

[ $SHOULD_FIX -eq 1 ] && ${SWIFT_FORMAT} -i ${COMMON_ARGS} && echo "Formatted the source code."
[ $SHOULD_LINT -eq 1 ] && ${SWIFT_FORMAT} lint -s ${COMMON_ARGS} && echo "Linted the source code."
