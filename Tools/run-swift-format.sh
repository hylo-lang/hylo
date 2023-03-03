#!/usr/bin/env bash

# Get the project directory from the current script
CUR_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CUR_DIR}/../" ; pwd -P )

# The targets that we are checking/formatting as parameters to `swift package plugin`
# Only top-level targets need to be present here
TARGETS_ARGS="--target CLI --target ValCommand --target FrontEnd --target Core --target IR --target CodeGenCXX --target ValModule --target Utils --target UtilsTests --target ValTests --target ValCommandTests"


function usage {
    echo "Usage: $(basename $0) [-h] fix|lint"
    echo "  -h          Display help"
    echo "  fix         Run swift-format to correct formatting"
    echo "  lint        Run swift-format to check for formatting errors"
    echo "  fix_lint    Run swift-format to correct formatting and then check for formatting errors"
}

while getopts ":h" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        *)
            echo "Invalid parameters"
            usage
            exit 1
            ;;
    esac
done

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

pushd ${PROJECT_DIR}
[ $SHOULD_FIX -eq 1 ] && swift package plugin --allow-writing-to-package-directory format-source-code --configuration .swift-format.json ${TARGETS_ARGS}
[ $SHOULD_LINT -eq 1 ] && swift package plugin lint-source-code --configuration .swift-format.json ${TARGETS_ARGS}
popd
