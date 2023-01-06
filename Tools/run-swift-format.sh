#!/bin/sh

# Get the project directory from the current script
CUR_DIR=$(dirname "$0")
PROJECT_DIR=$( cd "${CUR_DIR}/../" ; pwd -P )

function usage {
    echo "Usage: $(basename $0) [-h] fix|lint"
    echo "  -h          Display help"
    echo "  fix         Run swift-format to correct formatting"
    echo "  lint        Run swift-format to check for formatting errors"
}

# Check that `swift-format` exists and its in the PATH variable.
# XCode removes the PATH variable, so we also search in /usr/local/bin/swift-format
SWIFT_FORMAT=""
function check_executable {
    # Try finding the executable in PATH
    SWIFT_FORMAT=`which swift-format`
    if [ -f "${SWIFT_FORMAT}" ]; then
        return
    fi

    # Try /usr/local/bin/swift-format
    which swift-format >/dev/null
    if [ -f /usr/local/bin/swift-format ]; then
        SWIFT_FORMAT="/usr/local/bin/swift-format"
    else
        echo "warning: swift-format not installed"
        exit 1
    fi
}

function do_fix {
    check_executable
    ${SWIFT_FORMAT} --configuration ${PROJECT_DIR}/.swift-format.json -i -r -p ${PROJECT_DIR}/Sources ${PROJECT_DIR}/Tests ${PROJECT_DIR}/*.swift
}

function do_lint {
    check_executable
    ${SWIFT_FORMAT} lint --configuration ${PROJECT_DIR}/.swift-format.json -s -r -p ${PROJECT_DIR}/Sources ${PROJECT_DIR}/Tests ${PROJECT_DIR}/*.swift
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

MODE=$1
case $MODE in
    fix)
        do_fix
        ;;
    lint)
        do_lint
        ;;
    *)
        usage
        exit 1
esac
