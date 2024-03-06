#!/usr/bin/env bash

# This script sets the version of the compiler
# Pass the version as an argument, or omit the argument to set the version to the current HEAD git hash

VERSION_FILE="Sources/Driver/Version.swift"

if [ "$#" -ne 1 ]; then
    version=$(git rev-parse HEAD)
else
    version=$1
fi

echo -e "// DO NOT COMMIT CHANGES TO THIS FILE\nlet hcVersion = \"${version}\"" > $VERSION_FILE

# Ignore local changes to Version.swift
git update-index --assume-unchanged $VERSION_FILE

cat $VERSION_FILE
