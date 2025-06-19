#!/bin/bash
set -e

cfilepath="$1"
hyloPath="$2"

echo "Compiling $cfilepath via clang..."

# compile the c file to an object file
clang -fPIC -c "$cfilepath" -o "${cfilepath%.*}.o"

# create a static library
ar rcs "lib${cfilepath%.*}.a" "${cfilepath%.*}.o"

echo "Compiling $hyloPath via hc..."
swift run hc "$hyloPath" -L . -l "${cfilepath%.*}"

command="${hyloPath%.*}"
./"$command"