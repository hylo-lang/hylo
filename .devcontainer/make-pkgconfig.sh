#!/usr/bin/env bash
set -e
set -o pipefail

# Work around https://github.com/hylo-lang/llvm-build/issues/8
#
# We need to be resilient to no libzstd being found by pkg-config, as it is apparently not on linux.
zstd_dash_L="$(pkg-config --silence-errors --libs-only-L libzstd || true)"
if ! (llvm-config > /dev/null 2>&1); then
  if [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "cygwin" || "$OSTYPE" == "freebsd"* ]]; then
    export LD_LIBRARY_PATH="${zstd_dash_L#-L}:$LD_LIBRARY_PATH"
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    export DYLD_LIBRARY_PATH="${zstd_dash_L#-L}:$DYLD_LIBRARY_PATH"
  elif [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    export PATH="${zstd_dash_L#-L}:$PATH"
  fi
fi

version=$(llvm-config --version)
filename=$1

mkdir -p `dirname $filename`
touch $filename

# REVISIT(nickpdemarco):
# Why does macos need the standard library explicitly linked, while linux does not?
# This does not feel like the correct place for this logic.
machine="$(uname -s)"
case "${machine}" in
  Darwin*)  libs="-lc++";;
  *)        libs=""
esac

libs=()
for x in -L$(llvm-config --libdir) ${zstd_dash_L} ${libs} $(llvm-config --system-libs --libs analysis bitwriter core native passes target); do
    libs+=($(printf '%q' "$x"))
done
cflags=()
for x in $(llvm-config --cxxflags); do
    cflags+=($(printf '%q' "$x"))
done

echo Name: LLVM > $filename
echo Description: Low-level Virtual Machine compiler framework >> $filename
echo Version: $(echo ${version} | sed 's/\([0-9.]\+\).*/\1/') >> $filename
echo URL: http://www.llvm.org/ >> $filename
echo Libs: ${libs[@]} >> $filename
echo Cflags: ${cflags[@]} >> $filename

echo "$filename written:"
cat $filename
