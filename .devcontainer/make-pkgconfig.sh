#!/usr/bin/env bash
set -e
set -o pipefail

version=$(llvm-config --version)
filename=$1

mkdir -p `dirname $filename`
touch $filename

# Function to normalize path separators (replace backslashes with forward slashes)
normalize_path_separators() {
    echo "$1" | sed 's/\\/\//g'
}

# Function to replace absolute paths with relocatable paths
replace_with_relocatable_paths() {
    local input=$(normalize_path_separators "$1")
    local llvm_root=$(normalize_path_separators "$(llvm-config --prefix)")
    
    # Ensure llvm_root ends with a separator
    if [[ ! "$llvm_root" =~ /$ ]]; then
        llvm_root="${llvm_root}/"
    fi
    
    # Replace absolute LLVM root path with relocatable path
    echo "$input" | sed "s|${llvm_root}|\${pcfiledir}/../|g"
}

# Function to normalize spaces (replace multiple whitespace with single space and trim)
normalize_spaces() {
    echo "$1" | sed 's/[[:space:]]\+/ /g' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//'
}

# Get libraries
absolute_libdir=$(normalize_spaces "$(llvm-config --libdir)")
system_libs=$(normalize_spaces "$(llvm-config --system-libs --libs analysis bitwriter core native passes target)")
lib_attributes=$(replace_with_relocatable_paths "-L${absolute_libdir} ${system_libs}")

# Get CXX flags
cxxflags_output=$(normalize_spaces "$(llvm-config --cxxflags)")
cflags=$(replace_with_relocatable_paths "$cxxflags_output")

# Generate pkg-config content
echo Name: LLVM > $filename
echo Description: Low-level Virtual Machine compiler framework >> $filename
echo Version: $(echo ${version} | sed 's/\([0-9.]\+\).*/\1/') >> $filename
echo URL: http://www.llvm.org/ >> $filename
echo Libs: ${lib_attributes} >> $filename
echo Cflags: ${cflags} >> $filename

echo "$filename written:"
cat $filename
