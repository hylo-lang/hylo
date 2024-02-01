# Testing with:
# mkdir -p ~/src/hylo/.cmake-build && cd ~/src/hylo/.cmake-build && rm -f CMakeCache.txt && cmake -GNinja -S .. && cmake --build .

cmake_minimum_required(VERSION 3.28.2)

if(POLICY CMP0077)
  cmake_policy(SET CMP0077 NEW)
endif()

# MSVC runtime library flags are selected by an abstraction.
if(POLICY CMP0091)
  cmake_policy(SET CMP0091 NEW)
endif()

# enable LSP support
if(POLICY CMP0157)
  cmake_policy(SET CMP0157 NEW)
endif()

function(dump_cmake_variables)
    get_cmake_property(_variableNames VARIABLES)
    list (SORT _variableNames)
    foreach (_variableName ${_variableNames})
        if ((NOT DEFINED ARGV0) OR _variableName MATCHES ${ARGV0})
            message(STATUS "${_variableName}=${${_variableName}}")
        endif()
    endforeach()
endfunction()

project(SwiftPM LANGUAGES C Swift)

# option(BUILD_SHARED_LIBS "Build shared libraries by default" YES)

set(CMAKE_Swift_LANGUAGE_VERSION 5)
set(CMAKE_Swift_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/swift)
set(CMAKE_Swift_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules)
set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)
set(CMAKE_POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS})

# Toolchain Vended dependencies
find_package(dispatch QUIET)
find_package(Foundation QUIET)

include(FetchContent)

set(_HYLO_SAVED_BUILD_TESTING ${BUILD_TESTING})
set(_HYLO_SAVED_BUILD_EXAMPLES ${BUILD_EXAMPLES})
set(_HYLO_VENDOR_DEPENDENCIES)

set(BUILD_EXAMPLES NO)
set(BUILD_TESTING NO)

FetchContent_Declare(BigInt
  GIT_REPOSITORY https://github.com/attaswift/BigInt.git
  GIT_TAG        v5.3.0
  FIND_PACKAGE_ARGS CONFIG
)
# No built-in CMake support so declare the library here.
FetchContent_Populate(BigInt)
file(GLOB_RECURSE BigInt_FILES FOLLOW_SYMLINKS LIST_DIRECTORIES false CONFIGURE_DEPENDS ${bigint_SOURCE_DIR}/Sources/*.swift)
add_library(BigInt ${BigInt_FILES})

FetchContent_Declare(Durian
  GIT_REPOSITORY https://github.com/hylo-lang/Durian.git
  GIT_TAG        1.2
  FIND_PACKAGE_ARGS CONFIG
)

# FetchContent_Declare(SPIManifest
#   GIT_REPOSITORY https://github.com/SwiftPackageIndex/SPIManifest.git
#   GIT_TAG        v0.19.0
#   FIND_PACKAGE_ARGS CONFIG
# )

FetchContent_Declare(SwiftAlgorithms
  GIT_REPOSITORY https://github.com/apple/swift-algorithms.git
  GIT_TAG        1.2.0
  FIND_PACKAGE_ARGS CONFIG
)
# No built-in CMake support so declare the library here.
FetchContent_Populate(SwiftAlgorithms)
file(GLOB_RECURSE SwiftAlgorithms_FILES FOLLOW_SYMLINKS LIST_DIRECTORIES false CONFIGURE_DEPENDS ${swiftalgorithms_SOURCE_DIR}/Sources/*.swift)
add_library(Algorithms ${SwiftAlgorithms_FILES})
target_link_libraries(Algorithms PRIVATE RealModule)
# set_target_properties(Algorithms PROPERTIES
#   INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_Swift_MODULE_DIRECTORY})

FetchContent_Declare(ArgumentParser
  GIT_REPOSITORY https://github.com/apple/swift-argument-parser.git
  GIT_TAG        1.2.2
  FIND_PACKAGE_ARGS CONFIG
)

find_package(Git)
set(SwiftCollections_patch
  ${GIT_EXECUTABLE} apply ${CMAKE_CURRENT_SOURCE_DIR}/SwiftCollections.patch)
FetchContent_Declare(SwiftCollections
  GIT_REPOSITORY https://github.com/apple/swift-collections.git
  GIT_TAG        1.0.4
  PATCH_COMMAND ${SwiftCollections_patch}
  UPDATE_DISCONNECTED 1
  FIND_PACKAGE_ARGS CONFIG
)

FetchContent_Declare(SwiftFormat
  GIT_REPOSITORY https://github.com/apple/swift-format.git
  GIT_TAG        508.0.1
  FIND_PACKAGE_ARGS CONFIG
)

set(swift-numerics_patch
  ${GIT_EXECUTABLE} apply ${CMAKE_CURRENT_SOURCE_DIR}/swift-numerics.patch)
FetchContent_Declare(swift-numerics
  GIT_REPOSITORY https://github.com/apple/swift-numerics.git
  GIT_TAG        1.0.2
  PATCH_COMMAND ${swift-numerics_patch}
  UPDATE_DISCONNECTED 1
  FIND_PACKAGE_ARGS CONFIG
)

# FetchContent_Declare(SwiftSyntax
#   GIT_REPOSITORY https://github.com/apple/swift-syntax.git
#   GIT_TAG        main
#   FIND_PACKAGE_ARGS CONFIG
# )


# FetchContent_Declare(SwiftSystem
#   GIT_REPOSITORY https://github.com/apple/swift-system.git
#   GIT_TAG        1.1.1
#   FIND_PACKAGE_ARGS CONFIG
# )

# FetchContent_Declare(SwiftToolsSupportCore
#   GIT_REPOSITORY https://github.com/apple/swift-tools-support-core.git
#   GIT_TAG        0.5.2
#   FIND_PACKAGE_ARGS CONFIG
# )

FetchContent_Declare(SwiftyLLVM
  GIT_REPOSITORY https://github.com/hylo-lang/Swifty-LLVM.git
  GIT_TAG        main
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(BigInt Durian SwiftAlgorithms ArgumentParser SwiftCollections SwiftFormat swift-numerics SwiftyLLVM)

set(BUILD_TESTING ${_HYLO_SAVED_BUILD_TESTING})
set(BUILD_EXAMPLES ${_HYLO_SAVED_BUILD_EXAMPLES})

# Utils module
file(GLOB_RECURSE Utils_FILES FOLLOW_SYMLINKS LIST_DIRECTORIES false CONFIGURE_DEPENDS Sources/Utils/*.swift)
add_library(Utils ${Utils_FILES})
target_link_libraries(Utils PRIVATE Algorithms BigInt Collections)

add_subdirectory(Sources)
# add_subdirectory(cmake/modules)
