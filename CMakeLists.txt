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

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules)
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
FetchContent_MakeAvailable(BigInt)

FetchContent_Declare(Durian
  GIT_REPOSITORY https://github.com/hylo-lang/Durian.git
  GIT_TAG        1.2
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(Durian)

# FetchContent_Declare(SPIManifest
#   GIT_REPOSITORY https://github.com/SwiftPackageIndex/SPIManifest.git
#   GIT_TAG        v0.19.0
#   FIND_PACKAGE_ARGS CONFIG
# )
# FetchContent_MakeAvailable(SPIManifest)

FetchContent_Declare(SwiftAlgorithms
  GIT_REPOSITORY https://github.com/apple/swift-algorithms.git
  GIT_TAG        1.2.0
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(SwiftAlgorithms)

FetchContent_Declare(ArgumentParser
  GIT_REPOSITORY https://github.com/apple/swift-argument-parser.git
  GIT_TAG        1.2.2
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(ArgumentParser)

FetchContent_Declare(SwiftCollections
  GIT_REPOSITORY https://github.com/apple/swift-collections.git
  GIT_TAG        1.0.4
  FIND_PACKAGE_ARGS CONFIG
)
set(_HYLO_SAVED_CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH})
FetchContent_Populate(SwiftCollections)
list(APPEND CMAKE_MODULE_PATH ${swiftcollections_SOURCE_DIR}/cmake/modules)
FetchContent_MakeAvailable(SwiftCollections)
set(CMAKE_MODULE_PATH ${_HYLO_SAVED_CMAKE_MODULE_PATH})

FetchContent_Declare(SwiftFormat
  GIT_REPOSITORY https://github.com/apple/swift-format.git
  GIT_TAG        508.0.1
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(SwiftFormat)

FetchContent_Declare(swift-numerics
  GIT_REPOSITORY https://github.com/apple/swift-numerics.git
  GIT_TAG        1.0.2
  FIND_PACKAGE_ARGS CONFIG
)
set(_HYLO_SAVED_CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH})
FetchContent_Populate(swift-numerics)
list(APPEND CMAKE_MODULE_PATH ${swift-numerics_SOURCE_DIR}/cmake/modules)
FetchContent_MakeAvailable(swift-numerics)
set(CMAKE_MODULE_PATH ${_HYLO_SAVED_CMAKE_MODULE_PATH})

# FetchContent_Declare(SwiftSyntax
#   GIT_REPOSITORY https://github.com/apple/swift-syntax.git
#   GIT_TAG        main
#   FIND_PACKAGE_ARGS CONFIG
# )
# FetchContent_MakeAvailable(SwiftSyntax)

FetchContent_Declare(SwiftSystem
  GIT_REPOSITORY https://github.com/apple/swift-system.git
  GIT_TAG        1.1.1
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(SwiftSystem)

FetchContent_Declare(SwiftToolsSupportCore
  GIT_REPOSITORY https://github.com/apple/swift-tools-support-core.git
  GIT_TAG        0.5.2
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(SwiftToolsSupportCore)

FetchContent_Declare(SwiftyLLVM
  GIT_REPOSITORY https://github.com/hylo-lang/Swifty-LLVM.git
  GIT_TAG        main
  FIND_PACKAGE_ARGS CONFIG
)
FetchContent_MakeAvailable(SwiftyLLVM)

set(BUILD_TESTING ${_HYLO_SAVED_BUILD_TESTING})
set(BUILD_EXAMPLES ${_HYLO_SAVED_BUILD_EXAMPLES})

# Utils module
file(GLOB_RECURSE Utils_FILES FOLLOW_SYMLINKS LIST_DIRECTORIES false CONFIGURE_DEPENDS Sources/Utils/*.swift)
add_library(Utils ${Utils_FILES})
target_link_libraries(Utils private SwiftAlgorithms BigInt SwiftCollections)

add_subdirectory(Sources)
# add_subdirectory(cmake/modules)
