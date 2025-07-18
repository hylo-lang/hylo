#
# Standard project boilerplate
#
cmake_minimum_required(VERSION 3.30)
set(CMAKE_OSX_DEPLOYMENT_TARGET 14.0)

# Set the SDK path on macOS to ensure Swift can find system headers like tgmath.h
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(NOT CMAKE_OSX_SYSROOT)
    execute_process(
      COMMAND xcrun --show-sdk-path
      OUTPUT_VARIABLE SDK_PATH
      OUTPUT_STRIP_TRAILING_WHITESPACE
      RESULT_VARIABLE SDK_PATH_RESULT
    )
    if(SDK_PATH_RESULT EQUAL 0)
      set(CMAKE_OSX_SYSROOT ${SDK_PATH})
      message(STATUS "Setting CMAKE_OSX_SYSROOT to ${CMAKE_OSX_SYSROOT}")
    else()
      message(WARNING "Could not determine SDK path using xcrun --show-sdk-path")
    endif()
  endif()
endif()

project(Hylo
  VERSION 0.1.0
  DESCRIPTION "The Hylo programming language"
  HOMEPAGE_URL "https://hylo-lang.org"
  LANGUAGES C CXX Swift
)
enable_testing()
include(CTest)

list(PREPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules)
set(CMAKE_Swift_LANGUAGE_VERSION 5)

set(CMAKE_POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS}) # Needed?
set(CMAKE_Swift_COMPILE_OPTIONS
  -warnings-as-errors)
if(BUILD_TESTING)
  list(APPEND CMAKE_Swift_COMPILE_OPTIONS -enable-testing)
endif()

if(BUILD_TESTING)
  # We need to work something out, probably a DYLD_LIBRARY_PATH
  # setting, before shared libs can be tested.
  set(BUILD_SHARED_LIBS NO CACHE BOOL "Build shared libraries by default" FORCE)
endif()

if (PROJECT_IS_TOP_LEVEL)
  include(cmake/TopLevelDefaults.cmake)
endif()

add_subdirectory(Sources)

if(BUILD_TESTING)
  add_subdirectory(Tests)
endif()
