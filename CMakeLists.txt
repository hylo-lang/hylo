#
# Standard project boilerplate
#
cmake_minimum_required(VERSION 3.29)

# Workaround for https://github.com/apple/swift/issues/68947
cmake_policy(SET CMP0157 NEW)

project(Hylo
  VERSION 0.1.0
  DESCRIPTION "The Hylo programming language"
  HOMEPAGE_URL "https://hylo-lang.org"
  LANGUAGES C CXX Swift
)
enable_testing()
list(PREPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules)

set(CMAKE_POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS}) # Needed?
set(CMAKE_Swift_COMPILE_OPTIONS -warnings-as-errors)
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
