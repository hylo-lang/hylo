#
# Standard project boilerplate
#
cmake_minimum_required(VERSION 3.30)
set(CMAKE_OSX_DEPLOYMENT_TARGET 15.0)
set(CMAKE_Swift_LANGUAGE_VERSION 6)

project(Hylo
  VERSION 0.1.0
  DESCRIPTION "The Hylo programming language"
  HOMEPAGE_URL "https://hylo-lang.org"
  LANGUAGES C CXX Swift
)
enable_testing()
include(CTest)

list(PREPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules)

set(CMAKE_POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS}) # Needed?
set(CMAKE_Swift_COMPILE_OPTIONS
  -warnings-as-errors --verbose)
set(CMAKE_STATIC_LIBRARY_PREFIX_Swift "lib") # https://forums.swift.org/t/cmake-swift-6-2-generates-object-files-with-libmytargetname-lib-on-windows/82271
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
