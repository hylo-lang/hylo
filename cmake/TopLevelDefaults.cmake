# Commands used to create an easy development environment when
# building this project directly (not as a dependency).

# Without this, generated Xcode projects aren't debuggable.
set(CMAKE_XCODE_GENERATE_SCHEME YES)

#
# Hylo-standard dependency resolution.
#
include(FetchContent)

block()

  set(FETCHCONTENT_TRY_FIND_PACKAGE_MODE NEVER)
  FetchContent_Declare(Hylo-CMakeModules
    GIT_REPOSITORY https://github.com/hylo-lang/CMakeModules.git
    GIT_TAG        main
    OVERRIDE_FIND_PACKAGE
  )

endblock()
FetchContent_MakeAvailable(Hylo-CMakeModules)

list(PREPEND CMAKE_MODULE_PATH ${hylo-cmakemodules_SOURCE_DIR})

function(add_hylo_executable result_target)
  cmake_parse_arguments("" # <prefix>
    "" # <options>
    "PATH" # <one_value_keywords>
    "DEPENDENCIES" # <multi_value_keywords>
    ${ARGN})
  if(NOT _PATH)
    set(_PATH ${result_target})
  endif()
  set_recursive_file_glob(files ${_PATH}/*.swift)
  add_executable(${result_target} ${files})
  target_link_libraries(${result_target} PRIVATE ${_DEPENDENCIES})

  set_property(TARGET ${result_target} APPEND
    PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)
endfunction()

function(add_hylo_library result_target)
  cmake_parse_arguments("" # <prefix>
    "" # <options>
    "PATH" # <one_value_keywords>
    "DEPENDENCIES" # <multi_value_keywords>
    ${ARGN})
  if(NOT _PATH)
    set(_PATH ${result_target})
  endif()
  set_recursive_file_glob(files ${_PATH}/*.swift)
  add_library(${result_target} ${files})
  target_link_libraries(${result_target} ${_DEPENDENCIES})

  set_property(TARGET ${result_target} APPEND
    PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)

  target_compile_options(${result_target}
    PRIVATE $<$<BOOL:${BUILD_TESTING}>:-enable-testing>)

  set_target_properties(${result_target} PROPERTIES
    # This is required in order to be an XCTest testee.
    FRAMEWORK $<BOOL:${BUILD_TESTING}>)
endfunction()

function(add_hylo_test_of testee)
  cmake_parse_arguments("" # <prefix>
    "" # <options>
    "PATH;NAMED" # <one_value_keywords>
    "DEPENDENCIES" # <multi_value_keywords>
    ${ARGN})

  set(result_target "${_NAMED}")
  if(NOT _PATH)
    set(_PATH ${result_target})
  endif()
  set_recursive_file_glob(files ${_PATH}/*.swift)
  add_swift_xctest(${result_target} ${testee} ${files})
  target_link_libraries(${result_target} PRIVATE ${_DEPENDENCIES})

  set_property(TARGET ${result_target} APPEND
    PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)
endfunction()
