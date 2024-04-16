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

#
# The following should migrate into the above package when they are
# solid.
#

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
  set_recursive_file_glob(swift_files ${_PATH}/*.swift)

  add_swift_xctest(${result_target} ${testee} ${swift_files})
  target_link_libraries(${result_target} PRIVATE ${_DEPENDENCIES})
  set_property(TARGET ${result_target} APPEND
    PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)

  set_recursive_file_glob(hylo_files "${_PATH}/*.hylo")

  foreach(hylo_file ${hylo_files})

    cmake_path(
      RELATIVE_PATH hylo_file
      BASE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${_PATH}"
      OUTPUT_VARIABLE hylo_file_relative)

    string(REGEX REPLACE "[^A-Za-z0-9_]()|^([0-9])" "_\\1" target_fragment "${hylo_file_relative}")

    set(hylo_test_target "${result_target}_${target_fragment}")
    set(generated_swift_file "${CMAKE_CURRENT_BINARY_DIR}/${hylo_test_target}.swift")

    add_custom_command(
      OUTPUT ${generated_swift_file}
      # If the executable target depends on DLLs their directories need to be injected into the PATH
      # or they won't be found and the target will fail to run, so invoke it through cmake.  Because
      COMMAND
        ${CMAKE_COMMAND} -E env
        "PATH=$<SHELL_PATH:$<TARGET_RUNTIME_DLL_DIRS:GenerateHyloFileTests>;$ENV{PATH}>"
        --
        $<TARGET_FILE:GenerateHyloFileTests>
        -o "${generated_swift_file}"
        -n "${hylo_test_target}"
        "${hylo_file}"
      DEPENDS "${hylo_file}" GenerateHyloFileTests
      WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
      COMMENT "Generate Swift test file from ${hylo_file}")
    add_swift_xctest("${hylo_test_target}" ${testee} ${generated_swift_file})
    target_link_libraries(${hylo_test_target} PRIVATE ${_DEPENDENCIES})
    set_property(TARGET ${hylo_test_target} APPEND
      PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)
  endforeach()

endfunction()
