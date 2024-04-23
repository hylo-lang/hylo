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

# set_unique_subdirs(output_variable common_prefix <paths>...)
#
# Given that <paths> are all of the form
# ${common_prefix}/<some/partial/path>/filename, sets output_variable to
# the unique values of <some/partial/path>.
function(set_unique_subdirs output_variable common_prefix)
  set(output)
  foreach(p ${ARGN})
    cmake_path(RELATIVE_PATH p BASE_DIRECTORY "${common_prefix}")
    cmake_path(REMOVE_FILENAME p)
    cmake_path(GET p PARENT_PATH p)
    list(APPEND output ${p})
  endforeach()
  list(REMOVE_DUPLICATES output)
  set("${output_variable}" "${output}" PARENT_SCOPE)
endfunction()

# paths_with_prefix(output_variable prefix <paths>...)
#
# Sets output_variable to the members of <paths> having the given
# prefix.
function(paths_with_prefix output_variable prefix)
  set(result)
  foreach(s ${ARGN})
    cmake_path(IS_PREFIX prefix "${s}" has_prefix)
    if(has_prefix)
      list(APPEND result "${s}")
    endif()
  endforeach()
  set("${output_variable}" "${result}" PARENT_SCOPE)
endfunction()

function(add_hylo_test_of testee)
  cmake_parse_arguments("" # <prefix>
    "" # <options>
    "PATH;NAMED" # <one_value_keywords>
    "DEPENDENCIES" # <multi_value_keywords>
    ${ARGN})

  set(top_target "${_NAMED}")
  if(NOT _PATH)
    set(_PATH ${top_target})
  endif()
  set_recursive_file_glob(swift_files ${_PATH}/*.swift)

  if(swift_files)
    add_swift_xctest(${top_target} ${testee} ${swift_files})
    target_link_libraries(${top_target} PRIVATE ${_DEPENDENCIES})
    set_property(TARGET ${top_target} APPEND
      PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)
  endif()

  set_recursive_file_glob(hylo_files "${_PATH}/*.hylo")
  set(absolute_subdir_prefix "${CMAKE_CURRENT_SOURCE_DIR}/${_PATH}")
  set_unique_subdirs(hylo_subdirs ${absolute_subdir_prefix} ${hylo_files})

  foreach(hylo_subdir ${hylo_subdirs})
    paths_with_prefix(subdir_files "${absolute_subdir_prefix}" ${hylo_files})

    string(REGEX REPLACE "[^A-Za-z0-9_]()|^([0-9])" "_\\1" target_fragment "${hylo_subdir}")

    set(hylo_test_target "${top_target}_${target_fragment}")
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
        ${subdir_files}
      DEPENDS ${subdir_files} GenerateHyloFileTests
      WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
      COMMENT "Generate Swift test file for ${_PATH}/${hylo_subdir}")

    add_swift_xctest("${hylo_test_target}" ${testee} ${generated_swift_file})
    target_link_libraries(${hylo_test_target} PRIVATE ${_DEPENDENCIES})
    set_property(TARGET ${hylo_test_target} APPEND
      PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)
  endforeach()

endfunction()
