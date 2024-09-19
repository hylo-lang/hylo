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
    GIT_TAG        6577fca
    OVERRIDE_FIND_PACKAGE
  )

endblock()
FetchContent_MakeAvailable(Hylo-CMakeModules)

list(PREPEND CMAKE_MODULE_PATH ${hylo-cmakemodules_SOURCE_DIR})
#
# The following functions should migrate into Hylo-CMakeModules when they are
# solid.
#

# Applies setup steps used across hylo targets.
function(hylo_common_target_setup target)
  set_property(TARGET "${target}" APPEND
    PROPERTY CMAKE_Swift_FLAGS -warnings-as-errors)

  # On Windows, increase the stack size of executables; we do a lot of
  # tail recursion and it doesn't get optimized away in debug builds.
  #
  # TODO: we might want to figure out how to turn this increase off
  # when optimization is on.  That might be nontrivial for
  # multi-config generators.
  if(WIN32) # Means target system is Windows
    get_property(target_type TARGET "${target}" PROPERTY TYPE)
    if(target_type STREQUAL "EXECUTABLE")
      add_custom_command(
        TARGET "${target}"
        POST_BUILD
        COMMAND editbin.exe "$<TARGET_FILE:${target}>"
          /STACK:2097152 # Double the 1M default.
        VERBATIM)
    endif()
  endif()
endfunction()

# add_hylo_executable(<target-name>
#   [PATH <swift-source-root>]
#   [DEPENDENCIES <library-dependencies>])
#
# Adds an executable target called <target-name> composed of the
# ``.swift`` files in <swift-source-root> and its subdirectories (or in
# <target-name>/ and subdirectories if PATH is omitted), linking to
# the targets named in DEPENDENCIES.
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

  hylo_common_target_setup(${result_target})
endfunction()

# add_hylo_library(<target-name>
#   [PATH <swift-source-root>]
#   [DEPENDENCIES <library-dependencies>])
#
# Adds a library target called <target-name> composed of the
# ``.swift`` files in <swift-source-root> and its subdirectories (or in
# <target-name>/ and subdirectories if PATH is omitted), linking to
# the targets named in DEPENDENCIES.
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

  target_compile_options(${result_target}
    PRIVATE $<$<BOOL:${BUILD_TESTING}>:-enable-testing>)

  set_target_properties(${result_target} PROPERTIES
    # This is required in order to be an XCTest testee.
    FRAMEWORK $<BOOL:${BUILD_TESTING}>)

  hylo_common_target_setup(${result_target})
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

# paths_in_directory(output_variable directory <paths>...)
#
# Sets output_variable to the members of <paths> whose directory is
# <directory>.
function(paths_in_directory output_variable directory)
  set(result)
  foreach(s ${ARGN})
    cmake_path(GET s PARENT_PATH d)
    if(d PATH_EQUAL directory)
      list(APPEND result "${s}")
    endif()
  endforeach()
  set("${output_variable}" "${result}" PARENT_SCOPE)
endfunction()

# add_hylo_test_of(<testee>
#   NAMED <target-name>
#   [PATH <swift-source-root>]
#   [DEPENDENCIES <library-dependencies>])
#
# Using a source root directory <swift-source-root> (or
# <target-name>/ if PATH is omitted):
#
# - If any ``.swift`` files are found under the source root (recursively),
#   creates a test target from them called <target-name>, testing <testee>.
# - For each subdirectory of the source root containing ``.hylo``
#   files, creates a test target that runs the annotated hylo file
#   tests in that subdirectory.
#
# All created tests are linked to <library-dependencies>.
function(add_hylo_test_of testee)
  cmake_parse_arguments("" # <prefix>
    "" # <options>
    "PATH;NAMED" # <one_value_keywords>
    "DEPENDENCIES" # <multi_value_keywords>
    ${ARGN})

  # FIXME: Why this should be needed is a mystery.
  list(APPEND _DEPENDENCIES ArgumentParser)

  set(top_target "${_NAMED}")
  if(NOT _PATH)
    set(_PATH ${top_target})
  endif()
  set_recursive_file_glob(swift_files ${_PATH}/*.swift)

  if(swift_files)
    add_swift_xctest("${top_target}" ${testee} ${swift_files})
    target_link_libraries("${top_target}" PRIVATE ${_DEPENDENCIES})
    hylo_common_target_setup("${top_target}")
  endif()

  set_recursive_file_glob(hylo_files "${_PATH}/*.hylo")
  set(absolute_subdir_prefix "${CMAKE_CURRENT_SOURCE_DIR}/${_PATH}")
  set_unique_subdirs(hylo_subdirs ${absolute_subdir_prefix} ${hylo_files})

  foreach(hylo_subdir ${hylo_subdirs})
    paths_in_directory(subdir_files "${absolute_subdir_prefix}/${hylo_subdir}" ${hylo_files})

    string(REGEX REPLACE "[^A-Za-z0-9_]()|^([0-9])" "_\\1" target_fragment "${hylo_subdir}")

    set(maximum_batch_size 10)
    list(LENGTH subdir_files file_count)

    math(EXPR batch_count "(${file_count} + ${maximum_batch_size} - 1) / ${maximum_batch_size}")
    math(EXPR max_batch "${batch_count} - 1")

    foreach(batch RANGE ${max_batch})

      if(batch_count EQUAL 1)
        set(batch_suffix)
      else()
        set(batch_suffix "_${batch}")
      endif()

      set(hylo_test_target "${top_target}_${target_fragment}${batch_suffix}")
      set(generated_swift_file "${CMAKE_CURRENT_BINARY_DIR}/${hylo_test_target}.swift")
      math(EXPR batch_start "${batch} * ${file_count} / ${batch_count}")
      math(EXPR batch_end "(${batch} + 1) * ${file_count} / ${batch_count}")
      math(EXPR batch_size "${batch_end} - ${batch_start}")

      list(SUBLIST subdir_files ${batch_start} ${batch_size} batch_files)

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
          ${batch_files}
        DEPENDS ${batch_files} GenerateHyloFileTests
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        COMMENT "Generate Swift test file ${batch} for ${_PATH}/${hylo_subdir}")

      add_swift_xctest("${hylo_test_target}" ${testee} ${generated_swift_file})
      target_link_libraries(${hylo_test_target} PRIVATE ${_DEPENDENCIES})
      hylo_common_target_setup(${hylo_test_target})

      endforeach()
  endforeach()

endfunction()
