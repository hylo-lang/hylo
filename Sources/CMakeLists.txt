include(HyloUtilities)

find_package(SwiftArgumentParser)
find_package(SwiftCollections)
find_package(SwiftAlgorithms)
find_package(Durian)
find_package(BigInt)
find_package(Swifty-LLVM)

# LLVM is built this way, and we must stay compatible with it.
# Unfortunately this can't be a usage requirement:
# https://gitlab.kitware.com/cmake/cmake/-/issues/23246
set(CMAKE_Swift_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)
set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)

# The compiler's executable target.
add_hylo_executable(hc DEPENDENCIES Driver
  # TODO: why are these needed?
  Durian SwiftyLLVM)

add_hylo_executable(hylo-demangle DEPENDENCIES IR
  # TODO: why are these needed?
  Durian SwiftyLLVM Collections)
set_target_properties(hylo-demangle PROPERTIES Swift_MODULE_NAME HyloDemangle)

add_hylo_library(Driver
  DEPENDENCIES CodeGenLLVM FrontEnd IR StandardLibrary ArgumentParser
  # TODO: why is this needed?
  Collections)

add_hylo_library(FrontEnd
  DEPENDENCIES BigInt OrderedCollections Durian Utils)

add_hylo_library(IR
  DEPENDENCIES FrontEnd Utils)

add_hylo_library(CodeGenLLVM
  DEPENDENCIES FrontEnd IR Utils SwiftyLLVM
  PATH CodeGen/LLVM)

add_hylo_library(Interpreter
  DEPENDENCIES FrontEnd IR Utils Collections)

add_hylo_library(Utils
  DEPENDENCIES BigInt DequeModule Algorithms)

add_hylo_library(StandardLibrary
  DEPENDENCIES FrontEnd Utils
  PATH ../StandardLibrary)

add_custom_target(output_compiler_version
  "$<TARGET_FILE:hc>" --version
  DEPENDS hc
  VERBATIM)
