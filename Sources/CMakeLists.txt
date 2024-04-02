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

set(CMAKE_POSITION_INDEPENDENT_CODE ${BUILD_SHARED_LIBS}) # Needed?
set(CMAKE_Swift_COMPILE_OPTIONS -warnings-as-errors)

# The compiler's executable target.
# add_hylo_executable(hc DEPENDENCIES Driver)
# add_hylo_executable(hylo-demangle DEPENDENCIES IR)

# add_hylo_library(Driver
#   DEPENDENCIES CodeGenLLVM FrontEnd IR StandardLibrary SwiftArgumentParser)

# add_hylo_library(FrontEnd
#   DEPENDENCIES BigInt Collections Durian SwiftyLLVM Utils)

# add_hylo_library(IR
#   DEPENDENCIES FrontEnd Utils)

# add_hylo_library(CodeGenLLVM
#   DEPENDENCIES FrontEnd IR Utils SwiftyLLVM
#   PATH CodeGen/LLVM)

add_hylo_library(Utils
  DEPENDENCIES BigInt Collections Algorithms)

# add_hylo_library(StandardLibrary
#   DEPENDENCIES FrontEnd Utils
#   PATH ../StandardLibrary)
