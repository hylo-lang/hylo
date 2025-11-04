if(NOT APPLE)
  find_package(GenerateSwiftXCTestMain)
endif()

find_package(SwiftXCTest)
include(FetchContent)
FetchContent_Declare(SwiftTesting
  GIT_REPOSITORY https://github.com/swiftlang/swift-testing.git
  GIT_TAG main)
FetchContent_MakeAvailable(SwiftTesting)

add_hylo_library(TestUtils
  DEPENDENCIES FrontEnd Driver Utils SwiftXCTest SwiftTesting
  # TODO: why is this needed?
  SwiftyLLVM
  PATH ../Sources/TestUtils
)

add_hylo_executable(GenerateHyloFileTests
  DEPENDENCIES ArgumentParser Utils
  PATH ../Sources/GenerateHyloFileTests
)

add_hylo_test_of(Utils NAMED UtilsTests
  DEPENDENCIES Algorithms DequeModule)

add_hylo_test_of(Driver NAMED DriverTests
  DEPENDENCIES TestUtils
  # TODO: why are these needed?
  Durian SwiftyLLVM)

add_hylo_test_of(IR NAMED ManglingTests
  DEPENDENCIES FrontEnd TestUtils StandardLibrary)

add_hylo_test_of(FrontEnd NAMED HyloTests
  DEPENDENCIES IR TestUtils StandardLibrary Utils Algorithms)

add_hylo_test_of(Interpreter NAMED InterpreterTests
  DEPENDENCIES Interpreter FrontEnd IR TestUtils
  # TODO: why is this needed?
  Collections
)

add_hylo_test_of(Driver NAMED EndToEndTests
  DEPENDENCIES TestUtils
  # TODO: why are these needed?
  Durian SwiftyLLVM)

add_hylo_test_of(Driver NAMED LibraryTests
  DEPENDENCIES TestUtils
  # TODO: why are these needed?
  Durian SwiftyLLVM)
