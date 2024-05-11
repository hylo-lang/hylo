if(NOT APPLE)
  find_package(GenerateSwiftXCTestMain)
endif()

find_package(SwiftXCTest)

add_hylo_library(TestUtils
  DEPENDENCIES FrontEnd Driver Utils SwiftXCTest
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

add_hylo_test_of(Driver NAMED EndToEndTests
  DEPENDENCIES TestUtils
  # TODO: why are these needed?
  Durian SwiftyLLVM)

add_hylo_test_of(Driver NAMED LibraryTests
  DEPENDENCIES TestUtils
  # TODO: why are these needed?
  Durian SwiftyLLVM)
