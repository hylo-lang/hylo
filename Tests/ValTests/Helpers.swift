import Foundation
import Driver

struct UnwrapError: Error {

  let message: String

}

func unwrap<T>(_ value: T?, _ message: @autoclosure () -> String) throws -> T {
  if let value = value {
    return value
  } else {
    throw UnwrapError(message: message())
  }
}

func withTestCases(
  in subdirectory: String,
  _ action: (URL, inout Driver) throws -> [Diag]
) throws {
  let urls = try unwrap(
    Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: subdirectory),
    "No test case found")

  for url in urls {
    // Read the test case specification.
    var specParser = TestSpecParser()
    try specParser.scan(contentsOf: url)

    // Initialize the compiler driver.
    var driver = Driver()
    var checker = DiagChecker(context: driver.compiler, annotations: specParser.annotations)
    let handle = DiagDispatcher.instance.register(consumer: checker)

    // Run the test case.
    let diags = try action(url, &driver)

    // Compare the result of the test case with the specification.
    checker = DiagDispatcher.instance.unregister(consumer: handle) as! DiagChecker
    diags.forEach({ checker.consume($0) })
    checker.finalize()
  }
}
