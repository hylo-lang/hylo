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
  _ action: (SourceFile, inout Driver) throws -> [Diag]
) throws {
  let urls = try unwrap(
    Bundle.module.urls(forResourcesWithExtension: "val", subdirectory: subdirectory),
    "No test case found")

  for url in urls {
    // Read the test case specification.
    let source = try SourceFile(url: url)
    try withTestCase(source, action)
  }
}

func withTestCase(
  _ source: SourceFile,
  _ action: (SourceFile, inout Driver) throws -> [Diag]
) rethrows {
  // Read the test case specification.
  var specParser = TestSpecParser()
  specParser.scan(source)

  // Initialize the compiler driver.
  var driver = Driver()
  var checker = DiagChecker(context: driver.compiler, annotations: specParser.annotations)
  let handle = DiagDispatcher.instance.register(consumer: checker)

  // Run the test case.
  let diags = try action(source, &driver)

  // Compare the result of the test case with the specification.
  checker = DiagDispatcher.instance.unregister(consumer: handle) as! DiagChecker
  diags.forEach({ checker.consume($0) })
  checker.finalize()
}
