import Foundation
import PackagePlugin

/// The Swift Package Manager plugin that generates XCTest cases for annotated ".hylo" files as
/// part of our build process.
@main
struct TestGeneratorPlugin: SPMBuildToolPlugin {

  func buildCommands(
    context: PackagePlugin.PluginContext, target: PackagePlugin.Target
  ) throws -> [SPMBuildCommand]
  {
    guard let target = target as? SourceModuleTarget else { return [] }

    let testCases = (target.directory.url / "TestCases").platformString

    let inputPaths = try FileManager.default.subpathsOfDirectory(atPath: testCases)
      .filter { $0.hasSuffix(".hylo") }.map { Path($0).repaired }

    let outputPath = context.pluginWorkDirectory.appending("HyloFileTests.swift")

    try """
    -----------
    testCases: \(testCases)
    inputPaths: \(inputPaths)
    outputPath: \(outputPath)
    -----------
    """.appendToURL(fileURL: .init(fileURLWithPath: "/tmp/testgen-log.txt"))

    return [
      .buildCommand(
      displayName: "Generating XCTestCases into \(outputPath)",
      executable: .targetInThisPackage("GenerateHyloFileTests"),
      arguments: ["-o", outputPath.platformString, "-n", target.moduleName]
        + inputPaths.map(\.platformString) + ["ARGS"],
      inputFiles: inputPaths,
      outputFiles: [outputPath])]
  }

}

extension String {
  func appendLineToURL(fileURL: URL) throws {
    try (self + "\n").appendToURL(fileURL: fileURL)
  }

  func appendToURL(fileURL: URL) throws {
    let data = self.data(using: String.Encoding.utf8)!
    try data.append(fileURL: fileURL)
  }
}

extension Data {
  func append(fileURL: URL) throws {
    if let fileHandle = FileHandle(forWritingAtPath: fileURL.path) {
      defer {
        fileHandle.closeFile()
      }
      fileHandle.seekToEndOfFile()
      fileHandle.write(self)
    }
    else {
      try write(to: fileURL, options: .atomic)
    }
  }
}
