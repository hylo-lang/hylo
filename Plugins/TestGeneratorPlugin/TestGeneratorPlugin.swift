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

    // Using target.sourceFiles(withSuffix: ".hylo").map(\.path) as inputFiles creates noisy
    // warnings about unused sources.  Instead, find the input files relative to the target
    // directory and exclude them as sources in Package.swift.
    let testCases = target.directory.url / "TestCases"
    let inputPaths = try FileManager.default.subpathsOfDirectory(atPath: testCases.platformString)
      .filter { $0.hasSuffix(".hylo") }.map { (testCases/$0).spmPath }

    let outputPath = context.pluginWorkDirectory.appending("HyloFileTests.swift")

    return [
      .buildCommand(
      displayName: "Generating XCTestCases into \(outputPath)",
      executable: .targetInThisPackage("GenerateHyloFileTests"),
      arguments: ["-o", outputPath.platformString, "-n", target.moduleName]
        + inputPaths.map(\.platformString),
      inputFiles: inputPaths,
      outputFiles: [outputPath])]
  }

}
