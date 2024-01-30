import Foundation
import PackagePlugin

/// The Swift Package Manager plugin that generates XCTest cases for annotated ".hylo" files as
/// part of our build process.
@main
struct TestDiscoveryPlugin: SPMBuildToolPlugin {

  func buildCommands(
    context: PackagePlugin.PluginContext, target: PackagePlugin.Target
  ) throws -> [SPMBuildCommand]
  {
    guard let target = target as? SourceModuleTarget else { return [] }
    let sourcesToScrape = target.sourceFiles(withSuffix: ".swift").map(\.path)
    let outputPath = context.pluginWorkDirectory.appending("main.swift")

    return [
      .buildCommand(
      displayName: "Generating XCTestCase invocations \(outputPath)",
      executable: .targetInThisPackage("GenerateTestInvocations"),
      arguments: ["-o", outputPath.platformString] + sourcesToScrape.map(\.platformString),
      inputFiles: sourcesToScrape,
      outputFiles: [outputPath])]
  }

}
