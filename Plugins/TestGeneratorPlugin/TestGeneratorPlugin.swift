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
    let inputPaths = target.sourceFiles(withSuffix: ".hylo").map(\.path)
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
