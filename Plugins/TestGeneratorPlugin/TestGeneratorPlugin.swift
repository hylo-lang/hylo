import PackagePlugin

/// The Swift Package Manager plugin that generates XCTest cases for annotated ".hylo" files as
/// part of our build process.
@main
struct TestGeneratorPlugin: BuildToolPlugin {

  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputPaths = target.sourceFiles(withSuffix: "hylo").map(\.path)
    let outputPath = context.pluginWorkDirectory.appending("HyloFileTests.swift")

    let cmd = Command.buildCommand(
      displayName: "Generating XCTestCases into \(outputPath)",
      executable: try context.tool(named: "GenerateHyloFileTests").path,
      arguments: ["-o", outputPath, "-n", target.moduleName] + inputPaths,
      inputFiles: inputPaths,
      outputFiles: [outputPath])
    return [cmd]
  }

}
