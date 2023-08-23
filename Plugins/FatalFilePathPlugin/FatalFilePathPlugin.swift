import PackagePlugin

/// The Swift Package Manager plugin that generates XCTest cases for annotated ".hylo" files as
/// part of our build process.
@main
struct FatalFilePathPlugin: BuildToolPlugin {

  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let target = target as? SourceModuleTarget else { return [] }
    if !target.sourceFiles(withSuffix: "swift").contains(where: {_ in true}) { return [] }

    let outputPath = context.pluginWorkDirectory.appending("StandardLibraryFilePathOverrides.swift")

    let cmd = Command.buildCommand(
      displayName: "Generating \(outputPath)",
      executable: try context.tool(named: "GenerateFatalFilePathOverrides").path,
      arguments: ["-o", outputPath],
      inputFiles: [],
      outputFiles: [outputPath])

    return [cmd]
  }

}
