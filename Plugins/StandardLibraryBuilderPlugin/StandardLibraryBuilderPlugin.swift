import PackagePlugin

/// The Swift Package Manager plugin that builds the standard library.
@main
struct StandardLibraryBuilderPlugin: SPMBuildToolPlugin {

  func buildCommands(
    context: PackagePlugin.PluginContext, target: PackagePlugin.Target
  ) throws -> [SPMBuildCommand]
  {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputPaths = target.sourceFiles(withSuffix: ".hylo").map(\.path)
    let outputPath = context.pluginWorkDirectory

    if inputPaths.count != 0 { return [] }

    return [
      .buildCommand(
        displayName: "Building standard library module into .",
        executable: .targetInThisPackage("BuildStandardLibrary"),
        arguments: ["-o", outputPath.platformString, "-n", target.moduleName]
        + inputPaths.map(\.platformString),
      inputFiles: inputPaths,
      outputFiles: [outputPath])]
  }

}
