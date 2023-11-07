import PackagePlugin

/// The Swift Package Manager plugin that builds the standard library.
@main
struct StandardLibraryBuilderPlugin: PortableBuildToolPlugin {

  func portableBuildCommands(
    context: PackagePlugin.PluginContext, target: PackagePlugin.Target
  ) throws -> [PortableBuildCommand]
  {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputPaths = target.sourceFiles(withSuffix: ".hylo").map(\.path)
    let outputPath = context.pluginWorkDirectory

    if true {return []}
    return [
      .buildCommand(
        displayName: "Building standard library module into .",
        tool: .executableProduct(name: "BuildStandardLibrary"),
        arguments: ["-o", outputPath.platformString, "-n", target.moduleName]
        + inputPaths.map(\.platformString),
      inputFiles: inputPaths,
      outputFiles: [outputPath])]
  }

}
