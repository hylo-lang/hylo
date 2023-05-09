import PackagePlugin

@main
struct TestGeneratorPlugin: BuildToolPlugin {

  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputFiles = target.sourceFiles(withSuffix: "val")
    return try inputFiles.map { valFile in
      let swiftPath = context.pluginWorkDirectory.appending("TestVal_\(valFile.path.stem).swift")
      return .buildCommand(
        displayName: "Generating XCTestCase for \(valFile.path) into \(swiftPath)",
        executable: try context.tool(named: "GenerateTest").path,
        arguments: [ "\(target.name)_\(valFile.path.stem)_val", "-o", swiftPath ],
        inputFiles: [ valFile.path ],
        outputFiles: [ swiftPath ]
      )
    }
  }

}
