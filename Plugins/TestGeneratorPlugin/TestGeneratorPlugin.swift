import PackagePlugin

/// The Swift Package Manager plugin that generates XCTest cases for annotated .val files as part of
/// our build process.
@main
struct TestGeneratorPlugin: BuildToolPlugin {

  func createBuildCommands(context: PluginContext, target: Target) throws -> [Command] {
    guard let target = target as? SourceModuleTarget else { return [] }
    let inputPaths = target.sourceFiles(withSuffix: "val").map(\.path)
    let outputPath = context.pluginWorkDirectory.appending("ValFileTests.swift")

    let cmd = Command.buildCommand(
        displayName: "Generating XCTestCases for \(inputPaths.map(\.stem)) into \(outputPath)",
        executable: try context.tool(named: "GenerateValFileTests").path,
        arguments: [ "-o", outputPath, "-n", target.moduleName ] + inputPaths,
        inputFiles: inputPaths,
        outputFiles: [ outputPath ]
    )
    return [cmd]
  }

}
