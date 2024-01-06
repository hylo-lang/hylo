import Foundation
import PackagePlugin

/// The Swift Package Manager plugin that builds the standard library.
@main
struct StandardLibraryBuilderPlugin: SPMBuildToolPlugin {

  func buildCommands(
    context: PackagePlugin.PluginContext, target: PackagePlugin.Target
  ) throws -> [SPMBuildCommand] {
    let sourceDirectory = URL(fileURLWithPath: #filePath)
      .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
      / "StandardLibrary" / "Sources"

    let hostedSources = FileManager.default.enumerator(
      at: sourceDirectory,
      includingPropertiesForKeys: [.isRegularFileKey],
      options: [.skipsHiddenFiles, .skipsPackageDescendants])!
      .compactMap { $0 as? URL }
      .filter { $0.pathExtension == "hylo" }

    let freestandingRoot = (sourceDirectory / "Core").pathComponents
    let freestandingSources = hostedSources.filter { $0.pathComponents.starts(with: freestandingRoot) }

    func buildLibrary(name: String, sources: [URL]) -> SPMBuildCommand {
      let output = context.pluginWorkDirectory.url / (name + ".cbor")
      return .buildCommand(
        displayName: "Building \(name) standard library module into \(output.platformString)",
        executable:  .targetInThisPackage("BuildStandardLibrary"),
        arguments: ["-o", output.platformString] + sources.map(\.platformString),
        inputFiles: sources.map(\.spmPath),
        outputFiles: [output.spmPath])
    }

    return [
      buildLibrary(name: "hosted", sources: hostedSources),
      buildLibrary(name: "freestanding", sources: freestandingSources)
    ]
  }

}
