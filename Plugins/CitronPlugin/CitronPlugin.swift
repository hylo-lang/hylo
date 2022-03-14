import PackagePlugin

@main struct CitronPlugin: BuildToolPlugin {
    
  /// Instantiates the plugin. This happens once per invocation of the
  /// plugin; there is no facility for keeping in-memory state from one
  /// invocation to the next. Most plugins do not need to implement the
  /// initializer.
  init() {}

  /// Invoked by SwiftPM to create build commands for a particular target.
  /// The context parameter contains information about the package and its
  /// dependencies, as well as other environmental inputs.
  ///
  /// This function should create and return build commands or prebuild
  /// commands, configured based on the information in the context.
  func createBuildCommands(context: PluginContext, target: Target) async throws -> [Command] {
    return []
//        return [.buildCommand(displayName: "Citron", executable: .init(""), arguments: <#T##[CustomStringConvertible]#>)]
  }
}
