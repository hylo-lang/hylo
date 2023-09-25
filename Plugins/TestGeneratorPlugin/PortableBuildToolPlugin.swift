import PackagePlugin
import Foundation
#if os(Windows)
import WinSDK
#endif

#if os(Windows)
/// The name of the environment variable containing the executable search path.
fileprivate let pathEnvironmentVariable = "Path"
/// The separator between elements of the executable search path.
fileprivate let pathEnvironmentSeparator: Character = ";"
/// The file extension applied to binary executables
fileprivate let executableSuffix = ".exe"

fileprivate extension URL {

  /// Returns the URL given by removing all the elements of `suffix`
  /// from the tail of `pathComponents`, or` `nil` if `suffix` is not
  /// a suffix of `pathComponents`.
  func sansPathComponentSuffix<Suffix: BidirectionalCollection<String>>(_ suffix: Suffix)
    -> URL?
  {
    var r = self
    var remainingSuffix = suffix[...]
    while let x = remainingSuffix.popLast() {
      if r.lastPathComponent != x { return nil }
      r.deleteLastPathComponent()
    }
    return r
  }

}

fileprivate extension PackagePlugin.Target {

  /// The source files.
  var allSourceFiles: [URL] {
    return (self as? PackagePlugin.SourceModuleTarget)?.sourceFiles(withSuffix: "").map(\.path.url) ?? []
  }

}

fileprivate extension PackagePlugin.Package {

  /// The source files in this package on which the given executable depends.
  func sourceDependencies(ofProductNamed productName: String) throws -> [URL] {
    var result: Set<URL> = []
    let p = products.first { $0.name == productName }!
    var visitedTargets = Set<PackagePlugin.Target.ID>()

    for t0 in p.targets {
      if visitedTargets.insert(t0.id).inserted {
        result.formUnion(t0.allSourceFiles)
      }

      for t1 in t0.recursiveTargetDependencies {
        if visitedTargets.insert(t1.id).inserted {
          result.formUnion(t1.allSourceFiles)
        }
      }
    }
    return Array(result)
  }

}
#endif

// Workarounds for SPM's buggy `Path` type on Windows.
//
// SPM `PackagePlugin.Path` uses a representation that—if not repaired before used by a
// `BuildToolPlugin` on Windows—will cause files not to be found.
public extension Path {

  /// A string representation appropriate to the platform.
  var platformString: String {
    #if os(Windows)
    string.withCString(encodedAs: UTF16.self) { pwszPath in
      // Allocate a buffer for the repaired UTF-16.
      let bufferSize = Int(GetFullPathNameW(pwszPath, 0, nil, nil))
      var buffer = Array<UTF16.CodeUnit>(repeating: 0, count: bufferSize)
      // Actually do the repair
      _ = GetFullPathNameW(pwszPath, DWORD(bufferSize), &buffer, nil)
      // Drop the zero terminator and convert back to a Swift string.
      return String(decoding: buffer.dropLast(), as: UTF16.self)
    }
    #else
    string
    #endif
  }

  /// A `URL` referring to the same location.
  var url: URL { URL(fileURLWithPath: platformString) }

  /// A representation of `Self` that works on all platforms.
  var portable: Self {
    #if os(Windows)
    Path(self.platformString)
    #else
    self
    #endif
  }
}

public extension URL {

  /// A Swift Package Manager-compatible representation.
  var spmPath: Path { Path(self.path) }

  /// Returns `self` with the relative file path `suffix` appended.
  ///
  /// This is a portable version of `self.appending(path:)`, which is only available on recent
  /// macOSes.
  func appendingPath(_ suffix: String) -> URL {

#if os(macOS)
    if #available(macOS 13.0, *) { return self.appending(path: suffix) }
#endif

    return (suffix as NSString).pathComponents
      .reduce(into: self) { $0.appendPathComponent($1) }
  }

}

/// Defines functionality for all plugins having a `buildTool` capability.
public protocol PortableBuildToolPlugin: BuildToolPlugin {

  /// Returns the build commands for `target` in `context`.
  func portableBuildCommands(
    context: PackagePlugin.PluginContext,
    target: PackagePlugin.Target
  ) async throws -> [PortableBuildCommand]

}

extension PortableBuildToolPlugin {

  public func createBuildCommands(context: PluginContext, target: Target) async throws
    -> [PackagePlugin.Command]
  {

    return try await portableBuildCommands(context: context, target: target).map {
      try $0.spmCommand(in: context)
    }

  }

}

public extension PortableBuildCommand.Tool {

  /// A partial translation to SPM plugin inputs of an invocation.
  struct SPMInvocation {
    /// The executable that will actually run.
    let executable: PackagePlugin.Path
    /// The command-line arguments that must precede the ones specified by the caller.
    let argumentPrefix: [String]
    /// The source files that must be added as build dependencies if we want the tool
    /// to be re-run when its sources change.
    let additionalSources: [URL]
  }

  fileprivate func spmInvocation(in context: PackagePlugin.PluginContext) throws -> SPMInvocation {
    switch self {
    case .preInstalled(file: let pathToExecutable):
      return .init(executable: pathToExecutable.portable, argumentPrefix: [], additionalSources: [])

    case .executableProduct(name: let productName):
      #if !os(Windows)
      return try .init(
        executable: context.tool(named: productName).path.portable,
        argumentPrefix: [], additionalSources: [])
      #else
      // Instead of depending on context.tool(named:), which demands a declared dependency on the
      // tool, which causes link errors on Windows
      // (https://github.com/apple/swift-package-manager/issues/6859#issuecomment-1720371716),
      // Invoke swift reentrantly to run the GenerateResoure tool.

      //
      // If a likely candidate for the current toolchain can be found in the `Path`, prepend its
      // `bin/` dfirectory.
      //
      var searchPath = ProcessInfo.processInfo.environment[pathEnvironmentVariable]!
        .split(separator: pathEnvironmentSeparator).map { URL(fileURLWithPath: String($0)) }

      // SwiftPM seems to put a descendant of the currently-running Swift Toolchain/ directory having
      // this component suffix into the executable search path when plugins are run.
      let pluginAPISuffix = ["lib", "swift", "pm", "PluginAPI"]

      if let p = searchPath.lazy.compactMap({ $0.sansPathComponentSuffix(pluginAPISuffix) }).first {
        searchPath = [ p.appendingPathComponent("bin") ] + searchPath
      }

      // Try searchPath first, then fall back to SPM's `tool(named:)`
      let swift = try searchPath.lazy.map { $0.appendingPathComponent("swift" + executableSuffix) }
        .first { FileManager().isExecutableFile(atPath: $0.path) }
        ?? context.tool(named: "swift").path.url

      let scratchPath = FileManager().temporaryDirectory
        .appendingPathComponent(UUID().uuidString).path

      return .init(
        executable: swift.spmPath,
        argumentPrefix: [
          "run",
          // Only Macs currently use sandboxing, but nested sandboxes are prohibited, so for future
          // resilience in case Windows gets a sandbox, disable it on these reentrant builds.
          //
          // Currently if we run this code on a Mac, disabling the sandbox on this inner build is
          // enough to allow us to write on the scratchPath, which is outside any _outer_ sandbox.
          // I think that's an SPM bug. If they fix it, we'll need to nest scratchPath in
          // context.workDirectory and add an explicit build step to delete it to keep its contents
          // from being incorporated into the resources of the target we're building.
          "--disable-sandbox",
          "--scratch-path", scratchPath,
          "--package-path", context.package.directory.url.path,
          productName ],
        additionalSources:
          try context.package.sourceDependencies(ofProductNamed: productName))
      #endif
    }
  }
}

fileprivate extension PortableBuildCommand {

  /// Returns a representation of `self` for the result of a `BuildToolPlugin.createBuildCommands`
  /// invocation with the given `context` parameter.
  func spmCommand(in context: PackagePlugin.PluginContext) throws -> PackagePlugin.Command {

    switch self {
    case .buildCommand(
           displayName: let displayName,
           tool: let tool,
           arguments: let arguments,
           environment: let environment,
           inputFiles: let inputFiles,
           outputFiles: let outputFiles,
           pluginSourceFile: let pluginSourceFile):

      let i = try tool.spmInvocation(in: context)

      /// Guess at files that constitute this plugin, the changing of which should cause outputs to be
      /// regenerated (workaround for https://github.com/apple/swift-package-manager/issues/6936).
      let pluginSourceDirectory = URL(fileURLWithPath: pluginSourceFile).deletingLastPathComponent()

      // We could filter out directories, but why bother?
      let pluginSources = try FileManager()
        .subpathsOfDirectory(atPath: pluginSourceDirectory.path)
        .map { pluginSourceDirectory.appendingPath($0) }

      return .buildCommand(
        displayName: displayName,
        executable: i.executable,
        arguments: i.argumentPrefix + arguments,
        environment: environment,
        inputFiles: inputFiles.map(\.portable) + (pluginSources + i.additionalSources).map(\.spmPath),
        outputFiles: outputFiles.map(\.portable))

    case .prebuildCommand(
           displayName: let displayName,
           tool: let tool,
           arguments: let arguments,
           environment: let environment,
           outputFilesDirectory: let outputFilesDirectory):

      let i = try tool.spmInvocation(in: context)

      return .prebuildCommand(
        displayName: displayName,
        executable: i.executable,
        arguments: i.argumentPrefix + arguments,
        environment: environment,
        outputFilesDirectory: outputFilesDirectory.portable)
    }
  }

}


/// A command to run during the build.
public enum PortableBuildCommand {

  /// A command-line tool to be invoked.
  public enum Tool {

    /// The executable product named `name` in this package
    case executableProduct(name: String)

    /// The executable at `file`, an absolute path outside the build directory of the package being
    /// built.
    case preInstalled(file: PackagePlugin.Path)
  }

  /// A command that runs when any of its output files are needed by
  /// the build, but out-of-date.
  ///
  /// An output file is out-of-date if it doesn't exist, or if any
  /// input files have changed since the command was last run.
  ///
  /// - Note: the paths in the list of output files may depend on the list of
  ///   input file paths, but **must not** depend on reading the contents of
  ///   any input files. Such cases must be handled using a `prebuildCommand`.
  ///
  /// - Parameters:
  ///   - displayName: An optional string to show in build logs and other
  ///     status areas.
  ///   - tool: The command-line tool invoked to build the output files.
  ///   - arguments: Command-line arguments to be passed to the tool.
  ///   - environment: Environment variable assignments visible to the
  ///     tool.
  ///   - inputFiles: Files on which the contents of output files may depend.
  ///     Any paths passed as `arguments` should typically be passed here as
  ///     well.
  ///   - outputFiles: Files to be generated or updated by the tool.
  ///     Any files recognizable by their extension as source files
  ///     (e.g. `.swift`) are compiled into the target for which this command
  ///     was generated as if in its source directory; other files are treated
  ///     as resources as if explicitly listed in `Package.swift` using
  ///     `.process(...)`.
  ///   - pluginSourceFile: the path to a source file of the PortableBuildToolPlugin; allow the
  ///     default to take effect.
  case buildCommand(
        displayName: String?,
        tool: Tool,
        arguments: [String],
        environment: [String: String] = [:],
        inputFiles: [Path] = [],
        outputFiles: [Path] = [],
        pluginSourceFile: String = #filePath
       )

  /// A command that runs unconditionally before every build.
  ///
  /// Prebuild commands can have a significant performance impact
  /// and should only be used when there would be no way to know the
  /// list of output file paths without first reading the contents
  /// of one or more input files. Typically there is no way to
  /// determine this list without first running the command, so
  /// instead of encoding that list, the caller supplies an
  /// `outputFilesDirectory` parameter, and all files in that
  /// directory after the command runs are treated as output files.
  ///
  /// - Parameters:
  ///   - displayName: An optional string to show in build logs and other
  ///     status areas.
  ///   - tool: The command-line tool invoked to build the output files.
  ///   - arguments: Command-line arguments to be passed to the tool.
  ///   - environment: Environment variable assignments visible to the tool.
  ///   - workingDirectory: Optional initial working directory when the tool
  ///     runs.
  ///   - outputFilesDirectory: A directory into which the command writes its
  ///     output files.  Any files there recognizable by their extension as
  ///     source files (e.g. `.swift`) are compiled into the target for which
  ///     this command was generated as if in its source directory; other
  ///     files are treated as resources as if explicitly listed in
  ///     `Package.swift` using `.process(...)`.
  case prebuildCommand(
         displayName: String?,
         tool: Tool,
         arguments: [String],
         environment: [String: String] = [:],
         outputFilesDirectory: Path)

}
