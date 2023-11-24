/// Describes the compiler; used as source of truth for conditional compilation.
public struct CompilerConfiguration: Codable {

  /// The operating-system used when performing the compilation.
  let os: String

  /// The architecture of the machine doing the compilation.
  let arch: String

  /// The name of the compiler.
  let compiler: String

  /// The version of the compiler.
  let compilerVersion: SemanticVersion

  /// The version of the Hylo language version we are targeting.
  let hyloVersion: SemanticVersion

  /// The set of features supported in the current compilation.
  let features: [String]

  /// Creates an instance with the properties of the machine running this initializer, using features `f`.
  public init(_ f: [String] = []) {
    os = CompilerConfiguration.currentOS()
    arch = CompilerConfiguration.currentArch()
    compiler = "hc"
    compilerVersion = SemanticVersion(major: 0, minor: 1, patch: 0)
    hyloVersion = SemanticVersion(major: 0, minor: 1, patch: 0)
    features = f
  }

  /// The name of the operating system on which this function is run.
  private static func currentOS() -> String {
    #if os(macOS)
      return "macOS"
    #elseif os(Linux)
      return "Linux"
    #elseif os(Windows)
      return "Windows"
    #else
      return "Unknown"
    #endif
  }

  /// The architecture on which this function is run.
  private static func currentArch() -> String {
    #if arch(i386)
      return "i386"
    #elseif arch(x86_64)
      return "x86_64"
    #elseif arch(arm)
      return "arm"
    #elseif arch(arm64)
      return "arm64"
    #else
      return "Unknown"
    #endif
  }

}
