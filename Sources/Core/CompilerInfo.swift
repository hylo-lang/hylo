/// Describes the compiler; used as source of truth for conditional compilation.
public struct CompilerInfo {

  /// A version number with multiple parts; form: x(.y)*
  public struct VersionNumber: Codable, Comparable {

    public let parts: [Int]

    public init(_ p: [Int]) {
      parts = p
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
      return lhs.parts.lexicographicallyPrecedes(rhs.parts)
    }

  }

  /// The operating-system used when performing the compilation.
  let os: String
  /// The architecture of the machine doing the compilation.
  let arch: String
  /// The name of the compiler.
  let compiler: String
  /// The version of the compiler.
  let compilerVersion: VersionNumber
  /// The version of the Hylo language version we are targeting.
  let hyloVersion: VersionNumber

  /// We only need one instance of this struct, to represent the compiler information.
  public static let instance = CompilerInfo()

  private init() {
    os = CompilerInfo.currentOS()
    arch = CompilerInfo.currentArch()
    compiler = "hc"
    compilerVersion = VersionNumber([0, 1])
    hyloVersion = VersionNumber([0, 1])
  }

  /// The current OS name.
  private static func currentOS() -> String {
    #if os(macOS)
      return "MacOS"
    #elseif os(Linux)
      return "Linux"
    #elseif os(Windows)
      return "Windows"
    #else
      return "Unknown"
    #endif
  }

  /// The current OS name.
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
