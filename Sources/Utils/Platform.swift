/// The platform on which the compiler or interpreter is running/targeting.
public enum Platform {

  /// An operating system on which the compiler/interpreter/target program can run.
  public enum OperatingSystem: Codable, CustomStringConvertible, Sendable {

    case macOS, linux, windows

    /// String representation that matches the possible values used for conditional compilation.
    public var description: String {
      switch self {
      case .macOS: return "macOS"
      case .linux: return "Linux"
      case .windows: return "Windows"
      }
    }

  }

  /// An architecture on which the the compiler/interpreter/target program can run.
  public enum Architecture: String, Codable, CustomStringConvertible, Sendable {

    case i386, x86_64, arm, arm64

    /// String representation that matches the possible values used for conditional compilation.
    public var description: String {
      return rawValue
    }

  }

  #if os(macOS)
    /// The host operating system.
    public static let hostOperatingSystem: OperatingSystem = .macOS
  #elseif os(Linux)
    /// The host operating system.
    public static let hostOperatingSystem: OperatingSystem = .linux
  #elseif os(Windows)
    /// The host operating system.
    public static let hostOperatingSystem: OperatingSystem = .windows
  #endif

  #if arch(i386)
    /// The host architecture.
    public static let hostArchitecture: Architecture = .i386
  #elseif arch(x86_64)
    /// The host architecture.
    public static let hostArchitecture: Architecture = .x86_64
  #elseif arch(arm)
    /// The host architecture.
    public static let hostArchitecture: Architecture = .arm
  #elseif arch(arm64)
    /// The host architecture.
    public static let hostArchitecture: Architecture = .arm64
  #endif

}
