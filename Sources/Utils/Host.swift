/// The platform on which the compiler or interpreter is running.
public enum Host {

  /// An operating system on which the compiler or interpreter can run.
  public enum OperatingSystem: Codable, CustomStringConvertible {

    case macOS, linux, windows

    public var description: String {
      switch self {
      case .macOS: return "macOS"
      case .linux: return "Linux"
      case .windows: return "Windows"
      }
    }

  }

  /// An architecture on which the compiler or interpreter can run.
  public enum Architecture: String, Codable, CustomStringConvertible {

    case i386, x86_64, arm, arm64

    public var description: String {
      return rawValue
    }

  }

  #if os(macOS)
    /// The operating system.
    public static let operatingSystem: OperatingSystem = .macOS
  #elseif os(Linux)
    /// The operating system.
    public static let operatingSystem: OperatingSystem = .linux
  #elseif os(Windows)
    /// The operating system.
    public static let operatingSystem: OperatingSystem = .windows
  #endif

  #if arch(i386)
    /// The architecture.
    public static let architecture: Architecture = .i386
  #elseif arch(x86_64)
    /// The architecture.
    public static let architecture: Architecture = .x86_64
  #elseif arch(arm)
    /// The architecture.
    public static let architecture: Architecture = .arm
  #elseif arch(arm64)
    /// The architecture.
    public static let architecture: Architecture = .arm64
  #endif

  /// The name of the environment variable containing the executable search path.
  public static let pathEnvironmentVariable = operatingSystem == .windows ? "Path" : "PATH"

  /// The separator between elements of the environment's executable search path.
  public static let pathEnvironmentSeparator: Character = operatingSystem == .windows ? ";" : ":"

  /// The file extension applied to binary executables.
  public static let executableSuffix = operatingSystem == .windows ? ".exe" : ""

}
