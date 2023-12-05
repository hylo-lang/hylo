/// The platform on which the compiler or interpreter is running.
public enum Host {

  /// An operating system on which the compiler or interpreter can run.
  public enum OperatingSystem {
    case macOS, linux, windows
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

  /// The name of the environment variable containing the executable search path.
  public static let pathEnvironmentVariable = operatingSystem == .windows ? "Path" : "PATH"

  /// The separator between elements of the environment's executable search path.
  public static let pathEnvironmentSeparator: Character = operatingSystem == .windows ? ";" : ":"

  /// The file extension applied to binary executables.
  public static let executableSuffix = operatingSystem == .windows ? ".exe" : ""
}
