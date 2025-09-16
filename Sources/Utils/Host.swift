/// The platform on which the compiler or interpreter is running.
public enum Host: Sendable {

  /// The name of the environment variable containing the executable search path.
  public static let pathEnvironmentVariable =
    Platform.hostOperatingSystem == .windows ? "Path" : "PATH"

  /// The separator between elements of the environment's executable search path.
  public static let pathEnvironmentSeparator: Character =
    Platform.hostOperatingSystem == .windows ? ";" : ":"

  /// The file extension applied to binary executables.
  public static let executableSuffix = Platform.hostOperatingSystem == .windows ? ".exe" : ""

}
