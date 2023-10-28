public enum HostPlatform {
  #if os(Windows)
    /// The name of the environment variable containing the executable search path.
    public static let pathEnvironmentVariable = "Path"
    /// The separator between elements of the executable search path.
    public static let pathEnvironmentSeparator: Character = ";"
    /// The file extension applied to binary executables
    public static let executableSuffix = ".exe"
  #else
    /// The name of the environment variable containing the executable search path.
    public static let pathEnvironmentVariable = "PATH"
    /// The separator between elements of the executable search path.
    public static let pathEnvironmentSeparator: Character = ":"
    /// The file extension applied to binary executables
    public static let executableSuffix = ""
  #endif
}
