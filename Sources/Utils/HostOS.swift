#if os(Windows)
  /// The name of the environment variable containing the executable search path.
  public let pathEnvironmentVariable = "Path"
  /// The separator between elements of the executable search path.
  public let pathEnvironmentSeparator: Character = ";"
  /// The file extension applied to binary executables
  public let executableSuffix = ".exe"
#else
  /// The name of the environment variable containing the executable search path.
  public let pathEnvironmentVariable = "PATH"
  /// The separator between elements of the executable search path.
  public let pathEnvironmentSeparator: Character = ":"
  /// The file extension applied to binary executables
  public let executableSuffix = ""
#endif
