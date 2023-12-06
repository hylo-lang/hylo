import Utils

/// The configuration needed for conditional compilation.
public struct ConditionalCompilationConfiguration: Codable {

  /// The target operating system.
  let operatingSystem: Host.OperatingSystem

  /// The target architecture.
  let architecture: Host.Architecture

  /// The version of the compiler.
  let compilerVersion: SemanticVersion

  /// The version of the Hylo language version we are targeting.
  let hyloVersion: SemanticVersion

  /// `true` if we are compiling for freestanding mode.
  let freestanding: Bool

  public init(
    operatingSystem os: Host.OperatingSystem = Host.operatingSystem,
    architecture a: Host.Architecture = Host.architecture,
    compilerVersion cv: SemanticVersion = SemanticVersion(major: 0, minor: 1, patch: 0),
    hyloVersion hv: SemanticVersion = SemanticVersion(major: 0, minor: 1, patch: 0),
    freestanding f: Bool = false
  ) {
    self.operatingSystem = os
    self.architecture = a
    self.compilerVersion = cv
    self.hyloVersion = hv
    self.freestanding = f
  }

}
