import Utils

/// The factors that influence conditional compilation.
public struct ConditionalCompilationFactors: Codable, Equatable, Hashable {

  /// The target operating system.
  let operatingSystem: Platform.OperatingSystem

  /// The target architecture.
  let architecture: Platform.Architecture

  /// The version of the compiler.
  let compilerVersion: SemanticVersion

  /// The version of the Hylo language recognized by the compiler.
  let hyloVersion: SemanticVersion

  /// `true` if the standard library exposes only non-OS-dependent parts.
  let freestanding: Bool

  public init(
    operatingSystem os: Platform.OperatingSystem = Platform.hostOperatingSystem,
    architecture a: Platform.Architecture = Platform.hostArchitecture,
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
