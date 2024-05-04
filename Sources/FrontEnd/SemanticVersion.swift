/// A semantic version number.
public struct SemanticVersion: Codable, Comparable, Hashable {

  public let major: Int

  public let minor: Int

  public let patch: Int

  public init(major: Int, minor: Int, patch: Int) {
    self.major = major
    self.minor = minor
    self.patch = patch
  }

  public static func < (lhs: Self, rhs: Self) -> Bool {
    (lhs.major, lhs.major, lhs.patch) < (rhs.major, rhs.major, rhs.patch)
  }

}
