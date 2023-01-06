import Core
import FrontEnd

extension Diagnostic {

  /// Returns whether `l` should be logged before `r`.
  static func isLoggedBefore(_ l: Diagnostic, _ r: Diagnostic) -> Bool {
    guard let lhs = l.location else { return true }
    guard let rhs = r.location else { return false }

    if lhs.source == rhs.source {
      return lhs < rhs
    } else {
      return lhs.source.url.path.lexicographicallyPrecedes(rhs.source.url.path)
    }
  }

}
