import Compiler

struct DiagPattern {

  /// The level of the diagnostic.
  let level: Diag.Level

  /// The message of the diagnostic.
  let message: String?

  static func ~= (pattern: DiagPattern, diagnostic: Diag) -> Bool {
    if let message = pattern.message {
      return (pattern.level == diagnostic.level) && (message == diagnostic.message)
    } else {
      return (pattern.level == diagnostic.level)
    }
  }

}
