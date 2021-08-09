import Basic

struct DiagnosticPattern {

  /// The message of the diagnostic.
  let message: String?

  static func ~= (pattern: DiagnosticPattern, diagnostic: Diag) -> Bool {
    if let message = pattern.message {
      return message == diagnostic.message
    }

    return true
  }

}
