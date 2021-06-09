import Basic

struct DiagnosticPattern {

  /// The message of the diagnostic.
  let message: String?

  static func ~= (pattern: DiagnosticPattern, diagnostic: Diagnostic) -> Bool {
    if let message = pattern.message {
      return message == diagnostic.message
    }

    return true
  }

}
