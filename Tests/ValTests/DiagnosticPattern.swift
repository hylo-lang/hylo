import Basic

struct DiagnosticPattern {

  var message: String?

  static func ~= (pattern: DiagnosticPattern, diagnostic: Diagnostic) -> Bool {
    if let message = pattern.message {
      return message == diagnostic.message
    }

    return true
  }

}
