import Core

extension DiagnosticSet {

  func write<Output: TextOutputStream>(
    into output: inout Output, style: Diagnostic.TextOutputStyle = .unstyled
  ) {
    for d in elements.sorted(by: Diagnostic.isLoggedBefore) {
      d.write(into: &output, style: style)
    }
  }

  public func formatted(style: Diagnostic.TextOutputStyle = .unstyled) -> String {
    var r = ""
    write(into: &r, style: style)
    return r
  }
}

extension Diagnostic {

  /// Writes `self` into `output` using the given output style.
  func write<Output: TextOutputStream>(into output: inout Output, style: TextOutputStyle) {

    func write<T>(_ x: T, in style: String.ANSIStyle = String.unstyled) {
      output.write(style("\(x)"))
    }

    write(site.gnuStandardText, in: style.sourceRange)
    write(": ")
    write(level, in: style[level])
    write(": ")

    write(message, in: style.message)
    write("\n")

    writeWindow(into: &output)

    // Log the notes.
    for n in notes {
      n.write(into: &output, style: style)
    }
  }

  /// Writes the text of the source line and column indicator (the "window") into `output`.
  private func writeWindow<Output: TextOutputStream>(into output: inout Output) {
    // Write the first marked line followed by a newline.
    let firstMarkedLine = site.file.line(containing: site.start).text
    output.write(String(firstMarkedLine))
    if !(firstMarkedLine.last?.isNewline ?? false) { output.write("\n") }

    // Write the column indication for that line, followed by a newline.
    let startColumn = firstMarkedLine.distance(from: firstMarkedLine.startIndex, to: site.start)
    output.write(String(repeating: " ", count: startColumn))
    let markWidth = firstMarkedLine.distance(
      from: site.start, to: min(site.end, firstMarkedLine.endIndex))
    output.write(markWidth <= 1 ? "^" : String(repeating: "~", count: markWidth))
    output.write("\n")
  }

}

extension Diagnostic {

  public struct TextOutputStyle {
    fileprivate let sourceRange: String.ANSIStyle
    fileprivate let note: String.ANSIStyle
    fileprivate let warning: String.ANSIStyle
    fileprivate let error: String.ANSIStyle
    fileprivate let message: String.ANSIStyle

    fileprivate subscript(l: Diagnostic.Level) -> String.ANSIStyle {
      switch l {
      case .note: return note
      case .warning: return warning
      case .error: return error
      }
    }

    public static let unstyled = TextOutputStyle(
      sourceRange: String.unstyled,
      note: String.unstyled,
      warning: String.unstyled,
      error: String.unstyled,
      message: String.unstyled)

    public static let styled = TextOutputStyle(
      sourceRange: { $0.styled(.bold) },
      note: { $0.styled(.bold, .cyan) },
      warning: { $0.styled(.bold, .yellow) },
      error: { $0.styled(.bold, .red) },
      message: { $0.styled(.bold) })
  }

}

/// An ANSI [Select Graphic Rendition](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR) (SGR) escape code.
private enum ANSISGR: Int {

  /// Reset all SGR attributes
  case reset = 0

  case bold = 1
  case dimmed = 2
  case italic = 3
  case underline = 4
  case blink = 5
  case strike = 9
  case defaultFont = 10
  case black = 30
  case red = 31
  case green = 32
  case yellow = 33
  case blue = 34
  case magenta = 35
  case cyan = 36
  case white = 37
  case defaultColor = 39

  /// The textual representation that has an effect on an ANSI terminal
  var controlString: String {
    "\u{001B}[\(rawValue)m"
  }
}

extension String {
  fileprivate typealias ANSIStyle = (String) -> String
  fileprivate func styled(_ rendition: ANSISGR...) -> String {
    "\(list: rendition.map(\.controlString), joinedBy: "")\(self)\(ANSISGR.reset.controlString)"
  }
  fileprivate static let unstyled: ANSIStyle = { $0 }
}
