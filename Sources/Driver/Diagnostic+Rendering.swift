import Core

extension DiagnosticSet {

  /// Writes a textual representation of `self` into `output` using the given style.
  func render<Output: TextOutputStream>(
    into output: inout Output, style: Diagnostic.TextOutputStyle = .unstyled
  ) {
    for d in elements.sorted(by: Diagnostic.isLoggedBefore) {
      d.render(into: &output, style: style)
    }
  }

  /// Returns a textual representation of `self` using the given style.
  public func rendered(style: Diagnostic.TextOutputStyle = .unstyled) -> String {
    var r = ""
    render(into: &r, style: style)
    return r
  }
}

extension Diagnostic {

  /// Writes `self` into `output` using the given output style.
  func render<Output: TextOutputStream>(into output: inout Output, style: TextOutputStyle) {

    func write<T>(_ x: T, in style: String.ANSIStyle = String.unstyled) {
      output.write(style("\(x)"))
    }

    write(site.gnuStandardText, in: style.sourceRange)
    write(": ")
    write(level, in: style[level])
    write(": ")

    write(message, in: style.message)
    write("\n")

    renderWindow(into: &output)

    // Log the notes.
    for n in notes {
      n.render(into: &output, style: style)
    }
  }

  /// Writes the text of the source line and column indicator (the "window") into `output`.
  private func renderWindow<Output: TextOutputStream>(into output: inout Output) {
    // Write the first marked line followed by a newline.
    let firstMarkedLine = site.file.line(containing: site.startIndex).text
    output.write(String(firstMarkedLine))
    if !(firstMarkedLine.last?.isNewline ?? false) { output.write("\n") }

    // Write the column indication for that line, followed by a newline.
    let startColumn = firstMarkedLine.distance(
      from: firstMarkedLine.startIndex, to: site.startIndex)
    output.write(String(repeating: " ", count: startColumn))
    let markWidth = firstMarkedLine.distance(
      from: site.startIndex, to: min(site.endIndex, firstMarkedLine.endIndex))
    output.write(markWidth <= 1 ? "^" : String(repeating: "~", count: markWidth))
    output.write("\n")
  }

}

extension Diagnostic {

  /// Style transforms applied to the raw text of different parts of a diagnostic.
  public struct TextOutputStyle {

    /// How the site is rendered in this style.
    fileprivate let sourceRange: String.ANSIStyle

    /// How a “note:” label is rendered in this style.
    fileprivate let noteLabel: String.ANSIStyle

    /// How a “warning:” label is rendered in this style.
    fileprivate let warningLabel: String.ANSIStyle

    /// How an “error:” label is rendered in this style.
    fileprivate let errorLabel: String.ANSIStyle

    /// How a diagnostic message is rendered in this style.
    fileprivate let message: String.ANSIStyle

    /// How a label for diagnostic level `l` is rendered in this style.
    fileprivate subscript(l: Diagnostic.Level) -> String.ANSIStyle {
      switch l {
      case .note: return noteLabel
      case .warning: return warningLabel
      case .error: return errorLabel
      }
    }

    /// The identity style; applies no changes to text.
    public static let unstyled = TextOutputStyle(
      sourceRange: String.unstyled,
      noteLabel: String.unstyled,
      warningLabel: String.unstyled,
      errorLabel: String.unstyled,
      message: String.unstyled)

    /// The default style with colors and font weights.
    public static let styled = TextOutputStyle(
      sourceRange: { $0.styled(.bold) },
      noteLabel: { $0.styled(.bold, .cyan) },
      warningLabel: { $0.styled(.bold, .yellow) },
      errorLabel: { $0.styled(.bold, .red) },
      message: { $0.styled(.bold) })
  }

}

/// An ANSI [Select Graphic Rendition](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR) (SGR)
/// escape code.
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

  /// The textual representation of this code that has an effect on an ANSI terminal.
  var controlString: String {
    "\u{001B}[\(rawValue)m"
  }
}

extension String {

  /// A string transformation for applying (or not) ANSI terminal styling.
  fileprivate typealias ANSIStyle = (String) -> String

  /// Returns `self` with the given set of styles applied.
  fileprivate func styled(_ rendition: ANSISGR...) -> String {
    "\(list: rendition.map(\.controlString), joinedBy: "")"
      + "\(self)\(ANSISGR.reset.controlString)"
  }

  /// The identity transformation.
  fileprivate static let unstyled: ANSIStyle = { $0 }

}
