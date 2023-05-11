import Core

/// A type that can be used as a target to log message.
public protocol DiagnosticLog: TextOutputStream {

  /// Indicates whether this instance supports ANSI colors.
  var hasANSIColorSupport: Bool { get }

  /// Appends `text` to the stream, applying `style` iff `hasANSIColorSupport` is true.
  mutating func write(_ text: String, in style: [ANSIEscape])

}

extension DiagnosticLog {

  public mutating func write(_ text: String, in style: [ANSIEscape]) {
    if hasANSIColorSupport {
      let codes = style.map({ "\u{001B}[\($0.rawValue)m" }).joined()
      write(codes + text + "\u{001B}[0m")
    } else {
      write(text)
    }
  }

  /// Logs the contents of `diagnostics`.
  mutating func log(diagnostics: DiagnosticSet) {
    for d in diagnostics.elements.sorted(by: Diagnostic.isLoggedBefore) {
      log(diagnostic: d)
    }
  }

  /// Logs `diagnostic`.
  mutating func log(diagnostic: Diagnostic, asChild isChild: Bool = false) {
    // Log the location
    let siteFirst = diagnostic.site.first()
    let path = siteFirst.file.url.relativePath
    let (lineFirst, column) = siteFirst.lineAndColumn
    write("\(path):\(lineFirst):\(column): ", in: [.bold])

    // Log the level.
    if isChild {
      log(label: .note)
    } else {
      log(label: diagnostic.level)
    }

    // Log the message.
    write(diagnostic.message, in: [.bold])
    write("\n")

    // Log the window
    let site = diagnostic.site
    let line = site.first().line.text
    write(String(line))
    if !(line.last?.isNewline ?? false) { write("\n") }

    let padding = line.distance(from: line.startIndex, to: site.start)
    write(String(repeating: " ", count: padding))

    let count = line.distance(from: site.start, to: min(site.end, line.endIndex))
    if count > 1 {
      write(String(repeating: "~", count: count))
    } else {
      write("^")
    }
    write("\n")

    // Log the notes.
    for child in diagnostic.notes {
      log(diagnostic: child, asChild: true)
    }
  }

  /// Logs `message`.
  mutating func log(_ message: String, terminator: String = "\n") {
    write(message)
    write(terminator)
  }

  mutating func log(label: Diagnostic.Level) {
    switch label {
    case .note:
      write("note: ", in: [.bold, .cyan])
    case .warning:
      write("warning: ", in: [.bold, .yellow])
    case .error:
      write("error: ", in: [.bold, .red])
    }
  }

}

/// An SRG escape sequence.
public enum ANSIEscape: Int {

  case bold = 1
  case dimmed = 2
  case italic = 3
  case underline = 4
  case blink = 5
  case strike = 9
  case black = 30
  case red = 31
  case green = 32
  case yellow = 33
  case blue = 34
  case magenta = 35
  case cyan = 36
  case white = 37
  case `default` = 39

}
