/// A type that can be used as a target to log message.
public protocol Channel: TextOutputStream {

  /// Indicates whether this instance supports ANSI colors.
  var hasANSIColorSupport: Bool { get }

  /// Appends `text` to the stream, applying `style` iff `hasANSIColorSupport` is true.
  mutating func write(_ text: String, in style: [ANSIEscape])

}

extension Channel {

  public mutating func write(_ text: String, in style: [ANSIEscape]) {
    if hasANSIColorSupport {
      let codes = style.map({ "\u{001B}[\($0.rawValue)m" }).joined()
      write(codes + text + "\u{001B}[0m")
    } else {
      write(text)
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
