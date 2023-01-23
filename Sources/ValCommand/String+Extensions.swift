import Foundation

private let hasColorSupport = ProcessInfo.processInfo.environment["TERM"] != nil

/// An SRG escape sequence.
enum ANSIEscape: Int {

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

extension String.StringInterpolation {

  mutating func appendInterpolation(_ string: String, style: [ANSIEscape]) {
    appendInterpolation(string.styled(style))
  }

}

extension String {

  func styled(_ style: [ANSIEscape]) -> String {
    if !hasColorSupport { return self }
    let codes = style.map({ "\u{001B}[\($0.rawValue)m" }).joined()
    return codes + self + "\u{001B}[0m"
  }

}
