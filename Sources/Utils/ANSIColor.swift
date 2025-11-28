/// ANSI color and styling utilities for IR output.
///
/// This module provides a set of constants and functions for applying ANSI color codes to
/// strings, enabling syntax-highlighted console output for IR instructions and modules.

// MARK: - ANSI Escape Codes

/// ANSI escape sequences for text styling.
public enum ANSICode {
  
  // MARK: Colors
  
  /// Resets all styling to default.
  public static let reset = "\u{001B}[0m"
  
  /// Red foreground color.
  public static let red = "\u{001B}[31m"
  
  /// Green foreground color.
  public static let green = "\u{001B}[32m"
  
  /// Yellow foreground color.
  public static let yellow = "\u{001B}[33m"
  
  /// Blue foreground color.
  public static let blue = "\u{001B}[34m"
  
  /// Magenta foreground color.
  public static let magenta = "\u{001B}[35m"
  
  /// Cyan foreground color.
  public static let cyan = "\u{001B}[36m"
  
  /// White foreground color.
  public static let white = "\u{001B}[37m"
  
  /// Bright black (gray) foreground color.
  public static let brightBlack = "\u{001B}[90m"
  
  /// Bright red foreground color.
  public static let brightRed = "\u{001B}[91m"
  
  /// Bright green foreground color.
  public static let brightGreen = "\u{001B}[92m"
  
  /// Bright yellow foreground color.
  public static let brightYellow = "\u{001B}[93m"
  
  /// Bright blue foreground color.
  public static let brightBlue = "\u{001B}[94m"
  
  /// Bright magenta foreground color.
  public static let brightMagenta = "\u{001B}[95m"
  
  /// Bright cyan foreground color.
  public static let brightCyan = "\u{001B}[96m"
  
  // MARK: Styles
  
  /// Bold text.
  public static let bold = "\u{001B}[1m"
  
  /// Dim (faint) text.
  public static let dim = "\u{001B}[2m"
  
  /// Italic text.
  public static let italic = "\u{001B}[3m"
  
  /// Underlined text.
  public static let underline = "\u{001B}[4m"
  
}

// MARK: - Styling Functions

/// Returns `text` styled with ANSI codes for register operands (e.g., %0, %1).
///
/// Registers are styled in bright cyan to make them stand out as primary data flow elements.
public func styledRegister(_ text: String) -> String {
  "\(ANSICode.brightCyan)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for type identifiers.
///
/// Types are styled in bright green to distinguish them from other syntactic elements.
public func styledType(_ text: String) -> String {
  "\(ANSICode.brightBlack)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for instruction names.
///
/// Instruction names are styled in bright yellow with bold to emphasize the operation.
public func styledInstruction(_ text: String) -> String {
  "\(ANSICode.bold)\(ANSICode.brightGreen)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for numeric constant values.
///
/// Constants are styled in bright magenta to make literal values visually distinct.
public func styledConstant(_ text: String) -> String {
  "\(ANSICode.brightMagenta)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for keywords (e.g., fun, subscript, to).
///
/// Keywords are styled in bright blue with bold for prominence.
public func styledKeyword(_ text: String) -> String {
  "\(ANSICode.bold)\(ANSICode.brightBlue)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for comments.
///
/// Comments are styled in dim bright black (gray) to de-emphasize them.
public func styledComment(_ text: String) -> String {
  "\(ANSICode.dim)\(ANSICode.brightBlack)\(text)\(ANSICode.reset)"
}

/// Returns `text` styled with ANSI codes for function/block identifiers.
///
/// Function and block identifiers are styled in cyan to differentiate them from registers.
public func styledIdentifier(_ text: String) -> String {
  "\(ANSICode.cyan)\(text)\(ANSICode.reset)"
}
