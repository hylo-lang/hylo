/// A type responsible for formatting CXX code, as it is generated.
///
/// Users of the type specify what tokens they want written to the generated CXX code
/// and delegate the formatting logic. Instances of this type have ownership of the code
/// they generate.
struct CodeFormatter: TextOutputStream {

  /// The amount of indentation we apply to the code.
  private static let indentation: String = "  "

  /// The code that this object is writing.
  var code: String = ""

  /// The current level of indentation.
  private var indentationLevel: Int = 0
  /// True if we are starting a new line.
  /// Used to detect when we need to write indentation.
  private var atLineStart: Bool = false

  /// Adds new text to the output code.
  mutating func write(_ text: String) {
    writeIndentationAtLineStart()
    code.write(text)
  }

  /// Adds new text to the output code, with a new line at the end
  mutating func writeLine(_ text: String) {
    writeIndentationAtLineStart()
    code.write(text)
    writeNewline()
  }

  /// Adds a space to the code that is currently written.
  mutating func writeSpace() {
    writeIndentationAtLineStart()
    code.write(" ")
  }

  /// Write a new line to the current code.
  /// If the next line contains visible characters, they would be indented.
  mutating func writeNewline() {
    code.write("\n")
    atLineStart = true
  }

  /// Increases the indent level and adds a new line.
  /// Needs to be paired with a followup `dedent`.
  mutating func indent() {
    indentationLevel += 1
    writeNewline()
  }

  /// Decreases the indent level.
  /// This does not add a new line; it is typcally called just after a new line.
  /// Needs to be paired with a previous `indent`.
  mutating func dedent() {
    indentationLevel -= 1
    assert(indentationLevel >= 0)
  }

  /// Open a curly brace and increase the indent.
  /// Also writes a space before the brace, if we are not at the beginning of the line.
  mutating func beginBrace() {
    if !atLineStart {
      writeSpace()
    }
    write("{")
    indent()
  }
  /// Close a curly brace and decrease the indent.
  mutating func endBrace() {
    dedent()
    write("}")
  }

  /// If we are at line start, write the indentation.
  mutating private func writeIndentationAtLineStart() {
    if atLineStart {
      code.write(String(repeating: CodeFormatter.indentation, count: indentationLevel))
      atLineStart = false
    }
  }
}
