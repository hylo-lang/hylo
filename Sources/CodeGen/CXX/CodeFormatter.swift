/// Class responsible for the formatiing of the CXX code, as it is generated.
///
/// The callers will tell this class what tokens they want to write to the output CXX code,
/// and this will ensure that proper formatting will be added.
///
/// This class will have ownership of the code that is being written.
struct CodeFormatter: TextOutputStream {

  /// The amount of indentation we apply to the code.
  private var indentSize: Int

  /// The code that this object is writing.
  var code: String = ""

  /// The current level of indentation.
  private var curIndent: Int = 0
  /// True if the next text written should be indented.
  private var shouldIntent: Bool = false

  /// Initializes this object with the right formatting parameters.
  init(indentSize: Int = 2) {
    self.indentSize = indentSize
  }

  /// Adds new text to the output code.
  mutating func write(_ text: String) {
    checkAddIndent()
    code.write(text)
  }

  /// Adds new text to the output code, with a new line at the end
  mutating func writeLn(_ text: String) {
    checkAddIndent()
    code.write(text)
    addNewLine()
  }

  /// Adds a space to the code that is currently written.
  mutating func writeSpace() {
    checkAddIndent()
    code.write(" ")
  }

  /// Write a new line to the current code.
  /// If the next line contains visible characters, they would be indented.
  mutating func addNewLine() {
    code.write("\n")
    shouldIntent = true
  }

  /// Increases the indent level and adds a new line.
  /// Needs to be paired with a followup `exitIndentScope`.
  mutating func enterIndentScope() {
    curIndent += 1
    addNewLine()
  }

  /// Decreases the indent level.
  /// This does not add a new line; it is typcally called just after a new line.
  /// Needs to be paired with a previous `enterIndentScope`.
  mutating func exitIndentScope() {
    curIndent -= 1
    assert(curIndent >= 0)
  }

  /// Open a curly brace and increase the indent.
  mutating func beginBrace(withSpaceBefore space: Bool = true) {
    if space {
      writeSpace()
    }
    write("{")
    enterIndentScope()
  }
  /// Close a curly brace and decrease the indent.
  mutating func endBrace() {
    exitIndentScope()
    write("}")
  }

  /// Called when writing some new code (everything except newlines) to ensure that we
  /// are writing the indent when we are at the beginning of the line.
  mutating private func checkAddIndent() {
    if shouldIntent {
      code.write(String(repeating: " ", count: curIndent * indentSize))
      shouldIntent = false
    }
  }
}
