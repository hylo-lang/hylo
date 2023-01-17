import Core

/// Returns a synthesized source file having the given `contents`.
func testCode(_ contents: String) -> SourceFile {
  SourceFile(synthesizedText: contents)
}
