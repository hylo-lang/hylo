/// A helper object for printing indented text.
struct IndentPrinter {

  /// The indentation level.
  var indentationLevel = 0

  /// Writes `item` into `output` applying the current indentation level.
  public func write<Target: TextOutputStream>(
    _ item: String,
    terminator: String = "\n",
    to output: inout Target
  ) {
    let prefix = String(repeating: " ", count: indentationLevel * 2)
    let text = item
      .split(separator: "\n", omittingEmptySubsequences: false)
      .map({ line in "\(prefix)\(line)" })
      .joined(separator: "\n")
    print(text, terminator: terminator, to: &output)
  }

}
