import Utils

extension Module: CustomStringConvertible, TextOutputStreamable {

  public var description: String {
    var output = ""
    write(to: &output)
    return output
  }

  /// Returns a textual representation of the specified function.
  public func describe(function functionID: Function.ID) -> String {
    var output = ""
    write(function: functionID, to: &output)
    return output
  }

  /// Writes a textual representation of this instance into `output`.
  public func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(contentsOf: globals.enumerated(), separatedBy: "\n\n") { (s, e) in
      s.write("global @\(syntax.id).\(e.offset) = \(e.element)")
    }
    if !globals.isEmpty && !functions.isEmpty {
      output.write("\n\n")
    }
    output.write(contentsOf: functions.keys, separatedBy: "\n\n") { (s, f) in
      write(function: f, to: &s)
    }
  }

  /// Writes a textual representation of the specified function into `output`.
  public func write<Target: TextOutputStream>(function f: Function.ID, to output: inout Target) {
    let function = functions[f]!

    // Dumps the function in the module.
    if function.isSubscript {
      output.write("subscript \(function.name)(")
      output.write(function.inputs.lazy.descriptions())
      output.write("): \(function.output)")
    } else {
      output.write("fun \(function.name)(")
      output.write(function.inputs.lazy.descriptions())
      output.write(") -> \(function.output)")
    }

    if function.entry == nil { return }
    output.write(" {\n")

    for i in blocks(in: f) {
      output.write("\(i)(")
      output.write(
        self[i].inputs.enumerated().lazy
          .map({ (j, t) in "\(Operand.parameter(i, j)) : \(t)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in instructions(in: i) {
        output.write("  ")
        if !self[j].types.isEmpty {
          let r = self[j].types.indices.map({ (k) in Operand.register(j, k).description })
          output.write("\(list: r) = ")
        }
        output.write("\(self[j])\n")
      }
    }

    output.write("}")
  }

}
