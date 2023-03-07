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

  public func write<Target: TextOutputStream>(to output: inout Target) {
    var isFirst = true
    for f in functions.keys {
      if isFirst {
        isFirst = false
      } else {
        output.write("\n\n")
      }
      write(function: f, to: &output)
    }
  }

  /// Writes a textual representation of the specified function into `output`.
  public func write<Target: TextOutputStream>(function f: Function.ID, to output: inout Target) {
    let function = functions[f]!

    // Dumps the function in the module.
    if let debugName = function.debugName { output.write("// \(debugName)\n") }
    output.write("@lowered fun \(function.name)(")
    output.write(
      function.inputs.lazy
        .map({ (c, t) in "\(c) \(t)" })
        .joined(separator: ", "))
    output.write(") -> \(function.output) {\n")

    for i in blocks(in: f) {
      output.write("\(i)(")
      output.write(
        self[i].inputs.enumerated().lazy
          .map({ (j, t) in "\(Operand.parameter(block: i, index: j)) : \(t)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in instructions(in: i) {
        output.write("  ")
        if !self[j].types.isEmpty {
          let r = self[j].types.indices
            .map({ (k) in Operand.register(instruction: j, index: k).description })
          output.write("\(list: r) = ")
        }
        output.write("\(self[j])\n")
      }
    }

    output.write("}")
  }

}
