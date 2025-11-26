import FrontEnd
import Utils

extension Module: ColoredDescribable {

  public var coloredDescription: String {
    var output = ""
    writeColored(to: &output)
    return output
  }

}

extension Module {

  /// Writes a colored textual representation of this instance into `output`.
  public func writeColored<Target: TextOutputStream>(to output: inout Target) {
    output.write(contentsOf: functions.keys, separatedBy: "\n\n") { (s, f) in
      writeColored(function: f, to: &s)
    }
  }

  /// Returns a colored textual representation of the specified function.
  public func describeColored(function f: Function.ID) -> String {
    var output = ""
    writeColored(function: f, to: &output)
    return output
  }

  /// Writes a colored textual representation of the specified function into `output`.
  public func writeColored<Target: TextOutputStream>(function f: Function.ID, to output: inout Target) {
    let function = functions[f]!

    // Dumps the function in the module.
    output.write("\(styledComment("// \(debugDescription(f))"))\n")
    if !function.site.file.isSynthesized {
      output.write("\(styledComment("// \(function.site)"))\n")
    }

    output.write("\(styledKeyword(String(describing: function.linkage))) ")

    if function.isSubscript {
      output.write("\(styledKeyword("subscript")) \(styledIdentifier(String(describing: f)))(")
      output.write(function.inputs.lazy.coloredDescriptions())
      output.write("): \(styledType(String(describing: function.output)))")
    } else {
      output.write("\(styledKeyword("fun")) \(styledIdentifier(String(describing: f)))(")
      output.write(function.inputs.lazy.coloredDescriptions())
      output.write(") \(styledKeyword("->")) \(styledType(String(describing: function.output)))")
    }

    if function.entry == nil { return }
    output.write(" {\n")

    for i in blocks(in: f) {
      output.write("\(styledIdentifier(String(describing: i)))(")
      output.write(
        self[i].inputs.enumerated().lazy
          .map({ (j, t) in "\(Operand.parameter(i, j).coloredDescription) : \(t.coloredDescription)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in instructions(in: i) {
        output.write("  ")
        if let t = self[j].result {
          output.write("\(Operand.register(j).coloredDescription): \(t.coloredDescription) = ")
        }
        output.write("\(self[j].coloredDescription)\n")
      }
    }

    output.write("}")
  }

}

// MARK: - Helper extensions for colored parameter descriptions

extension Parameter: ColoredDescribable {

  public var coloredDescription: String {
    styledType(String(describing: type))
  }

}

extension LazySequence where Elements == [Parameter] {

  /// Returns colored descriptions of the receiver's elements separated by commas.
  fileprivate func coloredDescriptions() -> String {
    self.lazy.map(\.coloredDescription).joined(separator: ", ")
  }

}
