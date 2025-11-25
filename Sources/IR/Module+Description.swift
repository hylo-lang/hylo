import FrontEnd
import Utils

extension Module: CustomStringConvertible {

  public var description: String {
    var output = ""
    write(to: &output)
    return output
  }

}

extension Module: TextOutputStreamable {

  /// Writes a textual representation of this instance into `output`.
  public func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(contentsOf: functions.keys, separatedBy: "\n\n") { (s, f) in
      write(function: f, to: &s)
    }
  }

  /// Returns a textual representation of the specified function.
  public func describe(function f: Function.ID) -> String {
    var output = ""
    write(function: f, to: &output)
    return output
  }

  /// Writes a textual representation of the specified function into `output`.
  public func write<Target: TextOutputStream>(function f: Function.ID, to output: inout Target) {
    let function = functions[f]!

    // Dumps the function in the module.
    output.write("// \(debugDescription(f))\n")
    if !function.site.file.isSynthesized {
      output.write("// \(function.site)\n")
    }

    output.write("\(function.linkage) ")

    if function.isSubscript {
      output.write("subscript \(f)(")
      output.write(function.inputs.lazy.descriptions())
      output.write("): \(function.output)")
    } else {
      output.write("fun \(f)(")
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
        if let t = self[j].result {
          output.write("\(Operand.register(j)): \(t) = ")
        }
        output.write("\(self[j])\n")
      }
    }

    output.write("}")
  }

  /// Returns a textual description of `f` suitable for debugging.
  func debugDescription(_ f: Function.ID) -> String {
    switch f.value {
    case .existentialized(let base):
      return "Existentialized form of '\(debugDescription(base))'"
    case .lowered(let d):
      return program.debugDescription(d)
    case .monomorphized(let base, let arguments):
      return "Monomorphized form of '\(debugDescription(base))' for <\(list: arguments.values)>"
    case .synthesized(let d):
      return debugDescription(d)
    }
  }

  /// Returns a textual description of `d` suitable for debugging.
  func debugDescription(_ f: SynthesizedFunctionDecl) -> String {
    switch f.kind {
    case .globalInitialization(let d):
      return "Global initializer of '\(program.debugDescription(d))'"
    default:
      return f.description
    }
  }

  /// Returns a textual representation of the blocks in function `f` with call instructions
  /// showing the identifier of the callee function when available.
  public func describeBlocksWithCalleeIdentifiers(in f: Function.ID) -> String {
    var output = ""
    
    for i in blocks(in: f) {
      output.write("\(i)(")
      output.write(
        self[i].inputs.enumerated().lazy
          .map({ (j, t) in "\(Operand.parameter(i, j)) : \(t)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in instructions(in: i) {
        output.write("  ")
        if let t = self[j].result {
          output.write("\(Operand.register(j)): \(t) = ")
        }

        // Special handling for Call instructions to print function identifier
        let instruction = self[j]
        if let call = instruction as? Call,
          case .constant(let c) = call.callee,
          let funcRef = c as? FunctionReference,
          case .lowered(let declId) = funcRef.function.value,
          let functionDeclId = FunctionDecl.ID(declId),
          let functionName = program.ast[functionDeclId].identifier?.value
        {
          output.write("call @\(functionName)(\(list: call.arguments)) to \(call.output)\n")
        } else {
          output.write("\(instruction)\n")
        }
      }
    }
    
    return output
  }

}
