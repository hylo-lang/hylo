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

    for i in self[f].blockIDs {
      output.write("\(i)(")
      output.write(
        self[i, in: f].inputs.enumerated().lazy
          .map({ (j, t) in "\(Operand.parameter(i, j)) : \(t)" })
          .joined(separator: ", "))
      output.write("):\n")

      for j in self[f].instructions(in: i) {
        output.write("  ")
        if let t = self[j, in: f].result {
          output.write("\(Operand.register(j)): \(t) = ")
        }
        output.write("\(self[j, in: f])\n")
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
    case .projectionRamp(let b):
      return "Projection ramp of '\(debugDescription(b))'"
    case .projectionSlide(let b):
      return "Projection slide of '\(debugDescription(b))'"
    case .projectionCallerPlateau(let b, let r):
      return "Projection caller plateau of '\(debugDescription(b))' for region \(r)"
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

}
