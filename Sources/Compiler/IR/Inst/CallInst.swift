/// Invokes `callee` with `arguments`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `arguments` must contain as many operands as the callee's type.
public struct CallInst: Inst {

  /// The callee.
  public let callee: Operand

  /// The arguments of the call.
  public let arguments: [Operand]

  public let type: IRType

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("call ")
    callee.dump(into: &output, with: &printer)
    for argument in arguments {
      output.write(", ")
      argument.dump(into: &output, with: &printer)
    }
  }

}
