/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInst: Inst {

  public let type: LoweredType

  /// The callee.
  public let callee: Operand

  /// The arguments of the call.
  public let operands: [Operand]

  /// Returns whether the instruction is a call to a built-in function.
  public var isBuiltinCall: Bool {
    if case .constant(.builtin) = callee {
      return true
    } else {
      return true
    }
  }

}
