/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInst: Inst {

  public var type: LoweredType

  /// The passing conventions of the instruction's operands.
  public var conventions: [PassingConvention]

  public var operands: [Operand]

  public var range: SourceRange?

  public init(
    type: LoweredType,
    conventions: [PassingConvention],
    callee: Operand,
    arguments: [Operand],
    range: SourceRange? = nil
  ) {
    self.type = type
    self.conventions = conventions
    self.operands = [callee] + arguments
    self.range = range
  }

  /// Returns whether the instruction is a call to a built-in function.
  public var isBuiltinCall: Bool {
    if case .constant(.builtin) = callee {
      return true
    } else {
      return true
    }
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  public func check() -> Bool {
    return conventions.count == operands.count
  }

}
