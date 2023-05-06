import Core
import Utils

/// Invokes `callee` with `operands`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInstruction: Instruction {

  /// The type if the return value.
  public let returnType: LoweredType

  /// The callee and arguments of the call.
  public private(set) var operands: [Operand]

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    returnType: LoweredType,
    callee: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.returnType = returnType
    self.operands = [callee] + arguments
    self.site = site
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  public var types: [LoweredType] { [returnType] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension Module {

  /// Creates a `call` anchored at `anchor` applies `callee` using convention `calleeConvention` on
  /// `arguments` using `argumentConventions`.
  ///
  /// - Parameters:
  ///   - callee: The function to call.
  ///   - arguments: The arguments of the call; one of each input of `callee`'s type.
  func makeCall(
    applying callee: Operand,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> CallInstruction {
    let calleeType = LambdaType(type(of: callee).ast)!.strippingEnvironment
    precondition(calleeType.inputs.count == arguments.count)

    return CallInstruction(
      returnType: .object(program.relations.canonical(calleeType.output)),
      callee: callee,
      arguments: arguments,
      site: anchor)
  }

}
