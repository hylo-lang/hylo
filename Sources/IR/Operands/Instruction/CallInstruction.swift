import Core
import Utils

/// Invokes `callee` with `arguments` and writes its result to `output`.
///
/// `callee` must have a lambda type; the type of the instruction must be the same as output type
/// of the callee. `operands` must contain as many operands as the callee's type.
public struct CallInstruction: Instruction {

  /// The callee and arguments of the call.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    callee: Operand,
    output: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.operands = [callee, output] + arguments
    self.site = site
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The location at which the result of `callee` is stored.
  public var output: Operand { operands[1] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[2...] }

  /// The types of the instruction's results.
  public var types: [IRType] { [] }

  /// `true` iff the instruction denotes a call to a generic function.
  public var isGeneric: Bool {
    if let f = callee.constant as? FunctionReference {
      return !f.genericArguments.isEmpty
    } else {
      return false
    }
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallInstruction: CustomStringConvertible {

  public var description: String {
    "call \(callee)(\(list: arguments)) to \(output)"
  }

}

extension Module {

  /// Creates a `call` anchored at `site` that applies `callee` on `arguments` and writes its
  /// result to `output`.
  ///
  /// - Parameters:
  ///   - callee: The function to call.
  ///   - output: The location at which the result of `callee` is stored.
  ///   - arguments: The arguments of the call; one for each input of `callee`'s type.
  func makeCall(
    applying callee: Operand, to arguments: [Operand], writingResultTo output: Operand,
    at site: SourceRange
  ) -> CallInstruction {
    let calleeType = LambdaType(type(of: callee).ast)!.strippingEnvironment
    precondition(calleeType.inputs.count == arguments.count)
    precondition(isBorrowSet(output))

    return .init(callee: callee, output: output, arguments: arguments, site: site)
  }

  /// Returns `true` iff `o` is a `borrow [set]` instruction.
  fileprivate func isBorrowSet(_ o: Operand) -> Bool {
    guard
      let i = o.instruction,
      let s = self[i] as? BorrowInstruction
    else { return false }
    return s.capability == .set
  }

}
