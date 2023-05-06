import Core

/// Projects a value from a subscript bundle.
public struct ProjectBundleInstruction: Instruction {

  /// The type of the projected value.
  public let projection: AnyType

  /// The subscript bundle implementing the projections.
  public let callee: SubscriptDecl.ID

  /// The arguments of the call.
  ///
  /// In refined IR, operands to non-`sink` inputs must be the result of a `borrow` instruction and
  /// have no use before `project`.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    projection: AnyType,
    callee: SubscriptDecl.ID,
    operands: [Operand],
    site: SourceRange
  ) {
    self.projection = projection
    self.callee = callee
    self.operands = operands
    self.site = site
  }

  /// The types of the instruction's results.
  public var types: [LoweredType] { [.address(projection)] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension ProjectBundleInstruction: CustomStringConvertible {

  public var description: String {
    if operands.isEmpty {
      return "project_bundle \(callee)"
    } else {
      return "project_bundle \(callee), \(list: operands)"
    }
  }

}

extension Module {

  /// Creates a `project_bundle` anchored at `anchor` that takes applies subscript `s` on
  /// `arguments` to project a value of type `t`.
  func makeProjectBundle(
    _ t: AnyType,
    applying s: SubscriptDecl.ID,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> ProjectBundleInstruction {
    .init(projection: t, callee: s, operands: arguments, site: anchor)
  }

}
