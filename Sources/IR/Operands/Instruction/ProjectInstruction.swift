import Core

/// Projects a value.
public struct ProjectInstruction: Instruction {

  /// The type of the projected value.
  public let projection: RemoteType

  /// The subscript performing the projection.
  ///
  /// Subscripts, unlike functions, are not first-class values and are therefore not represented as
  /// IR operands.
  public let callee: Function.ID

  /// The arguments of the call.
  ///
  /// Operands to non-`sink` inputs must be the result of a `borrow` instruction and have no use
  /// before `project`.
  public let operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    projection: RemoteType,
    callee: Function.ID,
    operands: [Operand],
    site: SourceRange
  ) {
    self.projection = projection
    self.callee = callee
    self.operands = operands
    self.site = site
  }

  /// The types of the instruction's results.
  public var types: [LoweredType] { [.address(projection.bareType)] }

}

extension ProjectInstruction: CustomStringConvertible {

  public var description: String {
    if operands.isEmpty {
      return "project \(callee)"
    } else {
      return "project \(callee), \(list: operands)"
    }
  }

}

extension Module {

  /// Creates a `project` anchored at `anchor` that takes applies subscript `s` on `arguments` to
  /// project a value of type `t`.
  ///
  /// - Parameters:
  ///   - source: The address from which the capability is borrowed. Must have an address type.
  ///   - binding: The declaration of the binding to which the borrow corresponds, if any.
  func makeProject(
    _ t: RemoteType,
    applying s: Function.ID,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> ProjectInstruction {
    .init(projection: t, callee: s, operands: arguments, site: anchor)
  }

}
