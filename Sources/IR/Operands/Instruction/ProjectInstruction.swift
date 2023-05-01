import Core

/// Projects a value.
public struct ProjectInstruction: Instruction {

  /// The capability of the projected value.
  public let capability: AccessEffect

  /// The type of the projected value.
  public let projectionType: AnyType

  /// The callee and arguments of the call.
  public let operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    capability: AccessEffect,
    projectionType: AnyType,
    callee: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.capability = capability
    self.projectionType = projectionType
    self.operands = [callee] + arguments
    self.site = site
  }

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  /// The types of the instruction's results.
  public var types: [LoweredType] { [.address(projectionType)] }

}

extension ProjectInstruction: CustomStringConvertible {

  public var description: String {
    "project [\(capability)] \(list: operands)"
  }

}

extension Module {

  /// Creates a `project` anchored at `anchor` that takes applies subscript `s` on `arguments` to
  /// project a value of type `t` with capability `c`.
  ///
  /// - Parameters:
  ///   - capability: The capability being borrowed. Must be `.let`, `.inout`, or `.set`.
  ///   - source: The address from which the capability is borrowed. Must have an address type.
  ///   - binding: The declaration of the binding to which the borrow corresponds, if any.
  func makeProject(
    _ c: AccessEffect,
    _ t: AnyType,
    applying s: Operand,
    to arguments: [Operand],
    anchoredAt anchor: SourceRange
  ) -> ProjectInstruction {
    .init(capability: c, projectionType: t, callee: s, arguments: arguments, site: anchor)
  }

}
