import Core

/// Projects a value.
public struct Project: Instruction {

  /// The type of the projected value.
  public let projection: RemoteType

  /// The subscript implementing the projection.
  public let callee: Function.ID

  /// If `callee` is generic, the arguments to its generic parameter.
  public let parameterization: GenericArguments

  /// The arguments of the call.
  ///
  /// Operands to non-`sink` inputs must be the result of a `borrow` instruction requesting the
  /// same capability as `projection.access` and having no use before `project`.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    projection: RemoteType,
    callee: Function.ID,
    parameterization: GenericArguments,
    operands: [Operand],
    site: SourceRange
  ) {
    self.projection = projection
    self.callee = callee
    self.parameterization = parameterization
    self.operands = operands
    self.site = site
  }

  /// The types of the instruction's results.
  public var types: [IR.`Type`] {
    [.address(projection.bareType)]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension Project: CustomStringConvertible {

  public var description: String {
    if operands.isEmpty {
      return "project \(callee)"
    } else {
      return "project \(callee), \(list: operands)"
    }
  }

}

extension Module {

  /// Creates a `project` anchored at `site` that projects a value of type `t` by applying
  /// `callee`, which is parameterized by `parameterization`, on `arguments`.
  func makeProject(
    _ t: RemoteType,
    applying callee: Function.ID,
    parameterizedBy parameterization: GenericArguments,
    to arguments: [Operand],
    at site: SourceRange
  ) -> Project {
    .init(
      projection: t,
      callee: callee,
      parameterization: parameterization,
      operands: arguments,
      site: site)
  }

}
