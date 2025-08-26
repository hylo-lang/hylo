import FrontEnd
import Utils

/// Projects a value.
public struct Project: RegionEntry {

  public typealias Exit = EndProject

  /// The type of the projected value.
  public let projection: RemoteType

  /// The callee + arguments of the call.
  ///
  /// Operands to non-`sink` inputs must be the result of a `borrow` instruction requesting the
  /// same capability as `projection.access` and having no use before `project`.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// The callee.
  public var callee: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  /// The function reference to the callee.
  public var functionReference: FunctionReference {
    guard case .constant(let c) = callee else { unreachable() }
    return c as! FunctionReference
  }

  /// Creates an instance with the given properties.
  fileprivate init(
    projection: RemoteType,
    callee: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.projection = projection
    self.operands = [callee] + arguments
    self.site = site
  }

  /// The types of the instruction's results.
  public var result: IR.`Type`? {
    .address(projection.bareType)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension Project: CustomStringConvertible {

  public var description: String {
    return "project \(callee)(\(list: arguments))"
  }

}

extension Function {

  /// Creates a `project` anchored at `site` that projects a value of type `t` by applying `s`,
  /// which is a reference to a lowered subscript, on `arguments`.
  func makeProject(
    _ t: RemoteType, applying s: FunctionReference, to arguments: [Operand], at site: SourceRange
  ) -> Project {
    .init(projection: t, callee: .constant(s), arguments: arguments, site: site)
  }

  /// Creates a `project` anchored at `site` that projects a value of type `t` by applying `s`,
  /// which is a reference to a lowered subscript, on `arguments`.
  func makeProject(
    _ t: RemoteType, applying s: Operand, to arguments: [Operand], at site: SourceRange
  ) -> Project {
    .init(projection: t, callee: s, arguments: arguments, site: site)
  }

  /// Creates a `project` anchored at `site` that projects a value of type `t` by applying `s`,
  /// which is a reference to a lowered subscript, on `arguments`, inserting it at `p`.
  mutating func makeProject(
    _ t: RemoteType, applying s: FunctionReference, to arguments: [Operand], at site: SourceRange,
    insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(makeProject(t, applying: s, to: arguments, at: site), at: p)
  }

  /// Creates a `project` anchored at `site` that projects a value of type `t` by applying `s`,
  /// which is a reference to a lowered subscript, on `arguments`, inserting it at `p`.
  mutating func makeProject(
    _ t: RemoteType, applying s: Operand, to arguments: [Operand], at site: SourceRange,
    insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(makeProject(t, applying: s, to: arguments, at: site), at: p)
  }

}
