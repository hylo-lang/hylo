import FrontEnd

/// Initializes or assigns storage with a value.
public struct Move: Instruction, Sendable {

  /// The value moved into `target`.
  public private(set) var object: Operand

  /// The location to initialize or assign.
  public private(set) var target: Operand

  /// The conformance of `target`'s type to `Movable` implementing its move operators.
  public let movable: FrontEnd.Conformance

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    object: Operand, target: Operand, movable: FrontEnd.Conformance, site: SourceRange
  ) {
    self.object = object
    self.target = target
    self.movable = movable
    self.site = site
  }

  public var operands: [Operand] {
    [object, target]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0: object = new
    case 1: target = new
    default:
      preconditionFailure()
    }
  }

}

extension Module {

  /// Creates a `move` anchored at `site` that moves `value` into `storage` using the move
  /// operations defined by `movable`.
  ///
  /// This instruction is replaced during IR transformation by either the initialization or
  /// assignment of `storage`, depending on its initialization state.
  ///
  /// - Parameters:
  ///   - value: The object to move. Must have an address type.
  ///   - storage: The location to initialize or assign. Must have an address type.
  func makeMove(
    _ value: Operand, to storage: Operand, usingConformance movable: FrontEnd.Conformance,
    at site: SourceRange
  ) -> Move {
    precondition(type(of: value).isAddress)
    precondition(type(of: storage).isAddress)
    return .init(object: value, target: storage, movable: movable, site: site)
  }

}
