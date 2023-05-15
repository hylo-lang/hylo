import Core

/// Stores `object` at the specified location.
public struct StoreInstruction: Instruction {

  /// The object to store.
  public private(set) var object: Operand

  /// The location at which the object is stored.
  public private(set) var target: Operand

  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(object: Operand, at target: Operand, site: SourceRange) {
    self.object = object
    self.target = target
    self.site = site
  }

  public var types: [LoweredType] { [] }

  public var operands: [Operand] { [object, target] }

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

  /// Creates a `record` anchored at `anchor` that stores `object` in `memory`.
  ///
  /// - Parameters:
  ///   - object: The object to store. Must have an object type.
  ///   - memory: The location at which `object` is stored. Must have an address type.
  func makeStore(
    _ object: Operand,
    at memory: Operand,
    anchoredAt anchor: SourceRange
  ) -> StoreInstruction {
    precondition(type(of: object).isObject)
    precondition(type(of: memory).isAddress)

    return StoreInstruction(object: object, at: memory, site: anchor)
  }

}
