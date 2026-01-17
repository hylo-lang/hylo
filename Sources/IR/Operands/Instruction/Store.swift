import FrontEnd

/// Stores `object` at the specified location.
public struct Store: Instruction {

  /// The object to store.
  public private(set) var object: Operand

  /// The location at which the object is stored.
  public private(set) var target: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(object: Operand, at target: Operand, site: SourceRange) {
    self.object = object
    self.target = target
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

  /// Creates a `record` anchored at `site` that stores `object` at `target.
  ///
  /// - Parameters:
  ///   - object: The object to store. Must have an object type.
  ///   - target: The location at which `object` is stored. Must have an address type.
  func makeStore(_ object: Operand, at target: Operand, in f: Function.ID, at site: SourceRange) -> Store {
    precondition(self[f].type(of: object).isObject)
    precondition(self[f].type(of: target).isPlace)
    return .init(object: object, at: target, site: site)
  }

}
