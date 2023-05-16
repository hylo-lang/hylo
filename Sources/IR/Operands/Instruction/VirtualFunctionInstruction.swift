import Core

/// Returns the implementation of a requirement specified by a witness table.
public struct VirtualFunctionInstruction: Instruction {

  /// The type of the virtual function.
  public let interface: LambdaType

  /// The the requirement representing the virtual function.
  public private(set) var requirement: Requirement

  /// The witness table being read to identify `requirement`'s implementation.
  public private(set) var source: Operand

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with given properties.
  fileprivate init(
    interface: LambdaType, requirement: Requirement, source: Operand, site: SourceRange
  ) {
    self.interface = interface
    self.requirement = requirement
    self.source = source
    self.site = site
  }

  public var types: [LoweredType] { [.address(interface)] }

  public var operands: [Operand] { [.constant(requirement), source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    switch i {
    case 0:
      guard let r = new.constant as? Requirement else {
        preconditionFailure("invalid substitution")
      }
      requirement = r
    case 1:
      source = new
    default:
      preconditionFailure()
    }
  }

}

extension Module {

  /// Creates a `virtual_function` anchored at `anchor` that returns the address of the function
  /// of type `interface` that implements `requirement` as defined in `source`, which is a witness
  /// table.
  ///
  /// - Parameters:
  ///   - interface: The type of the virtual function.
  ///   - requirement: The ID of a function requirement.
  ///   - source: The address of a virtual table specifying how `requirement` is implemented.
  func makeVirtualFunction(
    _ interface: LambdaType,
    implementing requirement: Requirement,
    in source: Operand,
    anchoredAt anchor: SourceRange
  ) -> VirtualFunctionInstruction {
    precondition(interface[.isCanonical])
    return .init(interface: interface, requirement: requirement, source: source, site: anchor)
  }

}
