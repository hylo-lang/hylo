import Core

/// Creates a borrowed or consumed an access on an object.
public struct AccessInstruction: Instruction {

  /// The capabilities of the access.
  public let capabilities: AccessEffectSet

  /// The type of the accessed type.
  public let accessedType: AnyType

  /// The location of the root object on which an access is borrowed or consumed.
  public private(set) var source: Operand

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: VarDecl.Typed?

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    capabilities: AccessEffectSet,
    accessedType: AnyType,
    source: Operand,
    binding: VarDecl.Typed?,
    site: SourceRange
  ) {
    self.capabilities = capabilities
    self.accessedType = accessedType
    self.source = source
    self.binding = binding
    self.site = site
  }

  public var types: [LoweredType] { [.address(accessedType)] }

  public var operands: [Operand] { [source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension AccessInstruction: CustomStringConvertible {

  public var description: String {
    "access \(capabilities) \(source)"
  }

}

extension Module {

  /// Creates a `borrow` anchored at `anchor` that takes `capability` from `source`.
  ///
  /// - Parameters:
  ///   - capabilities: The capability being borrowed. Must not be empty.
  ///   - source: The address from which the capability is borrowed. Must have an address type.
  ///   - binding: The declaration of the binding to which the borrow corresponds, if any.
  func makeAccess(
    _ capabilities: AccessEffectSet,
    from source: Operand,
    correspondingTo binding: VarDecl.Typed? = nil,
    anchoredAt anchor: SourceRange
  ) -> AccessInstruction {
    precondition(!capabilities.isEmpty)
    precondition(type(of: source).isAddress)

    return .init(
      capabilities: capabilities,
      accessedType: type(of: source).ast,
      source: source,
      binding: binding,
      site: anchor)
  }

}
