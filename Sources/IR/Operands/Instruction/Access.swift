import Core

/// Creates a borrowed or consumed an access on an object.
///
/// IR generation sometimes cannot determine the capability used to access an object, as this
/// information is kept implicit in Val sources. For example, a `let`-binding declaration will
/// actually request a `sink` capability on its right hand side if the binding escapes. In these
/// cases, IR generation will emit `access` instructions with the set capabilities that may be
/// inferred from the syntax. These instructions are expected to be removed during IR analysis.
public struct Access: Instruction {

  /// The capabilities of the access.
  ///
  /// - Requires: Must be non-empty.
  public let capabilities: AccessEffectSet

  /// The type of the accessed type.
  public let accessedType: AnyType

  /// The location of the root object on which an access is borrowed or consumed.
  public private(set) var source: Operand

  /// The binding in source program to which the instruction corresponds, if any.
  public let binding: VarDecl.ID?

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    capabilities: AccessEffectSet,
    accessedType: AnyType,
    source: Operand,
    binding: VarDecl.ID?,
    site: SourceRange
  ) {
    self.capabilities = capabilities
    self.accessedType = accessedType
    self.source = source
    self.binding = binding
    self.site = site
  }

  public var types: [IR.`Type`] { [.address(accessedType)] }

  public var operands: [Operand] { [source] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension Access: CustomStringConvertible {

  public var description: String {
    "access \(capabilities) \(source)"
  }

}

extension Module {

  /// Creates an `access` anchored at `site` that takes `capability` from `source`.
  ///
  /// - Parameters:
  ///   - capabilities: The capability being borrowed. Must not be empty.
  ///   - source: The address from which the capability is borrowed. Must have an address type.
  ///   - binding: The declaration of the binding to which the borrow corresponds, if any.
  func makeAccess(
    _ capabilities: AccessEffectSet, from source: Operand,
    correspondingTo binding: VarDecl.ID? = nil,
    at site: SourceRange
  ) -> Access {
    precondition(!capabilities.isEmpty)
    precondition(type(of: source).isAddress)
    return .init(
      capabilities: capabilities,
      accessedType: type(of: source).ast,
      source: source,
      binding: binding,
      site: site)
  }

}
