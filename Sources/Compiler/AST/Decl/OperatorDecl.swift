/// An operator declaration.
public struct OperatorDecl: Decl {

  public static let kind = NodeKind.operatorDecl

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The notation of the operator.
  public let notation: SourceRepresentable<OperatorNotation>

  /// The name of the operator.
  public let name: SourceRepresentable<Identifier>

  /// The precedence group of the operator, if any.
  public let precedenceGroup: SourceRepresentable<PrecedenceGroup>?

  /// Creates an instance with the given properties and no `accessModifier`.
  public init(
    notation: SourceRepresentable<OperatorNotation>,
    name: SourceRepresentable<Identifier>,
    precedenceGroup: SourceRepresentable<PrecedenceGroup>?
  ) {
    self.notation = notation
    self.name = name
    self.precedenceGroup = precedenceGroup
  }

  /// Incorporates `accessModifier` into `self`.
  ///
  /// - Precondition: `self.accessModifier == nil`
  internal mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }

}
