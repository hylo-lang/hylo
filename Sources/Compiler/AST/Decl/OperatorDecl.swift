/// An operator declaration.
public struct OperatorDecl: Decl {

  public static let kind = NodeKind.operatorDecl

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The notation of the operator.
  public var notation: SourceRepresentable<OperatorNotation>

  /// The name of the operator.
  public var name: SourceRepresentable<Identifier>

  /// The precedence group of the operator, if any.
  public var precedenceGroup: SourceRepresentable<PrecedenceGroup>?

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    notation: SourceRepresentable<OperatorNotation>,
    name: SourceRepresentable<Identifier>,
    precedenceGroup: SourceRepresentable<PrecedenceGroup>? = nil
  ) {
    self.accessModifier = accessModifier
    self.notation = notation
    self.name = name
    self.precedenceGroup = precedenceGroup
  }

}
