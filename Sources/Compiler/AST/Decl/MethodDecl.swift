/// A method declaration.
public struct MethodDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.methodDecl

  /// The source range of the `fun` introducer, if any.
  public let introducerRange: SourceRange?

  /// The attributes of the declaration, if any.
  public private(set) var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The operator notation of the method.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the method.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the method, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The parameters of the method.
  public let parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the method, if any.
  public let output: AnyTypeExprID?

  /// The implementations of the method.
  public let impls: [NodeID<MethodImplDecl>]

  public init(
    introducerRange: SourceRange?,
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    parameters: [NodeID<ParameterDecl>] = [],
    output: AnyTypeExprID? = nil,
    impls: [NodeID<MethodImplDecl>] = []
  ) {
    self.introducerRange = introducerRange
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Incorporates the given decorations into `self`.
  ///
  /// - Requires: `self` is undecorated.
  internal mutating func incorporate(
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    memberModifier: SourceRepresentable<MemberModifier>?
  ) {
    precondition(self.attributes.isEmpty)
    precondition(self.accessModifier == nil)
    self.attributes = attributes
    self.accessModifier = accessModifier
  }

}
