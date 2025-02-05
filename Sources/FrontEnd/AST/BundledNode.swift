/// A projection from a `Program` of a node along with its extrinsic relationships.
@dynamicMemberLookup
public struct BundledNode<T: NodeIDProtocol, P: Program> {

  /// The program of which this node is a notional part.
  private let container: P

  /// The node's identity in `container.ast`.
  public let id: T

  /// Creates an instance bundling `container` with `id`.
  public init(_ id: T, in container: P) {
    self.container = container
    self.id = id
  }

  /// The site from which self was parsed.
  public var site: SourceRange {
    container.ast[id].site
  }

  /// The scope in which the node resides.
  ///
  /// - Requires: `self` does not identify a module.
  public var scope: AnyScopeID {
    container.nodeToScope[id]!
  }

}

extension BundledNode where T: ConcreteNodeID {

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target>(dynamicMember m: KeyPath<T.Subject, Target>) -> Target {
    container.ast[id][keyPath: m]
  }

  /// Accesses `m` in the corresponding AST node.
  public subscript<Target: NodeIDProtocol>(
    dynamicMember m: KeyPath<T.Subject, Target>
  ) -> BundledNode<Target, P> {
    .init(container.ast[id][keyPath: m], in: container)
  }

}

extension BundledNode where T: ScopeID {

  /// The declarations in this immediate scope.
  public var decls: DeclIDs {
    container.scopeToDecls[id, default: .init()]
  }

}

extension BundledNode where T == VarDecl.ID {

  /// The binding declaration containing this variable declaration.
  public var binding: BindingDecl.ID {
    container.varToBinding[id]!
  }

}

extension BundledNode: Hashable, Sendable {

  /// Equality comparison; only check the node ID.
  public static func == (lhs: Self, rhs: Self) -> Bool {
    lhs.id == rhs.id
  }

  /// Hashes the value of `id` into `hasher`.
  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }
}

extension BundledNode: CustomStringConvertible {

  public var description: String {
    String(site.text)
  }

}

extension BundledNode where T: DeclID, P == TypedProgram {

  /// The type of the declared entity.
  public var type: AnyType {
    container.declType[id]!
  }

}

extension BundledNode where T: ConcreteNodeID, T.Subject: CapturingDecl, P == TypedProgram {

  /// The implicit captures for the declared entity.
  public var implicitCaptures: [ImplicitCapture] {
    container.implicitCaptures[AnyDeclID(id)!, default: []]
  }

}

extension BundledNode where T: ExprID, P == TypedProgram {

  /// The type of this expression.
  public var type: AnyType {
    container.exprType[id]!
  }

}

extension BundledNode where T == NameExpr.ID, P == TypedProgram {

  /// The declaration referenced by this expression.
  public var referredDecl: DeclReference {
    container.referredDecl[id]!
  }

}

extension BundledNode where T == SequenceExpr.ID, P == TypedProgram {

  /// A representation of `self` that encodes its evaluation order.
  public var folded: FoldedSequenceExpr {
    container.foldedForm[id]!
  }

}

extension BundledNode where T == FunctionDecl.ID, P == TypedProgram {

  /// The attributes of this function.
  public var attributes: FunctionAttributes {
    container.functionAttributes[id]!
  }

}
