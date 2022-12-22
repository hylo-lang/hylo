import Utils

/// A generic type parameter.
public struct GenericTypeParameterType: TypeProtocol {

  /// The declaration that introduces the parameter.
  ///
  /// - Note: The ID may denote the declaration of a generic type parameter or associated type.
  public let decl: AnyDeclID

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  public init(_ decl: NodeID<AssociatedTypeDecl>, ast: AST) {
    self.init(decl: AnyDeclID(decl), name: ast[decl].name)
  }

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  public init(_ decl: NodeID<GenericParameterDecl>, ast: AST) {
    self.init(decl: AnyDeclID(decl), name: ast[decl].name)
  }

  /// Creates an instance denoting the `Self` parameter of `trait`.
  public init(selfParameterOf trait: NodeID<TraitDecl>, in ast: AST) {
    self.init(ast[trait].selfParameterDecl, ast: ast)
  }

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  private init(decl: AnyDeclID, name: String) {
    self.decl = decl
    self.name = Incidental(name)
  }

  public var flags: TypeFlags { [.isCanonical, .hasGenericTypeParameter] }

}

extension GenericTypeParameterType: CustomStringConvertible {

  public var description: String { name.value }

}
