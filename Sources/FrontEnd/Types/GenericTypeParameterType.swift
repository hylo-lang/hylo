import Utils

/// A generic type parameter.
public struct GenericTypeParameterType: TypeProtocol {

  /// The declaration that introduces the parameter.
  public let decl: GenericParameterDecl.ID

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  public init(_ decl: GenericParameterDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  /// Creates an instance denoting the `Self` parameter of `trait`.
  public init(selfParameterOf trait: TraitDecl.ID, in ast: AST) {
    self.init(ast[trait].receiver, ast: ast)
  }

  public var flags: ValueFlags { .hasSkolem }

}

extension GenericTypeParameterType: CustomStringConvertible {

  public var description: String { name.value }

}
