import Utils

/// A generic type parameter.
public struct GenericTermParameter: TermProtocol {

  /// The declaration that introduces the parameter.
  public let decl: GenericParameterDecl.ID

  /// The name of the parameter.
  public let name: Incidental<String>

  /// Creates an instance denoting the generic type parameter declared by `decl`.
  public init(_ decl: GenericParameterDecl.ID, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].baseName)
  }

  public var flags: ValueFlags { .hasSkolem }

}

extension GenericTermParameter: CustomStringConvertible {

  public var description: String { name.value }

}
