import Utils

/// A trait type.
public struct TraitType: TypeProtocol, Hashable {

  /// The declaration that introduces the trait.
  public let decl: NodeID<TraitDecl>

  /// The name of the trait.
  public let name: Incidental<String>

  public let flags: TypeFlags = .isCanonical

  public init(decl: NodeID<TraitDecl>, ast: AST) {
    self.decl = decl
    self.name = Incidental(ast[decl].name)
  }

}

extension TraitType {

  /// Returns the trait named `name`, declared in `ast.stdlib`.
  public init?(named name: String, ast: AST) {
    guard let stdlib = ast.stdlib else { return nil }
    for id in ast.topLevelDecls(stdlib) where id.kind == .traitDecl {
      let id = NodeID<TraitDecl>(converting: id)!
      if ast[id].name == name {
        self.init(decl: id, ast: ast)
        return
      }
    }
    return nil
  }

}

extension TraitType: CustomStringConvertible {

  public var description: String { name.value }

}
